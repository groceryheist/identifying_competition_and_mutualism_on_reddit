# run from the int_machine
all: data/seattle_subreddit_submissions.feather plots/subreddit_posts_timeseries data/included_timeseries.feather data/var_stan_data.pickle data/stan_models data/stan_models/post2016 data/var_stan_data_post2016.pickle

data/seattle_subreddit_submissions.feather: pull_seattle_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"

data/whatsthis_subreddit_submissions.feather: pull_whatsthis_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling whatsthis subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_whatsthis_subs.py; \
	echo "Done"

data/wallpaper_subreddit_submissions.feather: pull_wallpaper_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling wallpaper subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_wallpaper_subs.py; \
	echo "Done"

data/design_subreddit_submissions.feather: pull_design_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling design subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_design_subs.py; \
	echo "Done"


# min date for whatsthis? 

# Add arguments to build_var_data.py instead of having a bunch of different scripts
data/var_design_data.pickle: build_var_data.py var_data.py seasonality.py data/included_design.feather
	source ./bin/activate && python3 build_var_data.py --indata data/included_design.feather --output data/var_design_data.pickle --min-date '2014-09-15' --max-date '2018-04-02' --forecast_date '2019-08-31'

data/var_whatsthis_data.pickle: build_var_data.py var_data.py seasonality.py data/whatsthis_included.feather
	source ./bin/activate && python3 build_var_data.py --indata data/whatsthis_included.feather --output data/var_whatsthis_data.pickle --min-date '2013-10-31' --max-date '2019-10-31' --forecast_date '2020-04-30' --include-all-reddit='False'

data/var_stan_data.pickle: seasonality.py data/mlb/ data/mls.csv data/ncaa data/included_timeseries.feather build_var_data.py seasonality.py var_data.py 
	source ./bin/activate && python3 build_var_data.py

data/var_stan_data_post2016.pickle: seasonality.py data/mlb data/mls.csv data/ncaa data/included_timeseries_post2016.feather build_var_data.py var_data.py
	source ./bin/activate && python3 build_var_data.py --indata "data/included_timeseries_post2016.feather" --output "data/var_stan_data_post2016.pickle" --min_date='2016-10-01' --max_date='2019-10-31' --forecast_date='2020-04-30'

data/var_wallpaper_data.pickle: data/included_wallpaper.feather build_var_data.py var_data.py
	source ./bin/activate && python3 build_var_data.py --indata "data/included_wallpaper.feather" --output "data/var_wallpaper_data.pickle" --min_date='2014-11-01' --max_date='2019-10-31' --forecast_date='2020-04-30' --include-all-reddit='False'

data/var_wallpaper_comments.pickle: data/included_wallpaper_comments.feather build_var_data.py var_data.py
	source ./bin/activate && python3 build_var_data.py --indata "data/included_wallpaper_comments.feather" --output "data/var_wallpaper_comments.pickle" --min_date='2014-10-01' --max_date='2019-07-31' --forecast_date='2019-11-01' --include-all-reddit='False'

data/included_design.feather: choose_design_subreddits.R data/design_subreddit_submissions.feather helper.R
	Rscript choose_design_subreddits.R

data/included_wallpaper.feather: choose_wallpaper_subreddits.R data/wallpaper_subreddit_submissions.feather helper.R
	Rscript choose_wallpaper_subreddits.R

data/included_wallpaper_comments.feather: choose_wallpaper_subreddits.R data/wallpaper_subreddit_comments.feather helper.R
	Rscript choose_wallpaper_subreddits.R

data/whatsthis_included.feather: choose_whatsthis_subreddits.R data/whatsthis_subreddit_submissions.feather helper.R
	Rscript choose_whatsthis_subreddits.R

data/included_timeseries.feather: choose_subreddits.R data/seattle_subreddit_submissions.feather helper.R
	Rscript choose_subreddits.R

data/included_timeseries_post2016.feather: choose_post2016_subreddits.R data/seattle_subreddit_submissions.feather
	Rscript choose_post2016_subreddits.R

data/simulation_stan_data.pickle: simulate_var_data.py util.py
	python3 simulate_var_data.py

data/large_simulation_stan_data.pickle: simulate_var_data.py util.py
	python3 simulate_var_data.py

stan_models/simulated_stanmod.feather: fit_var.py data/simulation_stan_data.pickle
	./fit_var.py -p 1 --infile data/simulation_stan_data.pickle --output simulated --adapt-delta 0.9 &> stan_logs/var_simulated.log

stan_models/large_simulated_stanmod.feather: fit_var.py data/large_simulation_stan_data.pickle
	./fit_var.py -p 1 --infile data/large_simulation_stan_data.pickle --output large_simulated --adapt-delta 0.97 --max-tree-depth 12 --it 4000 &> stan_logs/var_large_simulated.log

plots/subreddit_posts_timeseries/: data/seattle_subreddit_submissions.feather
	Rscript describe_subreddit_activity.R

clean_intermediate:
	rm data/seattle_subreddit_submissions.feather
	rm data/included_timeseries.feather
	rm plots/subreddit_posts_timeseries/*

stan_code/heaps_poisson_seasonality.pkl: stan_code/heaps_poisson_seasonality.stan compile_model.py
	source ./bin/activate && srun -p comdata -A comdata --time=2:00:00 --pty compile_model.py 

stan_models:stan_code/heaps_poisson_seasonality.pkl data/var_stan_data.pickle
	srun -p comdata -A comdata --mem=120g --time=400:00:00 --pty run_var_jobs.sh var_jobs_task_list 7

stan_models/whatsthis/_Success_2:stan_code/heaps_poisson_seasonality.pkl data/var_whatsthis_data.pickle data/var_whatsthis_data.pickle
	source ./bin/activate; \
	./fit_var.py -p 1 --infile data/var_whatsthis_data.pickle --output whatsthis_2 --adapt-delta 0.97 --max-tree-depth 12 --it 4000 --seed=1774 &> stan_logs/var_whatsthis_2.log; \
	touch stan_models/whatsthis/_Success_2

stan_models/var_stan_p1_stanmod.feather: stan_code/heaps_poisson_seasonality.pkl fit_var.py
	source ./bin/activate; \
	./fit_var.py -p 1 --infile data/var_stan_data.pickle --output p1_stanmod --adapt-delta 0.95 --max-tree-depth 12 --it 4000 --seed=1775 &> stan_logs/var_seattle.log; \

stan_models/design_stanmod.feather:stan_code/heaps_poisson_seasonality.pkl data/var_design_data.pickle fit_var.py
	source ./bin/activate; \
	./fit_var.py -p 1 --infile data/var_design_data.pickle --output design --adapt-delta 0.95 --max-tree-depth 12 --it 4000 --seed=1775 &> stan_logs/var_design.log; \
	touch stan_models/design/_Success

remember_design:stan_models/design_stanmod.feather stan_models/design_stanmod.feather analyze_design.R model_interpretation.R
	Rscript analyze_design.R

stan_models/post2016:stan_code/heaps_poisson_seasonality.pkl data/var_stan_data_post2016.pickle
	srun -p comdata -A comdata --mem=120g --time=400:00:00 --pty run_var_jobs.sh var_jobs_task_list_post2016 6

stan_models/wallpaper/_Success:data/var_wallpaper_data.pickle fit_var.py
	./fit_var.py -p 1 --infile data/var_wallpaper_data.pickle --output wallpaper/var_p1_all  --adapt-delta 0.9 --max-treedepth 12 &> stan_logs/var_wallpaper_stan_p1.log
	touch stan_models/wallpaper/_Success
	# srun -p comdata -A comdata --mem=120g --time=200:00:00 --pty run_var_jobs.sh wallpaper_var_jobs 7; \
	# touch stan_models/wallpaper_comments/_Success

stan_models/wallpaper/_Try2:data/var_wallpaper_comments.pickle data/var_wallpaper_data.pickle fit_var.py
	srun -p comdata -A comdata --mem=120g --time=200:00:00 --pty run_var_jobs.sh wallpaper_var_jobs_refit 7; \
	touch stan_models/wallpaper_comments/_Try2

remember_simulation:stan_models/simulated_stanmod.feather analyze_simulation.R model_interpretation.R
	Rscript analyze_simulation.R

remember_large_simulation:stan_models/large_simulated_stanmod.feather analyze_simulation.R model_interpretation.R
	Rscript analyze_large_simulation.R


remember_wallpaper:stan_models/wallpaper/_Success analyze_wallpaper.R model_interpretation.R
	Rscript analyze_wallpaper.R

remember_seattle:stan_models/var_stan_p1_stanmod.feather analyze_seattle.R model_interpretation.R
	Rscript analyze_seattle.R



PHONY:remember_simulation clean_intermediate remember_walpaper remember_seattle remember_design
