# run from the int_machine
all: data/seattle_subreddit_submissions.feather plots/subreddit_posts_timeseries data/included_timeseries.feather data/var_stan_data.pickle data/stan_models data/stan_models/post2016 data/var_stan_data_post2016.pickle

data/seattle_subreddit_submissions.feather: pull_seattle_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"

data/var_stan_data.pickle: seasonality.py data/mlb/ data/mls.csv data/ncaa data/included_timeseries.feather build_var_data.py seasonality.py var_data.py
	source ./bin/activate && python3 build_var_data.py

data/var_stan_data_post2016.pickle: seasonality.py data/mlb data/mls.csv data/ncaa data/included_timeseries_post2016.feather build_var_data.py var_data.py
	source ./bin/activate && python3 build_var_data.py --indata "data/included_timeseries_post2016.feather" --output "data/var_stan_data_post2016.pickle" --min_date='2016-10-01' --max_date='2019-10-31' --forecast_date='2020-04-30'

data/included_timeseries.feather: choose_subreddits.R data/seattle_subreddit_submissions.feather
	Rscript choose_subreddits.R

data/included_timeseries_post2016.feather: choose_post2016_subreddits.R data/seattle_subreddit_submissions.feather
	Rscript choose_post2016_subreddits.R

plots/subreddit_posts_timeseries/: data/seattle_subreddit_submissions.feather
	Rscript describe_subreddit_activity.R

clean_intermediate:
	rm data/seattle_subreddit_submissions.feather
	rm data/included_timeseries.feather
	rm plots/subreddit_posts_timeseries/*

stan_code/heaps_poisson_seasonality.pkl: stan_code/heaps_poisson_seasonality.stan compile_model.py
	source ./bin/activate && srun -p comdata -A comdata --time=2:00:00 --pty compile_model.py 

data/stan_models:stan_code/heaps_poisson_seasonality.pkl data/var_stan_data.pickle
	srun -p comdata -A comdata --mem=120g --time=400:00:00 --pty run_var_jobs.sh var_jobs_task_list 7

data/stan_models/post2016:stan_code/heaps_poisson_seasonality.pkl data/var_stan_data_post2016.pickle
	srun -p comdata -A comdata --mem=120g --time=400:00:00 --pty run_var_jobs.sh var_jobs_task_list_post2016 6
