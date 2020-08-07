# run from the int_machine
all: data/seattle_subreddit_submissions.feather plots/subreddit_posts_timeseries data/included_timeseries.feather data/var_stan_data.pickle data/stan_models

data/seattle_subreddit_submissions.feather: pull_seattle_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"

data/var_stan_data.pickle: seasonality.py data/mlb/ data/mls.csv data/ncaa data/included_timeseries.feather build_var_data.py seasonality.py var_data.py
	source ./bin/activate && python3 build_var_data.py

data/included_timeseries.feather: choose_subreddits.R data/seattle_subreddit_submissions.feather
	Rscript choose_subreddits.R

plots/subreddit_posts_timeseries/: data/seattle_subreddit_submissions.feather
	Rscript describe_subreddit_activity.R

clean_intermediate:
	rm data/seattle_subreddit_submissions.feather
	rm data/included_timeseries.feather
	rm plots/subreddit_posts_timeseries/*

stan_code/heaps_poisson_seasonality.pkl: stan_code/heaps_poisson_seasonality.stan compile_model.py
	source ./bin/activate && srun -p comdata -A comdata --time=2:00:00 --pty compile_model.py 

data/stan_models:stan_code/heaps_poisson_seasonality.pkl data/var_stan_data.pickle
	srun -p comdata -A comdata --mem=120g --time=400:00:00 --pty run_var_jobs.sh 
