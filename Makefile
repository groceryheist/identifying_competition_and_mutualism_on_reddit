all: data/seattle_subreddit_submissions.feather plots/subreddit_posts_timeseries data/seasonality_levels.feather data/included_timeseries.feather

data/seattle_subreddit_submissions.feather: pull_seattle_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"

data/seasonality_levels.feather: seasonality.py
	source ./bin/activate && python3 seasonality.py

data/included_timeseries.feather: choose_subreddits.R data/seattle_subreddit_submissions.feather
	Rscript choose_subreddits.R

plots/subreddit_posts_timeseries: data/seattle_subreddit_submissions.feather
	Rscript describe_subreddit_activity.R

clean_intermediate:
	rm data/seattle_subreddit_submissions.feather
	rm data/included_timeseries.feather
	rm plots/subreddit_posts_timeseries/*
