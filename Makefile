all: data/seattle_subreddit_submissions.feather plots/subreddit_posts_timeseries

data/seattle_subreddit_submissions.feather: pull_seattle_subs.py comdata_output/reddit_submissions_by_subreddit.parquet
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"

plots/subreddit_posts_timeseries: data/seattle_subreddit_submissions.feather
	Rscript describe_subreddit_activity.R


clean_intermediate:
	rm data/seattle_subreddit_submissions.feather
	rm plots/subreddit_posts_timeseries/*
