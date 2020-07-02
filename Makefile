all: seattle_subreddits.feather


data/seattle_subreddits.feather:
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"


