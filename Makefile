all: data/seattle_subreddits.feather


data/seattle_subreddits.feather: pull_seattle_subs.py
	echo "Pulling subreddit histories from parquet"; \
	source ./bin/activate; \
	python3 pull_seattle_subs.py; \
	echo "Done"


