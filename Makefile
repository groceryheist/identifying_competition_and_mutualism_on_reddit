all: overleaf/main.pdf

overleaf/main.pdf:remember_overlaps.R
	cp remember_overlaps.R overleaf/resources
	$(MAKE) -C overleaf

remember_overlaps.RDS:ols_models/done analyze_cluster_vars.R
	Rscript analyze_cluster_vars.R

ols_models/done:var_jobs.sh var_ols.R
	rm ols_models/*.RDS* && \
	./monitor_memory.sh & \
	parallel --jobs 28 < var_jobs.sh && \
	touch ols_models/done

var_jobs.sh: fit_var_on_clusters.R data/subreddit_timeseries_authortf.parquet/_SUCCESS
	Rscript $<

data/subreddit_timeseries_authortf.parquet/_SUCCESS : cluster_timeseries.py choose_clusters.py data/reddit_comments_by_subreddit.parquet data/reddit_density/comment_terms_10000.feather  data/reddit_clustering/comment_terms_10000.feather data/reddit_clustering/best_author-tf.feather data/reddit_comments_by_subreddit.parquet

	python3 cluster_timeseries.py --term-clusters-path=data/reddit_clustering/comment_terms_10000.feather --author-clusters-path=data/reddit_clustering/best_author-tf.feather ---term-densities-path=data/reddit_density/comment_terms_10000.feather --author-densities-path=data/reddit_density/comment_authors_10000.feather --output=data/subreddit_timeseries_authortf.parquet

data/reddit_comments_by_subreddit.parquet:
	$(MAKE) -C cdsc_reddit/datasets

data/reddit_clustering/comment_terms_10000.feather:
	$(MAKE) -C cdsc_reddit/clustering

data/reddit_clustering/best_author-tf.feather:
	$(MAKE) -C cdsc_reddit/clustering

data/reddit_similarity/subreddit_authors-tf_similarities_10k_LSI/600.feather:
	$(MAKE) -C cdsc_reddit/similarity

data/reddit_density/comment_terms_10000.feather:
	$(MAKE) -c cdsc_reddit/reddit_density

data/reddit_density/comment_authors_10000.feather:
	$(MAKE) -c cdsc_reddit/reddit_density

