all: remember_overlaps.RDS

remember_overlaps.RDS:ols_models/done analyze_cluster_vars.R
	Rscript analyze_cluster_vars.R

ols_models/done:var_jobs.sh var_ols.R
	rm ols_models/*.RDS* && \
	./monitor_memory.sh & \
	parallel --jobs 28 < var_jobs.sh && \
	touch ols_models/done

var_jobs.sh: fit_var_on_clusters.R data/subreddit_timeseries_authortf.parquet/_SUCCESS
	Rscript $<

data/subreddit_timeseries.parquet: cluster_timeseries.py choose_clusters.py /gscratch/comdata/output/reddit_comments_by_subreddit.parquet /gscratch/comdata/output/reddit_density/comment_terms_10000.feather /gscratch/comdata/output/reddit_density/comment_authors_10000.feather /gscratch/comdata/output/reddit_clustering/comment_terms_10000.feather /gscratch/comdata/output/reddit_clustering/comment_authors_10000.feather

	python3 cluster_timeseries.py

data/subreddit_timeseries_authortf.parquet/_SUCCESS : cluster_timeseries.py choose_clusters.py /gscratch/comdata/output/reddit_comments_by_subreddit.parquet /gscratch/comdata/output/reddit_density/comment_terms_10000.feather /gscratch/comdata/output/reddit_density/subreddit_author_tf_similarities_10k_LSI/600.feather /gscratch/comdata/output/reddit_clustering/comment_terms_10000.feather /gscratch/comdata/output/reddit_clustering/best_author-tf.feather

	python3 cluster_timeseries.py --term-clusters-path=/gscratch/comdata/output/reddit_clustering/comment_terms_10000.feather --author-clusters-path=/gscratch/comdata/output/reddit_clustering/best_author-tf.feather ---term-densities-path=/gscratch/comdata/output/reddit_density/comment_terms_10000.feather --author-densities-path=/gscratch/comdata/output/reddit_density/subreddit_author_tf_similarities_10k_LSI/600.feather --output=data/subreddit_timeseries_authortf.parquet
