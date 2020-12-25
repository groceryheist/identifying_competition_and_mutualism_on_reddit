import pandas as pd
import numpy as np
from pyspark.sql import functions as f
from pyspark.sql import SparkSession
from choose_clusters import load_clusters, load_densities

clusters = load_clusters()
densities = load_densities()

spark = SparkSession.builder.getOrCreate()

df = spark.read.parquet("/gscratch/comdata/output/reddit_comments_by_subreddit.parquet")

df = df.withColumn('week',f.date_trunc('week',f.col("CreatedAt")))

# time of unique authors by series by week
ts = df.select(['subreddit','week','author']).distinct().groupby(['subreddit','week']).count()

ts = ts.repartition('subreddit')

# ts.write.parquet("data/subreddit_timseries.parquet")
ts = spark.read.parquet("data/subreddit_timseries.parquet")

spk_clusters = spark.createDataFrame(clusters)

ts = ts.join(spk_clusters, on='subreddit',how='inner')

spk_densities = spark.createDataFrame(densities)

ts = ts.join(spk_densities, on='subreddit',how='inner')

ts.write.parquet("data/subreddit_timeseries.parquet",mode='overwrite')
