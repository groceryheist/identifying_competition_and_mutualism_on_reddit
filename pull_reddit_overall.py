import pyarrow.feather as feather
from pyspark.sql import functions as f
from pyspark.sql import SparkSession
import logging

logging.getLogger("py4j").setLevel(logging.ERROR)

spark = SparkSession.builder.getOrCreate()
df = spark.read.parquet('/gscratch/comdata/output/reddit_submissions_by_author.parquet/')

# Date.trunc week starts on mondays
df = df.withColumn("week",f.date_trunc('week',df.CreatedAt))
df = df.select(['week','author']).distinct()
posts_per_week = df.groupBy("week").count().toPandas()

feather.write_feather(posts_per_week, 'data/all_reddit_posters_per_week.feather')
posts_per_week.to_csv('data/all_reddit_posts_per_week.csv')

df = spark.read.parquet('/gscratch/comdata/output/reddit_comments_by_author.parquet')

df = df.withColumn("week",f.date_trunc('week',df.CreatedAt))
df = df.select(['week','author']).distinct()
comments_per_week = df.groupBy("week").count().toPandas()

feather.write_feather(comments_per_week, 'data/all_reddit_commenters_per_week.feather')
comments_per_week.to_csv('data/all_reddit_comments_per_week.csv')
