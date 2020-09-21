# let's try using the pyarrow datasets feature
import pyarrow as pa
import pyarrow.parquet as pq
import pyarrow.dataset as ds
import pathlib

import pyspark
from pyspark.sql import functions as f
from pyspark.sql import SparkSession

spark = SparkSession.builder.getOrCreate()
df = spark.read.parquet('/gscratch/comdata/output/reddit_submissions_by_subreddit.parquet/')
wallpaper_subs = df.select('subreddit').distinct().filter(f.col('subreddit').contains('wallpaper'))
wallpaper_subs = wallpaper_subs.collect()
substring_search = [s.subreddit for s in wallpaper_subs]

main = ['wallpaper','wallpapers']
subreddits_to_track = set(main + substring_search)
dataset = ds.dataset(pathlib.Path('/gscratch/comdata/output/reddit_submissions_by_subreddit.parquet/'), format='parquet', partitioning='hive')

table = dataset.to_table(filter = ds.field('subreddit').isin(subreddits_to_track), columns=['id','author','subreddit','title','CreatedAt','permalink','score','ups','downs','over_18','url','num_comments','gilded','subreddit_subscribers','subreddit_id','stickied'])

# then write it to a feather file
import pyarrow.feather as feather
feather.write_feather(table, 'data/wallpaper_subreddit_submissions.feather', compression='lz4')

import pandas as pd

df = table.to_pandas()

df.to_csv("data/wallpaper_subreddit_submissions.csv")

comments = ds.dataset(pathlib.Path("/gscratch/comdata/output/reddit_comments_by_subreddit.parquet"), format='parquet', partitioning='hive')
table = comments.to_table(filter = ds.field('subreddit').isin(subreddits_to_track), columns=['id','author','subreddit','CreatedAt'])
feather.write_feather(table, 'data/wallpaper_subreddit_comments.feather',compression='lz4')
