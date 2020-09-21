# let's try using the pyarrow datasets feature
import pyarrow as pa
import pyarrow.parquet as pq
import pyarrow.dataset as ds
import pathlib

main = ['whatisthisthing']
# retrieved on 2020-8-13
wiki = ['AnimalID',
        'AnimalTracking',
        'BoneCollecting',
        'archaeology',
        'askdocs',
        'askvet',
        'coins'
        'findfashion',
        'fossilid',
        'guns',
        'hallmarks',
        'HelpMeFind',
        'herpetology',
        'howdoesthiswork',
        'identifythisfont',
        'knives',
        'mycology',
        'namethatcar',
        'swords',
        'symbology',
        'toys',
        'TipOfMyFork',
        'tipofmyjoystick',
        'tipofmypenis',
        'tipofmytongue',
        'translator',
        'watches',
        'whatsthisbird',
        'whatsthisfish',
        'whatisthisfish',
        'whatsthisplant',
        'whatsthisrock',
        'whatsthisworth',
        'whereisthis']

subreddits_to_track = main + wiki
subreddits_to_track = [s.lower() for s in subreddits_to_track]
dataset = ds.dataset(pathlib.Path('/gscratch/comdata/output/reddit_submissions_by_subreddit.parquet/'), format='parquet', partitioning='hive')
table = dataset.to_table(filter = ds.field('subreddit').isin(subreddits_to_track), columns=['id','author','subreddit','title','CreatedAt','permalink','score','ups','downs','over_18','url','num_comments','gilded','subreddit_subscribers','subreddit_id','stickied'])

import pyarrow.feather as feather
feather.write_feather(table, 'data/whatsthis_subreddit_submissions.feather', compression='lz4')

import pandas as pd

df = table.to_pandas()

df.to_csv("data/whatsthis_subreddit_submissions.csv")
