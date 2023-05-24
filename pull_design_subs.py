
# let's try using the pyarrow datasets feature
# let's try using the pyarrow datasets feature
import pyarrow as pa
import pyarrow.parquet as pq
import pyarrow.dataset as ds
import pathlib

# creating a Dataset object loads nothing into memory, it only crawls the directory to find all the files and infer the schema. 

# the sweet thing about a dataet is that we can filter by columns

main = ['crappydesign','assholedesign','mildlyinfuriating']

# mentioned in thread https://www.reddit.com/r/assholedesign/comments/a02ezp/meta_is_it_asshole_design_a_handy_flowchart/

thread = ['Designporn','designdesign','TooGoodOfADesign','ATBGE','GTBAE','dataisbeautiful','dataisugly','antiassholedesign','softwaregore','notmyjobs','don\'tdeadopeninside']

ahole_wiki = ['notmyjob','wellthatsucks']
crappy_wiki = ['Engrish','SoftwareGore','technope']
mildly_wiki = ['onejob']

related = ['expectationvsreality']


subreddits_to_track = main + thread + related + ahole_wiki + crappy_wiki + mildly_wiki
subreddits_to_track = [sr.lower() for sr in subreddits_to_track]

dataset = ds.dataset(pathlib.Path('/gscratch/comdata/output/reddit_submissions_by_subreddit.parquet/'), format='parquet', partitioning='hive')

table = dataset.to_table(filter = ds.field('subreddit').isin(subreddits_to_track), columns=['id','author','subreddit','title','CreatedAt','permalink','score','ups','downs','over_18','url','num_comments','gilded','subreddit_subscribers','subreddit_id','stickied'])

# then write it to a feather file
import pyarrow.feather as feather
feather.write_feather(table, 'data/design_subreddit_submissions.feather', compression='lz4')

import pandas as pd

df = table.to_pandas()

df.to_csv("data/design_subreddit_submissions.csv")
