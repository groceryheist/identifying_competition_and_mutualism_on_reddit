from datetime import datetime, timedelta
import seasonality
import pandas as pd
import numpy as np
from pyRemembeR import Remember

remember = Remember()

df = pd.read_feather("data/included_timeseries.feather")
df = df.rename(mapper= lambda n: n.replace('.','_'),axis='columns')

min_date = datetime(2012,1,1)
max_date = datetime(2016,1,1)
forecast_end = datetime(2017,1,1)

remember(min_date,'min_date')
remember(max_date,'max_date')
remember(forecast_end,'forecast_end')

df = df.loc[(df.week >= min_date) & (df.week <= forecast_end)]

df['season_cat'] = np.NaN

ytab = df.pivot(index='week',columns='subreddit',values='N_authors').reset_index(drop=False)

seahawks_season = seasonality.load_seahawks_seasonality(ytab)

df.loc[df['subreddit'] == 'seahawks','season_cat'] = seahawks_season.season_cat.values

sounders_season = seasonality.load_sounders_seasonality(ytab)

df.loc[df.subreddit=='soundersfc','season_cat'] = sounders_season.season_cat.values

mariners_season = seasonality.load_mariners_seasonality(ytab)

df.loc[df.subreddit=='mariners','season_cat'] = mariners_season.season_cat.values

# levels for colleges are: 1. football season; 2. academic year 3. other
udub_season = seasonality.load_udub_season(ytab)
df.loc[df.subreddit=='udub','season_cat'] = udub_season.season_cat.values

wsu_season =  seasonality.load_wsu_season(ytab)
df.loc[df.subreddit=='wsu','season_cat'] = wsu_season.season_cat.values

wwu_season = seasonality.load_wwu_season(ytab)
df.loc[df.subreddit=='wwu','season_cat'] = wwu_season.season_cat.values

df = df.reset_index(drop=True)

df.to_feather('data/var_df.feather')
df.to_csv('data/var_df.csv')
