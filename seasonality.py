# Get data for the following sports seasons
# NFL/Seahawks, "huskies", 'mariners' 'pnwhiking' 'pnwriders' 'seahawks' 'soundersfc' 'udub' 'wsu' 'wwu')

# For huskies we might need different dummies for different teams.
# for udub, wsu, wwu we'll use the academic calendars

# we obtain nfl data using the pygame-redux python package which queries data from nfl.com

# let's have 2 types of sports seasonality: type 1 are weeks where the team plays (either regular season or post season, include bye weeks). type 2 are weeks when there are other league events (either pre-season weeks, post-season weeks where the team doesn't play, and the nfl draft).


import pandas as pd
import numpy as np
import nflgame
from datetime import datetime, timedelta
from itertools import chain

def get_seahawks_seasonality_data():

    years = list(range(2009,2020))

    team = 'SEA'

    preseason_games = []
    regular_season_games = []
    playoff_games = []
    season_ends = []
    for year in years:
        try:
            reg_games = nflgame.games(year,kind='REG')
        except (TypeError, AttributeError):
            reg_games = []

        try:
            pre_games = nflgame.games(year,kind='PRE')
        except (TypeError, AttributeError):
            pre_games = []

        try:
            post_games = nflgame.games(year,kind='POST')
        except (TypeError, AttributeError):
            post_games = []

        games = reg_games + pre_games + post_games
        last_week = None

        for game in games:
            schedule = game.schedule
            day = datetime(schedule['year'],schedule['month'],schedule['day'])
            if last_week is None:
                last_week = day
            elif last_week < day:
                last_week = day

            if (game.home == team) or (game.away == team):
                week = schedule.get('week',None)
                schedule = game.schedule
                if schedule['season_type'] == 'REG':
                    regular_season_games.append((day,week,year))
                if schedule['season_type'] == 'POST':
                    playoff_games.append((day,week,year))
                if schedule['season_type'] == 'PRE':
                    preseason_games.append((day,week,year))
                    season_ends.append((day, year, year))

    #the data on playoff games was incomplete so we'll manually add some playoff games
    preseason_games.append((datetime(2019,8,8),1,2019))
    preseason_games.append((datetime(2019,8,18),2,2019))
    preseason_games.append((datetime(2019,8,24),3,2019))
    preseason_games.append((datetime(2019,8,29),4,2019))
    playoff_games.append((datetime(2017,1,7),1,2016))
    playoff_games.append((datetime(2017,1,14),2,2016))
    playoff_games.append((datetime(2019,1,5),1,2018))
    playoff_games.append((datetime(2020,1,5),1,2019))
    playoff_games.append((datetime(2020,1,12),2,2019))

    nfl_draft_dates = [
        datetime(2009,4,25),datetime(2009,4,26),
        datetime(2010,4,22),datetime(2010,4,24),
        datetime(2011,4,28),datetime(2011,4,30),
        datetime(2012,4,26),datetime(2012,4,28),
        datetime(2013,4,25),datetime(2013,4,27),
        datetime(2014,5,8),datetime(2014,5,10),
        datetime(2015,4,30),datetime(2015,5,2),
        datetime(2016,4,28),datetime(2016,4,30),
        datetime(2017,4,27),datetime(2017,4,29),
        datetime(2018,4,26),datetime(2018,4,28),
        datetime(2019,4,25),datetime(2019,4,27),
        datetime(2020,4,23),datetime(2020,4,25)
    ]
    nfl_draft_dates_2 = []
    i = 0
    for d in nfl_draft_dates:
        nfl_draft_dates_2.append((d, i%2+1,d.year))
        i = i + 1

    date, week_no, season = zip(* regular_season_games + preseason_games + playoff_games + nfl_draft_dates_2 + season_ends)

    type = ['reg' for i in regular_season_games] + ['pre' for i in preseason_games] + ['post' for i in playoff_games] + ['draft' for i in nfl_draft_dates] + ['end season' for i in season_ends]

    import pandas as pd
    output = pd.DataFrame({'date':date,'type':type,'subreddit':'seahawks','week_no':week_no,'season':season})
    output.to_feather("data/seasonality_levels.feather")
    output.to_csv("data/seasonality_levels.csv")

def load_seahawks_seasonality(ytab):
    seasonality_data = pd.read_feather("data/seasonality_levels.feather")

    season_end_dates = seasonality_data.loc[seasonality_data.type == 'end season']
    season_end_dates = season_end_dates.drop('week_no',axis='columns')
    seasonality_data =  seasonality_data.loc[seasonality_data.type != 'end season']

    ytab['next_week'] = ytab.week + timedelta(weeks=1)

    i,j = np.where((seasonality_data.date.values[:,None] >= ytab.week.values) & (seasonality_data.date.values[:,None] < ytab.next_week.values))

    seahawks_season = pd.DataFrame(np.column_stack([seasonality_data.values[i], ytab.values[j]]),columns=seasonality_data.columns.append(ytab.columns))

    seahawks_season = seahawks_season.append(ytab[~np.in1d(np.arange(len(ytab)),np.unique(j))],ignore_index=True,sort=False)

    # there are bye weeks, so if the previous week and the next week are regular season games, label the middle week as a bye week
    seahawks_season.loc[seahawks_season.type.isna(),'type'] = 'None'
    seahawks_season = seahawks_season.sort_values('week',ascending=True).reset_index()

    seahawks_season['next_week_no'] = seahawks_season.week_no.shift(-1)
    seahawks_season['next_week_type'] = seahawks_season.type.shift(-1)
    seahawks_season['prev_week_no'] = seahawks_season.week_no.shift(1)
    seahawks_season['prev_week_type'] = seahawks_season.type.shift(1)

    ## if the next week has a number greater than 1, then the current week has the type of the next week
    seahawks_season.loc[(seahawks_season.week_no.isna()) & (seahawks_season.next_week_no > 1) & (seahawks_season.next_week_type == 'reg'),"week_type"] = "reg"
    seahawks_season.loc[(seahawks_season.week_no.isna()) & (seahawks_season.next_week_no > 1) & (seahawks_season.next_week_type == 'reg')]

    ## if they make the playoffs then the week off before the playoffs counts as a game
    seahawks_season.loc[(seahawks_season.week_no.isna()) & (seahawks_season.next_week_no > 1) & (seahawks_season.next_week_type == 'post'),"week_type"] = "post"
    ## if they miss the playoffs, but it's before season end we'll account for that and treat it like the draft week
    seahawks_season['season'] = seahawks_season.season.fillna(method='ffill')
    seahawks_season = pd.merge(seahawks_season,season_end_dates,how='left', on='season',suffixes=['','_end'])
    seahawks_season.loc[seahawks_season.week_no.isna() & (seahawks_season.week <= seahawks_season.date_end),'type'] = 'post_loss'

    seahawks_season.loc[(seahawks_season.week_no.isna()),'type'] = 'offseason'

    seahawks_season = seahawks_season.loc[:,['type','week']]

    seahawks_season.loc[seahawks_season.type.isin({'offseason'}),'season_cat'] = 1
    seahawks_season.loc[seahawks_season.type.isin({'reg','post'}),'season_cat'] = 2
    seahawks_season.loc[seahawks_season.type.isin({'pre','draft','post_loss'}),'season_cat'] = 3
    seahawks_season = seahawks_season.drop_duplicates().reset_index(drop=True)
    seahawks_season.loc[:,'season_cat'] = seahawks_season.season_cat.astype(np.int32)
    return seahawks_season
