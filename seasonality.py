# Get data for the following sports seasons
# NFL/Seahawks, "huskies", 'mariners' 'pnwhiking' 'pnwriders' 'seahawks' 'soundersfc' 'udub' 'wsu' 'wwu')

# For huskies we might need different dummies for different teams.
# for udub, wsu, wwu we'll use the academic calendars

# we obtain nfl data using the pygame-redux python package which queries data from nfl.com

import pandas as pd
import nflgame
from datetime import datetime
from itertools import chain

years = list(range(2009,2020))

team = 'SEA'

preseason_games = []
regular_season_games = []
playoff_games = []

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
    for game in games:
        schedule = game.schedule
        if (game.home == team) or (game.away == team):
            day = datetime(schedule['year'],schedule['month'],schedule['day'])
            week = schedule.get('week',None)
            schedule = game.schedule
            if schedule['season_type'] == 'REG':
                regular_season_games.append((day,week))
            if schedule['season_type'] == 'POST':
                playoff_games.append((day,week))
            if schedule['season_type'] == 'PRE':
                preseason_games.append((day,week))

#the data on playoff games was incomplete so we'll manually add some playoff games
preseason_games.append((datetime(2019,8,8),1))
preseason_games.append((datetime(2019,8,18),2))
preseason_games.append((datetime(2019,8,24),3))
preseason_games.append((datetime(2019,8,29),4))
playoff_games.append((datetime(2017,1,7),1))
playoff_games.append((datetime(2017,1,14),2))
playoff_games.append((datetime(2019,1,5),1))
playoff_games.append((datetime(2020,1,5),1))
playoff_games.append((datetime(2020,1,12),2))

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
    nfl_draft_dates_2.append((d, i%2+1))
    i = i + 1

date, week_no = zip(* regular_season_games + preseason_games + playoff_games + nfl_draft_dates_2)

type = ['reg' for i in regular_season_games] + ['pre' for i in preseason_games] + ['post' for i in playoff_games] + ['draft' for i in nfl_draft_dates]

import pandas as pd
output = pd.DataFrame({'date':date,'type':type,'subreddit':'seahawks','week_no':week_no})
output.to_feather("data/seasonality_levels.feather")
output.to_csv("data/seasonality_levels.csv")
