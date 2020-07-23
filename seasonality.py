# Get data for the following sports seasons
# NFL/Seahawks, "huskies", 'mariners' 'pnwhiking' 'pnwriders' 'seahawks' 'soundersfc' 'udub' 'wsu' 'wwu')

# For huskies we might need different dummies for different teams.
# for udub, wsu, wwu we'll use the academic calendars

# we obtain nfl data using the pygame-redux python package which queries data from nfl.com

# let's have 2 types of sports seasonality: type 1 are weeks where the team plays (either regular season or post season, include bye weeks). type 2 are weeks when there are other league events (either pre-season weeks, post-season weeks where the team doesn't play, and the nfl draft).

from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import nflgame
from datetime import datetime, timedelta
from itertools import chain

### We account for seasonal interest in the Seattle sounders according to their sports schedule.
### We apply control variables in order of precedence (rules listed first supercede following rules).
### 1. MLS playoff games, CONECAFF games, and the US Open Cup count as "postseason games" Label (1), also includes egular season MLS playoff games are "regular season games" Label (2) 
### 2. Regional tournaments, friendlies, preseason games and the MLS super and reentry drafts are "low level events" Label (3)
###    also includes MLS playoffs after the sounders have lost or the MLS drafts.
### 3. other weeks are the off-season

def load_sounders_seasons(ytab):
    # obtained data through 2016 from https://raw.githubusercontent.com/jalapic/engsoccerdata/master/data-raw/mls.csv
    # other data manually entered from Wikipedia

    ytab['next_week'] = ytab.week + timedelta(weeks=1)
    ytab['season'] = 'offseason'

    mls_data = pd.read_csv("data/mls.csv",keep_date_col=True,parse_dates=['Date'])

    sounders_games = mls_data.loc[(mls_data.home == 'Seattle Sounders') | (mls_data.visitor == 'Seattle Sounders')]

    preseason_spans = pd.DataFrame({'min':[datetime(2009,2,9),datetime(2010,2,11),datetime(2011,2,4),datetime(2012,2,6),datetime(2013,2,5),datetime(2014,2,5),datetime(2015,2,18),datetime(2018,1,28),datetime(2019,2,9)],
                                    'max':[datetime(2009,3,12),datetime(2010,3,11),datetime(2011,3,9),datetime(2012,4,21),datetime(2013,2,22),datetime(2014,3,1),datetime(2015,2,28),datetime(2018,2,15),datetime(2019,2,23)]})

    i_preseason,j_preseason = np.where((preseason_spans['min'].values[:,None] <= ytab.week.values) & (preseason_spans['max'].values[:,None] >= ytab.week.values))

    sounders_friendlies = [datetime(2009,7,18), datetime(2009,8,5),datetime(2010,5,26),datetime(2010,7,18),datetime(2010,9,12),datetime(2011,7,20),datetime(2012,7,18),datetime(2014,7,6),datetime(2014,7,19),datetime(2015,2,4),datetime(2015,2,6),datetime(2015,2,19),datetime(2015,3,24),datetime(2016,2,3),datetime(2016,2,14),datetime(2016,2,17),datetime(2016,2,17),datetime(2016,2,6),datetime(2016,2,9),datetime(2016,7,5),datetime(2017,2,4),datetime(2017,2,7),datetime(2017,2,18),datetime(2017,2,22),datetime(2017,2,25),datetime(2017,3,25),datetime(2017,3,25),datetime(2017,7,8)]

    superdrafts = [datetime(2008,1,18),datetime(2009,1,15),datetime(2010,1,14),datetime(2010,1,13),datetime(2011,1,12),datetime(2013,1,17),datetime(2014,1,16),datetime(2015,1,15),datetime(2016,1,14),datetime(2017,1,13),datetime(2018,1,19),datetime(2019,1,11),datetime(2020,1,9)]

    reentrydrafts = [datetime(2010,12,8),datetime(2010,12,15),datetime(2011,12,5),datetime(2011,12,12),datetime(2012,12,7),datetime(2012,12,14),datetime(2013,12,12), datetime(2013,12,18), datetime(2014,12,12), datetime(2012,12,18),datetime(2015,12,11), datetime(2015,12,17), datetime(2016,12,16), datetime(2016,12,22), datetime(2017, 12, 15), datetime(2017, 12, 21), datetime(2018,12,14), datetime(2018,12,20), datetime(2019,11,26), datetime(2019,12,3)]

    preseason = pd.Series(sounders_friendlies + superdrafts + reentrydrafts)

    i_friendly, j_friendly = np.where((preseason.values[:,None] >= ytab.week.values) & (preseason.values[:,None] < ytab.next_week.values))

    # type 1 is regular season or playoff games
    postseason_spans_1 = mls_data.loc[mls_data['round']!='regular'].groupby('Season').Date.agg(['min','max']).reset_index(drop=False)

    postseason_spans_2 = pd.DataFrame({'min':[datetime(2017,10,25),datetime(2018,10,31),datetime(2019,10,19)],
                                       'max':[datetime(2017,12,9),datetime(2018,12,8),datetime(2019,10,19)],
                                       'Season':['2017','2018','2019']})

    postseason_spans = pd.concat([postseason_spans_1,postseason_spans_2]).reset_index(drop=True)
    

    i_mls_postseason, j_mls_postseason = np.where((postseason_spans['min'].values[:,None] <= ytab.week.values) &
                                                          (postseason_spans['max'].values[:,None] > ytab.week.values))

    j_3 = sorted(np.append(np.append(j_friendly, j_preseason),j_mls_postseason))

    ytab.iloc[j_3,ytab.columns=='season'] = 'preseason_or_league_event'

    regular_season_spans_1 = sounders_games.loc[sounders_games['round']=='regular'].groupby('Season').Date.agg(['min','max'])

    regular_season_spans_1 = regular_season_spans_1.reset_index()

        # manually entered data from 2017-2019 based on Wikipedia
    regular_season_spans_2 = pd.DataFrame({"min":[datetime(2017,3,3), datetime(2018,3,3), datetime(2019,3,2)],
                                           "max":[datetime(2017,9,22), datetime(2018,9,28), datetime(2019,9,6)],
                                           "Season":['2017','2018','2019']})
    
    regular_season_spans = pd.concat([regular_season_spans_1, regular_season_spans_2]).reset_index(drop=True)

    i_regular,j_regular = np.where((regular_season_spans['min'].values[:,None] <= ytab.week.values) & (regular_season_spans['max'].values[:,None] >= ytab.week.values))

    ytab.iloc[j_regular,ytab.columns=='season'] = 'regular_season'


    sounders_postseason_1 = mls_data.loc[(mls_data['round'] != 'regular') & ((mls_data['home'] == 'Seattle Sounders') |
                                                                              (mls_data['visitor'] == 'Seattle Sounders'))]

    sounders_postseason = pd.concat([pd.Series([datetime(2009,10,29), datetime(2009,11,8), datetime(2009,4,28), datetime(2009,5,26), datetime(2009,7,1), datetime(2009,7,7), datetime(2009,7,21), datetime(2009,9,2),datetime(2010,6,30),datetime(2010,7,7),datetime(2010,8,1),datetime(2010,9,5),datetime(2010,7,28),datetime(2010,8,3),datetime(2010,8,19),datetime(2010,8,25),datetime(2010,9,14),datetime(2010,9,22),datetime(2010,9,29),datetime(2010,10,19),datetime(2011,6,28),datetime(2011,7,13),datetime(2011,8,30),datetime(2011,9,4),datetime(2011,7,26),datetime(2011,8,3),datetime(2011,8,16),datetime(2011,8,23),datetime(2011,9,14),datetime(2011,9,20),datetime(2011,9,27),datetime(2011,10,18),datetime(2012,3,7),datetime(2012,3,14),datetime(2012,8,2),datetime(2012,8,30),datetime(2012,9,19),datetime(2012,10,24),datetime(2012,5,30),datetime(2012,6,5),datetime(2012,6,26),datetime(2012,7,11),datetime(2012,8,8),datetime(2013,3,6),datetime(2013,3,12),datetime(2013,4,2),datetime(2013,4,9),datetime(2013,5,29),datetime(2014,6,18),datetime(2014,6,24),datetime(2014,7,9),datetime(2014,8,13),datetime(2014,9,16),datetime(2015,6,16),datetime(2015,8,5),datetime(2015,8,19),datetime(2015,8,26),datetime(2015,9,23),datetime(2016,6,15),datetime(2016,6,28),datetime(2016,7,20),datetime(2016,2,23),datetime(2016,3,2),datetime(2017,10,29),datetime(2017,11,2),datetime(2017,11,21),datetime(2017,11,30),datetime(2017,12,9),datetime(2017,6,13),datetime(2017,6,28),datetime(2018,11,4),datetime(2018,11,8),datetime(2018,2,22),datetime(2018,3,1),datetime(2018,3,7),datetime(2018,3,14),datetime(2018,6,6),datetime(2019,10,19),datetime(2019,10,23),datetime(2019,10,29),datetime(2019,11,10),datetime(2019,6,12)]),sounders_postseason_1.Date]).sort_values()


    i_sounders_postseason, j_sounders_postseason = np.where((sounders_postseason.values[:,None] >= ytab.week.values) &
                                                            (sounders_postseason.values[:,None] < ytab.next_week.values))

    ytab.iloc[j_sounders_postseason,ytab.columns=='season'] = 'postseason'

    ytab.loc[ytab.season=='offseason','season_cat'] = 1
    ytab.loc[ytab.season=='preseason_or_league_event','season_cat'] = 2
    ytab.loc[ytab.season=='regular_season','season_cat'] = 3
    ytab.loc[ytab.season=='postseason','season_cat'] = 3
    ytab['season_cat'] = ytab['season_cat'].astype(np.int32)
    return(ytab)

def get_seahawks_seasonality_data(ytab):

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

def load_seahawks_seasons(ytab):
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
    seahawks_season.loc[seahawks_season.type.isin({'pre','draft','post_loss'}),'season_cat'] = 2
    seahawks_season.loc[seahawks_season.type.isin({'reg','post'}),'season_cat'] = 3
    seahawks_season = seahawks_season.drop_duplicates().reset_index(drop=True)
    seahawks_season.loc[:,'season_cat'] = seahawks_season.season_cat.astype(np.int32)
    return seahawks_season

# for baseball we can use the awesome retrosheet.org database
# for mariners we have: type 1: regular season + all star break (the mariners never made the playoffs during the study period)
# type 2: draft, postseason, preseason
# type 3: offseason 

def load_mariners_seasons(ytab):

    # helper function for parsing dates from retrosheet.org
    def dateparse(t):
        try:
            return datetime.strptime(str(t), "%Y%m%d")
        except (ValueError):
            return np.NaN


    playoff_data_files = ['GLDV','GLLC','GLWC','GLWS']

    ytab['next_week'] = ytab.week + timedelta(weeks=1)
    ytab['season'] = 'offseason'

    years = range(2009,2020)

    min_date = ytab.week.min()
    chunks = []

    for f in playoff_data_files:
        file_path = f"data/mlb/{f}.TXT"
        df = pd.read_csv(file_path,
                         names=['Date','Game_No','DOW','visitor','visitor_league','visitor_game_no','home','home_league','home_game_no','visitor_score','home_score','len_outs','day_night','completed_date']+[f"col_{i}" for i in range(14,161)],
                         quotechar='"',
                         parse_dates=[0,13],
                         date_parser=dateparse)
        chunks.append(df)

    postseason = pd.concat(chunks)

    postseason = postseason.drop([f"col_{i}" for i in range(14,161)],axis='columns')

    postseason['year'] = postseason.Date.dt.year

    postseason = postseason.loc[postseason.Date >= min_date]

    postseason_spans = postseason.groupby(postseason.year)['Date'].agg(['min','max']).reset_index(drop=False)
    
    i_postseason, j_postseason = np.where((postseason_spans['min'].values[:,None] <= ytab.week.values) &
                                          (postseason_spans['max'].values[:,None] > ytab.week.values))

    ytab.iloc[j_postseason,ytab.columns == 'season'] = 'postseason_loss'

    # preseason dates from springtrainingconnection.com
    preseason_spans = pd.DataFrame({'min':[datetime(2009,2,25),datetime(2010,3,3),datetime(2011,2,27),datetime(2012,3,2),datetime(2013,2,22),datetime(2014,2,27),datetime(2015,3,4),datetime(2016,3,2),datetime(2017,2,25),datetime(2018,2,23),datetime(2019,2,21)],
                                     'max':[datetime(2009,4,2),datetime(2010,4,1),datetime(2011,3,29),datetime(2012,4,4),datetime(2013,3,28),datetime(2014,3,29),datetime(2015,4,4),datetime(2016,4,2),datetime(2017,4,1),datetime(2018,3,27),datetime(2019,3,22)]})

    i_preseason, j_preseason = np.where((preseason_spans['min'].values[:,None] <= ytab.week.values) &
                                        (preseason_spans['max'].values[:,None] > ytab.week.values))

    ytab.iloc[j_preseason,ytab.columns=='season'] = 'preseason'

    # draft dates are irrelevant since it happens during the regular season
    # they come from Wikipedia
    # draft_spans = pd.DataFrame({'min':[datetime(2009,6,9),datetime(2010,6,7),datetime(2011,6,6),datetime(2012,6,4),datetime(2013,6,8),datetime(2014,6,5),datetime(2015,6,8),datetime(2016,6,9),datetime(2017,6,12),datetime(2018,6,4),datetime(2019,6,3)]
    #                             'max':[datetime(2009,6,11),datetime(2010,6,9),datetime(2011,6,8),datetime(2012,6,6),datetime(2013,6,8),datetime(2014,6,7),datetime(2015,6,10),datetime(2016,6,11),datetime(2017,6,14),datetime(2018,6,6),datetime(2019,6,5)]
    # })

    chunks = []

    for year in years:
        file_path = f"data/mlb/{year}SKED.TXT"
        df = pd.read_csv(file_path,names = ["Date","Game_No","DOW","visitor","visitor_league","vis_game_no","home","home_league","home_game_no","TOD","modified","makeup_date"],quotechar='"',parse_dates = [0,11],date_parser=dateparse)

        chunks.append(df)

    regular_season = pd.concat(chunks)
    regular_season = regular_season.loc[(regular_season.visitor == 'SEA') | (regular_season.home == 'SEA')]

    i_regular, j_regular = np.where((regular_season['Date'].values[:,None] >= ytab.week.values) &
                                    (regular_season['Date'].values[:,None] < ytab.next_week.values))


    ytab.iloc[j_regular,ytab.columns == 'season'] = 'regular'


    sea_postseason = postseason.loc[(postseason.visitor == 'SEA') |
                                    (postseason.home == 'SEA')]

    sea_postseason_spans = sea_postseason.groupby(sea_postseason.year)['Date'].agg(['min','max']).reset_index(drop=False)

    i_postseason_sea, j_postseason_sea = np.where((sea_postseason_spans['min'].values[:,None] <= ytab.week.values) &
                                          (sea_postseason_spans['max'].values[:,None] > ytab.week.values))

    ytab.iloc[j_postseason_sea,ytab.columns == 'season'] = 'postseason'

    ytab.loc[ytab.season == 'offseason','season_cat'] = 1
    ytab.loc[(ytab.season == 'preseason') | (ytab.season == 'postseason_loss'),'season_cat'] = 2
    ytab.loc[ytab.season == 'regular','season_cat'] = 3

    ytab.season_cat = ytab.season_cat.astype(np.int32)
    return(ytab)


def load_ncaa_games(f):
    def dateparse(d):
        try:
            date = datetime.strptime(str(d), "%m/%d/%Y")
            if date.month == 1:
                date = date.replace(year=date.year + 1)
            return date
        except ValueError:
            return np.NaN

    ## data from college football reference, curated by https://gamethread.redditcfb.com/gamedb.php
    soup = BeautifulSoup(open(f,'r').read(),features='lxml')
    game_dates = soup.find_all(attrs={"class":'date'})
    game_seasons = soup.find_all(attrs={"class":"season"})
    
    game_dates = map(dateparse,
                     map('/'.join,
                         zip(
                             map(lambda t: t.text,game_dates),
                             map(lambda t: t.text,game_seasons)
                         )
                     )
    )
    # strip_header
    game_dates = list(game_dates)
    return(game_dates)

ncaa_seasons = pd.DataFrame({'min':[datetime(2009,9,3), datetime(2010,9,2), datetime(2011,9,1), datetime(2012,8,30),datetime(2013,8,29), datetime(2014,8,27), datetime(2015,9,3), datetime(2016,8,26), datetime(2017,8,26), datetime(2018,8,25), datetime(2019,8,24)],
                             'max':[datetime(2009,12,19), datetime(2010,12,11), datetime(2011,12,10), datetime(2012,12,8), datetime(2013,12,14), datetime(2014,12,13), datetime(2015,12,12), datetime(2016,12,10), datetime(2017,12,9), datetime(2018,12,8), datetime(2018,12,14)]
    })

ncaa_postseasons = pd.DataFrame({'min':[datetime(2009,12,19), datetime(2010,12,18), datetime(2011,12,17), datetime(2012,12,15), datetime(2013,12,21), datetime(2014,12,20), datetime(2015,12,19), datetime(2016,12,17), datetime(2017,12,16), datetime(2018,12,15), datetime(2019,12,21)],
                                 'max':[datetime(2010,1,7), datetime(2011,1,10), datetime(2012,1,9), datetime(2013,1,7), datetime(2014,1,6), datetime(2015,1,12), datetime(2016,1,11), datetime(2017,1,9), datetime(2018,1,8), datetime(2019,1,7), datetime(2020,1,13)]
    })

## we apply these rules in order of precedence
## 1. if it's NCAA football season and the huskies haven't played their last game
## 2. if it's the school year
## 3. WWU doesnt have a football team at all :)
def load_uni_seasons(ytab, games_file, aca_spans):

    ytab['next_week'] = ytab.week + timedelta(weeks=1)
    ytab['season'] = 'offseason'

    # academic calendars obtained from websites.
    i_academic, j_academic = np.where((aca_spans['min'].values[:,None] <= ytab.week.values) &
                                      (aca_spans['max'].values[:,None] > ytab.week.values))

    ytab.iloc[j_academic,ytab.columns=='season'] = 'academic'

    if games_file is not None:
        game_dates = pd.DataFrame({"game_date":load_ncaa_games(games_file)})
        
        game_dates['year'] = game_dates.game_date.dt.year

        last_game = game_dates.groupby('year')['game_date'].max().reset_index(drop=False)

        last_game.year = last_game.year.astype('int')

        last_game = last_game.set_index("year")

        seasons = ncaa_seasons.copy()
        seasons['year'] = seasons['min'].dt.year
        seasons = seasons.join(last_game,on='year',lsuffix='_last')
        seasons.loc[seasons['max'] <= seasons.game_date,'max'] = seasons.game_date

        i_ncaa, j_ncaa = np.where((seasons['min'].values[:,None] <= ytab.week.values) &
                                  (seasons['max'].values[:,None] > ytab.week.values))

        ytab.iloc[j_ncaa,ytab.columns=='season'] = 'regular_season'

    ytab.loc[ ytab.season == 'offseason','season_cat'] = 1
    ytab.loc[ ytab.season == 'academic','season_cat'] = 2
    ytab.loc[ (ytab.season == 'regular_season'),'season_cat'] = 3
    ytab.season_cat = ytab.season_cat.astype(np.int32)
    return(ytab)

def load_udub_seasons(ytab):
    uw_aca_spans = pd.DataFrame({'min':[datetime(2009,9,30),datetime(2010,9,29), datetime(2011,9,28), datetime(2012,9,24),datetime(2013,9,25),datetime(2014,9,24), datetime(2015,9,30), datetime(2016,9,28), datetime(2017,9,27),datetime(2018,8,26), datetime(2019,9,25)],
                                 'max':[datetime(2010,6,11), datetime(2011,6,10), datetime(2012,6,8), datetime(2013,6,14), datetime(2014,6,13), datetime(2015,6,12), datetime(2016,6,10), datetime(2017,6,9), datetime(2018,6,8), datetime(2019,6,14), datetime(2020,6,12)]
    })

    return(load_uni_seasons(ytab, 'data/ncaa/_r_CFB Game Database_UW.html', uw_aca_spans))

def load_wsu_seasons(ytab):
    # from wsu registrar's website
    wsu_aca_spans = pd.DataFrame({'min':[datetime(2009,8,24),datetime(2010,8,23),datetime(2011,8,22), datetime(2012,8,20),datetime(2013,8,19),datetime(2014,8,25),datetime(2015,8,24),datetime(2016,8,22),datetime(2017,8,21),datetime(2018,8,20), datetime(2019,8,30)],
                                  'max':[datetime(2010,5,7), datetime(2011,5,6), datetime(2012,5,4), datetime(2013,5,3), datetime(2014,5,9),datetime(2015,5,8),datetime(2016,5,7), datetime(2017,5,5), datetime(2018,5,4),datetime(2019,5,3),datetime(2020,5,8)]
                                  })

    return(load_uni_seasons(ytab, 'data/ncaa/_r_CFB Game Database_WSU.html', wsu_aca_spans))

def load_wwu_seasons(ytab):

    ## from the WWU course catelogs on their website via the wayback machine
    wwu_aca_spans = pd.DataFrame({'min':[datetime(2009,9,23),datetime(2010,9,22), datetime(2011,9,21), datetime(2012,9,24),datetime(2013,9,25),datetime(2014,9,24), datetime(2015,9,24), datetime(2016,9,21), datetime(2017,9,27),datetime(2018,8,26), datetime(2019,9,25)],
                                 'max':[datetime(2010,6,11), datetime(2011,6,11), datetime(2012,6,9), datetime(2013,6,14), datetime(2014,6,13), datetime(2015,6,12), datetime(2016,6,10), datetime(2017,6,9), datetime(2018,6,15), datetime(2019,6,14), datetime(2020,6,12)]
    })

    return(load_uni_seasons(ytab,None, wwu_aca_spans))
