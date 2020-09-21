import pandas as pd
import numpy as np
from datetime import datetime
import seasonality as seas
import pyarrow.feather as feather

# class for organizing var data with holdout forecast
class VarData(object):

    seasonality_map = {'seahawks':seas.load_seahawks_seasons,
                       'soundersfc':seas.load_sounders_seasons,
                       'mariners':seas.load_mariners_seasons,
                       'udub':seas.load_udub_seasons,
                       'wsu':seas.load_wsu_seasons,
                       'wwu':seas.load_wwu_seasons,
                       'mycology':seas.solar,
                       'whatisthisbird':seas.solar,
                       'whatisthisplant':seas.solar,
                       'whatisthisfish':seas.solar}

    # input datafame at the subreddit-week level, with seasonality data
    # input 3 dates: begining and end of fit period (inclusive), and the final date for forecast evaluation
    @classmethod
    def from_df(cls, df, min_date, fit_date, forecast_date):

        def parse_if_string(d):
            if(type(d) == str):
                return date.fromisoformat(d)
            else:
                return d

        min_date, fit_date, forecast_date = map(parse_if_string, [min_date, fit_date, forecast_date])

        obj = cls()

        df = df.loc[:,['week','subreddit','N_authors']]
        df['N_authors'] = df['N_authors'].astype(np.int64)
        obj.df_fit = df.loc[(df.week >= min_date) & (df.week<=fit_date)]
        obj.df_forecast = df.loc[(df.week > fit_date) & (df.week <= forecast_date)]
        obj.tab_fit = obj.df_fit.pivot(index='week', columns='subreddit', values='N_authors').reset_index(drop=False)
        obj.tab_forecast = obj.df_forecast.pivot(index='week', columns='subreddit', values='N_authors').reset_index(drop=False)

        y_fit = obj.tab_fit.drop(['week'],axis='columns').to_numpy(dtype=np.int64).T
        y_forecast = obj.tab_forecast.drop(['week'],axis='columns').to_numpy(dtype=np.int64).T

        # y needs to be mxN
        d = {'y':y_fit.astype(np.int64),
             'N':y_fit.shape[1],
             'm':y_fit.shape[0],
             'forecast_len':y_forecast.shape[1],
             }


        obj.stan_data = d
        obj.add_seasonality()
        return obj


    # put the data in a dictionary in the format expeccted by the stan model.
    # we don't set priors or lags here though.
    def get_stan_data(self):
        return self.stan_data

    # vardata is the VarData that we're 
    def add_seasonality(self):
        seasonal_idx = self.tab_fit.columns.isin(VarData.seasonality_map.keys())
        self.stan_data['n_seas'] = np.array(seasonal_idx.sum(),dtype=int)
        self.stan_data['seasonality_idx'] = np.array(np.where(seasonal_idx == True)[0],dtype=int)

        n_seas_levels = []
        season = []

        for key in [k for k in VarData.seasonality_map.keys() if k in self.tab_fit.columns]:
            seasonality = VarData.seasonality_map[key](self.tab_fit)
            forecast_seasonality = VarData.seasonality_map[key](self.tab_forecast)
            n_seas_levels.append(len(set(seasonality.season_cat)))
            season.append(pd.concat([seasonality.season_cat, forecast_seasonality.season_cat]))


        self.stan_data['n_seas_levels'] = np.array(n_seas_levels,dtype=int)
        if season == list():
            self.stan_data['n_seas'] = 0
            self.stan_data['season'] = np.array([[0]],dtype=int)
            self.stan_data['seasonality_idx'] = np.array([0],dtype=int)
            self.stan_data['n_seas_levels'] = np.array([0],dtype=int)
        else:
            self.stan_data['season'] = np.array(season,dtype=int)
        
