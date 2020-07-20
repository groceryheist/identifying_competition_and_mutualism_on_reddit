#!/usr/bin/env python3

from datetime import datetime, timedelta
from util import *
from seasonality import load_seahawks_seasonality
np.set_printoptions(precision=None, suppress=True)

# test out seasonality
test_df = pd.read_feather("data/test_ts.feather")
test_df = test_df.rename(mapper = lambda s: s.replace(".","_"),axis='columns')

test_df = test_df.loc[test_df.subreddit.isin({"washington","seattle","eastside","seahawks"})]
test_df = test_df.loc[test_df.week > datetime.fromisoformat("2012-01-01")]
test_df_fit = test_df.loc[test_df.week < datetime.fromisoformat("2016-01-01")]
test_df_forecast = test_df.loc[(test_df.week >= datetime.fromisoformat("2016-01-01")) & (test_df.week < datetime.fromisoformat("2016-07-01"))]
ytab = test_df_fit.pivot(index='week',columns='subreddit',values='N_authors')
ytab = ytab.reset_index(drop=False)

y = ytab.drop('week',axis='columns').to_numpy()

test_data = {'y':y.T,
             'N':y.shape[0],
             'm':y.shape[1],
             'p':3,
             'm_diag':np.repeat(0,y.shape[1]),
             'scale_diag':1,
             'scale_offdiag':0,
             'df':10,
             'forecast_len':30,
             'es' : [1,0], # top-level prior for the means of the means (diag, off-diag)
             'fs' : [np.sqrt(1.4), np.sqrt(1.4)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
             'gs' : [2.1,2.1], # top-level prior for the position of the scales,
             'hs' : [1/3,1/3], # top-level prior for the precision of the scales,
             'df':10,
             'forecast_len':25,
             'alpha':0,
             'sd0': 7, # hyper prior precision of mu0
             'g0':4, # hyper prior for variance of m0
             'h0':3, # hyper prior for variance of m0} # degrees of freedom in the inverse wishart prior on the scale matrix.
}

test_data['n_seas'] = 1
test_data['seasonality_idx'] = [2]
test_data['n_seas_levels'] = [3]

# gotta build it.

seahawks_season = load_seahawks_seasonality(ytab)
test_data['season'] = seahawks_season.season_cat

## STILL NEED TO ADD SEASONALITY TO THE FORECAST

heaps_pois_seasonality  = unpickle_or_create("stan_code/heaps_poisson_seasonality.pkl",
                                             overwrite=True,
                                             function=pystan.StanModel,
                                             file='stan_code/heaps_poisson_seasonality.stan',
                                             model_name='heaps_poisson_seasonality')


test_seasonality_fit  = unpickle_or_create("stan_models/test_seasonality_fit.pkl",
                                           overwrite=True,
                                           function=heaps_pois_seasonality.sampling,
                                           chains=4,
                                           data=test_data,
                                           control={'adapt_delta':0.95,'max_treedepth':20})
