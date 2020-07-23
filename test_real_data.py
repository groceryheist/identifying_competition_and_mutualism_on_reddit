#!/usr/bin/env python3
from var_data import VarData
from datetime import datetime, timedelta
import pandas as pd
from util import unpickle_or_create, stan_pois_var_predict
import pystan
import numpy as np
from plotnine import *

np.set_printoptions(precision=None, suppress=True)

# test out seasonality
test_df = pd.read_feather("data/test_ts.feather")
test_df = test_df.rename(mapper = lambda s: s.replace(".","_"),axis='columns')

test_df = test_df.loc[test_df.subreddit.isin({"washington","seattle","eastside","seahawks","soundersfc"})]
test_df = test_df.loc[test_df.week > datetime.fromisoformat("2012-01-01")]
test_df_fit = test_df.loc[test_df.week < datetime.fromisoformat("2016-01-01")]

min_date = "2012-01-01"
fit_date = "2016-01-01"
forecast_date = "2017-01-01"

vardata = VarData.from_df(test_df,min_date,fit_date,forecast_date)

test_data = vardata.get_stan_data()

## add priors
test_data = {**test_data,
             **{'p':3,
                'm_diag':np.repeat(0,test_data['y'].shape[0]),
                'scale_diag':1,
                'scale_offdiag':0,
                'df':10,
                'es' : [1,0], # top-level prior for the means of the means (diag, off-diag)
                'fs' : [np.sqrt(1.4), np.sqrt(1.4)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
                'gs' : [2.1,2.1], # top-level prior for the position of the scales,
                'hs' : [1/3,1/3], # top-level prior for the precision of the scales,
                'df':10,
                'alpha':0,
                'sd0': 7, # hyper prior precision of mu0
                'g0':4, # hyper prior for variance of m0
                'h0':3, # hyper prior for variance of m0} # degrees of freedom in the inverse wishart prior on the scale matrix.
             }
}

print(test_data['N'])
print(test_data['m'])
print(test_data['season'].shape)

heaps_pois_seasonality  = unpickle_or_create("stan_code/heaps_poisson_seasonality.pkl",
                                             overwrite=False,
                                             function=pystan.StanModel,
                                             file='stan_code/heaps_poisson_seasonality.stan',
                                             model_name='heaps_poisson_seasonality')


test_seasonality_fit  = unpickle_or_create("stan_models/test_seasonality_fit.pkl",
                                           overwrite=False,
                                           function=heaps_pois_seasonality.sampling,
                                           chains=4,
                                           data=test_data,
                                           iter=3000,
                                           control={'adapt_delta':0.99,'max_treedepth':20})


df_pred = stan_pois_var_predict(test_seasonality_fit,test_data['y'].shape[1])

df_pred['subreddit'] = ""
df_pred.loc[df_pred.variable=='y1','subreddit'] = 'eastside'
df_pred.loc[df_pred.variable=='y2','subreddit'] = 'seahawks'
df_pred.loc[df_pred.variable=='y3','subreddit'] = 'seattle'
df_pred.loc[df_pred.variable=='y4','subreddit'] = 'soundersfc'
df_pred.loc[df_pred.variable=='y5','subreddit'] = 'washington'

df_pred.loc[:,'week'] = list(vardata.df_forecast.week)

p = ggplot(df_pred,aes(y='y',ymax='y_upper',ymin='y_lower', x='week',group='subreddit'))
p = p + geom_line(aes(y='N_authors',x='week',color='subreddit',group='subreddit'), data=vardata.df_fit, inherit_aes=False, linetype='solid')
p = p + geom_line() + geom_ribbon(alpha=0.2) + geom_line(aes(color='subreddit'), linetype='dotted',size=1.5)
p = p + theme(legend_position = 'top',panel_background=element_rect(fill='white'))
p.save("plots/test_forecast.png")

mut_forecast = test_seasonality_fit.extract(pars=['mut_forecast'])['mut_forecast']

srs = np.concatenate([np.repeat('eastside',mut_forecast.shape[1]), 
                      np.repeat('seahawks',mut_forecast.shape[1]),
                      np.repeat('seattle',mut_forecast.shape[1]),
                      np.repeat('soundersfc',mut_forecast.shape[1]),
                      np.repeat('washington',mut_forecast.shape[1])])

mut_df = pd.DataFrame({'mut':mut_forecast.mean((0)).flatten(),
                       'upper':np.quantile(mut_forecast,0.95,(0)).flatten(),
                       'lower':np.quantile(mut_forecast,0.05,(0)).flatten(),
                       'subreddit':srs,
                       'week':vardata.df_forecast.week})


p = ggplot(mut_df, aes(y='mut',ymax='upper',ymin='lower',x='week',group='subreddit',color='subreddit'))
p = p + geom_line()
p = p + geom_ribbon(alpha=0.2)
p = p + theme(legend_position = 'top',panel_background=element_rect(fill='white'))
p.save("plots/test_mut.png")

lambda_forecast = test_seasonality_fit.extract(pars=['lambda_new'])['lambda_new']

lambda_df = pd.DataFrame({'lambda':lambda_forecast.mean((0)).flatten(),
                          'upper':np.quantile(lambda_forecast,0.95,(0)).flatten(),
                          'lower':np.quantile(lambda_forecast,0.05,(0)).flatten(),
                          'subreddit':srs,
                          'week':vardata.df_forecast.week})

p = ggplot(lambda_df, aes(y='lambda',ymax='upper',ymin='lower',x='week',group='subreddit',color='subreddit'))
p = p + geom_line()
p = p + geom_ribbon(alpha=0.2)
p = p + theme(legend_position = 'top',panel_background=element_rect(fill='white'))
p.save("plots/test_lambda_forecast.png")


lambda_est = test_seasonality_fit.extract(pars=['lambda'])['lambda']


srs2 = np.concatenate([np.repeat('eastside',lambda_est.shape[1]), 
                      np.repeat('seahawks',lambda_est.shape[1]),
                      np.repeat('seattle',lambda_est.shape[1]),
                      np.repeat('soundersfc',lambda_est.shape[1]),
                      np.repeat('washington',lambda_est.shape[1])])


lambda_est_df = pd.DataFrame({'lambda':lambda_est.mean((0)).flatten(),
                              'upper':np.quantile(lambda_est,0.95,(0)).flatten(),
                              'lower':np.quantile(lambda_est,0.05,(0)).flatten(),
                              'subreddit':srs2,
                              'week':test_df_fit.week})

p = ggplot(lambda_est_df, aes(y='lambda',ymax='upper',ymin='lower',x='week',group='subreddit',color='subreddit'))
p = p + geom_line()
p = p + geom_ribbon(alpha=0.2)
p = p + theme(legend_position = 'top',panel_background=element_rect(fill='white'))
p.save("plots/test_lambda_est.png")
