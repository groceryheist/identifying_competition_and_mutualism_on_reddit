import pandas as pd
from  plotnine import *
theme_set(theme_minimal())
import pystan
import numpy as np
import os
import pickle
import sys
from util import *
from datetime import datetime
from concurrent.futures import ProcessPoolExecutor, as_completed
os.sched_setaffinity(0,range(os.cpu_count()))

# do it for posts

posts = pd.read_csv("bernie_subs_posts_per_week.csv", parse_dates=['created_week'])
posts, data = csv_to_stan_data(posts,'id',forecast_len=14)

# plot the data
p = ggplot(posts, aes(x='created_week', y='Y', group='subreddit',color='subreddit')) + geom_line()
p.save('plots/bernie_subs_posts_per_week.pdf')

sm_negbin = pickle.load(open('stan_code/var_1_negbin.pkl','rb'))
sm_pois = pickle.load(open('stan_code/var_1_pois.pkl','rb'))
sm_norm = pickle.load(open('stan_code/var_1.pkl','rb'))

# let's fit these models in parallel
executor = ProcessPoolExecutor(os.cpu_count())

posts_fit_negbin = executor.submit(unpickle_or_create,
                                   "stan_models/bernie_subs_posts_negbin.pkl",
                                   function=sm_negbin.sampling,
                                   overwrite=True,
                                   data=data,
                                   chains=4,
                                   iter=2000,
                                   control={'adapt_delta':0.9,
                                            'max_treedepth':14})

posts_fit_pois = executor.submit(unpickle_or_create,
                                 "stan_models/bernie_subs_posts_poisson.pkl",
                                 function=sm_pois.sampling,
                                 overwrite=True,
                                 data=data,
                                 chains=4,
                                 iter=2000,
                                 control={'adapt_delta':0.9,
                                          'max_treedepth':14})

posts_fit_norm = executor.submit(unpickle_or_create,
                                 "stan_models/bernie_subs_posts_normal.pkl",
                                 function=sm_norm.sampling,
                                 overwrite=True,
                                 data=data,
                                 chains=4,
                                 iter=2000,
                                 control={'adapt_delta':0.9,
                                          'max_treedepth':14})

posters = pd.read_csv("bernie_subs_posters_per_week.csv", parse_dates=['created_week'])
posters, data = csv_to_stan_data(posters,'author_fullname',forecast_len=14)

# we need to create zero rows where we don't have data 
data['forecast_len'] = 40

posters_fit_negbin = executor.submit(unpickle_or_create,
                                     "stan_models/bernie_subs_posters_negbin.pkl",
                                     function=sm_negbin.sampling,
                                     overwrite=True,
                                     data=data,
                                     chains=4,
                                     iter=2000,
                                     control={'adapt_delta':0.9,
                                              'max_treedepth':14})

posters_fit_pois = executor.submit(unpickle_or_create,
                                   "stan_models/bernie_subs_posters_poisson.pkl",
                                   function=sm_pois.sampling,
                                   overwrite=True,
                                   data=data,
                                   chains=4,
                                   iter=2000,
                                   control={'adapt_delta':0.9,
                                            'max_treedepth':14})

posters_fit_norm = executor.submit(unpickle_or_create,
                                   "stan_models/bernie_subs_posters_normal.pkl",
                                   function=sm_norm.sampling,
                                   overwrite=True,
                                   data=data,
                                   chains=4,
                                   iter=2000,
                                   control={'adapt_delta':0.9,
                                            'max_treedepth':14})

for fut in as_completed([posts_fit_negbin,
                         posts_fit_pois,
                         posts_fit_norm,
                         posters_fit_negbin,
                         posters_fit_pois,
                         posters_fit_norm]):
    print(fut.result())
