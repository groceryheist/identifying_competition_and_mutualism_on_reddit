from util import *
import pickle
from plotnine import *
import pystan
import pandas as pd
import os
os.sched_setaffinity(0,range(os.cpu_count()))

posts = pd.read_csv("bernie_subs_posts_per_week.csv", parse_dates=['created_week'])
posts, subs, data = csv_to_stan_data(posts,'id',forecast_len=14)

sm_pois = pickle.load(open('stan_code/var_1_pois.pkl','rb'))
posts_fit_pois = pickle.load(open("stan_models/bernie_subs_posts_poisson.pkl",'rb'))

beta = posts_fit_pois.extract(['beta'])['beta']

# visualize the estimated community matrix

beta_hat = beta.mean(axis=0)
beta_upper = np.quantile(beta, 0.975, axis=0)
beta_lower = np.quantile(beta, 0.025, axis=0)

sig = np.sign(beta_upper) == np.sign(beta_lower)
beta_hat = pd.DataFrame(beta_hat)
beta_hat = beta_hat.rename(columns={i:sub for i,sub in enumerate(subs)})
pos = beta_hat > 0
neg = beta_hat < 0
blue = pos & sig
yellow = neg & sig

beta_hat['subreddit'] = list(subs)
beta_hat = beta_hat[ ['subreddit'] + list(subs)]

def my_style(df):
    attr1 = 'background-color: blue'
    attr2 = 'background-color: yellow'
    return pd.DataFrame(np.where(blue, attr1, np.where(yellow,attr2,'')),index=df.index,columns=df.columns)

beta_hat.style.apply(my_style,axis=None, subset=list(subs))



# Let's color the matrix according to sign and significance
