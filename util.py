#!/usr/bin/env python3
import pandas as pd
import plotnine  # python ggplot2 clone
plotnine.options.figure_size = (10,7)
from  plotnine import *
theme_set(theme_minimal())
import subprocess
import logging
from datetime import datetime
import os
import pandas as pd
from  plotnine import *
theme_set(theme_minimal())
import pystan
import numpy as np
from scipy.stats import special_ortho_group
import pickle
import sys
import pystan
import numpy as np
import os
import pickle
import sys



def csv_to_stan_data(df,y_name,forecast_len=0):

    # we need to create zero rows where we don't have data 
    # we can use pivot_table to do this
    df = df.pivot_table(index='subreddit', columns='created_week',values=y_name,fill_value=0,aggfunc='sum').unstack()

    df = df.reset_index()
    df = df.rename(columns={0:'Y'})

    # let's start our analysis in October 2015

    df = df.loc[df.created_week >= datetime(2015,9,1)].reset_index()

    # lets try fitting a model to this data

    # transform the data into a NxK matrix 
    df_mat = df.pivot_table(index='created_week',columns='subreddit',values='Y',fill_value=0, aggfunc='sum')
    subs = df_mat.columns
    Y = np.array(df_mat)
    # drop the last (incomplete) week
    Y = Y[0:Y.shape[0]-1-forecast_len]
    K = Y.shape[1]
    N = Y.shape[0]
    return (df, subs, {'Y':Y.T,
                 'K':K,
                 'N':N,
                 'forecast_len':forecast_len})

# a community matrix is stable if no eigenvalues are greater than 1 
# this doesn' generate uniformly random stable matrixes, for
# more fun it generates matrixes with biased eigenvalues. 
# for uniform random stable matrixes set dist = np.random.uniform
def gen_fun_matrix(K,growth=0.5,growth_var=0.5, dist=None):
    # random eigenvalues on (-1,1)
    signs = np.random.binomial(1, 0.5, K)
    signs = np.power(-1,signs + 1)

    if dist is None:
        dist = lambda size: np.random.normal(loc=growth, scale=growth_var, size=size)

    lamda = dist(size=K)
    # the eigenvalues have to be less than 1 in magnitude
    rescale = np.abs(lamda).max()
    if rescale > 1:
        lamda = lamda / rescale

    lamda = signs * lamda

    # random orthogonal matrix
    Q = special_ortho_group.rvs(K)
    return np.dot(Q.T, np.diag(lamda) * Q)

def pd_round_week(s):
    s = np.where(s.dt.weekday==0,
                 s,
                 s - pd.DateOffset(weekday=0, weeks=1))
    s = pd.to_datetime(s)
    s = s.floor('D')
    return(s)

def plot_ar(fit, y_vec, true_forecast):

    y_gen = fit.extract(["y_new"])["y_new"]
    print(y_gen.shape)
    N = y_vec.shape[0]
    forecast_len = y_gen.shape[1]
    forecast_mean = y_gen.mean(0)
    forecast_upper = np.quantile(y_gen, 0.975, axis=0)
    forecast_lower = np.quantile(y_gen, 0.025, axis=0)

    labels = np.concatenate([np.repeat(str(i), N) for i in range(10)])
    df = pd.DataFrame({'y':forecast_mean,'Time step':N + np.arange(forecast_len),'ymax':forecast_upper,'ymin':forecast_lower})

    true_data=pd.DataFrame({'Time step':range(N+forecast_len), 'Hold-out':np.concatenate([y_vec, true_forecast])})

    p = ggplot(mapping=aes(x="Time step", y="y",ymax='ymax',ymin='ymin'), data=df)
    p = p + geom_line(color='red')
    p = p + geom_ribbon(alpha=0.5)
    p = p + geom_line(aes(x="Time step",y="Hold-out"), true_data, inherit_aes=False)
    #p = p + scale_color_manual(values=['red','black'],labels=['forecast','hold-out data'])
    p = p + scale_x_continuous() + theme(legend_position='bottom')
    p.save('plots/ar_1_nopriors_pred.pdf')
    p.draw()
    return p

def evolve_var_system(alpha, beta, sigma, y0, N, forecast_len, link_args=[], link = lambda x:x,nested_alpha=False):

    y_star = y0
    P = beta.shape[0]
    K = beta.shape[1]

    def y_next(y):

        if not nested_alpha:
            mut = alpha
            for i in range(P):
                mut = mut + np.matmul(y[i],beta[i])
            return np.random.multivariate_normal(mut, sigma)
        else:
            mut = alpha - alpha
            for i in range(P):
                mut = mut + np.matmul(y[i] - alpha, beta[i])
            return alpha + np.random.multivariate_normal(np.matmul(mut, beta[i]), sigma)

    for n in range(P,N):
        y_star.append(y_next(y_star))

    true_forecast = y_star[-(P+1):-1]
    for n in range(forecast_len):
        true_forecast.append(y_next(true_forecast))

    true_forecast = true_forecast[P:]
    y_star = np.column_stack(y_star)

    true_forecast = np.column_stack(true_forecast) 
        
    all_x = range(N + forecast_len)
    all_y_star = np.column_stack([y_star, true_forecast])
    y = link(y_star.T, *link_args)

    true_forecast = link(true_forecast.T, *link_args)
    
    true_forecast.shape
    all_y = np.row_stack([y, true_forecast])

    vardict = {'x':np.concatenate([all_x for i in range(K)]),
               'y_star':np.concatenate(all_y_star),
               'y':np.concatenate(all_y.T),
               'variable':np.concatenate([np.repeat(f"y{i}",N+forecast_len) for i in range(1,K+1)])
               }

    return (y,pd.DataFrame(vardict))

def build_forecast_df(y_new, N):
    forecast_len = y_new.shape[1]
    y_new_mean = y_new.mean(axis=0)
    y_new_upper = np.quantile(y_new, 0.975,axis=0)
    y_new_lower = np.quantile(y_new, 0.025,axis=0)

    df_pred = pd.DataFrame({
        'y' : np.concatenate([y_new_mean[:,i] for i in range(y_new_mean.shape[1])]),
        'y_upper' : np.concatenate([y_new_upper[:,i] for i in range(y_new_upper.shape[1])]),
        'y_lower' : np.concatenate([y_new_lower[:,i] for i in range(y_new_lower.shape[1])]),
        'x': np.concatenate([N + np.arange(forecast_len) for i in range(y_new.shape[2])]),
        'variable' : np.concatenate([np.repeat(f'y{i}',forecast_len) for i in range(1,1+y_new.shape[2])])
    })

    return df_pred

## numpy has a n, p parameterization
## for models we need a mu, theta parameterization
def convert_negbin_params(mu, theta):
    """
    Convert mean/dispersion parameterization of a negative binomial to the ones scipy supports

    See https://en.wikipedia.org/wiki/Negative_binomial_distribution#Alternative_formulations
    """
    r = theta
    var = mu + 1 / r * mu ** 2
    p = (var - mu) / var
    return r, 1 - p

def stan_negbin_var_predict(fit, N, mean_name='eta_new'):
    pars = fit.extract(['theta',mean_name])
    theta = pars['theta']
    eta_new = pars[mean_name]
    K = theta.shape[1]
    forecast_len = eta_new.shape[1]
    n_draws = eta_new.shape[0]

    y_new = np.empty_like(eta_new)
    for i in range(forecast_len):
        n, p = convert_negbin_params(np.exp(eta_new[:,i]), theta)
        y_new[:,i] = np.random.negative_binomial(n,p)

    return(build_forecast_df(y_new, N))


def stan_pois_var_predict(fit,N,param_name='lambda_new'):
    lambda_new = fit.extract(pars=[param_name])[param_name]
    y_new = np.random.poisson(np.exp(lambda_new))
    return build_forecast_df(y_new, N)

def stan_var_predict(fit,N):
    y_new = fit.extract('y_new')['y_new']
    return build_forecast_df(y_new, N)

### for testing stability is more important than sparsity
### figure out how to generate sparse stable matrixes later
### we might be able to do that by taking QR decompositions of sparse matrixes
def gen_system(K,N,sparsity=0.5,growth=3,growth_var=9,community=0.95, community_var=0.1,noise=3, seed=1234):

    beta = list()
    if type(community) is not list:
        beta.append(gen_fun_matrix(K, growth=community, growth_var=community_var))

    else:
        for i in range(len(community)):
            beta.append(gen_fun_matrix(K, growth=community[i], growth_var=community_var[i]))
        beta = np.array(beta)
        
    np.random.seed(seed)
    alpha = np.random.normal(growth,growth_var,K)
    # we want beta to be sparse
    # sparsify the betas

    sigma = np.array([np.random.normal(0,noise,K) for i in range(K)])
    sigma = np.dot(sigma,sigma.T)

    return (alpha,
            beta,
            sigma
    )

# save obj if filename not exists, or overwrite, else load
def unpickle_or_create(filename, overwrite, function, *args, **kwargs):
    if os.path.isfile(filename) and not overwrite:
        return pickle.load(open(filename, 'rb'))
    else:
        obj = function(*args, **kwargs)
        pickle.dump(obj, open(filename,'wb'))
        return obj

def git_hash(short=False):
    if short:
        return subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode().strip()
    else:
        subprocess.check_output(['git', 'rev-parse', 'HEAD']).decode().strip()


def init_logging(args):
    #handle -W
    if args.logging_destination:
        logging.basicConfig(filename=args.logging_destination, filemode='a', level=args.logging_level)
    else:
        logging.basicConfig(level=args.logging_level)

    export_git_hash = subprocess.check_output(['git', 'rev-parse', 'HEAD']).decode().strip()
    export_git_short_hash = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode().strip()
    export_time = str(datetime.now())

    logging.info(f"Starting at {export_time}.")
    logging.info(f"Last commit: {export_git_hash}")
    return(logging)

def get_loglevel(arg_loglevel):
    loglevel_mapping = { 'debug' : logging.DEBUG,
                         'info' : logging.INFO,
                         'warning' : logging.WARNING,
                         'error' : logging.ERROR,
                         'critical' : logging.CRITICAL }

    if arg_loglevel in loglevel_mapping:
        loglevel = loglevel_mapping[arg_loglevel]
        return loglevel
    else:
        print("Choose a valid log level: debug, info, warning, error, or critical", file=sys.stderr)
        return logging.INFO


                            
