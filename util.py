#!/usr/bin/env python3
import subprocess
import logging
from datetime import datetime
import os
import pandas as pd
from  plotnine import *
theme_set(theme_minimal())
import pystan
import numpy as np
import pickle
import sys


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

def evolve_var_system(alpha,beta,sigma,y0,N,forecast_len, link = lambda x:x):
    y_star = [y0]
    K = beta.shape[0]

    def y_next(y0):
        return np.random.multivariate_normal(alpha + np.matmul(y0,beta), sigma)

    for n in range(1, N):
        y_star.append(y_next(y_star[-1]))

    true_forecast = [y_star[-1]]
    for n in range(forecast_len):
        true_forecast.append(y_next(true_forecast[-1]))

    true_forecast = true_forecast[1:]

    all_x = range(N + forecast_len)
    all_y_star = y_star + true_forecast
    y = np.apply_along_axis(link,0,y_star)
    true_forecast = np.apply_along_axis(link, 0, true_forecast)
    all_y = np.concatenate([y, true_forecast])

    vardict = {'x':np.concatenate([all_x for i in range(K)]),
               'y_star':np.concatenate(np.column_stack(all_y_star)),
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

def stan_pois_var_predict(fit,N):
    lambda_new = fit.extract(pars=['lambda_new'])['lambda_new']
    y_new = np.random.poisson(np.exp(lambda_new))
    return build_forecast_df(y_new, N)

def stan_var_predict(fit,N):
    y_new = fit.extract('y_new')['y_new']
    return build_forecast_df(y_new, N)

def gen_system(K,N,sparsity=0.5,growth=3,growth_var=10,community=0.3,noise=3,seed=1234):
    np.random.seed(seed)
    alpha = np.random.normal(growth,growth_var,K)
    # we want beta to be sparse
    # sparsify the betas
    beta_zeros = np.array([np.random.binomial(1,sparsity,K) for i in range(K)])
    beta = np.array([np.random.normal(0,community,K) for i in range(K)])
    beta = np.multiply(beta, beta_zeros)
    sigma = np.array([np.random.normal(0,noise,K) for i in range(K)])
    sigma = np.dot(sigma,sigma.T)
    return (alpha,
            beta,
            sigma)

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


