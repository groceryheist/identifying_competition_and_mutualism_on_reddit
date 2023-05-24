#!/usr/bin/env python3
from var_data import VarData
from datetime import datetime, timedelta
import pandas as pd
from util import unpickle_or_create
import numpy as np
import fire
import pickle
import pystan

def fit_var(p, chains=4, it=3000, adapt_delta=0.97, max_treedepth=14, refresh=100, infile='data/var_stan_data.pickle', output=None, seed=1773, model_type='heaps_pois'):

    if output is None:
        output = f"var_stan_p{p}"

    np.set_printoptions(precision=None, suppress=True)
    
    vardata = pickle.load(open(infile,'rb'))
    if type(vardata) is VarData:
        stan_data = vardata.stan_data
        subreddits = sorted(set(vardata.df_fit.subreddit))
        print(f"fitting models for {subreddits}")

    else:
        stan_data = vardata
        subreddits = [f'sr_{i}' for i in range(stan_data['m'])]

    ## add priors
    stan_data = {**stan_data,
                 **{'p':p,
                    'm_diag':np.repeat(0,p),
                    'm_offdiag':np.repeat(0,p),
                    's_diag':np.repeat(2,p),
                    's_offdiag':np.repeat(4,p),
                    'scale_diag':1,
                    'scale_offdiag':0,
                    'es' : [0,0], # top-level prior for the means of the means (diag, off-diag)
                    'fs' : [np.sqrt(3), np.sqrt(3)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
                    'gs' : [2.1,2.1], # top-level prior for the position of the scales,
                    'hs' : [0.33,0.33], # top-level prior for the precision of the scales,

                    # 'es' : [0,0], # top-level prior for the means of the means (diag, off-diag)
                    # 'fs' : [np.sqrt(7), np.sqrt(7)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
                    # 'gs' : [21,21], # top-level prior for the position of the scales,
                    # 'hs' : [60,60], # top-level prior for the precision of the scales,
                    'df':stan_data['m']+3,
                    'alpha':0,
                    'sd0': 7, # hyper prior precision of mu0
                    'g0':4, # hyper prior for variance of m0
                    'h0':3 # hyper prior for variance of m0} # degrees of freedom in the inverse wishart prior on the scale matrix.
                 }
    }
    
    print(stan_data)
    it = int(it)
    
    if model_type == 'heaps_gauss':
        stan_data['y'] = stan_data['y'].T
        
        model  = unpickle_or_create("stan_code/heaps_statprior_means.pkl",
                                    overwrite=False,
                                    function=pystan.StanModel,
                                    file='stan_code/heaps_statprior_means.pkl',
                                    model_name='heaps_gauss')


    if model_type == 'pois':
        model  = unpickle_or_create("stan_code/var_poisson_seasonality.pkl",
                                    overwrite=False,
                                    function=pystan.StanModel,
                                    file='stan_code/var_poisson_seasonality.stan',
                                    model_name='var_poisson_seasonality')


    if model_type == 'heaps_pois':
        stan_data['m_diag'] = np.repeat(0,stan_data['y'].shape[0])
        model  = unpickle_or_create("stan_code/heaps_poisson_seasonality.pkl",
                                    overwrite=False,
                                    function=pystan.StanModel,
                                    file='stan_code/heaps_poisson_seasonality.stan',
                                    model_name='heaps_poisson_seasonality')

    




    fit = model.sampling(data=stan_data,
                         chains=int(chains),
                         iter=int(it),
                         refresh=int(refresh),
                         control={'adapt_delta':float(adapt_delta),
                                  'max_treedepth':int(max_treedepth)},
                         seed=seed
                         )
    
    output_format = f"stan_models/{output}_stanmod.pickle"
    pickle.dump(fit, open(output_format,'wb'))
    # save a permuted dataframe for analysis
    fit_df = fit.to_dataframe(permuted=True)
    fit_df = fit_df.reset_index(drop=True)
    print(pystan.check_hmc_diagnostics(fit))
    fit_df.to_feather(f"stan_models/{output}_stanmod.feather")
    
if __name__ == "__main__":
    fire.Fire(fit_var)
