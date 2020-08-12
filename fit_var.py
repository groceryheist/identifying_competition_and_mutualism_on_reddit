#!/usr/bin/env python3
from var_data import VarData
from datetime import datetime, timedelta
import pandas as pd
from util import unpickle_or_create
import numpy as np
import fire
import pickle
import pystan

def fit_var(p, chains=4, iter=3000, adapt_delta=0.97, max_treedepth=14, refresh=100, infile='data/var_stan_data.pickle', output=None):

    if output is None:
        output = f"var_stan_p{p}"

    np.set_printoptions(precision=None, suppress=True)
    
    vardata = pickle.load(open(infile,'rb'))
    stan_data = vardata.stan_data

    ## add priors
    stan_data = {**stan_data,
                 **{'p':p,
                    'm_diag':np.repeat(0,stan_data['y'].shape[0]),
                    'scale_diag':1,
                    'scale_offdiag':0,
                    'df':10,
                    'es' : [1,0], # top-level prior for the means of the means (diag, off-diag)
                    'fs' : [np.sqrt(1.4), np.sqrt(1.4)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
                    'gs' : [2.1,2.1], # top-level prior for the position of the scales,
                    'hs' : [1/3,1/3], # top-level prior for the precision of the scales,
                    'df':stan_data['m']+3,
                    'alpha':0,
                    'sd0': 7, # hyper prior precision of mu0
                    'g0':4, # hyper prior for variance of m0
                    'h0':3 # hyper prior for variance of m0} # degrees of freedom in the inverse wishart prior on the scale matrix.
                 }
    }
    
    
    heaps_pois_seasonality  = unpickle_or_create("stan_code/heaps_poisson_seasonality.pkl",
                                                 overwrite=False,
                                                 function=pystan.StanModel,
                                                 file='stan_code/heaps_poisson_seasonality.stan',
                                                 model_name='heaps_poisson_seasonality')

    fit = heaps_pois_seasonality.sampling(data=stan_data,
                                          chains=chains,
                                          iter=iter,
                                          refresh=refresh,
                                          control={'adapt_delta':adapt_delta,
                                                   'max_treedepth':max_treedepth},
                                          seed=1773
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
