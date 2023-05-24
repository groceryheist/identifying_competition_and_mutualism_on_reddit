#!/usr/bin/env python3
from util import unpickle_or_create
import pystan

heaps_pois_seasonality = unpickle_or_create("stan_code/heaps_poisson_seasonality.pkl",
                                            overwrite=True,
                                            function=pystan.StanModel,
                                            file='stan_code/heaps_poisson_seasonality.stan',
                                            model_name='heaps_poisson_seasonality')

