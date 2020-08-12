from datetime import datetime, timedelta
import pandas as pd
import numpy as np
from pyRemembeR import Remember
from var_data import VarData
import pickle

import fire

def build_var_data(indata="data/included_timeseries.feather",output='data/var_stan_data.pickle',min_date='2011-06-1',max_date='2015-12-1',forecast_date='2016-06-1',remember_prefix=''):
    remember = Remember()

    df = pd.read_feather(indata)
    df = df.rename(mapper= lambda n: n.replace('.','_'),axis='columns')

    min_date = datetime.fromisoformat(min_date)
    fit_date = datetime.fromisoformat(max_date)
    forecast_date = datetime.fromisoformat(forecast_date)

    remember(min_date,f'{remember_prefix}min_date')
    remember(fit_date,'{remember_prefix}max_date')
    remember(forecast_date,'{remember_prefix}forecast_date')

    vardata = VarData.from_df(df,min_date,fit_date,forecast_date)

    pickle.dump(vardata, open(output,'wb'))

# test_df = df.loc[df.subreddit.isin(['seattle','seahawks'])]
# test_vardata = VarData.from_df(test_df,min_date,fit_date,forecast_date)
# pickle.dump(test_vardata, open('data/var_stan_testdata.pickle','wb'))

if __name__=='__main__':
    fire.Fire(build_var_data)
