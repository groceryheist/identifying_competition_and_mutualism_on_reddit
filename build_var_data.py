from datetime import datetime, timedelta
import pandas as pd
import numpy as np
from pyRemembeR import Remember
from var_data import VarData
import pickle

import fire

def build_var_data(indata="data/included_timeseries.feather",output='data/var_stan_data.pickle',min_date='2011-06-01',max_date='2015-12-01',forecast_date='2016-06-01',remember_prefix=''):
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
    out_fit = vardata.df_fit.reset_index()
    out_fit.to_feather(f"{output.replace('.pickle','')}_fit.feather")
    out_forecast = vardata.df_forecast.reset_index()
    out_forecast.to_feather(f"{output.replace('.pickle','')}_forecast.feather")

    test_df = df.loc[df.subreddit.isin(['seattle','seahawks'])]
    test_vardata = VarData.from_df(test_df,min_date,fit_date,forecast_date)
    pickle.dump(test_vardata, open('data/var_stan_testdata.pickle','wb'))
    out_fit = test_vardata.df_fit.reset_index()
    out_fit.to_feather(f"{output.replace('.pickle','')}_test_fit.feather")
    out_forecast = test_vardata.df_forecast.reset_index()
    out_forecast.to_feather(f"{output.replace('.pickle','')}_test_forecast.feather")


if __name__=='__main__':
    fire.Fire(build_var_data)
