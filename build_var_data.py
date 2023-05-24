from datetime import datetime, date, timedelta
import pandas as pd
import numpy as np
from pyRemembeR import Remember
from var_data import VarData
import pickle

import fire

def build_var_data(indata="data/included_timeseries.feather",output='data/var_stan_data.pickle',min_date='2011-06-01',max_date='2015-12-01',forecast_date='2016-06-01',include_all_reddit=False, remember_prefix=''):
    remember = Remember()

    df = pd.read_feather(indata)
    df = df.loc[~df.week.isnull()]
    df = df.rename(mapper= lambda n: n.replace('.','_'),axis='columns')

    print(include_all_reddit)
    print(sorted(set(df.subreddit)))
    if not include_all_reddit:
        df = df.loc[df.subreddit != 'all.reddit']

    min_date = date.fromisoformat(min_date)
    fit_date = date.fromisoformat(max_date)
    forecast_date = date.fromisoformat(forecast_date)

    remember(min_date,f'{remember_prefix}min_date')
    remember(fit_date,'{remember_prefix}max_date')
    remember(forecast_date,'{remember_prefix}forecast_date')

    vardata = VarData.from_df(df,min_date,fit_date,forecast_date)

    pickle.dump(vardata, open(output,'wb'))
    out_fit = vardata.df_fit.reset_index()
    out_fit.to_feather(f"{output.replace('.pickle','')}_fit.feather")
    out_forecast = vardata.df_forecast.reset_index()
    out_forecast.to_feather(f"{output.replace('.pickle','')}_forecast.feather")

    if ('seattle' in set(df.subreddit)) and ('seattlewa' in set(df.subreddit)):

        test_df = df.loc[df.subreddit.isin(['seattle','seattlewa','mariners'])]
        test_vardata = VarData.from_df(test_df,min_date,fit_date,forecast_date)
        pickle.dump(test_vardata, open('data/var_stan_testdata.pickle','wb'))
        out_fit = test_vardata.df_fit.reset_index()
        out_fit.to_feather(f"{output.replace('.pickle','')}_test_fit.feather")
        out_forecast = test_vardata.df_forecast.reset_index()
        out_forecast.to_feather(f"{output.replace('.pickle','')}_test_forecast.feather")


    if ('wallpaper' in set(df.subreddit)) and ('wallpapers' in set(df.subreddit)):

        test_df = df.loc[df.subreddit.isin(['wallpaper','wallpapers'])]
        test_vardata = VarData.from_df(test_df,min_date,fit_date,forecast_date)
        pickle.dump(test_vardata, open('data/var_stan_testdata.pickle','wb'))
        out_fit = test_vardata.df_fit.reset_index()
        out_fit.to_feather(f"{output.replace('.pickle','')}_test_fit.feather")
        out_forecast = test_vardata.df_forecast.reset_index()
        out_forecast.to_feather(f"{output.replace('.pickle','')}_test_forecast.feather")



if __name__=='__main__':
    fire.Fire(build_var_data)
