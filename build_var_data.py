from datetime import datetime, timedelta
import pandas as pd
import numpy as np
from pyRemembeR import Remember
from var_data import VarData
import pickle

remember = Remember()

df = pd.read_feather("data/included_timeseries.feather")
df = df.rename(mapper= lambda n: n.replace('.','_'),axis='columns')

min_date = datetime(2011,6,1)
fit_date = datetime(2015,12,1)
forecast_date = datetime(2016,6,1)

remember(min_date,'min_date')
remember(fit_date,'max_date')
remember(forecast_date,'forecast_date')

vardata = VarData.from_df(df,min_date,fit_date,forecast_date)

pickle.dump(vardata, open('data/var_stan_data.pickle','wb'))

test_df = df.loc[df.subreddit.isin(['seattle','seahawks'])]
test_vardata = VarData.from_df(test_df,min_date,fit_date,forecast_date)
pickle.dump(test_vardata, open('data/var_stan_testdata.pickle','wb'))
