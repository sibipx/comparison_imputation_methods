import pandas as pd
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.ensemble import RandomForestRegressor
import time
import sys
from pympler import asizeof

def impute_IterativeImputer(df_train, df_test):

    # arrange columns in same order
    df_test = df_test[df_train.columns]

    # fit IterativeImputer
    start_fit = time.perf_counter()
    imp = IterativeImputer(estimator=RandomForestRegressor(n_estimators=100, n_jobs=-1),
                           verbose=0)
    imp.fit(df_train)
    end_fit = time.perf_counter()
    time_fit = end_fit - start_fit

    # impute train
    start_imp_train = time.perf_counter()
    df_train_imp = pd.DataFrame(imp.transform(df_train), columns=df_train.columns)
    end_imp_train = time.perf_counter()
    time_imp_train = end_imp_train - start_imp_train

    # impute test
    start_imp_test = time.perf_counter()
    df_test_imp = pd.DataFrame(imp.transform(df_test), columns=df_test.columns)
    end_imp_test = time.perf_counter()
    time_imp_test = end_imp_test - start_imp_test

    # make timings df
    df_timing = pd.DataFrame(columns=["train_fit", "train_predict", "test_predict"])
    df_timing.loc[0] = [time_fit, time_imp_train, time_imp_test]
    
    # object size
    object_size = asizeof.asizeof(imp)
    
    # list to store dataframes
    dfs = []
    
    dfs.append(df_train_imp)
    dfs.append(df_test_imp)
    dfs.append(df_timing)
    dfs.append(object_size)
    
    return dfs
