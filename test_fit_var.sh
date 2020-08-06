#!/usr/bin/env bash 
source ./bin/activate && python3 fit_var.py --p 2 --iter 1500 --adapt-delta 0.8 --max_treedepth 10 --infile data/var_stan_testdata.pickle --output=test_stan &> stan_logs/test.log
