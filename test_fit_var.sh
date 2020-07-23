#!/usr/bin/env bash 
python3 fit_var.py --p 1 --iter 1500 --adapt-delta 0.8 --max_treedepth 10 --infile data/var_stan_testdata.pickle &> stan_logs/test.log
