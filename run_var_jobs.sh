#!/usr/bin/env bash
source ./bin/activate
./monitor_memory.sh &
parallel --jobs 7 < var_jobs_task_list 

