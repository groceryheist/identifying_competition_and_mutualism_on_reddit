#!/usr/bin/env bash
source ./bin/activate
parallel --jobs 7 < var_jobs_task_list
