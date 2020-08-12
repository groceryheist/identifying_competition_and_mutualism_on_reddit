#!/usr/bin/env bash
source ./bin/activate
./monitor_memory.sh &
parallel --jobs $2 < $1

