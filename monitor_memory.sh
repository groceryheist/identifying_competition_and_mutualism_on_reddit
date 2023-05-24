#!/bin/bash -e

## from https://stackoverflow.com/questions/1868210/how-to-log-the-memory-consumption-on-linux

echo "      date     time $(free -m | grep total | sed -E 's/^    (.*)/\1/g')" > mem_usage.txt
while true; do
    echo "$(date '+%Y-%m-%d %H:%M:%S') $(free -m | grep Mem: | sed 's/Mem://g')" >> mem_usage.txt
    sleep 1
done
