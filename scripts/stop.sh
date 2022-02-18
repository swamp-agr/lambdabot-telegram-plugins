#!/bin/bash

set -e
set -x

# read PID from file
PID=$(cat ./telegram-lambdabot.pid)

# stop process
kill $PID

# drop PID from file
echo "" > ./telegram-lambdabot.pid
