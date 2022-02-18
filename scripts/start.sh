#!/bin/bash

set -e

# output errors and logs in the same stream
exec 2>&1

# prepare log directory
mkdir -p ./log

telegram-lambdabot -l DEBUG +RTS -A64m -AL256m -qn2 -RTS >> log/telegram-lambdabot.log &

# write PID to file
echo $! > ./telegram-lambdabot.pid
