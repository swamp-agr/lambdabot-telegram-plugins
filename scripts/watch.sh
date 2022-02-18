#!/bin/bash

set -e

# get bot PID from ps output.
TELEGRAM_LAMBDABOT_BOT_PID=$(ps aux | grep 'telegram-lambdabot' | grep -v grep | awk '{print $2}')

# test if process is running.
if ! [ -n "$TELEGRAM_LAMBDABOT_BOT_PID" ];
then
    . ./scripts/stop.sh
    sleep 5;
    . ./scripts/start.sh
fi
