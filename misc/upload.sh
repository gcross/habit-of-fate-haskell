#!/bin/sh
scp -i "~/.ssh/Habit of Fate.pem" .stack-work/install/x86_64-linux/lts-12.16/8.4.4/bin/habit-server $1:/home/ubuntu/bin
scp -r -i "~/.ssh/Habit of Fate.pem" -r data/app $1:/home/ubuntu/data
