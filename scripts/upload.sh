#!/bin/sh
stack build :test :habit-server && ssh -i "~/.ssh/Habit of Fate.pem" $1 "mkdir -p bin" && scp -i "~/.ssh/Habit of Fate.pem" .stack-work/install/x86_64-linux/lts-11.6/8.2.2/bin/habit-server $1:\~/bin && scp -r -i "~/.ssh/Habit of Fate.pem" .stack-work/install/x86_64-linux/lts-11.6/8.2.2/share/x86_64-linux-ghc-8.2.2/habit-of-fate-0.1.0.0/web $1:\~
