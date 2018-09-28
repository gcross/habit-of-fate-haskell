#!/bin/sh
sudo bin/habit-server --cert /etc/letsencrypt/live/habitoffate.com/fullchain.pem --key /etc/letsencrypt/live/habitoffate.com/privkey.pem --data done --host habitoffate.com
