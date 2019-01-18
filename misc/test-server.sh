#!/bin/sh
stack build :habit-server && sudo stack exec habit-server --allow-different-user -- --data gamedata --cert certs/device.crt --key certs/device.key
