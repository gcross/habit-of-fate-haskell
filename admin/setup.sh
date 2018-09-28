#!/bin/sh
yum -y install tmux wget zsh
chsh -s /usr/bin/zsh ec2-user
wget https://dl.eff.org/certbot-auto -O bin/certbot-auto
chmod a+x bin/certbot-auto
