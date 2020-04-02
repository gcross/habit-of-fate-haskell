#!/bin/sh
sed -i "s/’/'/g" $1
sed -i 's/“/"/g' $1
sed -i 's/“/"/g' $1
sed -i 's/”/"/g' $1
sed -i 's/…/.../g' $1
#fold -s -w 80 "$1" > "$1.wrapped"
#mv "$1.wrapped" "$1"
