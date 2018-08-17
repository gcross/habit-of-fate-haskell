#!/bin/sh
grep $1 `find sources -name '*.hs'`
