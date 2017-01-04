#!/bin/sh
stack build && (stack test || cat test.log)
