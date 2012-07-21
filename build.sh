#!/bin/sh
BASEDIR=$(dirname $0)

# compile everything
erlc $BASEDIR/*.erl
erlc $BASEDIR/lfe/*.erl
