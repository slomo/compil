#!/bin/sh
BASEDIR=$(dirname $0)

ERL_PATHS="-pz ${BASEDIR} -pz ${BASEDIR}/lfe"

if [ $# -eq 0 ]; then
    erl ${ERL_PATHS}
else
    erl -noshell -s compil main $1 $2 -b c -s init stop ${ERL_PATHS}
fi

