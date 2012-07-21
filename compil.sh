#!/bin/sh
BASEDIR=$(dirname $0)

# append paths
ERL_PATHS="-pz ${BASEDIR} -pz ${BASEDIR}/lfe"

# check number of parameters
if [ $# -eq 0 ]; then
    # start shell
    erl ${ERL_PATHS}
else
    # run compiler
    erl -noshell -s compil main $1 $2 -b c -s init stop ${ERL_PATHS}
fi

