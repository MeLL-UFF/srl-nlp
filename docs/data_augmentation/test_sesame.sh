#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"
FN_VERSION="1.5"


########################################################################################################################
# Python environment activation

echo "Activating python environment"
echo
. activate srl_nlp

if [ "$#" -lt 1 ]
    then
    FOLDER_CONSTRAINS=''
    else
    FOLDER_CONSTRAINS=("${*: 1}")
fi

set -e # Fail fast: exits if a simple command exits with a nonzero exit value.
       # A simple command is any command not part of an if, while, or until test, or part of an && or || list.


########################################################################################################################

#CCC
CPUS=4
MEM=64
QUEUE=x86_7d

trap 'kill $(jobs -p)' SIGINT
for model in targetid frameid argid
do
    for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name sesame -type d | grep "${FOLDER_CONSTRAINS}" | grep -v 'test')
        do
        (
        echo "Running Sesame on ${exp} : ${model}"
        cd ${exp}
        aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/sesame|\1|")

            echo "Testing ${model}"
    #        JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name test_${aug_name}_${model} -proj sesame"
            JOBHEADER=""
            ${JOBHEADER} python -m sesame.${model} --mode test --model_name ${model}
    ) &
    done
    wait
done