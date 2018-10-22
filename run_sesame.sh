#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"
FN_VERSION="1.5"


########################################################################################################################
# Python environment activation

echo "Activating python environment"
echo
. activate srl_nlp

set -e # Fail fast: exits if a simple command exits with a nonzero exit value.
       # A simple command is any command not part of an if, while, or until test, or part of an && or || list.


########################################################################################################################

#CCC
CPUS=4
GPUS=0
MEM=64
QUEUE=x86_24h

#Train & Test
TRAIN=1
TEST=0
if [ "$#" -lt 1 ]
    then
    FOLDER_CONSTRAINS=''
    else
    FOLDER_CONSTRAINS=("${*: 1}")
fi
echo "Folder constrains: '$FOLDER_CONSTRAINS'"
if [ ${TRAIN} -gt 0 ]
then
    trap 'kill $(jobs -p)' SIGINT                   # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT
    for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name sesame -type d | grep ${FOLDER_CONSTRAINS})
        do
        (
        echo "Running Sesame on ${exp}"
        cd ${exp}
        aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/sesame|\1|")
        if [ "$(ls -1 logs | wc -l)" -le 1 ]
        then
            echo -n "\t Preparing data"
            python -m sesame.preprocess;
        else
            echo -n "\t Data already prepared"
        fi
        for model in targetid frameid argid
            do
            JOBHEADER="jbsub -cores ${CPUS}+${GPUS} -mem ${MEM}g -q ${QUEUE} -name train_${aug_name}_${model} -proj sesame"
            #JOBHEADER=""
            echo -n "\t ${JOBHEADER} train"
            ${JOBHEADER} "python -m sesame.${model} --mode train --model_name ${model} --mode refresh"
            done
        ) &
    done
    wait                                                                        # WAITS FOR ALL PARALLEL CALLS TO FINISH
fi

if [ ${TEST} -gt 0 ]
then
    for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name sesame -type d | grep ${FOLDER_CONSTRAINS})
        do
        (
        echo "Running Sesame on ${exp}"
        cd ${exp}
        aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/sesame|\1|")
        for model in targetid frameid argid
            do
            echo "Testing ${model}"
            #JOBHEADER="jbsub -cores ${CPUS}+${GPUS} -mem ${MEM}g -q ${QUEUE} -name test_${aug_name}_${model} -proj sesame"
            JOBHEADER=""
            ${JOBHEADER} python -m sesame.${model} --mode test --model_name ${model}
            done
        )
        done
fi