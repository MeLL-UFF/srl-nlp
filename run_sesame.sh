#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"
FN_VERSION="1.5"

#TRAIN_MODE='--mode refresh'
TRAIN_MODE=''

set -e # Fail fast: exits if a simple command exits with a nonzero exit value.
       # A simple command is any command not part of an if, while, or until test, or part of an && or || list.


########################################################################################################################
# Python environment activation

echo "Activating python environment"
echo
. activate srl_nlp_gpu


########################################################################################################################
#CCC

CPUS=1
#GPUS=4
MEM=64
QUEUE=x86_12h

########################################################################################################################
### for model in targetid frameid argid

if [ "$#" -lt 1 ]
    then
    FOLDER_CONSTRAINS=''
    else
    FOLDER_CONSTRAINS=("${*: 1}")
fi

echo "Folder constrains: '$FOLDER_CONSTRAINS'"
trap 'kill $(jobs -p)' SIGINT                   # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT

for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name sesame -type d | grep ${FOLDER_CONSTRAINS})
    do
    (
    echo "Running Sesame on ${exp}"
    cd ${exp}
    aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/sesame|\1|")
    if [ "$(ls -1 logs | wc -l)" -le 1 ]
    then
        echo -e "\t Preparing data"
        python -m sesame.preprocess;
    else
        echo -e "\t Data already prepared"
    fi

    for model in frameid targetid argid
        do
        JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name train_${aug_name}_${model} -proj sesame"
#        JOBHEADER=""
        echo -e "\t ${JOBHEADER} train"
        ${JOBHEADER} python -m sesame.${model}  --mode train --model_name ${model} ${TRAIN_MODE}
        done
    ) &
    wait                                                                        # WAITS FOR ALL PARALLEL CALLS TO FINISH
done
