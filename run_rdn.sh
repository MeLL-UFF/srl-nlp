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
MEM=64
QUEUE=x86_7d

NUM_TREES=10

if [ "$#" -lt 1 ]
    then
    FOLDER_CONSTRAINS=''
    else
    FOLDER_CONSTRAINS=("${*: 1}")
fi
echo "Folder constrains: '$FOLDER_CONSTRAINS'"

trap 'kill $(jobs -p)' SIGINT                   # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT
for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name rdn -type d | grep ${FOLDER_CONSTRAINS})
    do
    echo "Running RDN on ${exp}"
    aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/rdn|\1|")

    for data_folder in $( ls -1 ${exp}/dataset_* -d)
    do
        (
        cd ${data_folder}
        (cd train
        ln -sf ../background.txt train_bk.txt
        echo " I am at $(pwd)"
        )
#            JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name train_${aug_name} -proj rdn"
        JOBHEADER=""
        echo -e "\t ${JOBHEADER} train"
        ${JOBHEADER} java -jar BoostSRL.jar -l -combine \
                          -train train/ \
                          -target frame_element_anno \
                          -trees ${NUM_TREES}
        )
    done
done