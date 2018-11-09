#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"

FN_VERSION="1.5"

DOCS_FOLDER_PATH="${HOME}/DataSets/framenet_docs/paper"
RDN_FOLDER="${HOME}/Code/MD/rdnboost"
CPUS=8
MEM=64
# QUEUE=x86_24h
QUEUE=x86_7d

BUILD_BLACKBOX=1
BUILD_SRL=0

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


rdn_setup(){
#$1 is the aug_folder folder to where to store rdn
    local rdn_exps="${1}"
    local aug_name=$(basename ${1})
    rm -r -f ${rdn_exps}                                                             # OPEN RDN FOLDER STRUCTURE SETTING
    mkdir -p ${rdn_exps}
    echo -e "\t\t\t Setting up RDNBoost on ${1}"
    for f in $( ls -1 ${RDN_FOLDER}/*.jar )
    do
        ln -s "${f}" "${rdn_exps}/"
    done
    echo "Copied jars"
    mkdir -p ${rdn_exps}/configs
    cp ${RDN_FOLDER}/configs/*.txt ${rdn_exps}/configs
    cp ${RDN_FOLDER}/*.txt ${rdn_exps}
    mkdir -p "${rdn_exps}/train_original/"
    for mode in train dev
        do
            for f in $( ls -1 ${1}/${mode}/*.xml )
            do
                ln -s "${f}" "${rdn_exps}/train_original/"
            done
        done
    echo "Running data base conversion of ${aug_name}"
        JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name rdn_${aug_name} -proj rdn"
#        JOBHEADER=""
        ${JOBHEADER} python -W ignore srl_nlp/generate_rdn_base.py ${rdn_exps}/train_original/ \
            --root  ${rdn_exps} \
            --num_cpus ${CPUS} \
            --log rdn_${aug_name}.log -v
}

# ----------------------------------------------------------------------------------------------------------------------


for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name rdn -type d | grep ${FOLDER_CONSTRAINS})
    do
    if [ "$(ls -1 ${exp}/train | wc -l)" -gt 1 ]
    then
        echo "Building rdn environment"
        rdn_setup "${exp}"
    else
        echo -e  "\t\t\t\t ${exp}/[train, dev] not filled. Skiping model folders"
    fi
        done
    done
done