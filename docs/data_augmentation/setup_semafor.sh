#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"

FN_VERSION="1.5"
FN_DATA="${HOME}/Code/MD/srl_nlp/srl_nlp/framenet/fndata-${FN_VERSION}"

CPUS=8
MEM=128
# QUEUE=x86_24h
QUEUE=x86_7d


########################################################################################################################
# Python environment activation

echo "Activating python environment"
echo
. activate srl_nlp

set -e # Fail fast: exits if a simple command exits with a nonzero exit value.
       # A simple command is any command not part of an if, while, or until test, or part of an && or || list.


########################################################################################################################
# Folder constrains

if [ "$#" -lt 1 ]
    then
    FOLDER_CONSTRAINS=''
    else
    FOLDER_CONSTRAINS=("${*: 1}")
fi

echo "Using '${FOLDER_CONSTRAINS}'constrain"

# ----------------------------------------------------------------------------------------------------------------------
semafor_config_setup(){
#$1 is the aug_folder folder to where to store semafor
    local semafor_exps="${1}"

    for f in $( ls -1 ${semafor_exps}/configs/*.txt )
    do
        local dataset_name=${f%.*}
        local config_name=$(basename ${dataset_name#"config"})
        echo -e "\t Building folder ${dataset_path}"
        dataset_name="dataset_${config_name}"
        local dataset_path=${semafor_exps}/dataset_${config_name}
        mkdir -p ${dataset_path}
        mkdir -p ${dataset_path}/train
        for f in $( ls -1 ${semafor_exps}/*.jar )
        do
            ln -sf "${f}" "${dataset_path}/"
        done
        ln -sf "${semafor_exps}/configs/${config_name}.txt" "${dataset_path}/background.txt"
        ln -sf "${semafor_exps}/framenet_facts.txt" ${dataset_path}
        ln -sf "${semafor_exps}/lex_facts.txt" ${dataset_path}
        cp ${semafor_exps}/train/*.txt ${dataset_path}/train
    done
}
# ----------------------------------------------------------------------------------------------------------------------

trap 'kill $(jobs -p)' SIGINT                   # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT
for exp in $(find ${ROOT_PATH}/fn${FN_VERSION} -name semafor -type d | grep ${FOLDER_CONSTRAINS})
    do
    (
    echo "Configuring semafor on ${exp}"
    cd ${exp}
    aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/semafor|\1|")
    if [ "$(ls -1 ${exp}/train | wc -l)" -gt 1 ]
    then
        semafor_config_setup "${exp}"
    else
        echo -e  "\t ${exp}/[train, dev] not filled. Skiping model folders"
    fi
    ) &
done
wait