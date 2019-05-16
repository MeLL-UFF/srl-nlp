#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"

FN_VERSION="1.5"
FN_DATA="${HOME}/Code/MD/srl_nlp/srl_nlp/framenet/fndata-${FN_VERSION}"

DOCS_FOLDER_PATH="${HOME}/DataSets/framenet_docs/paper"

CPUS=2
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

set -e # Fail fast: exits if a simple command exits with a nonzero exit value.
       # A simple command is any command not part of an if, while, or until test, or part of an && or || list.


########################################################################################################################
# Black Box methods
## Copy current data to folders


SESAME_FOLDER="${HOME}/Code/MD/open-sesame"
REL_SET_NAMES=(STRONG WEAK ALL subframe inheritance causative perspective precedence use)
#AUGMENTATIONS=(lexical syntactic semantic full)

if [ "$#" -lt 1 ]
    then
    AUGMENTATIONS=(lexical syntactic semantic full)
    else
    AUGMENTATIONS=("${@: 1}")
fi

echo "Using ${AUGMENTATIONS[*]} augmentation(s)"

# ----------------------------------------------------------------------------------------------------------------------
sesame_setup(){
#$1 is the aug_folder folder to where to store sesame
    rm -r -f "${1}/methods/sesame"                                                # OPEN SESAME FOLDER STRUCTURE SETTING
    mkdir -p "${1}/methods/sesame"
    echo -e "\t\t\t Setting up Sesame"
    for f in $( ls -1 ${SESAME_FOLDER} )
    do
        ln -s "${SESAME_FOLDER}/${f}" "${1}/methods/sesame/${f}"
    done
    rm -f -d "${1}/methods/sesame/data"
    mkdir -p "${1}/methods/sesame/data/fndata-${FN_VERSION}"
        for f in $(ls -1 -d ${SESAME_FOLDER}/data/glove*)
        do
            ln -s "${f}" "${1}/methods/sesame/data"
        done
        for f in $( ls -1 -d ${FN_DATA}/* )
        do
            ln -s "${f}" "${1}/methods/sesame/data/fndata-${FN_VERSION}"
        done
        local fulltext_path="${1}/methods/sesame/data/fndata-${FN_VERSION}/fulltext"
        rm -f -d ${fulltext_path}
        mkdir -p ${fulltext_path}
            for mode in train dev
            do
                for f in $( ls -1 -d ${1}/${mode}/*.xml )
                do
                    ln -s "${f}" "${fulltext_path}"
                done
            done
            for f in $( ls -1 -d ${BK_PATH}/test/*.xml )
                do
                    ln -s "${f}" "${fulltext_path}"
                done
    rm -f -d "${1}/methods/sesame/configurations"
    cp -r    "${SESAME_FOLDER}/configurations" "${1}/methods/sesame/configurations"
    sed -e   's/"version.*/"version": '${FN_VERSION}',/' -i \
             ${1}/methods/sesame/configurations/global_config.json
    rm -f -d "${1}/methods/sesame/logs"
    mkdir -p "${1}/methods/sesame/logs"
}                                                                               # END OF SESAME FOLDER STRUCTURE SETTING
# ----------------------------------------------------------------------------------------------------------------------


rdn_setup(){
#$1 is the aug_folder folder to where to store sesame
    local rdn_exps="${1}/methods/rdn"
    rm -r -f ${rdn_exps}                                                          # OPEN SESAME FOLDER STRUCTURE SETTING
    mkdir -p ${rdn_exps}
    echo -e "\t\t\t Setting up RDNBoost"
    for f in $( ls -1 ${RDN_FOLDER}/*.jar )
    do
        ln -s "${RDN_FOLDER}/${f}" "${rdn_exps}/${f}"
    done
    mkdir -p ${rdn_exps}/configs
    cp ${RDN_FOLDER}/config/*.txt ${rdn_exps}/configs

    for f in $( ls -1 ${rdn_exps}/*.jar )
    do
        local dataset_name=${f%.*}
        local config_name=${dataset_name#"config"}
        dataset_name=dataset_${config_name}
        local dataset_path=${rdn_exps}/dataset_${config_name}
        mkdir -p ${dataset_path}
        for f in $( ls -1 ${rdn_exps}/*.jar )
        do
            ln -s "${rdn_exps}/${f}" "${dataset_path}/${f}"
        done
        ln -s "${rdn_exps}/config/${f}" "${dataset_path}/background.txt"
        ln -s "${rdn_exps}/framenet_facts.txt" ${dataset_path}
        ln -s "${rdn_exps}/lex_facts.txt" ${dataset_path}
        mkdir -p "${dataset_path}/train_original/"
        for mode in train dev
        do
            for f in $( ls -1 ${1}/${mode} )
            do
                ln -s "${1}/${mode}/${f}" "${dataset_path}/train_original/${f}"
            done
        done
        local aug_name=$(basename ${1})
        JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name rdn_${aug_name}_${config_name} -proj srl"
#        JOBHEADER=""
        ${JOBHEADER} python ${dataset_path}/train_original/ \
            --root  ${dataset_path} \
            --num_cpus CPUS \
            --log rdn_${aug_name}_${config_name}.log -v
    done

    rm -f -d "${1}/methods/sesame/configurations"
    cp -r    "${SESAME_FOLDER}/configurations" "${1}/methods/sesame/configurations"
    sed -e   's/"version.*/"version": '${FN_VERSION}',/' -i \
             ${1}/methods/sesame/configurations/global_config.json
    rm -f -d "${1}/methods/sesame/logs"
    mkdir -p "${1}/methods/sesame/logs"
}

########################################################################################################################
# Black Box methods

echo "Building black-box model environment"

BK_PATH="${ROOT_PATH}/fn${FN_VERSION}"
mkdir -p "${BK_PATH}/test"

echo -e "\t Copying test files"
cp ${DOCS_FOLDER_PATH}/test/*.xml "${BK_PATH}/test"

if ! [ -d "${BK_PATH}/current/" ]
then
    echo -e "\t Creating 'current' folder with training and dev files"
    for mode in train dev
    do
        echo -e "\t\t Copying current ${mode} files"
        mkdir -p "${BK_PATH}/current/${mode}"
        cp ${DOCS_FOLDER_PATH}/${mode}/*.xml "${BK_PATH}/current/${mode}"
    done
    echo -e  "\t\t Creating black-box model folders'"
    echo -e  "\t\t\t Sesame on current FrameNet data"
    mkdir -p "${BK_PATH}/current/methods/sesame"
    sesame_setup "${BK_PATH}/current"
    mkdir -p "${BK_PATH}/current/methods/semafor"
else
    echo -e "\t Skipping 'current' folder creation"
fi
echo -e "\t Running aumentations"

for idx in "${!REL_SET_NAMES[@]}"
do
    rel_set_name=${REL_SET_NAMES[${idx}]}
    echo -e  "\t\t Augmentation '${rel_set_name}'"
    for augmentation in ${AUGMENTATIONS}
    do
        echo -e  "\t\t\t kind '${augmentation}'"
        aug_folder="${BK_PATH}/aug_${rel_set_name}_${augmentation}"
        trap 'kill $(jobs -p)' SIGINT           # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT
        for mode in train dev
        do
            echo -e "\t\t\t\t Mode: '${mode}'"
            mkdir -p ${aug_folder}/${mode}
            JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name gen_${augmentation}_${rel_set_name}_${mode} -proj srl"
#            JOBHEADER=""
            ${JOBHEADER} python -W ignore srl_nlp/resource_augmentation.py ${FN_DATA} \
                ${augmentation} --doc_paths ${DOCS_FOLDER_PATH}/${mode} ${aug_folder}/${mode} \
                --relations ${rel_set_name}  \
                --num_cpus ${CPUS} \
                --single_sentences \
                --log ${aug_folder}/${rel_set_name}_${mode}.log -v&
        done
        wait                                                                # WAITS FOR ALL PARALLEL CALLS TO FINISH
        echo -e "\t\t\t Done with modes"

        if [ -d "${aug_folder}/train" ]
        then
            echo -e  "\t\t\t\t Creating model folders"
            sesame_setup "${aug_folder}"
            # rdn_setup "${aug_folder}"
        else
            echo -e  "\t\t\t\t ${aug_folder}/[train, dev] not created. Skiping model folders"
        fi
    done
done
