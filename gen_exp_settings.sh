#!/usr/bin/env bash

#ROOT_PATH="${HOME}/Code/MD/srl_nlp/experiments"
ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"

FN_VERSION="1.5"
FN_DATA="${HOME}/Code/MD/srl_nlp/srl_nlp/framenet/fndata-${FN_VERSION}"

DOCS_FOLDER_PATH="${HOME}/DataSets/framenet_docs/paper"
CONFIG_FOLDER='' #TODO its for the RDN configs

CPUS=8
MEM=64
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
REL_SET_NAMES=(strong weak all)
IGNORED_RELS=(weak strong see_also)
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

if [ ${BUILD_BLACKBOX} -gt 0 ]
then
    echo "Building black-box model environment"

    BK_PATH="${ROOT_PATH}/fn${FN_VERSION}/blackbox"
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
        ignored_rel=${IGNORED_RELS[${idx}]}
        echo -e  "\t\t Augmentation '${rel_set_name}'"
        for augmentation in ${AUGMENTATIONS}
        do
            echo -e  "\t\t\t kind '${augmentation}'"
            aug_folder="${BK_PATH}/aug_${rel_set_name}_${augmentation}"
            trap 'kill $(jobs -p)' SIGINT           # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT
#            for mode in train dev
#            do
#                echo -e "\t\t\t\t Mode: '${mode}'"
#                mkdir -p ${aug_folder}/${mode}
#                JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name gen_${augmentation}_${ignored_rel}_${mode} -proj srl"
##                JOBHEADER=""
#                ${JOBHEADER} python -W ignore srl_nlp/resource_augmentation.py ${FN_DATA} \
#                    ${augmentation} --doc_paths ${DOCS_FOLDER_PATH}/${mode} ${aug_folder}/${mode} \
#                    --ignored_relations ${ignored_rel}  --num_cpus ${CPUS} \
#                    --log ${rel_set_name}_${augmentation}_${mode}.log -v&
#            done
#            wait                                                                # WAITS FOR ALL PARALLEL CALLS TO FINISH
            echo -e "\t\t\t Done with modes"

            echo -e  "\t\t\t\t Creating black-box model folders"
            sesame_setup "${aug_folder}"
            rm -r -f "${aug_folder}/methods/semafor"                             # OPEN SEMAFOR FOLDER STRUCTURE SETTING
            mkdir -p "${aug_folder}/methods/semafor"
                                                                                # END OF SESAME FOLDER STRUCTURE SETTING
        done
    done
fi
echo


########################################################################################################################
# SRL methods

if [ ${BUILD_SRL} -gt 0 ]
then
    echo "Building srl models environment"

    BK_PATH="${ROOT_PATH}/fn${FN_VERSION}/srl"
    CONFIG_FOLDER=""
    mkdir -p ${BK_PATH}
    mkdir -p "${BK_PATH}/test"                                                                      # COPY OF TEST FILES

    echo -e "\t Copying test files"
    cp ${DOCS_FOLDER_PATH}/test/*.xml "${BK_PATH}/test"

    echo -e "\t Copying train and dev files"
    for mode in train dev
    do
        echo -e "\t\t Copying current ${mode} files"
        mkdir -p "${BK_PATH}/current/${mode}"
        cp ${DOCS_FOLDER_PATH}/${mode}/*.xml "${BK_PATH}/current/${mode}"
    done

    echo -e "\t Running augmentations"                                                                   # AUGMENTATIONS
    REL_SET_NAMES=(strong weak all)
    IGNORED_RELS=(weak strong see_also)
    for idx in "${!REL_SET_NAMES[@]}"                                     # ITERATING OVER RELATION SETS AND THEIR NAMES
    do
        rel_set_name=${REL_SET_NAMES[${idx}]}
        ignored_rel=${IGNORED_RELS[${idx}]}
        echo -e  "\t\t Augmentation '${rel_set_name}'"
        for augmentation in lexical syntactic semantic full                      # ITERATING OVER TYPES OF AUGMENTATIONS
        do
            echo -e  "\t\t\t kind '${augmentation}'"
            aug_folder="${BK_PATH}/aug_${rel_set_name}_${augmentation}"
            mkdir -p ${aug_folder}
            for mode in train dev
            do
                echo -e "\t\t\t\t Mode: '${mode}'"
                mkdir -p ${aug_folder}/${mode}
#                python -W ignore srl_nlp/resource_augmentation.py ${FN_DATA} \
#                    ${augmentation} --doc_paths ${DOCS_FOLDER_PATH}/${mode} ${aug_folder}/${mode} \
#                    --ignored_relations ${ignored_rel}
            done
            for config in config0 config1 config2 #$(ls ${CONFIG_FOLDER})                                      # CONFIGS
            do
                echo -e "\t\t\t\t Config: '${config}'"
                mkdir -p ${aug_folder}/${config}
                echo -e  "\t\t\t\t\t Creating srl model folders'"                                          # SRL METHODS
                mkdir -p "${aug_folder}/${config}/methods/rdn_once"
                mkdir -p "${aug_folder}/${config}/methods/rdn_split"
#                mkdir -p "${aug_folder}/${config}/methods/amie_plus"
#                mkdir -p "${aug_folder}/${config}/methods/pra"
#                mkdir -p "${aug_folder}/${config}/methods/aleph"
#                mkdir -p "${aug_folder}/${config}/methods/propositionalization_aleph"
            done
        done
    done
fi




































