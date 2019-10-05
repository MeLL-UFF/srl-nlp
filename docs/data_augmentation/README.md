# Running experiments


## Create a new environment

```bash
conda create --name srl_nlp --file environment.yml
```

### Environment variables definition


The meaning of the variables is:
* `FN_DATA` and `FN_VERSION`: define the path to the specific FrameNet's version `FN_VERSION` files
* `ROOT_PATH`: path to all the experiments
* `BK_PATH`: path to experiments performed on a specific FrameNet's version
* `DOCS_FOLDER_PATH`: folder used in the construction of the different datasets.
   This folder must contain two folders, `train` and `test` each one containing the `.xml` files
* `REL_SET_NAMES`: list of relations to be used
* `AUGMENTATIONS`: list of augmentations to be tried
* `SESAME_FOLDER`: path to the sesame's root folder

```bash
FN_VERSION="1.5"
FN_DATA="${HOME}/Datasets/framenet/fndata-${FN_VERSION}"

ROOT_PATH="${HOME}/Projects/srl_nlp/experiments"
#ROOT_PATH="/dccstor/sallesd1/breno_tmp/experiments"
BK_PATH="${ROOT_PATH}/fn${FN_VERSION}"
DOCS_FOLDER_PATH="${HOME}/Datasets/framenet/fn_docs_exps"

REL_SET_NAMES=(STRONG WEAK ALL subframe inheritance causative perspective precedence use)
AUGMENTATIONS=(lexical syntactic semantic full)
SESAME_FOLDER="${HOME}/Projects/utils/open-sesame"
CPUS=2

```

#### CCC parameters
```bash
MEM=64
# QUEUE=x86_24h
QUEUE=x86_7d
```

### Testing



## Running augmentation methods


* Create test folder
```bash
mkdir -p "${BK_PATH}/test"

echo -e "\t Copying test files"
cp ${DOCS_FOLDER_PATH}/test/*.xml "${BK_PATH}/test"

```

* Copying files to base folder

```bash
if ! [[ -d "${BK_PATH}/current/" ]]
then
    echo -e "\t Creating 'current' folder with training and dev files"
    for mode in train dev
    do
        echo -e "\t\t Copying current ${mode} files"
        mkdir -p "${BK_PATH}/current/${mode}"
        cp ${DOCS_FOLDER_PATH}/${mode}/*.xml "${BK_PATH}/current/${mode}"
    done
    mkdir -p "${BK_PATH}/current/methods/"
else
    echo -e "\t Skipping 'current' folder creation"
fi
echo -e "\t Running augmentations"
```

* Create augmentation folders. This might take a while. The folder structure would like it:
>   * `ROOT_PATH`
>     * fn1.5
>       * aug_use_lexical
>         * dev
>           * file_name.xml
>           * ...
>         * train
>       * aug_use_syntactic
>         * dev
>           * file_name.xml
>           * ...
>         * train
>       ...
     
```bash
for idx in "${!REL_SET_NAMES[@]}"
do
    rel_set_name=${REL_SET_NAMES[${idx}]}
    echo -e  "\t\t Augmentation '${rel_set_name}'"
    for augmentation in ${AUGMENTATIONS}
    do
        echo -e  "\t\t\t kind '${augmentation}'"
        aug_folder="${BK_PATH}/aug_${rel_set_name}_${augmentation}"
        trap 'kill $(jobs -p)' SIGINT               # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT
        for mode in train dev
        do
            echo -e "\t\t\t\t Mode: '${mode}'"
            mkdir -p ${aug_folder}/${mode}
            # JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} 
            #            -name gen_${augmentation}_${rel_set_name}_${mode} -proj srl"
            JOBHEADER=""
            ${JOBHEADER} python -W ignore -m srl_nlp.data_augmentation.scripts.resource_augmentation ${FN_DATA} \
                ${augmentation} --doc_paths ${DOCS_FOLDER_PATH}/${mode} ${aug_folder}/${mode} \
                --relations ${rel_set_name}  \
                --num_cpus ${CPUS} \
                --single_sentences \
                --log ${aug_folder}/${rel_set_name}_${mode}.log -v #&
        done
        wait                                                                    # WAITS FOR ALL PARALLEL CALLS TO FINISH
        echo -e "\t\t\t Done with modes"
    done
done
```

## Running Semantic frame parsing


### Preparing files

* Defines Sesame setup auxiliary function
```bash
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
}
```

* Runs sesame setup for each subfolder in `BK_PATH`
```bash
for idx in "${!REL_SET_NAMES[@]}"
do
    rel_set_name=${REL_SET_NAMES[${idx}]}
    echo -e  "\t\t Augmentation '${rel_set_name}'"
    for augmentation in ${AUGMENTATIONS}
    do
        echo -e  "\t\t\t kind '${augmentation}'"
        aug_folder="${BK_PATH}/aug_${rel_set_name}_${augmentation}"
        
        if [[ -d "${aug_folder}/train" ]]
        then
            echo -e  "\t\t\t\t Creating model folders"
            sesame_setup "${aug_folder}"
        else
            echo -e  "\t\t\t\t ${aug_folder}/[train, dev] not created. Skipping model folders"
        fi
    done
done
```

### Running open-sesame

```bash
FOLDER_CONSTRAIN=''
# TRAIN_MODE='--mode refresh'
TRAIN_MODE=''
```

```bash
echo "Folder constrains: '${FOLDER_CONSTRAINS}'"
trap 'kill $(jobs -p)' SIGINT                       # GUARANTEES THAT ALL PARALLEL CALLS WILL BE KILLED TOGETHER IF EXIT

for exp in $(find ${BK_PATH} -name sesame -type d | grep ${FOLDER_CONSTRAINS})
    do
    (
    echo "Running Sesame on ${exp}"
    cd ${exp}
    aug_name=$( echo ${exp} | sed -E "s|^.*/([^/]*)/methods/sesame|\1|")

    if [[ "$(ls -1 logs | wc -l)" -le 1 ]]                    # IF THERE IS NOTHING IN THE LOGS FOLDER, PREPARE THE DATA
    then
        echo -e "\t Preparing data"
        python -m sesame.preprocess;
    else
        echo -e "\t Data already prepared"
    fi

    for model in frameid targetid argid
        do
#        JOBHEADER="jbsub -cores ${CPUS} -mem ${MEM}g -q ${QUEUE} -name train_${aug_name}_${model} -proj sesame"
        JOBHEADER=""
        echo -e "\t ${JOBHEADER} train"
        ${JOBHEADER} python -m sesame.${model}  --mode train --model_name ${model} ${TRAIN_MODE}
        done
    ) &
    wait                                                                        # WAITS FOR ALL PARALLEL CALLS TO FINISH
done
```

### Evaluating results