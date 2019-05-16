#!/usr/bin/env bash

set -e
#exp_folder="${HOME}/Code/MD/srl_nlp/experiments"
exp_folder="/dccstor/sallesd1/breno_tmp/experiments"
report_py="${HOME}/Code/MD/srl_nlp/notebooks/doc_graphs.py"
fn_version=1.5

. activate srl_nlp

trap 'kill $(jobs -p)' SIGINT

# aug_folder=$1
for aug_folder in $( ls -1 ${exp_folder}/fn${fn_version} |grep -v test)
do
    data_set_folder="${exp_folder}/fn${fn_version}/${aug_folder}"
    mkdir -p ${data_set_folder}/graphs
    echo "Saving report for ${data_set_folder}"
    python -W ignore ${report_py} ${data_set_folder}/graphs/ \
                                --train \
                                ${data_set_folder}/train \
                                --dev \
                                ${data_set_folder}/dev \
                                --test \
                                ${exp_folder}/fn${fn_version}/test \
                                --fn_folder \
                                ${HOME}/Code/MD/srl_nlp/srl_nlp/framenet/fndata-${fn_version} \
                                -v &> ${data_set_folder}/graphs/report.log &

done
wait