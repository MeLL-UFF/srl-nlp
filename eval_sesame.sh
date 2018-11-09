#!/usr/bin/env bash

set -e
#exp_folder="${HOME}/Code/MD/srl_nlp/experiments"
exp_folder="/dccstor/sallesd1/breno_tmp/experiments"
report_py="${HOME}/Code/MD/srl_nlp/eval_sesame.py"

model="argid"
score_file_suffix="methods/sesame/logs/${model}/${model}-prediction-analysis.log"
fn_version=1.5

. activate srl_nlp

#aug_folder=$1
echo "folder, precision, recall, f1"
for aug_folder in $( ls -1 ${exp_folder}/fn${fn_version}/blackbox |grep -v test)
do
    data_set_folder="${exp_folder}/fn${fn_version}/blackbox/${aug_folder}"
    score_path="${data_set_folder}/${score_file_suffix}"
    echo ${score_path} | python -W ignore ${report_py} | xargs -i echo "${aug_folder}, {}"
done