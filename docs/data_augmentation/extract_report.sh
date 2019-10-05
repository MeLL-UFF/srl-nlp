#!/usr/bin/env bash
base_folder="/dccstor/sallesd1/breno_tmp/experiments/fn1.5"
out_path="/Users/brenow/google_drive/Mestrado UFF/pics/coverage2"
threshold=3
host="ccc8"

#set -e # Fail fast: exits if a simple command exits with a nonzero exit value.
#       # A simple command is any command not part of an if, while, or until test, or part of an && or || list.

if [[ ! -d ${out_path} ]]
    then
        echo "Making path \"${out_path}\""
        mkdir -p "${out_path}"
fi

echo "Loading list of files"
for file in $(ssh ${host} "find ${base_folder} -name coverage${threshold}.png -type f")
do
    if [[ ${file} == */*aug_*_*/graphs/* ]]
    then
        new_file=$(echo ${file} | sed -E "s|.*aug_(.*)_(.*)/graphs/.*|cov_\2_\1.png|")
    elif [[ ${file} == */current/graphs/* ]]
    then
        new_file="cov_current.png"
    else
        echo "Weird file path '${file}'"
        continue
    fi
    echo "${host}:${file} -> local:\"${out_path}/${new_file}\""
    scp ${host}:${file} "${out_path}/${new_file}"
done
