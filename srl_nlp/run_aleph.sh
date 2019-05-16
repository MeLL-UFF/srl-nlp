#!/bin/bash

if [ "$#" -gt "0" ]; then
	direct=$1
	prefix=$2
	#echo ">$direct"
	#echo ">$prefix"
	echo ":- consult('uw_aleph/aleph').
:- read_all('$direct/$prefix').
:- induce.
:- halt." | yap
else
	>&2 echo "Usage: $0 dir file_prefix"
fi