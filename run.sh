#!/bin/bash

# Read WITNESSTREEPATH from config file
source config

# Run the witnessTree RScript to generate messages
Rscript ${WITNESSTREEPATH}RScripts/witnessTree.R

# Write time and date into log file in the tmp/ folder
DATE=$(date +%Y-%m-%d" "%H:%M:%S)
echo ${DATE} >> tmp/logfile.txt
