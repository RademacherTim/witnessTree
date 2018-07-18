#!/bin/bash

# Copy all files from the data directory
cp ${DATAPATH}* ${WITNESSTREEPATH}/data/

# Run the witnessTree RScript to generate messages
RScript ${WITNESSTREEPATH}RScripts/witnessTree.R

# Write time and date into log file in the tmp/ folder
echo date >> tmp/logfile.txt
