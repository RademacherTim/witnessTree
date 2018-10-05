#!/bin/bash

# Read WITNESSTREEPATH from config file
source config

# Run the witnessTree RScript to generate messages
Rscript ${WITNESSTREEPATH}RScripts/witnessTree.R

# Run twitterbot to post generated messages
python pythonScripts/twitterbot.py ${consumer_key} ${consumer_secret} ${access_token} ${access_token_secret}

# Write time and date into log file in the tmp/ folder
DATE=$(date +%Y-%m-%d" "%H:%M:%S)
echo ${DATE} >> tmp/logfile.txt
