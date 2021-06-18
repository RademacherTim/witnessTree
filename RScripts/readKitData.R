#=======================================================================================
# Read climate data from the Harvard Forest weather station.
#---------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('read_csv'))       library ('tidyverse')
if (!existsFunction ('as_datetime'))    library ('lubridate')
if (!existsFunction ('drive_download')) library ('googledrive')
if (!existsFunction ('read_excel'))     library ('readxl')
#if (!existsFunction ('esat'))       library ('plantecophys')

# Download the spreadsheet with the raw data 
#----------------------------------------------------------------------------------------
spreadsheet <- suppressMessages (
  googledrive::drive_download (file = 'https://docs.google.com/spreadsheets/d/1d52LFGaTsMX6CfduuL31_GPkRPcs5GP6Uhcv0pXRzzA/edit?ts=60c26319#gid=1238602773', 
                               overwrite = TRUE,
                               path = paste0 (path,'/rawData/witness_tree_raw_feed.xlsx'))
)

# Read the downloaded spreasheet with the raw data
#----------------------------------------------------------------------------------------
infoSheet <- read_excel (paste0 (path,'/rawData/witness_tree_raw_feed.xlsx'),
                         sheet = 'nodeInfo')
rawFeed <- read_excel (paste0 (path,'/rawData/witness_tree_raw_feed.xlsx'),
                       sheet = 'rawData') 

# Filter for only data from this particular witness tree 
#----------------------------------------------------------------------------------------
nodeID <- infoSheet [['Node']] [infoSheet [['Name']] == 'Witness_002']
rawFeed <- rawFeed %>% filter (Node == nodeID)

# Wrangle data to be in reasonable formats 
#----------------------------------------------------------------------------------------
rawFeed <- rawFeed %>% mutate (Timestamp = as_datetime (Timestamp))
#=======================================================================================
