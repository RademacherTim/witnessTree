#=======================================================================================#
# This is the main script running the witness tree bot. 
# See README.Rmd for more information.
#
# Home repository: https://github.com/TTRademacher/witnessTree
#
# Project lead: Tim Tito Rademacher (rademacher.tim@gmail.com)
#
# Acknowledgements: Thanks to David Basler, Clarisse Hart, Hannah Robbins, Kyle Wyche, 
#                   Shawna Greyeyes.
#
#---------------------------------------------------------------------------------------#
 

# To-do list:
#---------------------------------------------------------------------------------------#

# Get the absolute path to the directory 
#---------------------------------------------------------------------------------------#
args = commandArgs (trailingOnly=TRUE)
if (length (args) == 0) {
  stop ("Error: At least one argument must be supplied (path to wtinessTree directory).n",
        call.=FALSE)
} else if (length (args) == 1) {
  # default output file
  path = args [1]
} else {
  stop ("Error: Too many command line arguments supplied to R.")
}
print (path)

# Suppress warning messages 
#---------------------------------------------------------------------------------------#
options (warn = -1) # To turn warnings back on use options (warn = 0)

# Load dependencies
#---------------------------------------------------------------------------------------#
suppressPackageStartupMessages (library ('tidyverse'))
suppressPackageStartupMessages (library ('lubridate'))
source  (sprintf ('%sRScripts/messageHandling.R', path))
source  (sprintf ('%sRScripts/checkEvents.R',     path))
source  (sprintf ('%sRScripts/readClimate.R',     path))
source  (sprintf ('%sRScripts/checkClimate.R',    path))
source  (sprintf ('%sRScripts/checkPhysiology.R', path))
source  (sprintf ('%sRScripts/treeStats.R',       path))

# Read in previously generated messages, if not first iteration
#---------------------------------------------------------------------------------------#
if (file.exists (sprintf ('%smessages/messages.csv', path))) {
  messages <- read_csv (sprintf ('%smessages/messages.csv', path), 
                        col_names = T, col_types = cols())
} else { # create a tibble for messages 
  messages <- tibble (priority = 0,  # priority of message to be posted (int; 
                                     # between 0 for low and 10 for highest)
                      fFigure  = F,  # whether it comes with a figure (logical)
                      message  = '', # the message itself (char) 
                      hashtags = '', # hastags going with the message (char)
                      expires  = as.POSIXct (Sys.time ()) - 10e9) # expiration date of the message
  names (messages) <- c ('priority','fFigure','message','hashtags','expires')
}

# Purge expired messages
#---------------------------------------------------------------------------------------#
messages <- checkExpirationDatesOf (messages)

# Re-evaluate priority of messages
#---------------------------------------------------------------------------------------#
messages <- reEvaluatePriorityOf (messages)

# Read climate data
#---------------------------------------------------------------------------------------#
readClimate ()

# Generate new messages concerning regularly recurrent events
#---------------------------------------------------------------------------------------#
messages <- helloWorld                     (messages) # on the launch date (2019-04-15) only
messages <- checkNewYears                  (messages) #  1st  of January
messages <- checkNationalWildLifeDay       (messages) #  4th  of March
messages <- checkPiDay                     (messages) #  14th of March
messages <- checkInternationalDayOfForests (messages) #  21st of March
messages <- checkWorldWaterDay             (messages) #  22nd of March
messages <- checkBirthday                  (messages) #  12th of April
messages <- checkArborDay                  (messages) #  Last Friday in April
messages <- checkMothersDay                (messages) #  Second Sunday in May
messages <- checkEarthDay                  (messages) #  22nd of April
messages <- checkSpringEquinox             (messages) # ~20th of March 
messages <- checkAutumnEquinox             (messages) # ~22nd of September
messages <- checkSummerSolstice            (messages) #  21st of June
messages <- checkWinterSolstice            (messages) #  21st of December
messages <- checkHalloween                 (messages) #  31st of October

# Generate new messages concerning phenology
#---------------------------------------------------------------------------------------#
#messages <- startOfGrowingSeason (messages)
#messages <- endOfGrowingSeason   (messages)

# Generate new messages concerning meteorological & climatic events
#---------------------------------------------------------------------------------------#
messages <- checkExtremeTemperatures (messages) # Test whether it is the hottest or coldest 
                                                # temperature on record (in memory).
messages <- monthlyClimateSummary (messages) # If it is the beginning of the month 
                                             # summarise and compare last months climate 
                                             # to the long term average.
messages <- checkFrost (messages) # Check for first frost of the autumn and late frost 
                                  # events.

# Selection of message, figure and images for the current iterations
#---------------------------------------------------------------------------------------#
message <- selectMessage (messages)

# Delete the selected message from the messages tibble 
#---------------------------------------------------------------------------------------#
messages <- deleteMessage (messages, message)

# Write message to messages/ folder named after date and time when it should be scheduled 
#---------------------------------------------------------------------------------------#
if (dim (message) [1] > 0) {
  write_csv (x    = message,
             path = sprintf ('%smessages/%s.csv', path,
                             format (Sys.time (), "%Y-%m-%d_%H")))
}
 
# Save unused messages and figures in tmp/ folder for next iteration # TTR Does actually save all messages. Could solve it by deleting it when it is selected in selectMessage().
#---------------------------------------------------------------------------------------#
if (dim (messages) [1] > 0) {
  write_csv (x    = messages,
             path = sprintf ('%smessages/messages.csv', path))
}

# Create log files
#---------------------------------------------------------------------------------------#
write_csv (x         = as.tibble (sprintf ('%s', format (Sys.time (), "%Y-%m-%d %H:%M"))),
           path      = sprintf ('%smessages/logfile.csv', path),
           col_names = FALSE,
           append    = TRUE)
#=======================================================================================#