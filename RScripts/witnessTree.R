#=======================================================================================#
# This is the main script running the witness tree bot. 
# See README.Rmd for more information.
#
# Home repository: https://github.com/TTRademacher/witnessTree
#
# Project lead: Tim Tito Rademacher (rademacher.tim@gmail.com)
#
# Developpers:  Kyle Wyche 
#               David Basler
#
#---------------------------------------------------------------------------------------#
 
# Load dependencies
#---------------------------------------------------------------------------------------#
require ('tibble')
require ('dplyr')
require ('readr')
require ('lubridate')
source  ('./RScripts/selectMessage.R')
source  ('./RScripts/checkEvents.R')
source  ('./RScripts/checkExpiration.R')

# Set working directory to the parent directory (witnessTree/)
#---------------------------------------------------------------------------------------#
setwd ('../') 

# Read in previously generated messages, if not first iteration
#---------------------------------------------------------------------------------------#
if (file.exists ('./messages/messages.csv')) {
  messages <- read_csv ('./messages/messages.csv', header = T)
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
messages <- checkExpirationOf (messages)

# Re-evaluate priority of messages
#---------------------------------------------------------------------------------------#
messages <- reEvaluatePriorityOf (messages)

# Generate new messages concerning regularly recurrent events
#---------------------------------------------------------------------------------------#
messages <- checkNewYears       (messages) # 01-01
messages <- checkPiDay          (messages) # 14-03
messages <- checkBirthday       (messages) # 
messages <- checkArborDay       (messages) # 
messages <- checkEarthDay       (messages) # 
messages <- checkSpringEquinox  (messages) # 
messages <- checkAutumnEquinox  (messages) # 
messages <- checkSummerSolstice (messages) # 
messages <- checkWinterSolstice (messages) # 
messages <- checkHalloween      (messages) # 


# Generate new messages concerning phenology
#---------------------------------------------------------------------------------------#
messages <- startOfGrowingSeason (messages)
messages <- endOfGrowingSeason   (messages)

# Generate new messages concerning climatic events
#---------------------------------------------------------------------------------------#
#messages <- checkFrost    (messages) # TTR Does not exist yet
#messages <- checkHeatwave (messages) # TTR Does not exist yet
 
# Selection of message, figure and images for the current iterations
#---------------------------------------------------------------------------------------#
message <- selectMessage (messages)
  
# Write message to messages/ folder named after date and time when it should be scheduled 
#---------------------------------------------------------------------------------------#
if (exists ('message')) {
  write_csv (x    = message,
             file = sprintf ('./messages/%s.csv', 
                             format (Sys.time (), "%Y-%m-%d_%H")),
             row.names = FALSE)
} 

# Save unused messages and figures in tmp/ folder for next iteration
#---------------------------------------------------------------------------------------#
write_csv (x    = messages,
           file = './messages/messages.csv',
           row.names = FALSE)

# Create log files
#---------------------------------------------------------------------------------------#
write_csv (x = sprintf ('%s', format (Sys.time (), "%Y-%m-%d %H:%M")),
             file = './messages/logfile.csv')

#=======================================================================================#