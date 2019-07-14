#=======================================================================================#
# This is the main script running the witness tree bot. 
# See README.Rmd for more information.
#
# Home repository: https://github.com/TTRademacher/witnessTree
#
# Project lead: Tim Tito Rademacher (rademacher.tim@gmail.com)
#
# Acknowledgements: Thanks to David Basler, Clarisse Hart, Hannah Robbins, Kyle Wyche, 
#                   Shawna Greyeyes for their invaluable contributions.
#
#---------------------------------------------------------------------------------------#
 

# To-do list:
#---------------------------------------------------------------------------------------#

# Get the absolute path to the witnessTree, images and data directories 
#---------------------------------------------------------------------------------------#
args = commandArgs (trailingOnly=TRUE)
if (length (args) == 0) {
  stop ("Error: At least one argument must be supplied (path to witnessTree directory).",
        call.=FALSE)
} else if (length (args) >= 1) {
  # default output file
  path       = args [1]
  imagesPath = args [2]
  dataPath   = args [3]
} else {
  stop ("Error: Too many command line arguments supplied to R.")
}
print (path)
print (imagesPath)
print (dataPath)

# Suppress warning messages 
#---------------------------------------------------------------------------------------#
options (warn = -1) # To turn warnings back on use options (warn = 0)

# Load dependencies
#---------------------------------------------------------------------------------------#
suppressPackageStartupMessages (library ('tidyverse'))
suppressPackageStartupMessages (library ('lubridate'))
source  (sprintf ('%sRScripts/postHandling.R',     path))
source  (sprintf ('%sRScripts/checkEvents.R',      path))
source  (sprintf ('%sRScripts/readClimate.R',      path))
source  (sprintf ('%sRScripts/checkClimate.R',     path))
source  (sprintf ('%sRScripts/calcSapFlow.R',      path))
source  (sprintf ('%sRScripts/calcRadialGrowth.R', path))
source  (sprintf ('%sRScripts/checkPhysiology.R',  path))
source  (sprintf ('%sRScripts/checkCommunity.R',   path))
source  (sprintf ('%sRScripts/treeStats.R',        path))

# Read in previously generated posts, if not first iteration
#---------------------------------------------------------------------------------------#
if (file.exists (sprintf ('%sposts/posts.csv', path))) {
  posts <- read_csv (sprintf ('%sposts/posts.csv', path), 
                     col_names = T, col_types = cols())
} else { # create a tibble for posts
  posts <- tibble (priority    = 0,  # priority of message to be posted (int; 
                                     # between 0 for low and 10 for highest)
                   fFigure     = F,  # boolean whether it comes with a figure or not
                   figureName  = '', # text string with the figure name.  
                   message     = '', # the message itself (char) 
                   hashtags    = '', # hastags going with the message (char)
                   expires     = as.POSIXct (Sys.time ()) - 10e9) # expiration date of the message
  names (posts) <- c ('priority','fFigure','figureName','message','hashtags','expires')
}

# Purge expired posts
#---------------------------------------------------------------------------------------#
posts <- checkExpirationDatesOf (posts)

# Re-evaluate priority of posts
#----------------------------------------------------------------------------------------
posts <- reEvaluatePriorityOf (posts)

# Read climate data
#----------------------------------------------------------------------------------------
readClimate ()

# Generate new posts concerning regularly recurrent events
#----------------------------------------------------------------------------------------
posts <- helloWorld                     (posts) # on the launch date (2019-04-15) only
posts <- checkNewYears                  (posts) #  1st  of January
posts <- checkNationalWildLifeDay       (posts) #  4th  of March
posts <- checkPiDay                     (posts) #  14th of March
posts <- checkInternationalDayOfForests (posts) #  21st of March
posts <- checkWorldWaterDay             (posts) #  22nd of March
posts <- checkBirthday                  (posts) #  12th of April
posts <- checkArborDay                  (posts) #  Last Friday in April
posts <- checkMothersDay                (posts) #  Second Sunday in May
posts <- checkEarthDay                  (posts) #  22nd of April
posts <- checkSpringEquinox             (posts) # ~20th of March 
posts <- checkAutumnEquinox             (posts) # ~22nd of September
posts <- checkSummerSolstice            (posts) #  21st of June
posts <- checkWinterSolstice            (posts) #  21st of December
posts <- checkHalloween                 (posts) #  31st of October

# Generate new posts concerning phenology
#---------------------------------------------------------------------------------------#
#posts <- startOfGrowingSeason (posts)
#posts <- endOfGrowingSeason   (posts)

# Generate new posts concerning meteorological & climatic events
#---------------------------------------------------------------------------------------#
posts <- checkExtremeTemperatures (posts) # Test whether it is the hottest or coldest 
                                          # temperature on record (in memory).
posts <- monthlyClimateSummary (posts) # If it is the beginning of the month summarise 
                                       # and compare last months climate to the long 
                                       #term average.
posts <- checkFrost    (posts) # Check for first frost of the autumn 
                               # and late frost in early growing season.
posts <- checkHeatWave (posts) # Check for a heat wave.
posts <- checkStorm    (posts) # Check for storm or rather a windy day.

# Generate new posts concerning the community surrounding the tree
#----------------------------------------------------------------------------------------
posts <- explainSeedDispersal   (posts)
posts <- checkCommunityWildlife (posts)

# Generate new posts concerning physiology
#----------------------------------------------------------------------------------------
posts <- monthlyRadGrowthSummary (posts)

# Selection of post, figure and images for the current iterations
#----------------------------------------------------------------------------------------
post <- selectPost (posts)

# Delete the selected post from the posts tibble 
#---------------------------------------------------------------------------------------#
posts <- deletePost (posts, post)

# Check whether the bot has already posted four messages last week
#---------------------------------------------------------------------------------------#
pastPostDates <- as.POSIXct (list.files (sprintf ('%s/messages/', path)),
                             format = "%Y-%m-%d_%H")
numberOfPostsLastWeek <- length (pastPostDates [pastPostDates > Sys.Date () - 7         & 
                                                !is.na  (pastPostDates)                 &
                                                !is.nan (pastPostDates)])
if (numberOfPostsLastWeek >= 4) { # If the bot has already posted four messages
  # Add post back to posts tibble, as it will not be posted right now
  #-------------------------------------------------------------------------------------#
  posts <- rbind (posts, post)
} else {
  # Write post to messages/ folder named after date and time when it should be scheduled 
  #-------------------------------------------------------------------------------------#
  if (dim (post) [1] > 0) {
    write_csv (x    = post,
               path = sprintf ('%sposts/%s.csv', path,
                               format (Sys.time (), "%Y-%m-%d_%H")),
               na   = "")
  }
}
 
# Save unused posts and figures in tmp/ folder for next iteration 
#---------------------------------------------------------------------------------------#
if (dim (posts) [1] > 0) {
  write_csv (x    = posts,
             path = sprintf ('%sposts/posts.csv', path))
}

# Create log files
#---------------------------------------------------------------------------------------#
write_csv (x         = as.data.frame (sprintf ('%s', format (Sys.time (), "%Y-%m-%d %H:%M"))),
           path      = sprintf ('%sposts/logfile.csv', path),
           col_names = FALSE,
           append    = TRUE)
#=======================================================================================#