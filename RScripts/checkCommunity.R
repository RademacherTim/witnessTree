#=======================================================================================#
# Functions to generate messages about the community around the tree.
#
#       Event                                   Date                 
#---------------------------------------------------------------------------------------#
#   1)  explainSeedDispersal                    Between 1st of Sep and 30th of Nov
#---------------------------------------------------------------------------------------#

# Explain seed dispersal
#---------------------------------------------------------------------------------------#
explainSeedDispersal <- function (mtable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) > '09-01' & substring (Sys.Date (), 6, 10) < '11-30'| 
      TEST == 1) {
    postDetails <- getPostDetails ("explainSeedDispersal")
    message   <- sprintf (postDetails [["Message"]])
    expirDate <- sprintf ("%s-11-30 23:59:59 %s", format (Sys.Date (), format = '%Y'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  
  return (mtable)
} 

# Check for wildlife images (visitors) at the tree
#----------------------------------------------------------------------------------------
checkCommunityWildlife <- function (mtable, TEST = 0) {
  
  # Check whether there is a new wildlife photo
  #--------------------------------------------------------------------------------------
  memory <- read_csv ('memory.csv', col_types = cols ())
  listOfVisitors <- list.files (path = sprintf ('%s/wildlifeCam/',imagesPath), 
                                pattern = '.jpg')
  if (as.numeric (substring (tail (listOfVisitors, n = 1), 20, 23)) > 
      memory [['numberOfPreviousVisitors']] | TEST >= 1) {
    
    # Get message depending on time of year
    #------------------------------------------------------------------------------------
    if (substring (Sys.Date (), 6, 10) >  '03-21' & 
        substring (Sys.Date (), 6, 10) <= '06-21' | 
        TEST == 1) { # it is spring
      postDetails <- getPostDetails ("checkCommunityWildlife - spring")
    } else if (substring (Sys.Date (), 6, 10) >  '06-21' & 
               substring (Sys.Date (), 6, 10) <= '09-21' | 
               TEST == 2) { # it is summer
      postDetails <- getPostDetails ("checkCommunityWildlife - summer")
    } else if (substring (Sys.Date (), 6, 10) >  '09-21' & 
               substring (Sys.Date (), 6, 10) <= '11-21' | 
               TEST == 3) { # it is fall
      postDetails <- getPostDetails ("checkCommunityWildlife - fall")
    } else if (substring (Sys.Date (), 6, 10) >  '11-21' & 
               substring (Sys.Date (), 6, 10) <= '03-21' | 
               TEST == 4) { # it is winter
      postDetails <- getPostDetails ("checkCommunityWildlife - winter")
    }
    message   <- sprintf (postDetails [["Message"]])
    delay     <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", 
                          format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = sprintf ('%s/wildlifeCam/%s',imagesPath, tail (listOfVisitors, n = 1)),
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
    
    # Increase the wildlife counter in the memory
    memory [['numberOfPreviousVisitors']] <- memory [['numberOfPreviousVisitors']] + 1
    write_csv (memory, 'memory.csv')
  } 
  
  return (mtable)
} 
#=======================================================================================#