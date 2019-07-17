#========================================================================================
# Functions to generate messages based on tree morphology.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Explain tree dimensions                 20th day of the month
#
#----------------------------------------------------------------------------------------

# Explain dimensions (from LIDAR facts) 
#----------------------------------------------------------------------------------------
explainDimensions <- function (mtable, TEST = 0) {
  if (substring (Sys.time (), 9, 10) == "20" | TEST == 1) {
    postDetails <- getPostDetails ('explainDimensions')
    if (substring (postDetails [['Message']], 1, 3) == 'I a') {
      message <- sprintf (postDetails [["Message"]], surfaceArea)
    } else if (substring (postDetails [['Message']], 1, 3) == 'Wow') {
      message <- sprintf (postDetails [["Message"]], round (branchLength / 1609.34, 1), round (branchLength / 1000.0,1))
    } else if (substring (postDetails [['Message']], 1, 3) == 'Sci') {
      message <- sprintf (postDetails [["Message"]], treeHeight, round (treeHeight * 3.28084, 0))
    } else {
      message <- postDetails [["Message"]]
    }
    delay       <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate   <- sprintf ("%s 23:59:59 %s", 
                            format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    mtable      <- add_row (mtable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = postDetails [["FigureName"]], 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate) 
  } 
  return (mtable)
} 
#========================================================================================