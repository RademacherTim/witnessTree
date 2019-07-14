#========================================================================================
# Functions to generate messages depending on the physiological status of the tree.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Monthly sapflow summary                 second week of the month
#   x)  Start of sap flow                       variable
#   x)  Peak of sap flow                        variable
#   x)  End of sap flow                         variable
#   x)  Total growing season sap flow           variable
#   x)  Start of wood growth                    variable
#   x)  Peak of wood growth                     variable
#   x)  End of wood growth                      variable
#   x)  Total growing season wood growth        variable
#
#----------------------------------------------------------------------------------------


# Monthly sap flow summary
#----------------------------------------------------------------------------------------
monthlySapFlowSummary <- function (mtable, TEST = 0) {
  
  # Check whether it is the second week of the month
  if (ceiling (day (Sys.Date ()) / 7) == 2 | TEST == 1) {
  # Read in sap flow data
    
  # Compare the sap flow to the previous month
  #
  } 
  return (mtable)
} 

# Monthly radial growth (i.e. stem and branch diameter) summary
#----------------------------------------------------------------------------------------
monthlyRadGrowthSummary <- function (mtable, TEST = 0) {
  
  # Check whether it is the second week of the month
  if (ceiling (day (Sys.Date ()) / 7) == 4 | TEST == 1) {

    # Get radial growth for the last 30 days and the 30 days prior to that
    #------------------------------------------------------------------------------------
    radGrowth <- calcRadGrowth (pdm_calibration_path = dataPath, 
                                temporalRes = 'monthly',
                                PLOT = T)
    
    # Check whether radial growth is fast or slower than in the previous month
    #------------------------------------------------------------------------------------
    if (radGrowth [1, 1] > radGrowth [1, 2]) { # current month grew more
      postDetails <- getPostDetails ("monthlyRadGrowthSummary - fast")
      message     <- sprintf (postDetails [["Message"]], radGrowth [1, 1])
      delay       <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,8))
      expirDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone)
      mtable    <- add_row (mtable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/figures/monthlyGrowth_%s.png',path,Sys.Date ()), 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate)
    } else if (radGrowth [1, 1] <= radGrowth [1, 2] & 
               radGrowth [1, 1] > 0.05) { # last month grew as much or less
      postDetails <- getPostDetails ("monthlyRadGrowthSummary - slow")
      message     <- sprintf (postDetails [["Message"]], radGrowth [1, 1])
      delay       <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,8))
      expirDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone)
      mtable    <- add_row (mtable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/figures/monthlyGrowth_%s.png',path,Sys.Date ()), 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate)
    } else if (radGrowth [1,1] <= 0.05) { # tree grew only very little and is dormant?
      postDetails <- getPostDetails ("monthlyRadGrowthSummary - dormant")
      message     <- sprintf (postDetails [["Message"]], radGrowth [1, 1])
      delay       <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,8))
      expirDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone)
      mtable    <- add_row (mtable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/figures/monthlyGrowth_%s.png',path,Sys.Date ()), 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate)
    }
  }
  
  # Return the updated message table
  #--------------------------------------------------------------------------------------
  return (mtable)
} 

# Start of sap flow
#----------------------------------------------------------------------------------------
startSapFlow <- function (mtable, TEST = 0) {
  # Read in sap flow data
  # Check whether the sap flow has increased substantially ove rthe past week
  # If so, send beginning of sap flow message
  #if ( | TEST == 1) {
  #} 
  return (mtable)
} 
#========================================================================================