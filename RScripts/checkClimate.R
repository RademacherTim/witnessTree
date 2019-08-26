#========================================================================================
# Functions to generate messages depending on the climate around the tree.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Hottest temperature experienced         variable
#   2)  Coldest temperature experienced         variable
#   3)  Wettest day experienced                 variable Shawna will program this!
#   4)  Hottest day experienced                 variable
#   5)  Coldest day experienced                 variable
#   6)  Wettest week experienced                variable
#   7)  Hottest week experienced                variable
#   8)  Coldest week experienced                variable
#   9)  Wettest month experienced               variable
#  10)  Hottest month experienced               variable
#  11)  Coldest month experienced               variable
#  12)  Wettest year experienced                variable
#  13)  Hottest year experienced                variable
#  14)  Coldest year experienced                variable
#  15)  Latest frost event                      variable
#  16)  Lastest snow                            variable 
#  17)  Monthly summary of climate              beginning of following month
#  18)  Annual summary of climate               beginning of following year
#  19)  Growing season summary of climate       at the beginning of November?
#  18)  Frost event                             variable TR - Not made yet
#  xx)  Heat wave                               variable TR - Not made yet
#  xx)  Drought                                 variable TR - Not made yet
#  xx)  Heavy rain                              variable TR - Not made yet
#  xx)  Still standing after heavy wind/storm   variable TR - Not made yet "I am still standing"
#  XX)  Maple sirup season                      variable TR - Not made yet "My bro's juices are flowing"
#----------------------------------------------------------------------------------------
# All functions require weather station records to be read in and converted to a 
# structure such as in readClimate.R
#----------------------------------------------------------------------------------------

# To-Do list:
# TR - I might also want to delay messages for December and last year, so that they do not come out straight after new year's resolutions.
# TR - I need to introduce a memory or something to make sure that I am not repeating the same messages for hottest/coldest temperature overa nd over again.

# Convert degrees Celsius to Fahrenheit
#----------------------------------------------------------------------------------------
CtoF <- function (degC, difference = F) {
  if (!difference) {
    degF <- (degC * 9.0 / 5.0) + 32.0
  } else {
    degF <- degC * 9.0 / 5.0
  }
  return (degF)
}

# Convert millimeters to inches
#----------------------------------------------------------------------------------------
mmtoInches <- function (mm) {
  inches <- mm / 25.4
  return (inches)
}

# Hottest or coldest temperature on record (in memory)
#----------------------------------------------------------------------------------------
checkExtremeTemperatures <- function (ptable, TEST = 0) {
  
  # load depdendencies
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('month')) library ('lubridate') 
  if (!exists ('airt')) source (sprintf ('%sRScripts/readClimate.R', path))
  
  # Check whether the current temperature is the hottest temperature on record 
  #--------------------------------------------------------------------------------------
  if (max (airt [['airt']], na.rm = T) <= tail (airt [['airt']], n = 1) | TEST == 1) {
    HOTTEST <- TRUE
    HOT     <- FALSE
  } else if (tail (airt [['rank']], n = 1) <=  100 | TEST == 2) {
    HOTTEST <- FALSE
    HOT     <- TRUE
  } else {
    HOTTEST <- FALSE
    HOT     <- FALSE
  }
  
  # Check whether the yesterday was the warmest day on record
  #--------------------------------------------------------------------------------------
  if (max (dailyAirt [['airt']], na.rm = T) <= 
      head (tail (dailyAirt [['airt']], n = 2), n = 1) | TEST == 3) {
    HOTTESTDAY <- TRUE
    HOTDAY     <- FALSE
  } else if (head (tail (dailyAirt [['rank']], n = 2), n = 1) <= 50 | TEST == 4) {
    HOTTESTDAY <- FALSE
    HOTDAY     <- TRUE
  } else {
    HOTTESTDAY <- FALSE
    HOTDAY     <- FALSE
  }
  
  # Check whether the last week was the warmest week on record
  #--------------------------------------------------------------------------------------
  if (max (weeklyAirt [['airt']], na.rm = T) <= 
      head (tail (weeklyAirt [['airt']], n = 2), n = 1) | TEST == 5) { 
    HOTTESTWEEK <- TRUE
    HOTWEEK     <- FALSE
  } else if (head (tail (weeklyAirt [['rank']], n = 2), n = 1) <= 30 | TEST == 6) {
    HOTTESTWEEK <- FALSE
    HOTWEEK     <- TRUE
  } else {
    HOTTESTWEEK <- FALSE
    HOTWEEK     <- FALSE
  }
  
  # Check whether the last month was the warmest month on record
  #--------------------------------------------------------------------------------------
  if (max (monthlyAirt [['airt']], na.rm = T) <= 
      head (tail (monthlyAirt [['airt']], n = 2), n = 1) | TEST == 7) { 
    HOTTESTMONTH <- TRUE
    HOTMONTH     <- FALSE
  } else if (head (tail (monthlyAirt [['rank']], n = 2), n = 1) <= 20 | TEST == 8) {
    HOTTESTMONTH <- FALSE
    HOTMONTH     <- TRUE
  } else {
    HOTTESTMONTH <- FALSE
    HOTMONTH     <- FALSE
  }
  
  # Check whether the last year was the warmest year on record
  #--------------------------------------------------------------------------------------
  if (max (yearlyAirt [['airt']], na.rm = T) <= 
      head (tail (yearlyAirt [['airt']], n = 2), n = 1) | TEST == 9) { 
    HOTTESTYEAR <- TRUE
    HOTYEAR     <- FALSE
  } else if (head (tail (yearlyAirt [['rank']], n = 2), n = 1) <= 10 | TEST == 10) {
    HOTTESTYEAR <- FALSE
    HOTYEAR     <- TRUE
  }  else {
    HOTTESTYEAR <- FALSE
    HOTYEAR     <- FALSE
  }
  
  # Check whether the current temperature is the coldest temperature on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (airt [['airt']])
  if (min (airt [['airt']], na.rm = T) >= 
      tail (airt [['airt']], n = 1) | TEST == 11) {
    COLDEST <- TRUE
    COLD    <- FALSE
  } else if (tail (tempRank, n = 1) <= 100 | TEST == 12) {
    COLDEST <- FALSE
    COLD    <- TRUE
  } else {
    COLDEST <- FALSE
    COLD    <- FALSE
  }
  temperatureC <- tail (airt [['airt']], n = 1)
  temperatureF <- CtoF (temperatureC)
  
  # Check whether the yesterday was the coldest day on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (dailyAirt [['airt']]) 
  if (min (dailyAirt [['airt']], na.rm = T) >= 
      head (tail (dailyAirt [['airt']], n = 2), n = 1) | TEST == 13) {
    COLDESTDAY <- TRUE
    COLDDAY    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 50 | TEST == 14) {
    COLDESTDAY <- FALSE
    COLDDAY    <- TRUE
  } else {
    COLDESTDAY <- FALSE
    COLDDAY    <- FALSE
  }
  dailyTemperatureC <- head (tail (dailyAirt [['airt']], n = 2), n = 1)
  dailyTemperatureF <- CtoF (dailyTemperatureC)
  
  # Check whether the last week was the coldest week on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (weeklyAirt [['airt']]) 
  if (min (weeklyAirt [['airt']], na.rm = T) >= 
      head (tail (weeklyAirt [['airt']], n = 2), n = 1) | TEST == 15) { 
    COLDESTWEEK <- TRUE
    COLDWEEK    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 30 | TEST == 16) {
    COLDESTWEEK <- FALSE
    COLDWEEK    <- TRUE
  } else {
    COLDESTWEEK <- FALSE
    COLDWEEK    <- FALSE
  }
  weeklyTemperatureC <- head (tail (weeklyAirt [['airt']], n = 2), n = 1)
  weeklyTemperatureF <- CtoF (weeklyTemperatureC)
  
  # Check whether the last month was the coldest month on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (monthlyAirt [['airt']]) 
  if (min (monthlyAirt [['airt']], na.rm = T) >= 
      head (tail (monthlyAirt [['airt']], n = 2), n = 1) | TEST == 17) { 
    COLDESTMONTH <- TRUE
    COLDMONTH    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 20 | TEST == 18) {
    COLDESTMONTH <- FALSE
    COLDMONTH    <- TRUE
  }  else {
    COLDESTMONTH <- FALSE
    COLDMONTH    <- FALSE
  }
  monthlyTemperatureC <- head (tail (monthlyAirt [['airt']], n = 2), n = 1)
  monthlyTemperatureF <- CtoF (monthlyTemperatureC)
  
  # Check whether the last year was the warmest year on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (yearlyAirt [['airt']]) 
  if (max (yearlyAirt [['airt']], na.rm = T) >= 
      head (tail (yearlyAirt [['airt']], n = 2), n = 1) | TEST == 19) { 
    COLDESTYEAR <- TRUE
    COLDYEAR    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 10 | TEST == 20) {
    COLDESTYEAR <- FALSE
    COLDYEAR    <- TRUE
  } else {
    COLDESTYEAR <- FALSE
    COLDYEAR    <- FALSE
  }
  yearlyTemperatureC <- head (tail (yearlyAirt [['airt']], n = 2), n = 1)
  yearlyTemperatureF <- CtoF (yearlyTemperatureC) 
  
  # Send message if it is the hottest or coldest temperature
  #-------------------------------------------------------------------------------------
  if (HOT | HOTTEST | COLD | COLDEST | HOTDAY | HOTTESTDAY | COLDDAY | COLDESTDAY | 
      HOTWEEK | HOTTESTWEEK | COLDWEEK | COLDESTWEEK | HOTTESTMONTH | COLDESTMONTH | 
      HOTTESTYEAR | COLDESTYEAR | TEST >= 1) {
    priority  <- 9
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    if (HOTTEST) {
      postDetails <- getPostDetails ('hottest')
      message    <- sprintf (postDetails [['Message']], round (temperatureC, 1), 
                             round (temperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s %s", format (Sys.time () + delay * 60 * 60, 
                                              format = '%Y-%m-%d %H:%M:%S'), treeTimeZone)
    } else if (HOT){
      postDetails <- getPostDetails ('hot')
      rank <- tail (airt [['rank']], n = 1)
      message    <- sprintf (postDetails [['Message']], 
                             round (temperatureC, 1),
                             round (temperatureF, 1), 
                             rank, findOrdinalSuffix (rank))
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s %s", format (Sys.time () + delay * 60 * 60, 
                                              format = '%Y-%m-%d %H:%M:%S'), treeTimeZone)
    } else if (HOTTESTDAY) {
      postDetails <- getPostDetails ('hottestDay')
      message   <- sprintf (postDetails [['Message']], round (dailyTemperatureC, 1), 
                            round (dailyTemperatureF, 1))
      # Expires at the end of the day
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date (), format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (HOTDAY) {
      postDetails <- getPostDetails ('hotDay')
      rank <- head (tail (dailyAirt [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['Message']], round (dailyTemperatureC, 1), 
                            round (dailyTemperatureF, 1), rank, findOrdinalSuffix (rank))
      
      # Expires at the end of the day
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date (), format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (HOTTESTWEEK) {
      postDetails <- getPostDetails ('hottestWeek')
      message   <- sprintf (postDetails [['Message']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    } else if (HOTWEEK) {
      postDetails <- getPostDetails ('hotWeek')
      message   <- sprintf (postDetails [['Message']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    } else if (HOTTESTMONTH) {
      postDetails <- getPostDetails ('hottestMonth')
      message   <- sprintf (postDetails [['Message']], round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (HOTMONTH) {
      postDetails <- getPostDetails ('hotMonth')
      rank <- head (tail (monthlyAirt [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['Message']], treeLocationName, rank, 
                            findOrdinalSuffix (rank), round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    } else if (HOTTESTYEAR) {
      postDetails <- getPostDetails ('hottestYear')
      message   <- sprintf (postDetails [['Message']], year (Sys.Date ()) - 1,
                            round (yearlyTemperatureC, 1), 
                            round (yearlyTemperatureF, 1),
                            round (mean (yearlyAirt [['airt']], na.rm = T), 1),
                            round (CtoF (mean (yearlyAirt [['airt']], na.rm = T)), 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (HOTYEAR) {
      postDetails <- getPostDetails ('hotYear')
      rank <- head (tail (yearlyAirt [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['Message']], rank, findOrdinalSuffix (rank), 
                            round (yearlyTemperatureC, 1), 
                            round (yearlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDEST) {
      postDetails <- getPostDetails ('coldest')
      message   <- sprintf (postDetails [['Message']], round (temperatureC, 1), 
                            round (temperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s %s", format (Sys.time () + delay * 60 * 60, 
                                              format = '%Y-%m-%d %H:%M:%S'), treeTimeZone)
    } else if (COLD){
      postDetails <- getPostDetails ('cold')
      rank <- tail (rank (airt [['airt']]), n = 1)
      message    <- sprintf (postDetails [['Message']], 
                             round (temperatureC, 1),
                             round (temperatureF, 1), 
                             rank, findOrdinalSuffix (rank))
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s %s", format (Sys.time () + delay * 60 * 60, 
                                              format = '%Y-%m-%d %H:%M:%S'), treeTimeZone)
    } else if (COLDESTDAY) {
      pastTemperatureC  <- tail (head (sort (airt [['airt']]), n = 2), n = 1)
      pastTemperatureF  <- CtoF (pastTemperatureC)
      temp <- max (airt [['day']] [airt [['airt']] == pastTemperatureC], na.rm = T)
      dateOfLastColdest <- sprintf ('%s %s', month (temp, label = T, abbr = F), year (temp)) 
      postDetails <- getPostDetails ('coldestDay')
      message   <- sprintf (postDetails [['Message']], dateOfLastColdest, 
                            round (pastTemperatureC, 1), round (pastTemperatureF, 1), 
                            round (dailyTemperatureC, 1), round (dailyTemperatureF, 1)) # TR still nedds work
      
      # Expires at the end of the day
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date (), format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (COLDDAY) {
      postDetails <- getPostDetails ('coldDay')
      rank <- head (tail (rank (dailyAirt [['airt']]), n = 2), n = 1)
      message <- sprintf (postDetails [['Message']], round (dailyTemperatureC, 1), 
                          round (dailyTemperatureF, 1), rank, findOrdinalSuffix (rank))
      
      # Expires at the end of the day
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date (), format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (COLDESTWEEK) {
      postDetails <- getPostDetails ('coldestWeek')
      message   <- sprintf (postDetails [['Message']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (COLDWEEK) {
      postDetails <- getPostDetails ('coldWeek')
      message   <- sprintf (postDetails [['Message']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDESTMONTH) {
      postDetails <- getPostDetails ('coldestMonth')
      message   <- sprintf (postDetails [['Message']], round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (COLDMONTH) {
      postDetails <- getPostDetails ('coldMonth')
      rank <- head (tail (rank (monthlyAirt [['airt']]), n = 2), n = 1)
      message   <- sprintf (postDetails [['Message']], treeLocationName, rank, 
                            findOrdinalSuffix (rank),
                            round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDESTYEAR) {
      postDetails <- getPostDetails ('coldestYear')
      message   <- sprintf (postDetails [['Message']], year (Sys.Date ()) - 1,
                            round (yearlyTemperatureC, 1), 
                            round (yearlyTemperatureF, 1), treeLocationName,
                            round (mean (yearlyAirt [['airt']], na.rm = T), 1),
                            round (CtoF (mean (yearlyAirt [['airt']], na.rm = T)), 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                             treeTimeZone)
    } else if (COLDYEAR) {
      postDetails <- getPostDetails ('coldYear')
      rank <- head (tail (rank (yearlyAirt [['airt']]), n = 2), n = 1)
      message <- sprintf (postDetails [['Message']], rank, findOrdinalSuffix (rank), 
                          round (yearlyTemperatureC, 1), 
                          round (yearlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8))
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + delay, 
                                                    format = '%Y-%m-%d'), treeTimeZone)
    }
    ptable <- add_row (ptable, 
                       priority    = postDetails [["Priority"]], 
                       figureName  = postDetails [["FigureName"]], 
                       fFigure     = postDetails [["fFigure"]],
                       message     = message, 
                       hashtags    = postDetails [["Hashtags"]], 
                       expires     = expireDate)
  } 
  
  # Return the table with posts
  #--------------------------------------------------------------------------------------
  return (ptable)
} 

#extremePrecipitation <- function (ptable, TEST = 0) {
  
  # Check whether the yesterday was the wettest day on record
  
  # Check whether the last week was the wettest week on record
  
  # Check whether the last month was the wettest month on record
  
  # Check whether the last year was the wettest year on record
  
  # Send message if it is the wettest
#  ptable    <- add_row (ptable, 
#                        priority = postDetails [["Priority"]], 
#                        fFigure  = postDetails [["fFigure"]], 
#                        message  = message, 
#                        hashtags = postDetails [["Hashtags"]], 
#                        expires  = expirDate)
#  return (ptable)
#} 

# Summarise and compare last month's climate at the beginning of the month
#----------------------------------------------------------------------------------------
monthlyClimateSummary <- function (ptable, TEST = 0) {
  
  # Check whether it is the first day of the month
  #--------------------------------------------------------------------------------------
  if (substring (Sys.Date (), 9, 10) == '01' & substring (Sys.time (), 12, 13) == '13'| TEST >= 1) {
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    #--------------------------------------------------------------------------------------
    acMonthlyAirt <- head (tail (monthlyAirt [['airt']], n = 2), n = 1) 
    meMonthlyAirt <- mean (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1])
    sdMonthlyAirt <- sd   (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1])
    diMonthlyAirt <- acMonthlyAirt - meMonthlyAirt
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    #--------------------------------------------------------------------------------------
    acMonthlyPrec <- head (tail (monthlyPrec [['prec']], n = 2), n = 1) 
    meMonthlyPrec <- mean (monthlyPrec [['prec']] [month (monthlyPrec [['month']]) == month (Sys.Date ()) - 1])
    sdMonthlyPrec <- sd   (monthlyPrec [['prec']] [month (monthlyPrec [['month']]) == month (Sys.Date ()) - 1])
    diMonthlyPrec <- acMonthlyPrec - meMonthlyPrec
    
    # determine rainy days in previous month
    #--------------------------------------------------------------------------------------
    rainyDays <- sum (dailyPrec [['prec']] [month (dailyPrec [['day']]) == month (Sys.Date ()) - 1 &
                                            year (dailyPrec [['day']]) == year (Sys.Date ())] != 0, 
                      na.rm = T)
    
    # get the monthly max temperature
    #--------------------------------------------------------------------------------------
    maxAirT <-max (dailyMaxAirt [['airt']] [month (dailyMaxAirt [['day']]) == month (Sys.Date ()) - 1 &
                                            year (dailyMaxAirt [['day']]) == year (Sys.Date ())], 
                    na.rm = T)
    
    # Get previous month
    #--------------------------------------------------------------------------------------
    prMonth <- month (month (Sys.Date ()) - 1, label = T, abbr = F)
    
    # Choose what to talk about
    #--------------------------------------------------------------------------------------
    if (diMonthlyAirt < sdMonthlyAirt & diMonthlyPrec < sdMonthlyPrec | TEST == 1) { # Just an close-to-average month 
      postDetails <- getPostDetails ('monthlyClimateSummary - normal', 
                                     gs_posts_key = gsPostsKey)
      message        <- sprintf (postDetails [['Message']], round (acMonthlyAirt, 1), 
                                 round (CtoF (acMonthlyAirt), 1), round (acMonthlyPrec, 1), 
                                 treeLocationName, prMonth)
    } else if (abs (diMonthlyAirt) >= sdMonthlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
      if (diMonthlyAirt < 0 | TEST == 2) { # Cold month
        postDetails <- getPostDetails ('monthlyClimateSummary - cold', 
                                       gs_posts_key = gsPostsKey)
        message        <- sprintf (postDetails [['Message']], round (meMonthlyAirt, 1), 
                                   round (CtoF (meMonthlyAirt), 1), round (-diMonthlyAirt, 1), 
                                   treeLocationName, prMonth)
      } else if (diMonthlyAirt > 0 | TEST == 3) { # Warm month
        postDetails <- getPostDetails ('monthlyClimateSummary - warm', 
                                       gs_posts_key = gsPostsKey)
        message        <- sprintf (postDetails [['Message']], round (meMonthlyAirt, 1),
                                   round (CtoF (meMonthlyAirt), 1), prMonth, 
                                   round (diMonthlyAirt, 1), 
                                   round (CtoF (diMonthlyAirt, difference  = T), 1), 
                                   treeLocationName)
      }
    } else if (abs (diMonthlyPrec) >= sdMonthlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
      if (diMonthlyPrec < 0  | TEST == 4) { # Dry month
        postDetails <- getPostDetails ('monthlyClimateSummary - dry', 
                                       gs_posts_key = gsPostsKey)
        message        <- sprintf (postDetails [['Message']], 
                                   round (maxAirT, 1),
                                   round (CtoF (maxAirT), 1), 
                                   round (meMonthlyPrec, 1), 
                                   treeLocationName)
      } else if (diMonthlyAirt > 0 | TEST == 5) { # Wet month
        postDetails <- getPostDetails ('monthlyClimateSummary - wet', 
                                       gs_posts_key = gsPostsKey)
        message        <- sprintf (postDetails [['Message']], 
                                   format (Sys.Date () - 1, '%d %B'), 
                                   '23:59h',
                                   round (acMonthlyPrec, 1), 
                                   round (mmtoInches (acMonthlyPrec)), 
                                   rainyDays,
                                   days_in_month (month (Sys.Date () - 1)),
                                   prMonth)
      }
    }
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority   = postDetails [["Priority"]], 
                          fFigure    = postDetails [["fFigure"]],
                          figureName = postDetails [["FigureName"]], 
                          message    = message, 
                          hashtags   = postDetails [["Hashtags"]], 
                          expires    = expirDate)
  }
  
  # return the table with posts
  #--------------------------------------------------------------------------------------
  return (ptable)
} 

# Summarise and compare last month's climate at the beginning of the month
#----------------------------------------------------------------------------------------
# monthlyClimateSummaryFollowUp <- function (ptable, TEST = 0) {
#   
#   # Check whether it is the seventh day of the month
#   #--------------------------------------------------------------------------------------
#   if (substring (Sys.Date (), 9, 10) == '07' & substring (Sys.time (), 12, 13) == '13'| TEST >= 1) {
#     
#     # Calculate mean and standard deviation for monthly temperature for all months such as 
#     # the previous (i.e. May) 
#     #--------------------------------------------------------------------------------------
#     acMonthlyAirt <- head (tail (monthlyAirt [['airt']], n = 2), n = 1) 
#     meMonthlyAirt <- mean (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1])
#     diMonthlyAirt <- acMonthlyAirt - meMonthlyAirt
#     
#     # determine rainy days in previous month
#     #--------------------------------------------------------------------------------------
#     rainyDays <- sum (dailyPrec [['prec']] [month (dailyPrec [['day']]) == month (Sys.Date ()) - 1 &
#                                               year (dailyPrec [['day']]) == year (Sys.Date ())] != 0, 
#                       na.rm = T)
# 
#     # Calculate mean and standard deviation for monthly temperature for all months such as 
#     # the previous (i.e. May) 
#     #--------------------------------------------------------------------------------------
#     baseline <- month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1 &
#                 year  (monthlyAirt [['month']]) >= 1964                    &
#                 year  (monthlyAirt [['month']]) <= 1993
#     meMonthlyAirtBase <- mean (monthlyAirt [['airt']] [baseline])
#     diMonthlyAirtBase <- acMonthlyAirt - meMonthlyAirtBase
#     
#     # Get previous month
#     #--------------------------------------------------------------------------------------
#     prMonth <- month (month (Sys.Date ()) - 1, label = T, abbr = F)
#     
#     # Choose what to talk about
#     #--------------------------------------------------------------------------------------
#     if (diMonthlyAirt < diMonthlyAirtBase | TEST == 1) { 
#       postDetails <- getPostDetails ('monthlyClimateSummaryFollowUp - warmer', 
#                                      gs_posts_key = gsPostsKey)
#       message        <- sprintf (postDetails [['Message']], round (acMonthlyAirt, 1), 
#                                  round (CtoF (acMonthlyAirt), 1), round (acMonthlyPrec, 1), 
#                                  treeLocationName, prMonth)
#     } else if (abs (diMonthlyAirt) >= sdMonthlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
#       if (diMonthlyAirt < 0 | TEST == 2) { # Cold month
#         postDetails <- getPostDetails ('monthlyClimateSummary - cold', 
#                                        gs_posts_key = gsPostsKey)
#         message        <- sprintf (postDetails [['Message']], round (meMonthlyAirt, 1), 
#                                    round (CtoF (meMonthlyAirt), 1), round (-diMonthlyAirt, 1), 
#                                    treeLocationName, prMonth)
#       } else if (diMonthlyAirt > 0 | TEST == 3) { # Warm month
#         postDetails <- getPostDetails ('monthlyClimateSummary - warm', 
#                                        gs_posts_key = gsPostsKey)
#         message        <- sprintf (postDetails [['Message']], round (meMonthlyAirt, 1),
#                                    round (CtoF (meMonthlyAirt), 1), prMonth, 
#                                    round (diMonthlyAirt, 1), 
#                                    round (CtoF (diMonthlyAirt, difference  = T), 1), 
#                                    treeLocationName)
#       }
#     } else if (abs (diMonthlyPrec) >= sdMonthlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
#       if (diMonthlyPrec < 0  | TEST == 4) { # Dry month
#         postDetails <- getPostDetails ('monthlyClimateSummary - dry', 
#                                        gs_posts_key = gsPostsKey)
#         message        <- sprintf (postDetails [['Message']], 
#                                    round (maxAirT, 1),
#                                    round (CtoF (maxAirT), 1), 
#                                    round (meMonthlyPrec, 1), 
#                                    treeLocationName)
#       } else if (diMonthlyAirt > 0 | TEST == 5) { # Wet month
#         postDetails <- getPostDetails ('monthlyClimateSummary - wet', 
#                                        gs_posts_key = gsPostsKey)
#         message        <- sprintf (postDetails [['Message']], 
#                                    format (Sys.Date () - 1, '%d %B'), 
#                                    '23:59h',
#                                    round (acMonthlyPrec, 1), 
#                                    round (mmtoInches (acMonthlyPrec)), 
#                                    rainyDays,
#                                    days_in_month (month (Sys.Date () - 1)),
#                                    prMonth)
#       }
#     }
#     delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
#     expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
#     ptable    <- add_row (ptable, 
#                           priority   = postDetails [["Priority"]], 
#                           fFigure    = postDetails [["fFigure"]],
#                           figureName = postDetails [["FigureName"]], 
#                           message    = message, 
#                           hashtags   = postDetails [["Hashtags"]], 
#                           expires    = expirDate)
#   }
#   # return the table with posts
#   #--------------------------------------------------------------------------------------
#   return (ptable)
# } 


# Summarise and compare last year's climate at the beginning of the year
#----------------------------------------------------------------------------------------
annualClimateSummary <- function (ptable, TEST = 0) {
  
  # Check whether it is the first day of the year
  #---------------------------------------------------------------------------------------#
  if (substring (Sys.Date (), 6, 10) == '01-01' & substring (Sys.time (), 12, 15) == '12:0'| 
      TEST >= 1) {
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    acYearlyAirt <- head (tail (yearlyAirt [['airt']], n = 2), n = 1) 
    meYearlyAirt <- mean (yearlyAirt [['airt']] [2:31]) # Compare to 1964-1993 mean
    sdYearlyAirt <- sd   (yearlyAirt [['airt']] [2:31])
    diYearlyAirt <- acYearlyAirt - meYearlyAirt
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    acYearlyPrec <- head (tail (yearlyPrec [['prec']], n = 2), n = 1) 
    meYearlyPrec <- mean (yearlyPrec [['prec']] [2:31])
    sdYearlyPrec <- sd   (yearlyPrec [['prec']] [2:31])
    diYearlyPrec <- acYearlyPrec - meYearlyPrec
    
    # Choose what to talk about
    if (diYearlyAirt < sdYearlyAirt & diYearlyPrec < sdYearlyPrec | TEST == 1) { # Just an close-to-average Year 
      postDetails <- getPostDetails ('annualClimateSummary - normal')
      message        <- sprintf (postDetails [['Message']], round (acYearlyAirt, 1), round (acYearlyPrec, 1), year (Sys.Date ()) - 1)
    } else if (abs (diYearlyAirt) >= sdYearlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
      if (diYearlyAirt < 0 | TEST == 2) { # Cold Year
        postDetails <- getPostDetails ('annualClimateSummary - cold')
        message        <- sprintf (postDetails [['Message']], round (meYearlyAirt, 1), round (-diYearlyAirt, 1), prYear)
      } else if (diYearlyAirt > 0 | TEST == 3) { # Warm Year
        postDetails <- getPostDetails ('annualClimateSummary - warm')
        message        <- sprintf (postDetails [['Message']], round (meYearlyAirt, 1), round (-diYearlyAirt, 1), prYear)
      }
    } else if (abs (diYearlyPrec) >= sdYearlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
      if (diYearlyPrec < 0  | TEST == 4) { # Dry Year
        postDetails <- getPostDetails ('annualClimateSummary - dry')
        message        <- sprintf (postDetails [['Message']], round (meYearlyPrec, 1), round (-diYearlyPrec, 1), prYear)
      } else if (diYearlyAirt > 0 | TEST == 5) { # Wet Year
        postDetails <- getPostDetails ('annualClimateSummary - wet')
        message        <- sprintf (postDetails [['Message']], round (meYearlyPrec, 1), round (-diYearlyPrec, 1), prYear)
      }
    }
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,8))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority   = postDetails [["Priority"]], 
                          fFigure    = postDetails [["fFigure"]],
                          figureName = postDetails [["FigureName"]], 
                          message    = message, 
                          hashtags   = postDetails [["Hashtags"]], 
                          expires    = expirDate)
  }
  
  return (ptable)
}

# Check for first frost event of the year and late frosts during the growing season
#----------------------------------------------------------------------------------------
checkFrost <- function (ptable, TEST = 0) {
  
  # Assume there has been no frost
  #--------------------------------------------------------------------------------------
  FROST <- FALSE
  
  # Check for first frost (after July)
  #--------------------------------------------------------------------------------------
  if ((substring (Sys.Date (), 6, 10) >= '07-31' & tail (airt [['airt']], n = 1) < 0.0) | TEST == 1) {
    postDetails <- getPostDetails ('checkFrost - first')
    FROST <- TRUE
  }
  
  # Check for late frosts (after April and with at least three preceeding frost-free days)
  #--------------------------------------------------------------------------------------
  if ((substring (Sys.Date (), 6, 10) >= '05-01' &         # after April
       substring (Sys.Date (), 6, 10) <= '08-01' &         # before August
       tail (airt [['airt']], n = 1) < 0.0 &               # frost, aka air temperature below freezing
       sum (airt [['airt']] [airt [['day']] >= format (Sys.Date () - 3, '%Y-%m-%d') & 
                             airt [['day']] <  format (Sys.Date (),     '%Y-%m-%d') & 
                             !is.na  (airt [['airt']])                                &
                             !is.nan (airt [['airt']])] <= 0.0, na.rm = T) < 1 )| # no frost in preceeding three days
      TEST == 2) {                                        # or we are testing
    postDetails <- getPostDetails ('checkFrost - late')
    FROST <- TRUE
  }

  # Determine number of preceeding frost free days
  #--------------------------------------------------------------------------------------
  if (FROST) {
    frostFreeDays <- 0 
    NOFROST <- TRUE
    while (NOFROST) { # go back in time
      # Select only temperatures during the day prior to the last checked day
      temps <- airt [['airt']] [airt [['day']] >= format (Sys.Date () - (frostFreeDays), '%Y-%m-%d')]
      if (sum (temps < 0.0, na.rm = T) > 0.0) {
        NOFROST = FALSE
      } else {
        frostFreeDays <- frostFreeDays + 1
      }
    } 
  
    # Compose post details
    #------------------------------------------------------------------------------------
    message   <- sprintf (postDetails [['Message']],  frostFreeDays) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  return (ptable)
}

# Check for a heatwave
#----------------------------------------------------------------------------------------
checkHeatWave <- function (ptable, TEST = 0) {

  # Define heatwave threshold
  #-------------------------------------------------------------------------------------#
  # Here a heatwave is define as more than two days that have daily maximum temperatures
  # exceeding the 90th percentile of the Fisher Meterological Station record (Shaler only 
  # had daily temperatures, thus the mean and max would be identical) for the current month.
  #-------------------------------------------------------------------------------------#
  percentile90th <- quantile (dailyMaxAirt [['airt']] [as.numeric (substring (dailyMaxAirt [['day']], 1, 4)) > 2002 &
                                                       month (dailyMaxAirt [['day']]) == month (Sys.Date ())],
                              probs = 0.9, na.rm = T)

  # Check for a heatwave 
  #-------------------------------------------------------------------------------------#
  if ((tail (dailyMaxAirt [['airt']], n = 1)               > percentile90th & 
       head (tail (dailyMaxAirt [['airt']], n = 2), n = 1) > percentile90th) | TEST == 1) {

    # Count days since the start of the heatwave
    #-------------------------------------------------------------------------------------#
    heatWaveDays <- 0
    HEATWAVE <- T
    while (HEATWAVE) {
      temp <- head (tail (dailyMaxAirt [['airt']], n = heatWaveDays+1), n = 1)  
      if (temp > percentile90th) {
        heatWaveDays <- heatWaveDays + 1
      } else {
        HEATWAVE <- F
      }
    }
    
    # Parse message and expiration date
    #-------------------------------------------------------------------------------------#
    postDetails <- getPostDetails ('checkHeatWave')
    message   <- sprintf (postDetails [['Message']],  heatWaveDays, month (Sys.Date (), label = T, abbr = F)) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [["fFigure"]],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  return (ptable)
}

# Check for a windy day/storm (i.e. day with max windspeed above 15 m/s)
#----------------------------------------------------------------------------------------
checkStorm <- function (ptable, TEST = 0){
  
  # Set Storm boolean to FALSE by default
  #--------------------------------------------------------------------------------------
  STORM <- FALSE
  
  # Check whether the max wind speed for the day was above 15 m/s 
  #--------------------------------------------------------------------------------------
  if (tail (gust [['gust']], n = 1) > 15.0 | TEST == 1) {
    
    # Parse message and expiration date
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails ('checkStorm - windy')
    message   <- sprintf (postDetails [['Message']],  round (tail (wind [['wind']], n = 1), 1), 
                          round (tail (gust [['gust']], n = 1)*2.23694, 1), treeLocationName) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    STORM <-TRUE
  }
  
  # Check whether rainfall during the storm was above 50 mm
  #--------------------------------------------------------------------------------------
  precIn24Hours <- sum (tail (prec [['prec']], n = 24 * 4), na.rm = T) 
  if (precIn24Hours > 50.0 | TEST == 2) {
    
    # Parse message and expiration date
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails ('checkStorm - wet')
    message   <- sprintf (postDetails [['Message']],  round (precIn24Hours / 10.0, 1), 
                          round (mmtoInches (precIn24Hours), 1)) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    STORM <- TRUE
  }
  
  if (STORM) {
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [["fFigure"]],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  # Return post details to main script
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# Check for rainfall above 1.5mm per fifteen minutes
#----------------------------------------------------------------------------------------
checkRainfall <- function (ptable, TEST = 0) {

  # Check for pretty heavy rain (more than 1.5mm per fifteen minutes)
  #--------------------------------------------------------------------------------------
  if (tail (prec [['prec']], n = 1) > 1.5 | TEST == 1) {

    # Load dependencies if necessary
    #------------------------------------------------------------------------------------
    if (!existsFunction ('cols')) library ('tidyverse')

    # Plot figure of growth and rainfall over the last two weeks
    #------------------------------------------------------------------------------------
    radGrowth <- calcRadialGrowth (temporalRes = 'daily', 
                                   pdm_calibration_path = dataPath, 
                                   PLOT = TRUE)
    
    # Get post details    
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails (fName = 'checkRainfall')
    if (substring (postDetails [['Message']], 1, 1) == 'T') {
      message <- sprintf (postDetails [['Message']], 
                          round (max (radGrowth [['dailyGrowth']], na.rm = TRUE), 2))
    } else {
      message <- postDetails [['Message']]
    }
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7)) * 60.0 * 60.0
    expirDate <- sprintf ("%s %s", format (Sys.time () + delay, format = '%Y-%m-%d %H:%M:%S'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority    = postDetails [['Priority']], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = sprintf ('%s/tmp/dailyGrowth_%s.png',path,Sys.Date ()), 
                          message     = message, 
                          hashtags    = postDetails [['Hashtags']], 
                          expires     = expirDate)
  }
  
  # Return the post details
  #--------------------------------------------------------------------------------------
  return (ptable)
}
#=======================================================================================#