#=======================================================================================#
# Functions to generate messages depending on the climate around the tree.
#
#       Event                                   Date                 
#---------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------#
# All functions require weather station records to be read in and converted to a 
# structure such as in readClimate.R
#---------------------------------------------------------------------------------------#

# To-Do list:
# TR - I might also want to delay messages for December and last year, so that they do not come out straight after new year's resolutions.
# TR - I need to introduce a memory or something to make sure that I am not repeating the same messages for hottest/coldest temperature overa nd over again.

# Hottest or coldest temperature on record (in memory)
#---------------------------------------------------------------------------------------#
checkExtremeTemperatures <- function (mtable, TEST = 0) {
  
  # Check whether the current temperature is the hottest temperature on record 
  #-------------------------------------------------------------------------------------#
  if (max (airt [['airt']], na.rm = T) < tail (airt [['airt']], n = 1) | TEST == 1) {
    HOTTEST <- T
  } else {
    HOTTEST <- F
  }
  
  # Check whether the yesterday was the warmest day on record
  #-------------------------------------------------------------------------------------#
  if (max (dailyAirt [['airt']], na.rm = T) < head (tail (dailyAirt [['airt']], n = 2), n = 1) | TEST == 2) {
    HOTTESTDAY <- T
  } else {
    HOTTESTDAY <- F
  }
  
  # Check whether the last week was the warmest week on record
  #-------------------------------------------------------------------------------------#
  if (max (weeklyAirt [['airt']], na.rm = T) < head (tail (weeklyAirt [['airt']], n = 2), n = 1) | TEST == 3) { 
    HOTTESTWEEK <- T
  } else {
    HOTTESTWEEK <- F
  }
  
  # Check whether the last month was the warmest month on record
  #-------------------------------------------------------------------------------------#
  if (max (monthlyAirt [['airt']], na.rm = T) < head (tail (monthlyAirt [['airt']], n = 2), n = 1) | TEST == 4) { 
    HOTTESTMONTH <- T
  } else {
    HOTTESTMONTH <- F
  }
  
  # Check whether the last year was the warmest year on record
  #-------------------------------------------------------------------------------------#
  if (max (yearlyAirt [['airt']], na.rm = T) < head (tail (yearlyAirt [['airt']], n = 2), n = 1) | TEST == 5) { 
    HOTTESTYEAR <- T
  } else {
    HOTTESTYEAR <- F
  }
  
  # Check whether the current temperature is the coldest temperature on record
  #-------------------------------------------------------------------------------------#
  if (min (airt [['airt']], na.rm = T) > tail (airt [['airt']], n = 1) | TEST == 6) {
    COLDEST <- T
  } else {
    COLDEST <- F
  }
  temperatureC <- tail (airt [['airt']], n = 1)
  temperatureF <- (temperatureC * 9.0 / 5.0) + 32
  
  # Check whether the yesterday was the coldest day on record
  #-------------------------------------------------------------------------------------#
  if (min (dailyAirt$airt, na.rm = T) > head (tail (dailyAirt$airt, n = 2), n = 1) | TEST == 7) {
    COLDESTDAY <- T
  } else {
    COLDESTDAY <- F
  }
  dailyTemperatureC <- head (tail (dailyAirt [['airt']], n = 2), n = 1)
  dailyTemperatureF <- (dailyTemperatureC * 9.0 / 5.0) + 32
  
  # Check whether the last week was the coldest week on record
  #-------------------------------------------------------------------------------------#
  if (min (weeklyAirt [['airt']], na.rm = T) > head (tail (weeklyAirt [['airt']], n = 2), n = 1) | TEST == 8) { 
    COLDESTWEEK <- T
  } else {
    COLDESTWEEK <- F
  }
  weeklyTemperatureC <- head (tail (weeklyAirt [['airt']], n = 2), n = 1)
  weeklyTemperatureF <- (weeklyTemperatureC * 9.0 / 5.0) + 32
  
  # Check whether the last month was the coldest month on record
  #-------------------------------------------------------------------------------------#
  if (min (monthlyAirt [['airt']], na.rm = T) > head (tail (monthlyAirt [['airt']], n = 2), n = 1) | TEST == 9) { 
    COLDESTMONTH <- T
  } else {
    COLDESTMONTH <- F
  }
  monthlyTemperatureC <- head (tail (monthlyAirt [['airt']], n = 2), n = 1)
  monthlyTemperatureF <- (monthlyTemperatureC * 9.0 / 5.0) + 32
  
  # Check whether the last year was the warmest year on record
  #-------------------------------------------------------------------------------------#
  if (max (yearlyAirt [['airt']], na.rm = T) > head (tail (yearlyAirt [['airt']], n = 2), n = 1) | TEST == 10) { 
    COLDESTYEAR <- T
  } else {
    COLDESTYEAR <- F
  }
  
  # Send message if it is the hottest or coldest temperature
  #-------------------------------------------------------------------------------------#
  if (HOTTEST | COLDEST | HOTTESTDAY | COLDESTDAY | HOTTESTWEEK | COLDESTWEEK | HOTTESTMONTH | COLDESTMONTH | TEST) {
    priority  <- 9
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (HOTTEST) {
      messageDetails <- getMessageDetails ('hottest')
      message    <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires four hours after occurance
      expireDate <- sprintf ("%s %s", format (Sys.time () + 4* 60 * 60, format = '%Y-%m-%d %H:%M:%S'), treeTimeZone)
    } else if (COLDEST) {
      messageDetails <- getMessageDetails ('coldest')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires four hours after occurance
      expireDate <- sprintf ("%s %s", format (Sys.time () + 4 * 60 * 60, format = '%Y-%m-%d %H:%M:%S'), treeTimeZone)
    } else if (HOTTESTDAY) {
      messageDetails <- getMessageDetails ('hottestDay')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires at the end of the day
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDESTDAY) {
      messageDetails <- getMessageDetails ('coldestDay')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires at the end of the day
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    } else if (HOTTESTWEEK) {
      messageDetails <- getMessageDetails ('hottestWeek')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires within the next three days
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + 3, format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDESTWEEK) {
      messageDetails <- getMessageDetails ('coldestWeek')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires within the next three days
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + 3, format = '%Y-%m-%d'), treeTimeZone)
    } else if (HOTTESTMONTH) {
      messageDetails <- getMessageDetails ('hottestMonth')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires within the next ten days
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + 10, format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDESTMONTH) {
      messageDetails <- getMessageDetails ('coldestMonth')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires within the next ten days
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + 10, format = '%Y-%m-%d'), treeTimeZone)
    } else if (HOTTESTYEAR) {
      messageDetails <- getMessageDetails ('hottestYear')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires within the next three weeks
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + 21, format = '%Y-%m-%d'), treeTimeZone)
    } else if (COLDESTYEAR) {
      messageDetails <- getMessageDetails ('coldestYear')
      message   <- sprintf (messageDetails [['Message']], round (temperatureC, 1), round (temperatureF, 1))
      
      # Expires within the next three weeks
      expireDate <- sprintf ("%s 23:59:59", format (Sys.Date () + 21, format = '%Y-%m-%d'), treeTimeZone)
    }
    mtable    <- add_row (mtable, 
                         priority    = messageDetails [["Priority"]], 
                         figureName  = messageDetails [["FigureName"]], 
                         message     = message, 
                         hashtags    = messageDetails [["Hashtags"]], 
                         expires     = expirDate)
  } 
  return (mtable)
} 

#extremePrecipitation <- function (mtable, TEST = 0) {
  
  # Check whether the yesterday was the wettest day on record
  
  # Check whether the last week was the wettest week on record
  
  # Check whether the last month was the wettest month on record
  
  # Check whether the last year was the wettest year on record
  
  # Send message if it is the wettest
#  mtable    <- add_row (mtable, 
#                        priority = messageDetails [["Priority"]], 
#                        fFigure  = messageDetails [["fFigure"]], 
#                        message  = message, 
#                        hashtags = messageDetails [["Hashtags"]], 
#                        expires  = expirDate)
#  return (mtable)
#} 

# Summarise and compare last months climate at the beginning of the month
#---------------------------------------------------------------------------------------#
monthlyClimateSummary <- function (mtable, TEST = 0) {
  
  # Check whether it is the first day of the month
  #---------------------------------------------------------------------------------------#
  if (substring (Sys.Date (), 9, 10) == '01' & substring (Sys.time (), 12, 15) == '12:0'| TEST) {
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    acMonthlyAirt <- head (tail (monthlyAirt [['airt']], n = 2), n = 1) 
    meMonthlyAirt <- mean (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1])
    sdMonthlyAirt <- sd   (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1])
    diMonthlyAirt <- acMonthlyAirt - meMonthlyAirt
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    acMonthlyPrec <- head (tail (monthlyPrec [['prec']], n = 2), n = 1) 
    meMonthlyPrec <- mean (monthlyPrec [['prec']] [month (monthlyPrec [['month']]) == month (Sys.Date ()) - 1])
    sdMonthlyPrec <- sd   (monthlyPrec [['prec']] [month (monthlyPrec [['month']]) == month (Sys.Date ()) - 1])
    diMonthlyPrec <- acMonthlyPrec - meMonthlyPrec
    
    # Get previous month
    prMonth <- month (month (Sys.Date ()) - 1, label = T)
    
    # Choose what to talk about
    if (diMonthlyAirt < sdMonthlyAirt & diMonthlyPrec < sdMonthlyPrec | TEST == 1) { # Just an close-to-average month 
      messageDetails <- getMessageDetails ('monthlyClimateSummary - normal')
      message        <- sprintf (messageDetails [['Message']], round (acMonthlyAirt, 1), round (acMonthlyPrec, 1), prMonth)
    } else if (abs (diMonthlyAirt) >= sdMonthlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
      if (diMonthlyAirt < 0 | TEST == 2) { # Cold month
        messageDetails <- getMessageDetails ('monthlyClimateSummary - cold')
        message        <- sprintf (messageDetails [['Message']], round (meMonthlyAirt, 1), round (-diMonthlyAirt, 1), prMonth)
      } else if (diMonthlyAirt > 0 | TEST == 3) { # Warm month
        messageDetails <- getMessageDetails ('monthlyClimateSummary - warm')
        #message        <- sprintf (messageDetails [['Message']], round (meMonthlyAirt, 1), round (-diMonthlyAirt, 1), prMonth)
      }
    } else if (abs (diMonthlyPrec) >= sdMonthlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
      if (diMonthlyPrec < 0  | TEST == 4) { # Dry month
        messageDetails <- getMessageDetails ('monthlyClimateSummary - dry')
        #message        <- sprintf (messageDetails [['Message']], round (meMonthlyPrec, 1), round (-diMonthlyPrec, 1), prMonth)
      } else if (diMonthlyAirt > 0 | TEST == 5) { # Wet month
        messageDetails <- getMessageDetails ('monthlyClimateSummary - wet')
        #message        <- sprintf (messageDetails [['Message']], round (meMonthlyPrec, 1), round (-diMonthlyPrec, 1), prMonth)
      }
    }
    
    # Expires after five days
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + 5, format = '%Y-%m-%d'), treeTimeZone) 
    mtable    <- add_row (mtable, 
                          priority = messageDetails [["Priority"]], 
                          fFigure  = messageDetails [["fFigure"]], 
                          message  = message, 
                          hashtags = messageDetails [["Hashtags"]], 
                          expires  = expirDate)
  }
  
  return (mtable)
} 

# Check for first frost event of the year and late frosts during the growing season
#---------------------------------------------------------------------------------------#
checkFrost <- function (mtable, TEST = 0) {
  
  # Check for first frost (after July)
  #-------------------------------------------------------------------------------------#
  if ((substring (Sys.Date (), 6, 10) >= '07-31' & tail (airt [['airt']], n = 1) < 0.0) | TEST == 1) {
    messageDetails <- getMessageDetails ('checkFrost - first')
    delay <- as.numeric (substring (messageDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  # Check for late frosts (after April and with at least three preceeding frost-free days)
  #-------------------------------------------------------------------------------------#
  if ((substring (Sys.Date (), 6, 10) >= '05-01' &         # after April
       substring (Sys.Date (), 6, 10) <= '08-01' &         # before August
       tail (airt [['airt']], n = 1) < 0.0 &               # frost, aka air temperature below freezing
       sum (airt [['airt']] [airt [['daily']] >= format (Sys.Date () - 3, '%Y-%m-%d') & 
                             airt [['daily']] <  format (Sys.Date (),     '%Y-%m-%d') & 
                             !is.na  (airt [['airt']])                                &
                             !is.nan (airt [['airt']])] <= 0.0, na.rm = T) < 1 )| # no frost in preceeding three days
      TEST == 2) {                                        # or we are testing
    
    # Determine number of preceeding frost free days
    #-------------------------------------------------------------------------------------#
    frostFreeDays <- 0 
    NOFROST <- T
    while (NOFROST) { # go back in time
      # Select only temperatures during the day prior to the last checked day
      temps <- airt [['airt']] [airt [['daily']] >= format (Sys.Date () - (frostFreeDays), '%Y-%m-%d')]
      if (sum (temps < 0.0, na.rm = T) > 0.0) {
        NOFROST = F
      } else {
        frostFreeDays <- frostFreeDays + 1
      }
    } 
    
    # Parse message and expiration date
    #-------------------------------------------------------------------------------------#
    messageDetails <- getMessageDetails ('checkFrost - first')
    message   <- sprintf (messageDetails [['Message']],  frostFreeDays) 
    delay <- as.numeric (substring (messageDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  return (mtable)
}

# Check for a heatwave
#---------------------------------------------------------------------------------------#
checkHeatWave <- function (mtable, TEST = 0) {

  # Define heatwave threshold
  #-------------------------------------------------------------------------------------#
  # Here a heatwave is define as more than two days that have daily maximum temperatures
  # exceeding the 90th percentile of the Fisher Meterological Station record (Shaler only 
  # had daily temperatures, thus the mean and max would be identical) for the current month.
  #-------------------------------------------------------------------------------------#
  percentile90th <- quantile (dailyMaxAirt [['airt']] [as.numeric (substring (dailyMaxAirt [['daily']], 1, 4)) > 2002 &
                                                       month (dailyMaxAirt [['daily']]) == month (Sys.Date ())],
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
    messageDetails <- getMessageDetails ('checkHeatWave')
    message   <- sprintf (messageDetails [['Message']],  heatWaveDays, month (Sys.Date (), label = T, abbr = F)) 
    delay <- as.numeric (substring (messageDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  return (mtable)
}

# Check for a windy day/storm (i.e. day with max windspeed above 4 m/s???)
#---------------------------------------------------------------------------------------#
checkStorm <- function (mtable, TEST = 0){
  
  
  # Check whether the max wind speed for the day was above 5 m/s 
  #-------------------------------------------------------------------------------------#
  if (tail (wind [['wind']], n = 1) > 5.0 | TEST == 1) {
    
    # Parse message and expiration date
    #-------------------------------------------------------------------------------------#
    messageDetails <- getMessageDetails ('checkStorm')
    message   <- sprintf (messageDetails [['Message']],  round (tail (wind [['wind']], n = 1), 1), 
                          round (tail (wind [['wind']], n = 1)*2.23694, 1), treeLocationName) 
    delay <- as.numeric (substring (messageDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  return (mtable)
}

#=======================================================================================#