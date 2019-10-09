#========================================================================================
# Functions to generate messages depending on specific recurring dates such as:
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   0)  Hello World!                            14th of April 2019
#   1)  New Years                               1st of January       
#   2)  National Wildlife Day                   4th of March
#   3)  Pi Day                                  14th of March
#   4)  International day of forests            21st of March
#   5)  World Water Day                         22nd of March
#   6)  Birthday                                tbd
#   7)  Arbor Day                               Last Friday in April
#   8)  Mother's Day                            Second Sunday of May
#   9)  Earth Day                               22nd of April
#   10) Spring Equinox                          ~20th of March
#   11) Autumn Equinox                          ~23rd of September
#   12) Summer Solstice                         ~21st of June
#   13) Winter Solstice                         ~21st of December
#   14) Halloween                               31st of October
#   15) Monthly engagement reminder             third week of each month 
#
# Possible additions: - Changing of times! Spring forward and fall back!
#                     - Thanks Giving
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('day')) library ('lubridate')

# 0 - Hello world! message (post once on 15th of July)
#----------------------------------------------------------------------------------------
helloWorld <- function (ptable, TEST = 0) {
  if (substring (Sys.time (), 1, 13) == '2019-07-17 12' | TEST == 1) {
    postDetails <- getPostDetails ("helloWorld")
    message     <- sprintf (postDetails [["Message"]], treeLocationName, treeState, 
                            treeWebPage)
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), 
                          treeTimeZone)
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

# 1 - New Years (annual post on 1st of January)
#----------------------------------------------------------------------------------------
checkNewYears <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '01-01' | TEST == 1) {
    postDetails <- getPostDetails ("checkNewYears")
    message   <- sprintf (postDetails [["Message"]], round (meanAnnualCarbonSequestration, 0), 
                          round (2.20462 * meanAnnualCarbonSequestration, 0), year (Sys.time ()))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
    } 
  return (ptable)
} # TTR To do: - maybe change to comparison to CO2 sequestered in last year?

# 2 - National Wildlife Day (annual post on 4th of March)
#----------------------------------------------------------------------------------------
checkNationalWildLifeDay <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '03-04' | TEST == 1) {
    postDetails <- getPostDetails ("checkNationalWildLifeDay")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
    } 
  return (ptable)
}

# 3 - Pi Day (annual post on 14th of March)
#----------------------------------------------------------------------------------------
checkPiDay <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '03-14' | TEST == 1) {
    postDetails <- getPostDetails ("checkPiDay")
    message <- ifelse (substring (postDetails [["Message"]],16,16) == "P", 
                       postDetails [["Message"]],
                       sprintf (postDetails [["Message"]], round (dbh, 2), round (sapWoodArea,1)))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
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
# TTR To do: - find out how to render pi as the greek letter on twitter. 

# 4 - International Day of Forests Script (annual post falls on the 21st of March)
#----------------------------------------------------------------------------------------
checkInternationalDayOfForests <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == "03-21" | TEST == 1) {
    postDetails <- getPostDetails ("checkInternationalDayOfForests")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (ptable)
}

# 5 - World Water Day Script (annual post falls on the 22nd of March)
#----------------------------------------------------------------------------------------
checkWorldWaterDay <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == "03-22" | TEST == 1) {
    postDetails <- getPostDetails ("checkWorldWaterDay")
    if (substring (postDetails [["Message"]],2,2) == "i") {
      message <- sprintf (postDetails [["Message"]], percentWaterContent) 
    } else {
      message <- postDetails [["Message"]]      
    }
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
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

# 6 - Birthday (tbd)
#----------------------------------------------------------------------------------------
checkBirthday <- function (ptable, TEST = 0) { ## calculate stats for how much witnesstree has grown in a year
  if (substring (Sys.Date (), 6, 10) == substring (birthDay, 6, 10) | TEST == 1) {
    postDetails <- getPostDetails ('checkBirthday')
    message   <- sprintf (postDetails [["Message"]], age, findOrdinalSuffix (age))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)  
  }
  return(ptable)
} 
# TTR To do: Set birthday (ask John O'Keefe, maybe?)

# 7- Arbor Day Script (annual post falls on the last Friday in April)
#----------------------------------------------------------------------------------------
checkArborDay <- function (ptable, TEST = 0) {
  if (as.numeric (substring (Sys.Date (), 1, 4))%%4 == 0) { # Define the 30th of April in a leap year
    doy <- 120 
  } else { # not a leap year
    doy <- 121
  }
  if ((weekdays (Sys.Date ()) == 'Friday') & 
      (months   (Sys.Date ()) == 'April' ) &
      (as.numeric (strftime (Sys.Date (), format = '%j')) > (doy - 6)) | TEST == 1) {
    postDetails <- getPostDetails ('checkArborDay')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (ptable)
}

# 8 - Mother's Day Script (annual post falls on the second Sunday in May)
#----------------------------------------------------------------------------------------
checkMothersDay <- function (ptable, TEST = 0) {
  if (as.numeric (substring (Sys.Date (), 1, 4))%%4 == 0) { # Define 1st of may in a leap year
    doy <- 127 
  } else { # not a leap year
    doy <- 128 
  }
  if ((weekdays (Sys.Date ()) == 'Sunday') & 
      (months   (Sys.Date ()) == 'May'   ) &
      (as.numeric (strftime (Sys.Date (), format = '%j')) > doy) | TEST == 1) {
    postDetails <- getPostDetails ('checkMothersDay')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (ptable)
}

# 9 - Earth Day Script (annual post on 22nd of April)
#----------------------------------------------------------------------------------------
checkEarthDay <- function (ptable, TEST = 0) {
  if (substring (Sys.time (), 6, 10) == "04-22" | TEST == 1) {
    postDetails <- getPostDetails ('checkEarthDay')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (ptable)
}

# 10 - Spring Equinox (annual post) 
#----------------------------------------------------------------------------------------
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#----------------------------------------------------------------------------------------
checkSpringEquinox <- function (ptable, TEST = 0) {
  
  if (!existsFunction ('year')) library ('lubridate')
  
  solarDates <- suppressWarnings (read_csv (file = sprintf ('%ssolarDates.csv', dataPath), 
                                            skip = 3,
                                            col_types = cols ()))
  solarDates [['Vernal Equinox']] <- as.POSIXct (sprintf ('%s %s', 
                                                          solarDates [['Year']], 
                                                          solarDates [['Vernal Equinox']]), 
                                                 format = '%Y %m/%d %H:%M', 
                                                 tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  vernalDate <- solarDates [['Vernal Equinox']] [index] # Extract this years date 
  attributes (vernalDate)$tzone <- treeTimeZone         # Change timezone

  # Check whether it is that day
  if (       Sys.time ()  >= vernalDate         & 
      month (Sys.Date ()) == month (vernalDate) & 
      day   (Sys.Date ()) == day   (vernalDate) | TEST == 1) { 
    postDetails <- getPostDetails ('checkSpringEquinox')
    if (substring (postDetails [['Message']], 1, 1) == 'D') {
      message <- postDetails [['Message']]
    } else if (substring (postDetails [['Message']], 1, 1) == 'F') {
      temperatureC <- tail (airt [['airt']], n = 1)
      message <- sprintf (postDetails [['Message']], round (temperatureC, 1), 
                          round (CtoF (temperatureC), 1), treeLocationName)
    } else if (substring (postDetails [['Message']], 1, 1) == 'S') {
      message <- sprintf (postDetails [['Message']],  hour (vernalDate), minute (vernalDate))
    }
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate) 
  } 
  return (ptable)
} 

# 11 - Autumn Equinox (annual post)
#----------------------------------------------------------------------------------------
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#----------------------------------------------------------------------------------------
checkAutumnEquinox <- function (ptable, TEST = 0) {
  
  if (!existsFunction ('year')) library ('lubridate')
  
  solarDates <- suppressWarnings (read_csv (file = sprintf ('%ssolarDates.csv', dataPath), 
                                            skip = 3,
                                            col_types = cols ()))
  solarDates [['Autumnal Equinox']] <- as.POSIXct (sprintf ('%s %s', 
                                                          solarDates [['Year']], 
                                                          solarDates [['Autumnal Equinox']]), 
                                                 format = '%Y %m/%d %H:%M', 
                                                 tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  autumnalDate <- solarDates [['Autumnal Equinox']] [index] # Extract this years date
  attributes (autumnalDate)$tzone <- treeTimeZone           # Change timezone
  if (       Sys.time ()  >=        autumnalDate  & 
      month (Sys.Date ()) == month (autumnalDate) & 
      day   (Sys.Date ()) == day   (autumnalDate) | TEST == 1) { 
    postDetails <- getPostDetails ('checkAutumnEquinox')
    if (substring (postDetails [['Message']],2,2) == 'A') {
      message <- sprintf (postDetails [['Message']],  hour (autumnalDate), minute (autumnalDate))
    } else if (substring (postDetails [['Message']],2,2) == 'L') {
      message <- postDetails [['Message']]
    }
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate) 
  } 
  return (ptable)
}

# 12 - Summer Solstice (annual post)
#----------------------------------------------------------------------------------------
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#----------------------------------------------------------------------------------------
checkSummerSolstice <- function (ptable, TEST = 0) {
  
  if (!existsFunction ('year')) library ('lubridate')
  
  solarDates <-  suppressWarnings (read_csv (file = sprintf ('%ssolarDates.csv', dataPath), 
                                             skip = 3,
                                             col_types = cols ()))
  solarDates [['Summer Solstice']] <- as.POSIXct (sprintf ('%s %s', 
                                                           solarDates [['Year']], 
                                                           solarDates [['Summer Solstice']]), 
                                                   format = '%Y %m/%d %H:%M', 
                                                   tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  solsticeDate <- solarDates [['Summer Solstice']] [index] # Extract this years date 
  attributes (solsticeDate)$tzone <- treeTimeZone         # Change timezone
  if (       Sys.time ()  >=        solsticeDate  & 
      month (Sys.Date ()) == month (solsticeDate) & 
      day   (Sys.Date ()) == day   (solsticeDate) | TEST == 1) { 
    postDetails <- getPostDetails ('checkSummerSolstice')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)  
  } 
  return (ptable)
}

# 13 - Winter Solstices (annual post)
#----------------------------------------------------------------------------------------
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#----------------------------------------------------------------------------------------
checkWinterSolstice <- function (ptable, TEST = 0) {
  
  if (!existsFunction ('year')) library ('lubridate')
  
  solarDates <-  suppressWarnings (read_csv (file = sprintf ('%ssolarDates.csv', dataPath), 
                                             skip = 3,
                                             col_types = cols ()))
  solarDates [['Winter Solstice']] <- as.POSIXct (sprintf ('%s %s', 
                                                           solarDates [['Year']], 
                                                           solarDates [['Winter Solstice']]), 
                                                  format = '%Y %m/%d %H:%M', 
                                                  tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  solsticeDate <- solarDates [['Winter Solstice']] [index] # Extract this years date 
  attributes (solsticeDate)$tzone <- treeTimeZone          # Change timezone
  if (       Sys.time ()  >=        solsticeDate  & 
      month (Sys.Date ()) == month (solsticeDate) & 
      day   (Sys.Date ()) == day   (solsticeDate) | TEST == 1) { 
    postDetails <- getPostDetails ('checkWinterSolstice')
    expirDate       <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)   
  } 
  return (ptable)
}

# 14 - Halloween (annual post)
#----------------------------------------------------------------------------------------
checkHalloween <- function (ptable, TEST = 0) {
  if (substring (Sys.time (), 6, 10) == "10-31" | TEST == 1) {
    postDetails <- getPostDetails ('checkHalloween')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['Message']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate) 
  } 
  return (ptable)
}

# 15 - Monthly engagement reminder (every third week of the month)
#----------------------------------------------------------------------------------------
monthlyEngagementReminder <- function (ptable, TEST = 0) {
  if (ceiling (lubridate::day (Sys.Date ()) / 7) == 3 | TEST == 1) {
    postDetails <- getPostDetails ('monthlyEngagementReminder')
    message     <- sprintf (postDetails [["Message"]], treeWebPage)
    delay       <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate   <- sprintf ("%s 23:59:59 %s", 
                            format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable      <- add_row (ptable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = postDetails [["FigureName"]], 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate) 
  } 
  
  # return table with posts
  #----------------------------------------------------------------------------------------
  return (ptable)
}
