#=======================================================================================#
# Functions to generate messages depending on specific recurring dates such as:
#
#       Event                                   Date                 
#---------------------------------------------------------------------------------------#
#   0)  Hello World!                            14th of April 2019
#   1)  New Years                               1st of January       
#   2)  National Houseplant Appreciation Day    10th of January #TTR Not sure we will have this one
#   2)  Pi Day                                  14th of March
#   3)  Birthday                                tbd
#   4)  Spring Equinox                          20th of March
#    )  World Water Day                         22nd of March
#   5)  Earth Day                               22nd of April
#   6)  Arbor Day                               Last Friday in April
#   7)  Mother's Day                            Second Sunday of May
#   8)  Summer Solstice                         21st of June
#   9)  Autumn Equinox                          23rd of September
#   10) Halloween                               31st of October
#   11) Winter Solstice                         21st of December
# Changing of times! Spring forward and fall back!
#
#---------------------------------------------------------------------------------------#

# Load dependencies
#---------------------------------------------------------------------------------------#
suppressMessages (require ('RcppBDT'))

# Hello world! message (post once on 15th of July)
#---------------------------------------------------------------------------------------#
helloWorld <- function (mtable, TEST = 0) {
  if (Sys.Date () == '2019-06-27' | TEST == 1) {
    messageDetails <- getMessageDetails ("helloWorld")
    message   <- sprintf (messageDetails [["Message"]], treeLocationName)
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
} 

# New Years (annual post on 1st of January)
#---------------------------------------------------------------------------------------#
checkNewYears <- function (mtable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '01-01' | TEST == 1) {
    messageDetails <- getMessageDetails ("checkNewYears")
    message   <- sprintf (messageDetails [["Message"]], round (meanAnnualCarbonSequestration, 0), year (Sys.time ()))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
    } 
  return (mtable)
} # TTR To do: - maybe change to comparison to CO2 sequestered in last year?

# National Wildlife Day (annual post on 4th of March)
#---------------------------------------------------------------------------------------#
checkNationalWildLifeDay <- function (mtable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '03-04' | TEST == 1) {
    messageDetails <- getMessageDetails ("checkNationalWildLifeDay")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
    } 
  return (mtable)
}

# Pi Day (annual post on 14th of March)
#---------------------------------------------------------------------------------------#
checkPiDay <- function (mtable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '03-14' | TEST == 1) {
    messageDetails <- getMessageDetails ("checkPiDay")
    message <- ifelse (substring (messageDetails [["Message"]],16,16) == "P", 
                       messageDetails [["Message"]],
                       sprintf (messageDetails [["Message"]], dbh_cyl, sapFlowArea))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
} 
# TTR To do: - find out how to render pi as the greek letter on twitter. 
#            - add the diameter and circumference to the second message 
#            - add image of LIDAR model 

# International Day of Forests Script (annual post falls on the 21st of March)
#---------------------------------------------------------------------------------------#
checkInternationalDayOfForests <- function (mtable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == "03-21" | TEST == 1) {
    messageDetails <- getMessageDetails ("checkInternationalDayOfForests")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
}

# World Water Day Script (annual post falls on the 22nd of March)
#---------------------------------------------------------------------------------------#
checkWorldWaterDay <- function (mtable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == "03-22" | TEST == 1) {
    messageDetails <- getMessageDetails ("checkWorldWaterDay")
    message <- ifelse (substring (messageDetails [["Message"]],3,3) == "i", 
                       sprintf (messageDetails [["Message"]], percentWaterContent),
                       messageDetails [["Message"]])
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
}

# Birthday 
#---------------------------------------------------------------------------------------#
checkBirthday <- function (mtable, TEST = 0) { ## calculate stats for how much witnesstree has grown in a year
  if (substring (Sys.Date (), 6, 10) == substring (birthDay, 6, 10) | TEST == 1) {
    len <- nchar (as.character (age))
    if (substring (as.character (age), len, len) == 1) {
      subst <- 'st'
    } else if (substring (as.character (age), len, len) == 2) {
      subst <- 'nd'
    } else {
      subst <- 'th'
    }
    messageDetails <- getMessageDetails ('checkBirthday')
    message   <- sprintf (messageDetails [["Message"]], age, subst)
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)  
  }
  return(mtable)
} 
# TTR To do: Set birthday (ask John O'Keefe, maybe?)

# Arbor Day Script (annual post falls on the last Friday in April)
#---------------------------------------------------------------------------------------#
checkArborDay <- function (mtable, TEST = 0) {
  if (as.numeric (substring (Sys.Date (), 1, 4))%%4 == 0) { # Define the 30th of April in a leap year
    doy <- 120 
  } else { # not a leap year
    doy <- 121
  }
  if ((weekdays (Sys.Date ()) == 'Friday') & 
      (months   (Sys.Date ()) == 'April' ) &
      (as.numeric (strftime (Sys.Date (), format = '%j')) > (doy - 6)) | TEST == 1) {
    messageDetails <- getMessageDetails ('checkArborDay')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
}

# Mother's Day Script (annual post falls on the second Sunday in may)
#---------------------------------------------------------------------------------------#
checkMothersDay <- function (mtable, TEST = 0) {
  if (as.numeric (substring (Sys.Date (), 1, 4))%%4 == 0) { # Define 1st of may in a leap year
    doy <- 127 
  } else { # not a leap year
    doy <- 128 
  }
  if ((weekdays (Sys.Date ()) == 'Sunday') & 
      (months   (Sys.Date ()) == 'May'   ) &
      (as.numeric (strftime (Sys.Date (), format = '%j')) > doy) | TEST == 1) {
    messageDetails <- getMessageDetails ('checkMothersDay')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
}

# Earth Day Script (annual post)
#---------------------------------------------------------------------------------------#
checkEarthDay <- function (mtable, TEST = 0) {
  if (substring (Sys.time (), 6, 10) == "04-22" | TEST == 1) {
    messageDetails <- getMessageDetails ('checkEarthDay')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  return (mtable)
}


# Spring Equinox (annual post) 
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkSpringEquinox <- function (mtable, TEST = 0) {
  solarDates <- suppressWarnings (read_csv (file = './data/solarDates.csv', 
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
    messageDetails <- getMessageDetails ('checkSpringEquinox')
    if (substring (messageDetails [['Message']],2,2) == 'D') {
      message <- messageDetails [['Message']]
    } else if (substring (messageDetails [['Message']],2,2) == 'F') {
      temperatureC <- tail (airt [['airt']], n = 1)
      message <- sprintf (messageDetails [['Message']], temperatureC, treeLocationName)
    } else if (substring (messageDetails [['Message']],2,2) == 'T') {
      message <- sprintf (messageDetails [['Message']],  hour (vernalDate), minute (vernalDate))
    }
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate) 
  } 
  return (mtable)
} 


# Autumn Equinox (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkAutumnEquinox <- function (mtable, TEST = 0) {
  solarDates <- suppressWarnings (read_csv (file = './data/solarDates.csv', 
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
    messageDetails <- getMessageDetails ('checkAutumnEquinox')
    if (substring (messageDetails [['Message']],2,2) == 'A') {
      message <- sprintf (messageDetails [['Message']],  hour (autumnalDate), minute (autumnalDate))
    } else if (substring (messageDetails [['Message']],2,2) == 'L') {
      message <- messageDetails [['Message']]
    }
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate) 
  } 
  return (mtable)
}

# Summer Solstice (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkSummerSolstice <- function (mtable, TEST = 0) {
  solarDates <-  suppressWarnings (read_csv (file = './data/solarDates.csv', 
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
    messageDetails <- getMessageDetails ('checkSummerSolstice')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]], 
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)  
  } 
  return (mtable)
}


# Winter Solstices (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the data folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkWinterSolstice <- function (mtable, TEST = 0) {
  solarDates <-  suppressWarnings (read_csv (file = './data/solarDates.csv', 
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
    messageDetails <- getMessageDetails ('checkWinterSolstice')
    expirDate       <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate)   
  } 
  return (mtable)
}

# Halloween (annual post)
#---------------------------------------------------------------------------------------#
checkHalloween <- function (mtable, TEST = 0) {
  if (substring (Sys.time (), 6, 10) == "10-31" | TEST == 1) {
    messageDetails <- getMessageDetails ('checkHalloween')
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- add_row (mtable, 
                          priority    = messageDetails [["Priority"]],
                          fFigure     = messageDetails [['fFigure']],
                          figureName  = messageDetails [["FigureName"]], 
                          message     = messageDetails [['Message']], 
                          hashtags    = messageDetails [["Hashtags"]], 
                          expires     = expirDate) 
  } 
  return (mtable)
}
