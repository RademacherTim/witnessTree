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
    message     <- sprintf (postDetails [["MessageText"]], treeLocationName, treeState, 
                            treeWebPage)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
  } 
  return (ptable)
} 

# 1 - New Years (annual post on 1st of January)
#----------------------------------------------------------------------------------------
checkNewYears <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '01-01' | TEST == 1) {
    postDetails <- getPostDetails ("checkNewYears")
    message   <- sprintf (postDetails [["MessageText"]], round (meanAnnualCarbonSequestration, 0), 
                          round (2.20462 * meanAnnualCarbonSequestration, 0), year (Sys.time ()))
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
    } 
  return (ptable)
} # TTR To do: - maybe change to comparison to CO2 sequestered in last year?

# 2 - National Wildlife Day (annual post on 4th of March)
#----------------------------------------------------------------------------------------
checkNationalWildLifeDay <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '03-04' | TEST == 1) {
    postDetails <- getPostDetails ("checkNationalWildLifeDay")
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
    } 
  return (ptable)
}

# 3 - Pi Day (annual post on 14th of March)
#----------------------------------------------------------------------------------------
checkPiDay <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == '03-14' | TEST == 1) {
    postDetails <- getPostDetails ("checkPiDay")
    message <- ifelse (substring (postDetails [["MessageText"]],16,16) == "P", 
                       postDetails [["MessageText"]],
                       sprintf (postDetails [["MessageText"]], round (dbh, 2), round (sapWoodArea,1)))
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
  } 
  return (ptable)
} 
# TTR To do: - find out how to render pi as the greek letter on twitter. 

# 4 - International Day of Forests Script (annual post falls on the 21st of March)
#----------------------------------------------------------------------------------------
checkInternationalDayOfForests <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == "03-21" | TEST == 1) {
    postDetails <- getPostDetails ("checkInternationalDayOfForests")
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
  } 
  return (ptable)
}

# 5 - World Water Day Script (annual post falls on the 22nd of March)
#----------------------------------------------------------------------------------------
checkWorldWaterDay <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) == "03-22" | TEST == 1) {
    postDetails <- getPostDetails ("checkWorldWaterDay")
    if (substring (postDetails [["MessageText"]],2,2) == "i") {
      message <- sprintf (postDetails [["MessageText"]], percentWaterContent) 
    } else {
      message <- postDetails [["MessageText"]]      
    }
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
  } 
  return (ptable)
}

# 6 - Birthday (tbd)
#----------------------------------------------------------------------------------------
checkBirthday <- function (ptable, TEST = 0) { ## calculate stats for how much witnesstree has grown in a year
  if (substring (Sys.Date (), 6, 10) == substring (birthDay, 6, 10) | TEST == 1) {
    postDetails <- getPostDetails ('checkBirthday')
    message   <- sprintf (postDetails [["MessageText"]], age, findOrdinalSuffix (age))
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))  
  }
  return (ptable)
} 
# TR To do: Set birthday (ask John O'Keefe, maybe?)

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
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
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
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
  } 
  return (ptable)
}

# 9 - Earth Day Script (annual post on 22nd of April)
#----------------------------------------------------------------------------------------
checkEarthDay <- function (ptable, TEST = 0) {
  if (substring (Sys.time (), 6, 10) == "04-22" | TEST == 1) {
    postDetails <- getPostDetails ('checkEarthDay')
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))
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
    if (substring (postDetails [['MessageText']], 1, 1) == 'D') {
      message <- postDetails [['MessageText']]
    } else if (substring (postDetails [['MessageText']], 1, 1) == 'F') {
      temperatureC <- tail (airt [['airt']], n = 1)
      message <- sprintf (postDetails [['MessageText']], round (temperatureC, 1), 
                          round (CtoF (temperatureC), 1), treeLocationName)
    } else if (substring (postDetails [['MessageText']], 1, 1) == 'S') {
      message <- sprintf (postDetails [['MessageText']],  hour (vernalDate), minute (vernalDate))
    }
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0)) 
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
    if (substring (postDetails [['MessageText']],2,2) == 'A') {
      message <- sprintf (postDetails [['MessageText']],  hour (autumnalDate), minute (autumnalDate))
    } else if (substring (postDetails [['MessageText']],2,2) == 'L') {
      message <- postDetails [['MessageText']]
    }
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0)) 
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
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))  
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
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0))   
  } 
  return (ptable)
}

# 14 - Halloween (annual post)
#----------------------------------------------------------------------------------------
checkHalloween <- function (ptable, TEST = 0) {
  if (substring (Sys.time (), 6, 10) == "10-31" | TEST == 1) {
    postDetails <- getPostDetails ('checkHalloween')
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = postDetails [['MessageText']], 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expiresIn (0)) 
  } 
  return (ptable)
}

# 15 - Monthly engagement reminder (every third week of the month)
#----------------------------------------------------------------------------------------
monthlyEngagementReminder <- function (ptable, TEST = 0) {
  
  # check whether it is the third week of the month
  #--------------------------------------------------------------------------------------
  if (ceiling (lubridate::day (Sys.Date ()) / 7) == 3 | TEST >= 1) {
    
    # download phenocam images and check that the download worked
    #------------------------------------------------------------------------------------
    IOStatus <- getPhenocamImagesAndData (siteName = 'Harvard Forest', # name of phenocam sites
                                          DOWNLOAD = TRUE,
                                          GCC = FALSE)
    if (IOStatus != 0) {
      stop ('Error: Phenocam images were not downloaded properly for monthly engagement reminder!') 
    } else {
      print ('Phenocam images were downloaded for monthly engagement reminder.')
    }
    
    # check whether it is an odd month
    #------------------------------------------------------------------------------------
    if (lubridate::month (Sys.Date ()) %% 2 == 1 | TEST == 1) {
      postDetails <- getPostDetails ('monthlyEngagementReminder - How are you?') 
      FigureName  <- 'harvardbarn_PhenoCamImage'
    } else if (lubridate::month (Sys.Date ()) %% 2 == 0 | TEST == 2) {
      postDetails <- getPostDetails ('monthlyEngagementReminder - Selfie')
      FigureName  <- 'witnesstree_PhenoCamImage'
    }
    message     <- sprintf (postDetails [["MessageText"]], treeWebPage)
    delay       <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    ptable      <- add_row (ptable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/tmp/%s.jpg', path, FigureName), 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expiresIn (delay)) 
  } 
  
  # return table with posts
  #----------------------------------------------------------------------------------------
  return (ptable)
}
