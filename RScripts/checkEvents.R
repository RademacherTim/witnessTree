#=======================================================================================#
# Functions to generate messages depending on specific recurring dates such as:
#
#       Event                                   Date                 
#---------------------------------------------------------------------------------------#
#   1)  New Years                               1st of January       
#   2)  National Houseplant Appreciation Day    10th of January #TTR Not sure we will have this one
#   2)  Pi Day                                  14th of March
#   3)  Birthday                                tbd
#   4)  Spring Equinox                          20th of March
#   5)  Earth Day                               22nd of April
#   6)  Arbor Day                               27th of April
#   7)  Summer Solstice                         21st of June
#   8)  Autumn Equinox                          23rd of September
#   9)  Halloween                               31st of October
#   10) Winter Solstice                         21st of December
#
#---------------------------------------------------------------------------------------#


# Load dependencies
#---------------------------------------------------------------------------------------#
require ('RcppBDT')

# New Years (annual post on 1st of January)
#---------------------------------------------------------------------------------------#
checkNewYears <- function (mtable) {
  if (substring (Sys.Date (), 6, 10) == '01-01') {
    message   <- sprintf ('Happy New Year!! During my life I fixed roughly %s kg of carbon per year. My resolution for %s is to beat that. What is your resolution?', round (meanAnnualCarbonSequestration, 0), year (Sys.time ()))
    priority  <- 10
    hashtag   <- c ('#NewYears', '#happynewyear')
    expirDate <- as.POSIXct (Sys.Date (), format = '%Y-%m-%d')
    mtable    <- rbind (mtable, c (priority, F, message, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: - maybe change to comparison to CO2 sequester in last year?

# National Wildlife Day (annual post on 4th of March)
#---------------------------------------------------------------------------------------#
checkNationalWildLifeDay <- function (mtable) {
  if (substring (Sys.Date (), 6, 10) == '03-04') {
    message   <- sprintf ("Look who's visited me earlier this year! Happy National Wildlife Day!")
    priority  <- 10
    hashtag   <- c ('#Nationalwildlifeday', '#wildlife')
    expirDate <- as.POSIXct (Sys.Date (), format = '%Y-%m-%d')
    mtable    <- rbind (mtable, c (priority, F, message, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: - Ought to have an image or even several images from the wild life camera at the tree!

# Pi Day (annual post on 14th of March)
#---------------------------------------------------------------------------------------#
checkPiDay <- function (mtable) {
  if (substring (Sys.Date (), 6, 10) == '03-14') {
    pidaymessages <- c ('Happy Pi Day! Pi is extra important to those of us shaped like a cylinder. How many digits of Pi can you recite from memory? Tweet below!',
                        sprintf ('Happy Pi Day! Thanks to Pi I can estimate my trunk diameter (%s cm) or even the area through which sap flows (%s m2).', dbh_cyl, sapFlowArea))
    pidaymes  <- sample (pidaymessages, 1)
    priority  <- 10
    hashtag   <- '#PiDay'
    expirDate <- as.POSIXct (Sys.Date (), format = '%Y-%m-%d')
    mtable    <- rbind (mtable, c (priority, FALSE, message, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: - find out how to render pi as the greek letter on twitter. 
  #            - add the diameter and circumference to the second message 

# Birthday 
#---------------------------------------------------------------------------------------#
checkBirthday <- function (mtable) { ## calculate stats for how much witnesstree has grown in a year
  if (substring (Sys.Date (), 6, 10) == substring (birthDay, 6, 10)) {
    len <- nchar (as.character (age))
    if (as.character (age) [len-1:len] == 1) {
      subst <- 'st'
    } else if (as.character (age) [len-1:len] == 2) {
      subst <- 'nd'
    } else {
      subst <- 'th'
    }
    message   <- sprintf ("Do you know what day it is??? Today is my %s%s birthday!", age, subst) # will remove date format from message
    priority  <- 10
    hashtag   <- "#birthday"
    expirDate <- as.POSIXct (Sys.Date (), format = "%Y-%m-%d")
    mtable    <- rbind (mtable, c (priority, FALSE, message, hashtag, expirDate))
  }
  return(mtable)
} # TTR To do: Set birthday (ask John O'Keefe, maybe?)


# Arbor Day Script (annual post falls on the last Friday in April)
#---------------------------------------------------------------------------------------#
checkArborday <- function (mtable) {
  if (as.numeric (substring (Sys.Date (), 1, 4))%%4 == 0) { # leap year
    doy <- 90 
  } else { # not a leap year
    doy <- 91
  }
  if ((weekdays (Sys.Date ()) == 'Friday') & 
      (months   (Sys.Date ()) == 'April' ) &
      (as.numeric (strftime (Sys.Date (), format = '%j')) > (doy - 6))) {
    message   <- "Happy Arbor day everyone! Did you plant a tree today? Share your comments below"
    priority  <- 10
    hashtag   <- "#ArborDay" # will remove date format from hashtag
    expirDate <- format (Sys.Date (), "%m %d %Y")
    mtable    <- rbind (mtable, c (priority, TRUE, message, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: Include some better message!

# Earth Day Script (annual post)
#---------------------------------------------------------------------------------------#
checkEarthday <- function (mtable) {
  if (substring (Sys.time (), 6, 10) == "04-22") {
    message   <- "Happy Earth Day! Let's all plant a Tree!"
    priority  <- 10
    hashtag   <- "#Earthday"
    expirDate <- format (Sys.Date (),"%m %d %Y")
    mtable <- rbind (mtable, c (priority,TRUE, message, hashtag, expirDate))
  } 
  return (mtable)
}


# Spring Equinox (annual post) 
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkSpringEquinox <- function (mtable) {
  timezone <- 'EST' # TTR Should be passed as parameter
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
  solarDates [['Vernal Equinox']] <- as.POSIXct (sprintf ('%s %s', 
                                                          solarDates [['Year']], 
                                                          solarDates [['Vernal Equinox']]), 
                                                 format = '%Y %m/%d %H:%M', 
                                                 tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  vernalDate <- solarDates [['Vernal Equinox']] [index] # Extract this years date 
  attributes (vernalDate)$tzone <- timezone             # Change timezone

  # Check whether it is that day
  if (       Sys.time ()  >= vernalDate         & 
      month (Sys.Date ()) == month (vernalDate) & 
      day   (Sys.Date ()) == day   (vernalDate)) { 
    message   <- sprintf ("The first day of spring just started at %s:%sh today! Get ready for bulking season!!", 
                          hour (vernalDate), minute (vernalDate))
    priority  <- 10
    hashtag   <- "#1stdayofspring"
    expirDate <- format (Sys.Date (),"%m %d %Y")
    mtable <- rbind (mtable, c (priority, TRUE, message, hashtag, expirDate))
  } 
  return (mtable)
} 


# Autumn Equinox (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkAutumnEquinox <- function (mtable) {
  timezone <- 'EST'  # TTR Should be passed as parameter
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
  solarDates [['Autumnal Equinox']] <- as.POSIXct (sprintf ('%s %s', 
                                                          solarDates [['Year']], 
                                                          solarDates [['Autumnal Equinox']]), 
                                                 format = '%Y %m/%d %H:%M', 
                                                 tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  autumnalDate <- solarDates [['Autumnal Equinox']] [index] # Extract this years date
  attributes (autumnalDate)$tzone <- timezone               # Change timezone
  if (       Sys.time ()  >=        autumnalDate  & 
      month (Sys.Date ()) == month (autumnalDate) & 
      day   (Sys.Date ()) == day   (autumnalDate)) { 
    message   <- sprintf ("Astronomically it is the first day of autmn since %s:%sh. I will keep the leaf peepers posted about colour changes to come.",
                          hour (autumnalDate), minute (autumnalDate))
    priority  <- 10
    hashtag   <- "#1stdayofautumn"
    expirDate <- format (Sys.Date (), "%m %d %Y")
    mtable <- rbind (mtable, c (priority, TRUE, message, hashtag, expirDate))
  } 
  return (mtable)
}

#Summer Solstice (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkSummerSolstice <- function (mtable) {
  timezone <- 'EST'  # TTR Should be passed as parameter
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
  solarDates [['Summer Solstice']] <- as.POSIXct (sprintf ('%s %s', 
                                                           solarDates [['Year']], 
                                                           solarDates [['Summer Solstice']]), 
                                                   format = '%Y %m/%d %H:%M', 
                                                   tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  solsticeDate <- solarDates [['Summer Solstice']] [index] # Extract this years date 
  attributes (solsticeDate)$tzone <- timezone              # Change timezone
  if (       Sys.time ()  >=        solsticeDate  & 
      month (Sys.Date ()) == month (solsticeDate) & 
      day   (Sys.Date ()) == day   (solsticeDate)) { 
    message   <- c ("Soak up that sun and photosynthesis! It's the longest day of the year!",
                    "It is mid-summer and this is the time when wood growth tends to be highest!")
    selectedMessage <- sample (message, 1)
    priority  <- 10
    hashtag   <- "#summersolstice #midsummer"
    expirDate <- format (Sys.Date (), "%m %d %Y")
    mtable    <- rbind (mtable, c (priority, TRUE, selectedMessage, hashtag, expirDate))
  } 
  return (mtable)
}


# Winter Solstices (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkWinterSolstice <- function (mtable) {
  timezone <- 'EST'  # TTR Should be passed as parameter
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
  solarDates [['Winter Solstice']] <- as.POSIXct (sprintf ('%s %s', 
                                                           solarDates [['Year']], 
                                                           solarDates [['Winter Solstice']]), 
                                                  format = '%Y %m/%d %H:%M', 
                                                  tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  solsticeDate <- solarDates [['Winter Solstice']] [index] # Extract this years date 
  attributes (solsticeDate)$tzone <- timezone              # Change timezone
  if (       Sys.time ()  >=        solsticeDate  & 
      month (Sys.Date ()) == month (solsticeDate) & 
      day   (Sys.Date ()) == day   (solsticeDate)) { 
    message   <- c ("Shortest day of the year! Not too much sunlight today so get as much as you can!",
                    "Happy midwinter! Finally, the days will start getting longer again.",
                    "Well it's the shortest day of the year here in Massachusetts, 
                    but for trees in the Southern hemisphere today's the longest day.")
    selectedMessage <- sample (message, 1)
    priority        <- 10
    hashtag         <- "#Wintersolstice #midwinter"
    expirDate       <- format (Sys.Date (), "%m %d %Y")
    mtable          <- rbind (mtable, c (priority, TRUE, selectedMessage, hashtag, expirDate))
  } 
  return (mtable)
}

# Halloween (annual post)
#---------------------------------------------------------------------------------------#
checkHalloween <- function (mtable) {
  if (substring (Sys.time (), 6, 10) == "10-31") {
    message   <- "Happy Halloween! What's your costume? Tweet below!"
    priority  <- 10
    hashtag   <- "#Halloween"
    expirDate <- format (Sys.Date (), "%m %d %Y")
    mtable    <- rbind (mtable, c (priority, TRUE, message, hashtag, expirDate))
  } 
  return (mtable)
}
