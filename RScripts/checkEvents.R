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

# Hello world! message (post once on 15th of April)
#---------------------------------------------------------------------------------------#
helloWorld <- function (mtable, TEST = F) {
  if (substring (Sys.Date (), 6, 10) == '04-15' | TEST) {
    message   <- sprintf ("Hello World! I am a #witnessTree at %s. Follow me to learn more about life as a tree and the environment.", treeLocationName)
    priority  <- 10
    hashtags  <- sprintf ("#IAmAlive #HarvardForest")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable    <- rbind (mtable, c (priority, F, message, hashtags, expirDate))
  } 
  return (mtable)
} 

# New Years (annual post on 1st of January)
#---------------------------------------------------------------------------------------#
checkNewYears <- function (mtable, TEST = F) {
  if (substring (Sys.Date (), 6, 10) == '01-01' | TEST) {
    message   <- sprintf ("Happy #NewYear!! During my life I fixed roughly %s kg of carbon per year. My #resolution for %s is to beat that. What is your resolution?", round (meanAnnualCarbonSequestration, 0), year (Sys.time ()))
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
  } 
  return (mtable)
} # TTR To do: - maybe change to comparison to CO2 sequester in last year?

# National Wildlife Day (annual post on 4th of March)
#---------------------------------------------------------------------------------------#
checkNationalWildLifeDay <- function (mtable, TEST = F) {
  if (substring (Sys.Date (), 6, 10) == '03-04' | TEST) {
    message   <- sprintf ("Look who's visited me earlier this year! Happy #NationalWildlifeDay!")
    priority  <- 10
    hashtags  <- sprintf ("#wildlife #witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
  } 
  return (mtable)
} # TTR To do: - Ought to have an image or even several images from the wild life camera at the tree!

# Pi Day (annual post on 14th of March)
#---------------------------------------------------------------------------------------#
checkPiDay <- function (mtable, TEST = F) {
  if (substring (Sys.Date (), 6, 10) == '03-14' | TEST) {
    piDayMessages <- c ('Happy P#iDay! Pi is extra important to those of us shaped like a cylinder. How many digits of Pi can you recite from memory? Tweet below!',
                        sprintf ('Happy #PiDay! Thanks to Pi I can estimate my trunk diameter (%s cm) or even the area through which sap flows (%s m2).', dbh_cyl, sapFlowArea))
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      piDayMes  <- sample (piDayMessages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = piDayMes, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = piDayMessages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = piDayMessages [2], hashtags = hashtags, expires = expirDate))
    }  
  } 
  return (mtable)
} # TTR To do: - find out how to render pi as the greek letter on twitter. 
  #            - add the diameter and circumference to the second message 

# International Day of Forests Script (annual post falls on the 21st of March)
#---------------------------------------------------------------------------------------#
checkInternationalDayOfForests <- function (mtable, TEST = F) {
  if (substring (Sys.Date (), 6, 10) == "03-21" | TEST) {
    messages   <- c (sprintf ("It's the international day of forests! How many types of forests do you know?"),
                    sprintf ("Some call us the 'lungs of the Earth' but we are much more than that. Happy international day of forests!"))
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      message  <- sample (messages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = messages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = messages [2], hashtags = hashtags, expires = expirDate))
    }  } 
  return (mtable)
}

# World Water Day Script (annual post falls on the 22nd of March)
#---------------------------------------------------------------------------------------#
checkWorldWaterDay <- function (mtable, TEST = F) {
  if (substring (Sys.Date (), 6, 10) == "03-22" | TEST) {
    messages   <- c (sprintf ("Did you know that roughly %s%% of me is water? Every day lots of water flows through my trunk to my leaves.", percentWaterContent),
                     sprintf ("Drink up! At night the pores in my leaves close and my trunk swell with water taken-up by my roots. During the day the pore are open and I transpire."))
    priority  <- 10
    hashtags  <- sprintf ("#WorldWaterDay #witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      message  <- sample (messages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = messages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = messages [2], hashtags = hashtags, expires = expirDate))
    }  } 
  return (mtable)
}

# Birthday 
#---------------------------------------------------------------------------------------#
checkBirthday <- function (mtable, TEST = F) { ## calculate stats for how much witnesstree has grown in a year
  if (substring (Sys.Date (), 6, 10) == substring (birthDay, 6, 10) | TEST) {
    len <- nchar (as.character (age))
    if (as.character (age) [len-1:len] == 1) {
      subst <- 'st'
    } else if (as.character (age) [len-1:len] == 2) {
      subst <- 'nd'
    } else {
      subst <- 'th'
    }
    message   <- sprintf ("Do you know what day it is??? Today is my %s%s #birthday!", age, subst)
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
  }
  return(mtable)
} # TTR To do: Set birthday (ask John O'Keefe, maybe?)


# Arbor Day Script (annual post falls on the last Friday in April)
#---------------------------------------------------------------------------------------#
checkArborDay <- function (mtable, TEST = F) {
  if (as.numeric (substring (Sys.Date (), 1, 4))%%4 == 0) { # leap year
    doy <- 90 
  } else { # not a leap year
    doy <- 91
  }
  if ((weekdays (Sys.Date ()) == 'Friday') & 
      (months   (Sys.Date ()) == 'April' ) &
      (as.numeric (strftime (Sys.Date (), format = '%j')) > (doy - 6)) | TEST) {
    message   <- "Happy #ArborDay everyone! Did you plant a tree today? Share your comments below."
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
  } 
  return (mtable)
} # TTR To do: Include some better message!

# Earth Day Script (annual post)
#---------------------------------------------------------------------------------------#
checkEarthDay <- function (mtable, TEST = F) {
  if (substring (Sys.time (), 6, 10) == "04-22" | TEST) {
    earthDayMessages  <- c ("Join us as we celebrate #EarthDay! Trees are really important to balancing the amount of carbon dioxide in the air and creating livable conditions for humans.",
                            "You don't know how to celebrate #EarthDay? You could plant a tree, or get outside and figure out what species of trees are near your home.")
    
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      earthDayMes  <- sample (earthDayMessages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = earthDayMessage, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = earthDayMessages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = earthDayMessages [2], hashtags = hashtags, expires = expirDate))
    }
  } 
  return (mtable)
}


# Spring Equinox (annual post) 
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkSpringEquinox <- function (mtable, TEST = F) {
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
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
      day   (Sys.Date ()) == day   (vernalDate) | TEST) { 
    sprEquMessages <- c (sprintf ("The first day of spring just started at %s:%sh today! Get ready for bulking season!!", 
                                  hour (vernalDate), minute (vernalDate)),
                         sprintf ("First day of #spring and it's %s degree C here in the forest.", temperature, expression (degree)),
                         sprintf ("Do you already see any oak seedlings, where you live, on this first day of astronomical spring?"))
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree #1stDayOfSpring")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      sprEquMes  <- sample (sprEquMessages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sprEquMessage, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sprEquMessages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sprEquMessages [2], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sprEquMessages [3], hashtags = hashtags, expires = expirDate))
    }
  } 
  return (mtable)
} 


# Autumn Equinox (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkAutumnEquinox <- function (mtable, TEST = F) {
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
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
      day   (Sys.Date ()) == day   (autumnalDate) | TEST) { 
    autEquMessages   <- c (sprintf ("Astronomically it is the first day of autmn since %s:%sh. I will keep the leaf peepers posted about colour changes at #HarvardForest.",
                                    hour (autumnalDate), minute (autumnalDate)),
                           sprintf ("Leaf colours will be changing soon, as astronomically it is the first day of #autmn today."))
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree #1stDayOfAutumn #winterIsComing")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      autEquMes  <- sample (autEquMessages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = autEquMessage, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = autEquMessages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = autEquMessages [2], hashtags = hashtags, expires = expirDate))
    }
  } 
  return (mtable)
}

#Summer Solstice (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkSummerSolstice <- function (mtable, TEST = F) {
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
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
      day   (Sys.Date ()) == day   (solsticeDate) | TEST) { 
    sumSolMessages   <- c (sprintf ("Soak up that sun and photosynthesis! It's the longest day of the year!"),
                           sprintf ("It is mid-summer and this is the time when wood growth tends to be highest!"))
    priority  <- 10
    hashtags  <- sprintf ("#midsummer #solstice #witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      sumSolMes  <- sample (sumSolMessages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sumSolMessage, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sumSolMessages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = sumSolMessages [2], hashtags = hashtags, expires = expirDate))
    }
  } 
  return (mtable)
}


# Winter Solstices (annual post)
#---------------------------------------------------------------------------------------#
# The dates are taken from a file in the messages folder (solarDates.tsv), which contains
# dates calculated by NASA (https://data.giss.nasa.gov/ar5/srvernal.html) from 2018 to
# 2068. The original file is not comma-separated.
#---------------------------------------------------------------------------------------#
checkWinterSolstice <- function (mtable, TEST = F) {
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
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
      day   (Sys.Date ()) == day   (solsticeDate) | TEST) { 
    winSolMessages   <- c (sprintf ("Shortest day of the year! Not too much sunlight today so get as much as you can!"),
                           sprintf ("Happy midwinter! Finally, the days will start getting longer again."),
                           sprintf ("Well it's the shortest day of the year here in Massachusetts, but for trees in the Southern hemisphere today's the longest day."))
    priority        <- 10
    hashtags        <- sprintf ("#midwinter #solstice #witnessTree")
    expirDate       <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    if (!TEST) {
      winSolMes  <- sample (winSolMessages, 1)
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = winSolMessage, hashtags = hashtags, expires = expirDate))
    } else {
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = winSolMessages [1], hashtags = hashtags, expires = expirDate))
      mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = winSolMessages [2], hashtags = hashtags, expires = expirDate))
    }
  } 
  return (mtable)
}

# Halloween (annual post)
#---------------------------------------------------------------------------------------#
checkHalloween <- function (mtable, TEST = F) {
  if (substring (Sys.time (), 6, 10) == "10-31" | TEST) {
    message   <- "Happy #Halloween! What's your #costume? Are you going as a tree? Tweet below!"
    priority  <- 10
    hashtags  <- sprintf ("#witnessTree")
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), format = '%Y-%m-%d'), treeTimeZone)
    mtable <- rbind (mtable, c (priority = priority, fFigure = F, message = message, hashtags = hashtags, expires = expirDate))
  } 
  return (mtable)
}
