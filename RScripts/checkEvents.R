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
require ('readr')
require ('lubridate')

# New Years (annual post on 1st of January)
#---------------------------------------------------------------------------------------#
checkNewYears <- function (mtable) {
  if (substring (Sys.Date (), 6, 10) == '01-01') {
    message   <- 'Happy New Years!! I would join you in celebration but at the moment my roots are quite fixed. What are your plans for the day?'
    priority  <- 10
    hashtag   <- '#NewYears'
    expirDate <- as.POSIXct (Sys.Date (), format = '%Y-%m-%d')
    mtable    <- rbind (mtable, c (priority, F, message, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: - change to include growth in terms of CO2 sequestered of previous year 
  #              and a new year resolution!

# Pi Day (annual post on 14th of March)
#---------------------------------------------------------------------------------------#
checkPiDay <- function (mtable) {
  if (substring (Sys.Date (), 6, 10) == '03-14') {
    message   <- c (sprintf ('Happy Pi Day! How many digits of Pi do you know? Tweet below!'),
                             'Happy Pi Day! Thanks to Pi I can estimate my stem diameter from its girth, if we assume that my stem is a circle.')
    pidaymes  <- sample (pidaymessage, 1)
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
  if (substring (Sys.Date (), 6, 10) == "07-09") {
    message   <- sprintf ("Do you know what day it is??? Today is my Birthday! %s",substring(date(),1,10)) # will remove date format from message
    priority  <- 10
    hashtag   <- ("#It'sMyBirthday")
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
    message   <- ("Happy Arbor day everyone! Did you plant a tree today? Share your comments below")
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
  solarDates <- read_csv (file = './messages/solarDates.csv', 
                          skip = 3)
  solarDates [['Vernal Equinox']] <- as.POSIXct (paste (solarDates [['Year']], 
                                                        solarDates [['Vernal Equinox']]), 
                                                 format = '%Y %m/%d %H:%M', 
                                                 tz = 'GMT')
  index <- which (solarDates [['Year']] == year (Sys.time ()))
  vernalDate <- solarDates [['Vernal Equinox']] [index]
  if (Sys.time () >= vernalDate) { 
    message   <- "It's the first day of spring! Get ready for bulking season!!"
    priority  <- 10
    hashtag   <- "#1stdayofspring"
    expirDate <- format (Sys.Date (),"%m %d %Y")
    mtable <- rbind (mtable, c (priority, TRUE, message, hashtag, expirDate))
  } 
  return (mtable)
} # I could add something about the exact timing of the equinox. 


#Autumn Equinox (annual post)
#---------------------------------------------------------------------------------------#
checkAutumnEquinox <- function (mtable) {
  if (substring (Sys.time (), 6, 10) == "09-23") { # Date needs to be adjusted to account for annual shifts in equinox date
    autumnequinoxmessage<-("It's the last day of summer and the first of autumn! My trunk has grown ______ this season!!")
    #calulate priority
    priority<-1
    hashtag<-("#1stdayofautumn")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,springequinoxmessage,hashtag,expirDate))
  } 
  return(mtable)
}

#Summer Solstice (annual post)
#---------------------------------------------------------------------------------------#
checkSummersolstice<-function(mtable){
  if(substring(Sys.time(),6,10)=="06-21"){
    summersolsticemessage<-("Soak up that sun and photosynthesis! It's the longest day of the year!")
    #calulate priority
    priority<-1
    hashtag<-("#summersolstice%s!")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,summersolsticemessage,hashtag,expirDate))
  } 
  return(mtable)
}


#Winter Solstices (annual post)
#---------------------------------------------------------------------------------------#
checkWintersolstice<-function(mtable){
  if(substring(Sys.time(),6,10)=="12-21"){
    wintersolsticemessage<-c("Shortest day of the year! Not too much sunlight today so get as much as you can!", 
                             "Happy midwinter! Finally, the days will start getting longer again.", 
                             "Well it's the shortest day of the year here in Massachusetts, 
                             but for those trees in the Southern hemisphere where today's the longest day: 
                             Soak up that sun and photosynthesis!")
    selectedMessage<-sample(wintersolsticemessage,1)
    #calulate priority
    priority<-1
    hashtag<-("#Wintersolstice")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,selectedMessage,hashtag,expirDate))
  } 
  return(mtable)
}

#Halloween (annual post)
#---------------------------------------------------------------------------------------#
checkHalloween<-function(mtable){
  if(substring(Sys.time(),6,10)=="10-31"){
    halloweenmessage<-("Happy Halloween! What's your costume? Tweet below!")
    #selectedMessage<-sample(pidaymessage,1)
    #calulate priority
    #figure<- Halloween related image
    priority<-1
    hashtag<-("#Halloween%s!")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,halloweenmessage,hashtag,expirDate))
  } 
  return(mtable)
}