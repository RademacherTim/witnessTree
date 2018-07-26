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

# New Years (annual post on 1st of January)
#---------------------------------------------------------------------------------------#
checkNewYears <- function (mtable) {
  if (substring (Sys.time (), 6, 10) == '01-01') {
    newyearsmes <- 'Happy New Years!! I'd join you in celebration but at the moment my roots are quite fixed. What are your plans for the day?''
    priority    <- 10
    hashtag     <- '#NewYears'
    expirDate   <- as.POSIXct (Sys.Date (), format = '%Y-%m-%d')
    mtable      <- rbind (mtable, c (priority, F, newyearsmes, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: - change to include growth in terms of CO2 sequestered of previous year 
  #              and a new year resolution!

# Pi Day (annual post on 14th of March)
#---------------------------------------------------------------------------------------#
checkPiDay <- function (mtable) {
  if (substring (Sys.time (), 6, 10) == '03-14') {
    pidaymes  <- c (sprintf ('Happy Pi Day! How many digits of Pi do you know? Tweet below!'),
                             'Happy Pi Day! Thanks to Pi I can estimate my stem diameter from its girth, if we assume that my stem is a circle.')
    pidaymes  <- sample (pidaymessage, 1)
    priority  <- 10
    hashtag   <- '#PiDay'
    expirDate <- as.POSIXct (Sys.Date (), format = '%Y-%m-%d')
    mtable    <- rbind (mtable, c (priority, FALSE, pidaymes, hashtag, expirDate))
  } 
  return (mtable)
} # TTR To do: - find out how to render pi as the greek letter on twitter. 
  #            - add the diameter and circumference to the second message 

# Birthday 
#---------------------------------------------------------------------------------------#
checkBirthday <- function (mtable) { ## calculate stats for how much witnesstree has grown in a year
  if (substring (Sys.time (), 6, 10) == "07-09") {
    birthdaymessage <- sprintf ("Do you know what day it is??? Today is my Birthday! %s",substring(date(),1,10)) # will remove date format from message
    priority  <- 10
    hashtag   <- ("#It'sMyBirthday")
    expirDate <- as.POSIXct (Sys.Date (), format = "%Y-%m-%d")
    mtable    <- rbind (mtable, c (priority, FALSE, birthdaymessage, hashtag, expirDate))
  }
  return(mtable)
} # TTR To do: Set birthday (ask John O'Keefe, maybe?)


# Arbor Day Script (annual post falls on the last Friday in April)
#---------------------------------------------------------------------------------------#
checkArborday<-function(mtable){
  if(substring(Sys.time(),6,10)=="04-27"){
  arbordaymessage<-("Happy Arbor day everyone! Did you plant a tree today? Share your comments below")
  #calulate priority
  priority<-1
  hashtag<-("#ArborDay") # will remove date format from hashtag
  expirDate<-format(Sys.Date(),"%m %d %Y")
  mtable<-rbind(mtable,c(priority,TRUE,arbordaymessage,hashtag,expirDate))
  } 
  return(mtable)
}

#Earth Day Script (annual post)
#---------------------------------------------------------------------------------------#
checkEarthday<-function(mtable){
  if(substring(Sys.time(),6,10)=="04-22"){
    earthdaymessage<-("Happy Earth Day! Let's all plant a Tree")
    #calulate priority
    priority<-1
    hashtag<-("#Earthday")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,earthdaymessage,hashtag,expirDate))
  } 
  return(mtable)
}


#Spring Equinox (annual post) 
#---------------------------------------------------------------------------------------#
checkSpringequinox<-function(mtable){
  if(substring(Sys.time(),6,10)=="03-20"){ # Time needs to be adjusted to account for annual shifts in equinox date
    springequinoxmessage<-("It's the first day of spring! Get ready for bulking season!!")
    #calulate priority
    priority<-1
    hashtag<-("#1stdayofspring")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,springequinoxmessage,hashtag,expirDate))
  } 
  return(mtable)
}


#Autumn Equinox (annual post)
#---------------------------------------------------------------------------------------#
checkAutumnequinox<-function(mtable){
  if(substring(Sys.time(),6,10)=="09-23"){ # Date needs to be adjusted to account for annual shifts in equinox date
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
