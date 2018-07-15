'''
EVENTS: 
Birthday
Arbor Day
Pi Day
Earth Day
Spring Eqinox
Summer Solstice
Autumn Equinox(needs work)
Halloween
Winter Solstice
New Years
'''

#Birthday Script (annual post) (change BDay date)
check_birthday<-function(mtable){ ## calculate stats for how much witnesstree has grown in a year
  if (substring(Sys.time(),6,10)=="07-09"){
    birthdaymessage<-sprintf("Do you know what day it is??? Today is my Birthday! %s",substring(date(),1,10)) # will remove date format from message
    #calulate priority
    priority<-1
    hashtag<-("#It'sMyBirthday")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,birthdaymessage,hashtag, expirDate))
  } #else{}
  return(mtable)
}
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_birthday(messages)
messages



#Arbor Day Script (annual post falls on the last Friday in April)
check_arborday<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_arborday(messages)
messages


#Earth Day Script (annual post)
check_earthday<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_earthday(messages)
messages


#Spring Equinox (annual post) 
check_springequinox<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_springequinox(messages)
messages


#Autumn Equinox (annual post)
check_autumnequinox<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_springequinox(messages)
messages

#Summer Solstice (annual post)
check_summersolstice<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_summersolstice(messages)
messages


#Winter Solstices (annual post)
check_wintersolstice<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_wintersolstice(messages)
messages


#Pi Day (annual post)
check_piday<-function(mtable){
  if(substring(Sys.time(),6,10)=="03-14"){
    pidaymessage<-("Happy Pi Day! How many digits of Pi do you know? Tweet below!")
    #selectedMessage<-sample(pidaymessage,1)
    #calulate priority
    priority<-1
    hashtag<-("#PiDay")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,pidaymessage,hashtag,expirDate))
  } 
  return(mtable)
}
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_piday(messages)
messages


#Halloween (annual post)
check_halloween<-function(mtable){
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
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_piday(messages)
messages

#New Years (annual post)
check_newyears<-function(mtable){
  if(substring(Sys.time(),6,10)=="01-01"){
    newyearsmessage<-("Happy New Years!! I'd join you in celebration but at the moment my roots are quite fixed. What are your plans for the day?")
    #selectedMessage<-sample(pidaymessage,1)
    #calulate priority
    #figure<- Halloween related image
    priority<-1
    hashtag<-("#NewYears")
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,TRUE,newyearsmessage,hashtag,expirDate))
  } 
  return(mtable)
}
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<- check_newyears(messages)
messages
