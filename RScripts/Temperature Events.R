#Heat Wave (occasional post)
check_heatwave<-function(temp,mtable){
  ## stats
  rt<-rev(temp>30)
  i<-0
  while(rt[i+1]==TRUE) {i=i+1}
  i  # the last i days were larger than 30
  if (i>5){
    # Message variation matrix
    heatwavemessages<-c("It's a heatwave",sprintf("it's hot since %d days",i),"Heatwave!!!!")
    selectedMessage<-sample(heatwavemessages,1)
    #calulate priority
    priority<-10
    hashtag<-sprintf("#Heatwave%s",substring(Sys.time(),1,4))
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,FALSE,selectedMessage,hashtag,expirDate))
  }
  return(mtable)
}
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
airtemperature<-data$AirTemperature_2p0[3339:length(data$AirTemperature_2p0)] #3339 is where the NA stops and numeric numbers start
messages<- check_heatwave(airtemperature,messages)
messages

#Frost Event (occasional post)
check_frost<-function(temp,mtable){
  ## stats
  rt<-rev(temp<0)
  i<-0
  while(rt[i+1]==TRUE) {i=i+1}
  i  # the last i days were larger than 30
  if (i>5){
    # Message variation matrix
    frostmessages<-c("We've got a frost event!","Keep an eye for Frosty the Snowman!",sprintf("There's been a frost for %d days now",i))
    selectedMessage<-sample(frostmessages,1)
    #calulate priority
    priority<-10
    hashtag<-sprintf("#Frost%s",substring(Sys.time(),1,4))
    expirDate<-format(Sys.Date(),"%m %d %Y")
    mtable<-rbind(mtable,c(priority,FALSE,selectedMessage,hashtag,expirDate))
  }
  return(mtable)
}
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
airtemperature<-data$AirTemperature_2p0[3339:length(data$AirTemperature_2p0)] #3339 is where the NA stops and numeric numbers start
messages<- check_frost(airtemperature,messages)
messages

#Drought Event

#Heavy Rains


