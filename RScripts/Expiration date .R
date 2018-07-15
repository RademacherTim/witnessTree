#expiration date
check_expiration<-function(mtable){
  #Sys.Date<-format(Sys.Date(), "%m %d %Y")
  for (i in 1: dim(mtable)[1]){
    #if(i==1)temptable <-matrix(ncol = 5)
     if(as.POSIXct(mtable[i,5],format="%m %d %Y")>=as.POSIXct(Sys.Date(), format="%Y-%m-%d")){
       if (exists("temptable")){
         temptable<- rbind(temptable,mtable[i,])
       } else {
         temptable<-matrix(ncol=5,data=mtable[i,])
       }
    }
  }
  return (temptable)
}
messages <-matrix(ncol = 5, c(1,FALSE,"Message","Hashtag", "07 08 1996"))  # emtpy table
messages<-check_expiration(messages)
messages

