setwd("~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data")

data1 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs.csv",
                       header=F,
                       skip=4)
header_data1 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs.csv",
                         nrows = 1,skip=1,header = FALSE,stringsAsFactors = FALSE)
names(data1)<-header_data1

data2 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_07.csv",
                 header=F,
                 skip=4)
header_data2 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_07.csv",
                         nrows = 1,skip=1,header = FALSE,stringsAsFactors = FALSE)
names(data2)<-header_data2

data3 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_08.csv",
                  header=F,
                  skip=4)
header_data3 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_08.csv",
                         nrows = 1,skip=1,header = FALSE,stringsAsFactors = FALSE)
names(data3)<-header_data3

data4 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_11.csv",
                  header=F,
                  skip=4)
header_data4 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_11.csv",
                         nrows = 1,skip=1,header = FALSE,stringsAsFactors = FALSE)
names(data4)<-header_data4

data5 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_21.csv",
                  header=F,
                  skip=4)
header_data5 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_21.csv",
                         nrows = 1,skip=1,header = FALSE,stringsAsFactors = FALSE)
names(data5)<-header_data5

data6 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_25.csv",
                  header=F,
                  skip=4)
header_data6 <- read.csv(file="~/Desktop/Harvard/Summer Project/witnesstree/data/Datalogger data/Cold Trees_Outputs6_25.csv",
                  nrows = 1,skip=1,header = FALSE,stringsAsFactors = FALSE)
names(data6)<-header_data6



data_header<-unique(c(names(data5),names(data6)))

setdiff(data_header,names(data6))

data<-as.data.frame(matrix(NA,ncol=length(data_header)))
names(data)<-data_header

data[1:nrow(data1),names(data1)]<-data1
data[1:nrow(data1),"TIMESTAMP"]<-data1$TIMESTAMP
rows<-nrow(data)
data[(rows+1):(rows+nrow(data2)),names(data2)]<-data2
rows<-nrow(data)
data[(rows+1):(rows+nrow(data3)),names(data3)]<-data3
rows<-nrow(data)
data[(rows+1):(rows+nrow(data4)),names(data4)]<-data4
rows<-nrow(data)
data[(rows+1):(rows+nrow(data5)),names(data5)]<-data5
rows<-nrow(data)
data[(rows+1):(rows+nrow(data6)),names(data6)]<-data6
rows<-nrow(data)
data1
str(data1)    





