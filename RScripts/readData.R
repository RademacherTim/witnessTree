# install and load dependencies, unless they are already installed
if (exists ('rbind.fill', mode = 'function')) {
  install.packages ('plyr')
}
library (plyr)

# read big chill datalogger data
data1 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_07.dat',
                   skip = 4, header = F)
dataNames1 <- names (read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_07.dat',
                               skip = 1, header = T))
names (data1) <-  dataNames1

data2 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_08.dat',
                   skip = 4, header = F)
dataNames2 <- names (read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_08.dat',
                               skip = 1, header = T))
names (data2) <-  dataNames2

data3 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_11.dat',
                   skip = 4, header = F)
dataNames3 <- names (read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_11.dat',
                               skip = 1, header = T))
names (data3) <-  dataNames3

data4 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_21.dat',
                   skip = 4, header = F)
dataNames4 <- names (read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_21.dat',
                               skip = 1, header = T))
names (data4) <-  dataNames4

data5 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_25.dat',
                   skip = 4, header = F)
dataNames5 <- names (read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs6_25.dat',
                               skip = 1, header = T))
names (data5) <-  dataNames5

data6 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs.dat',
                  skip = 4, header = F)
data7 <- read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs_1a.dat',
                   skip = 4, header = F)
dataNames6 <- names (read.csv ('/home/rademachert/cold trees/Cold Trees_Outputs.dat',
                               skip = 1, header = T))
names (data6) <-  dataNames6
names (data7) <-  dataNames6

# combine the various data sets 
data <- rbind.fill (data1, data2, data3, data4, data5, data6, data7)
data$TIMESTAMP <- as.POSIXct (data$TIMESTAMP, format = '%Y-%m-%d %H:%M:%S')

# read harvard forest climate data from fisher meterological station 
metShaler.past    <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv'))
metFisher.current <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/dfm.csv'))
metFisher.past    <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-06-daily-m.csv'))

# combine the different meterological data sets
metDataHF <- rbind.fill (metShaler.past, metFisher.past, metFisher.current) 
metDataHF$date <- as.POSIXct (metDataHF$date, format = '%Y-%m-%d')

# delete unnecessary files
rm (metFisher.current, metFisher.past, metShaler.past,
    data1, data2, data3, data4, data5, data6, data7, 
    dataNames1, dataNames2, dataNames3, dataNames4, dataNames5, dataNames6)
