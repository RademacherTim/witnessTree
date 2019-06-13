#=======================================================================================#
# Read climate data from the Harvard Forest weather station.
#---------------------------------------------------------------------------------------#

# Function to read in climate data
#---------------------------------------------------------------------------------------#
readClimate <- function (TEST = F) {
  
  # Load dependencies
  #---------------------------------------------------------------------------------------#
  if (!existsFunction ('read_csv'))   library ('tidyverse')
  if (!existsFunction ('floor_date')) library ('lubridate')

  # Read climate data from the appropriate weather station, default is the Fisher 
  # meterorological station at Harvard Forest 
  #-----------------------------------------------------------------------------------------#
  suppressWarnings (met_HF_shaler  <- read_csv (file = url ('http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv'),
                                                  col_types = cols()))
  met_HF_shaler$TIMESTAMP <- as.POSIXct (met_HF_shaler$date, 
                                         format = '%Y-%m-%d',
                                         tz = 'EST') 
  suppressWarnings (met_HF_old <- read_csv (file = url ('http://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv'),
                                            col_types = cols()))
  met_HF_old$TIMESTAMP <- as.POSIXct (met_HF_old$datetime, 
                                      format = '%Y-%m-%d %H:%M:%S',
                                      tz = 'EST') 
  suppressWarnings (met_HF_current <- read_csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/qfm.csv'),
                                                col_types = cols()))
  met_HF_current$TIMESTAMP <- as.POSIXct (met_HF_current$datetime, 
                                          format = '%Y-%m-%d %H:%M:%S',
                                          tz = 'EST') 

  dates <- c (met_HF_shaler$TIMESTAMP, met_HF_old$TIMESTAMP, met_HF_current$TIMESTAMP) 
  airt <<- tibble (TIMESTAMP = dates, airt = c (as.numeric (met_HF_shaler$airt), met_HF_old$airt, met_HF_current$airt))
  prec <<- tibble (TIMESTAMP = dates, prec = c (as.numeric (met_HF_shaler$prec), met_HF_old$prec, met_HF_current$prec))
  
  # Add variable for different period to airt (i.e. day, week, month, year)
  airt <<- add_column(airt, daily = format (airt [['TIMESTAMP']], '%Y-%m-%d'))
  airt [['week']]  <<- floor ((airt [['TIMESTAMP']] - min (airt [['TIMESTAMP']], na.rm = T)) / dweeks (1))
  airt [['month']] <<- floor_date (airt [['TIMESTAMP']], 'month')
  airt [['year']]  <<- floor_date (airt [['TIMESTAMP']], 'year')
  
  # Create mean airt over varying periods (i.e. day, week, month, year)
  dailyAirt   <- airt %>% group_by (daily) %>% summarise (airt = mean (airt, na.rm = T))
  dailyAirt   <<- dailyAirt [!is.na (dailyAirt [['daily']]),]
  weeklyAirt  <<- airt %>% group_by (week) %>% summarise (airt = mean (airt, na.rm = T))
  weeklyAirt  <<- weeklyAirt [!is.na (weeklyAirt [['week']]), ]
  monthlyAirt <<- airt %>% group_by (month) %>% summarise (airt = mean (airt, na.rm = T))
  monthlyAirt <<- monthlyAirt [!is.na (monthlyAirt [['month']]), ]
  yearlyAirt  <<- airt %>% group_by (year) %>% summarise (airt = mean (airt, na.rm = T))
  yearlyAirt  <<- yearlyAirt [!is.na (yearlyAirt [['year']]), ]
  
  
  # Add variable for different period to prec (i.e. day, week, month, year)
  prec <<- add_column (prec, daily = format (prec [['TIMESTAMP']], '%Y-%m-%d'))
  prec [['week']]  <<- floor ((prec [['TIMESTAMP']] - min (prec [['TIMESTAMP']], na.rm = T)) / dweeks (1))
  prec [['month']] <<- floor_date (prec [['TIMESTAMP']], 'month')
  prec [['year']]  <<- floor_date (prec [['TIMESTAMP']], 'year')
  
  # Create mean prec over varying periods (i.e. day, week, month, year)
  dailyPrec   <<- prec %>% group_by (daily) %>% summarise (prec = sum (prec, na.rm = T))
  dailyPrec   <<- dailyPrec [!is.na (dailyPrec [['daily']]),]
  weeklyPrec  <<- prec %>% group_by (week) %>% summarise (prec = sum (prec, na.rm = T))
  weeklyPrec  <<- weeklyPrec [!is.na (weeklyPrec [['week']]), ]
  monthlyPrec <<- prec %>% group_by (month) %>% summarise (prec = sum (prec, na.rm = T))
  monthlyPrec <<- monthlyPrec [!is.na (monthlyPrec [['month']]), ]
  yearlyPrec  <<- prec %>% group_by (year) %>% summarise (prec = sum (prec, na.rm = T))
  yearlyPrec  <<- yearlyPrec [!is.na (yearlyPrec [['year']]), ]
  
} 
#=======================================================================================#