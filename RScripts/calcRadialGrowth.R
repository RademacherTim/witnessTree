#========================================================================================
# This script contains a function to growth increases based on DaBa dendrometers. The 
# script is dependent on calibration files in the data directory.
#----------------------------------------------------------------------------------------

calcRadGrowth <- function (pdm_calibration_path, PLOT = FALSE) {
  
  # Load dependencies
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('tibble')) library ('tidyverse')
  if (!existsFunction ('year'))   library ('lubridate')
  source (sprintf ('%s/RScripts/pdm1.R', path))
  
  # Get and collate raw datalogger output 
  #--------------------------------------------------------------------------------------
  temp1 <- read_csv (file = sprintf ('%s/witnessTreeDendrometerTable.dat', dataPath), 
                     col_types = cols (),
                     skip = 4,
                     col_names = c ('datetime','recordNumber','batteryVoltage',
                                    'panelTemperature','dbhQuercusRubra1','dbhQuercusRubra1p2',
                                    'dbhQuercusRubra2','dbhQuercusRubra3','dbhAcerRubrum1',
                                    'dbhAcerRubrum2','dbhAcerRubrum3','dbhPinusStrobus1',
                                    'dbhPinusStrobus2','dbhPinusStrobus3'))
  temp2 <-  read_csv (file = sprintf ('%s/witnessTree_DendrometerTable.dat', dataPath),
                      skip = 4,
                      col_types = cols (),
                      col_names = c ('datetime','recordNumber','batteryVoltage',
                                     'panelTemperature','dbhQuercusRubra1','dbhQuercusRubra1p2',
                                     'dbhQuercusRubra2','dbhQuercusRubra3','dbhAcerRubrum1',
                                     'dbhAcerRubrum2','dbhAcerRubrum3','dbhPinusStrobus1',
                                     'dbhPinusStrobus2','dbhPinusStrobus3'))
  data <- rbind (temp1, temp2); rm (temp1, temp2)
  
  
  # Get calibration data for the point dendrometer  
  #--------------------------------------------------------------------------------------
  cal_data <- get_calibration_data (pdm_calibration_path)
  
  # Define excitation voltage [V]
  #--------------------------------------------------------------------------------------
  v_excitation <- 2 # Need to check the excitation voltage
  data1 <- cbind (data [['datetime']],
                  voltage_to_position (cal_data$sensor_id [2], 
                                       data [['dbhQuercusRubra1']], 
                                       v_excitation, 
                                       cal_data))
  names (data1) [1] <-'datetime' 
  data2 <- cbind (data [['datetime']],
                  voltage_to_position (cal_data$sensor_id [1], 
                                       data [['dbhQuercusRubra1p2']], 
                                       v_excitation, 
                                       cal_data))
  names (data2) [1] <-'datetime' 
  
  # Plot growth
  #--------------------------------------------------------------------------------------
  if (PLOT) {
    par (mar = c (5, 5, 1, 1))
    plot (x = data [['datetime']],
          y = (data1$pos_lm - data1$pos_lm [1]),
          ylim = c (0.0, 0.6),
          xlab = 'date',
          ylab = 'growth (mm)',
          las = 1,
          typ = 'l',
          col = '#666666')
    lines (x = data [['datetime']],
           y = data2$pos_lm - data2$pos_lm [1],
           lty = 2,
           col = 'grey')
  }
  
  # Compile monthly growth for the last month and previous
  #--------------------------------------------------------------------------------------
  since         <- format (tail (data [['datetime']], n = 1) - 60*60*24*29, '%Y-%m-%d')   
  monthlyGrowth <- c (tail (data1 [['pos_lm']], n = 1) - data1 [['pos_lm']] [data1 [['datetime']] == since],
                      tail (data2 [['pos_lm']], n = 1) - data2 [['pos_lm']] [data2 [['datetime']] == since])
  to   <- ymd (since) - 1
  from <- ymd (since) - 30 
  if (from < as.POSIXct ('2019-05-24', format = '%Y-%m-%d')) from <- ymd ("2019-05-24") 
  previousMonthGrowth <- c (data1 [['pos_lm']] [data1 [["datetime"]] == to] - 
                            data1 [['pos_lm']] [data1 [['datetime']] == from],
                            data2 [['pos_lm']] [data2 [['datetime']] == to] - 
                            data2 [['pos_lm']] [data2 [['datetime']] == from])
  
  radGrowth <- tibble (monthlyGrowth, previousMonthGrowth)
  
  return (radGrowth)
}
#========================================================================================