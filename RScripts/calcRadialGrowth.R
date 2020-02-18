#========================================================================================
# This script contains a function to growth increases based on DaBa dendrometers. The 
# script is dependent on calibration files in the data directory.
#----------------------------------------------------------------------------------------

calcRadialGrowth <- function (pdm_calibration_path, temporalRes = 'monthly', PLOT = FALSE) {
  
  # Load dependencies
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('tibble')) library ('tidyverse')
  if (!existsFunction ('year'))   library ('lubridate')
  source (sprintf ('%s/RScripts/pdm1.R', path))
  
  # Get and collate raw datalogger output 
  #--------------------------------------------------------------------------------------
  temp1 <- read_csv (file = sprintf ('%sdata/witnessTreeDendroTable2019.dat', path), 
                     col_types = cols (),
                     skip = 4,
                     col_names = c ('datetime','recordNumber','batteryVoltage',
                                    'panelTemperature','dbhQuercusRubra1','dbhQuercusRubra1p2',
                                    'dbhQuercusRubra2','dbhQuercusRubra3','dbhAcerRubrum1',
                                    'dbhAcerRubrum2','dbhAcerRubrum3','dbhPinusStrobus1',
                                    'dbhPinusStrobus2','dbhPinusStrobus3'))
  temp2 <-  read_csv (file = sprintf ('%sdata/witnessTreeDendroTable2020.dat', path),
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
  v_excitation <- 2.5
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
  
  # If monthly resolution is required
  #--------------------------------------------------------------------------------------
  if (temporalRes == 'daily') {
    
    # Compile daily growth
    #------------------------------------------------------------------------------------
    since       <- format (tail (data [['datetime']], n = 1) - 60*60*24, '%Y-%m-%d %H:%M')   
    dailyGrowth <- c (tail (data1 [['pos_lm']], n = 1) - 
                      data1 [['pos_lm']] [data1 [['datetime']] == since],
                      tail (data2 [['pos_lm']], n = 1) - 
                      data2 [['pos_lm']] [data2 [['datetime']] == since])
    radGrowth <- tibble (dailyGrowth)
    
    # Make plot of growth over the last two weeks
    #------------------------------------------------------------------------------------
    if (PLOT) {
      
      # Read precipitation data, if it has not been read in yet
      #------------------------------------------------------------------------------------
      if (!exists ('dailyPrec')) readClimate ()
      
      from <- as.POSIXct (since) - 60*60*24*14
      png (sprintf ('./tmp/dailyGrowth_%s.png',Sys.Date ()),
           width = 955,
           height = 500)
      par (mar = c (5, 5, 1, 5))
      condition <- dailyPrec [['day']] >= from & 
                   dailyPrec [['day']] <= tail (data2 [['datetime']], n = 1)
      barplot (dailyPrec [['prec']] [condition], col = '#91bfdb66', yaxt = 'n',
               ylim = c (0, 1.1 * max (dailyPrec [['prec']] [condition], na.rm = T)), 
               border = FALSE)
      axis (4, las = 1)
      par (new = TRUE)
      plot (x = data2 [['datetime']] [data2 [['datetime']] >= from & 
                                      data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)],
            y = data2$pos_lm [data2 [['datetime']] >= from & 
                                data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] - 
              data2$pos_lm [data2 [['datetime']] == from],
            lwd = 4,
            col = '#ff9999',
            xlab = 'date',
            ylab = 'growth (mm) over the last two weeks',
            las = 1,
            typ = 'l',
            ylim = c (min (c (data2$pos_lm [data2 [['datetime']] >= from & 
                                            data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] - 
                              data2$pos_lm [data2 [['datetime']] == from], 
                              data1$pos_lm [data1 [['datetime']] >= from & 
                                            data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] - 
                              data1$pos_lm [data1 [['datetime']] == from])), 
                      max (c (data2$pos_lm [data2 [['datetime']] >= from & 
                                            data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] - 
                              data2$pos_lm [data2 [['datetime']] == from], 
                              data1$pos_lm [data1 [['datetime']] >= from & 
                                            data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] - 
                              data1$pos_lm [data1 [['datetime']] == from]))))
      lines (x = data1 [['datetime']] [data1 [['datetime']] >= from & 
                                       data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] ,
             y = data1$pos_lm [data1 [['datetime']] >= from & 
                               data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] - 
               data1$pos_lm [data1 [['datetime']] == from],
             lwd = 4,
             col = '#6a3d9a')
      abline (v = as.POSIXct (since), lwd = 2, col = '#999999', lty = 2)
      legend (x = data1 [['datetime']] [data1 [['datetime']] == from],
              y = tail (data2$pos_lm, n = 1) - data2$pos_lm [data2 [['datetime']] == from],
              legend = c ('branch', 'trunk'),
              lwd = 3,
              col = c ('#ff9999','#6a3d9a'),
              box.lty = 0,
              bg = 'transparent')
      dev.off ()
    }
  } else if (temporalRes == 'monthly') {
    
    # Compile monthly growth for the last month and previous
    #------------------------------------------------------------------------------------
    since         <- format (tail (data [['datetime']], n = 1) - 60*60*24*29, '%Y-%m-%d')   
    monthlyGrowth <- c (tail (data1 [['pos_lm']], n = 1) - 
                        data1 [['pos_lm']] [data1 [['datetime']] == since],
                        tail (data2 [['pos_lm']], n = 1) - 
                        data2 [['pos_lm']] [data2 [['datetime']] == since])
    to   <- ymd (since) - 1
    from <- ymd (since) - 30 
    if (from < as.POSIXct ('2019-05-24', format = '%Y-%m-%d')) from <- ymd ("2019-05-24") 
    previousMonthGrowth <- c (data1 [['pos_lm']] [data1 [["datetime"]] == to] - 
                              data1 [['pos_lm']] [data1 [['datetime']] == from],
                              data2 [['pos_lm']] [data2 [['datetime']] == to] - 
                              data2 [['pos_lm']] [data2 [['datetime']] == from])
  
    radGrowth <- tibble (monthlyGrowth, previousMonthGrowth)

    # Read precipitation data, if it has not been read in yet
    #------------------------------------------------------------------------------------
    if (!exists ('dailyPrec')) source (sprintf ('%sRScripts/readClimate.R', path))
    
    # Plot growth of the two previous months
    #------------------------------------------------------------------------------------
    if (PLOT) {
      png (sprintf ('./tmp/monthlyGrowth_%s.png',Sys.Date ()),
           width = 955,
           height = 500)
      par (mar = c (5, 5, 1, 5))
      condition <- dailyPrec [['day']] >= from & 
                   dailyPrec [['day']] <= tail (data2 [['datetime']], n = 1)
      barplot (dailyPrec [['prec']] [condition], col = '#91bfdb66', yaxt = 'n',
               ylim = c (0, 1.1*max (dailyPrec [['prec']] [condition])), border = F)
      axis (4, las = 1)
      mtext (4, line = 2.5, text = 'daily total precipitation (mm)')
      par (new = T)
      plot (x = data2 [['datetime']] [data2 [['datetime']] >= from & 
                                      data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)],
            y = data2$pos_lm [data2 [['datetime']] >= from & 
                              data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] - 
              data2$pos_lm [1],
            lwd = 4,
            col = '#ff9999aa',
            xlab = 'date',
            ylab = 'growth (mm)',
            las = 1,
            typ = 'l',
            ylim = c (0, 1.1 * max (c (data2$pos_lm [data2 [['datetime']] >= from & 
                                                     data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] - 
                                       data2$pos_lm [1],
                                       data1$pos_lm [data1 [['datetime']] >= from & 
                                                     data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] - 
                                       data1$pos_lm [1]), na.rm = TRUE)))
      lines (x = data2 [['datetime']] [data2 [['datetime']] >= since & 
                                       data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] ,
             y = data2$pos_lm [data2 [['datetime']] >= since & 
                               data2 [['datetime']] <= tail (data2 [['datetime']], n = 1)] - 
                 data2$pos_lm [1],
             lwd = 4,
             col = '#ff9999')
      lines (x = data1 [['datetime']] [data1 [['datetime']] >= from & 
                                       data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] ,
             y = data1$pos_lm [data1 [['datetime']] >= from & 
                               data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] - 
                 data1$pos_lm [1],
            lwd = 4,
            col = '#6a3d9aaa')
      lines (x = data1 [['datetime']] [data1 [['datetime']] >= since & 
                                         data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] ,
             y = data1$pos_lm [data1 [['datetime']] >= since & 
                                 data1 [['datetime']] <= tail (data1 [['datetime']], n = 1)] - 
               data1$pos_lm [1],
             lwd = 4,
             col = '#6a3d9a')
      abline (v = as.POSIXct (since), lwd = 2, lty = 2, col = '#777777')
      legend (x = data1 [['datetime']] [data1 [['datetime']] == from],
              y = tail (data2$pos_lm, n = 1) - data2$pos_lm [1],
              legend = c ('branch', 'trunk'),
              lwd = 3,
              col = c ('#ff9999','#6a3d9a'),
              box.lty = 0,
              bg = 'transparent')
      legend (x = data1 [['datetime']] [data1 [['datetime']] == from],
              y = (tail (data2$pos_lm, n = 1) - data2$pos_lm [1]) * 0.9,
              legend = 'precipitation',
              lwd = 3,
              col = '#91bfdb66',
              box.lty = 0,
              bg = 'transparent')
      dev.off ()
    }
  }  else if (temporalRes == 'annual') { # If annual resolution is required
    #------------------------------------------------------------------------------------
    
    # Compile monthly growth for the last month and previous
    #------------------------------------------------------------------------------------
    since        <- sprintf ('%s-01-01', year (Sys.Date ()))   
    if (year (Sys.Date ()) == 2019) {
      annualGrowth <- tail (data1 [['pos_lm']], n = 1) - 
                      head (data1 [['pos_lm']], n = 1) + 1
    } else {
      annualGrowth <- tail (data1 [['pos_lm']], n = 1) - 
                            data1 [['pos_lm']] [data1 [['datetime']] == since]
      
    }
    radGrowth <- tibble (annualGrowth)
  }
  
  # Return the radial growth for the appropriate time interval
  #------------------------------------------------------------------------------------
  return (radGrowth)
}
#========================================================================================