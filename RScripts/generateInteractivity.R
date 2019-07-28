#========================================================================================
# Generate responses for interactive functions              
#----------------------------------------------------------------------------------------
generateInteractiveResponses <- function (TEST = 0) {
  
  # create responses tibble
  #--------------------------------------------------------------------------------------
  responses <- tibble (season = NA, topic = NA, reply = NA)
  
  # check air temperature
  #------------------------------------------------------------------------------------
  airTemp <- tail (airt [['airt']], n = 1)
  meanAirTemp <- mean (dailyAirt [['airt']] [yday (dailyAirt [['day']]) == yday (Sys.Date ())],
                       na.rm = T)
  
  # check precipitation
  #------------------------------------------------------------------------------------
  precip <- sum (tail (dailyPrec [['prec']], n = 14), na.rm = T) # biweekly sum
  from <- head (tail (dailyPrec [['day']], n = 14), n = 1)
  for (iYr in 1964:year (Sys.Date ())) {
    sum14 <- sum (dailyPrec [['prec']] [year (dailyPrec [['day']]) == iYr & 
                                          yday (dailyPrec [['day']]) >= yday (from) & 
                                          yday (dailyPrec [['day']]) <= yday (Sys.Date ())],
                  na.rm = T)
    if (iYr == 1964) {
      meanPrecip <- tibble (year = iYr, mean = sum14)
    } else {
      meanPrecip <- add_row (meanPrecip, year = iYr, mean = sum14)
    }
  }
  meanPrecip <- mean (meanPrecip [['mean']], na.rm = T)
  
  # determine whether it is the growing season or not
  #--------------------------------------------------------------------------------------
  if (Sys.Date () > as.POSIXct ('15-03', format = '%d-%m') & 
      Sys.Date () < as.POSIXct ('15-10', format = '%d-%m')) { # growing season
    
    # set season to growing season
    #------------------------------------------------------------------------------------
    season <- 'growing season'
    
    # get responses based on temperature
    #------------------------------------------------------------------------------------
    if (round (airTemp) > round (meanAirTemp)) { # hot
      postDetails <- getPostDetails ('generateInteractivity - growing season - hot', 
                                     gs_posts_key = gsPostsKey)
    } else if (round (airTemp) == round (meanAirTemp)) { # average
      postDetails <- getPostDetails ('generateInteractivity - growing season - average temperature', 
                                     gs_posts_key = gsPostsKey)
    } else if (airTemp < meanAirTemp) { # cold
      postDetails <- getPostDetails ('generateInteractivity - growing season - cold', 
                                     gs_posts_key = gsPostsKey)
    }
    postDetails [['Message']] <- sprintf (postDetails [['Message']], round (airTemp, 1),
                                          round (9.0 / 5.0 * (airTemp) + 32, 1))
    responses <- add_row (responses, season = season, topic = 'temperature', 
                          reply = postDetails [['Message']])
    
    # get responses based on precip
    #------------------------------------------------------------------------------------
    if (round (precip) > round (meanPrecip)) { # wet
      postDetails <- getPostDetails ('generateInteractivity - growing season - wet', 
                                     gs_posts_key = gsPostsKey)
      growth <- calcRadialGrowth (pdm_calibration_path = dataPath, temporalRes = 'annual')
      postDetails [['Message']] <- sprintf (postDetails [['Message']], round (dbh * 100.0 + bark + growth / 10, 2))
    } else if (round (precip) == round (meanPrecip)) { # average
      postDetails <- getPostDetails ('generateInteractivity - growing season - average', 
                                     gs_posts_key = gsPostsKey)
    } else if (round (precip) < round (meanPrecip)) { # dry
      postDetails <- getPostDetails ('generateInteractivity - growing season - dry', 
                                     gs_posts_key = gsPostsKey)
    }
    responses <- add_row (responses, season = season, topic = 'rainfall', 
                          reply = postDetails [['Message']])

  } else { # off-season
    
    # set season to growing season
    #------------------------------------------------------------------------------------
    seasons <- 'off-season'
    if (round (airTemp) > round (meanAirTemp)) { # hot
      postDetails <- getPostDetails ('generateInteractivity - off season - hot', 
                                     gs_posts_key = gsPostsKey)
    } else if (round (airTemp) == round (meanAirTemp)) { # average
      postDetails <- getPostDetails ('generateInteractivity - off season - average', 
                                     gs_posts_key = gsPostsKey)
    } else if (round (airTemp) < round (meanAirTemp)) { # cold
      postDetails <- getPostDetails ('generateInteractivity - off season - cold', 
                                     gs_posts_key = gsPostsKey)
    }
    postDetails [['Message']] <- sprintf (postDetails [['Message']], round (airTemp, 1),
                                          round (9.0 / 5.0 * (airTemp) + 32, 1))
    responses <- add_row (responses, season = season, topic = 'temperature', 
                          reply = postDetails [['Message']])
    
    # check precipitation
    #------------------------------------------------------------------------------------
  }
  
  
  # delete first row in responses tibble
  #--------------------------------------------------------------------------------------
  responses <-  responses [-1, ]
  
  # write messages to csv file
  #--------------------------------------------------------------------------------------
  write_csv (responses, path = './tmp/interactiveResponses.csv')
  
  # return zero exit status to signal that it ran smoothly
  #--------------------------------------------------------------------------------------
  return (0)
}
#----------------------------------------------------------------------------------------
# - Should create better definition of the growing season
#========================================================================================