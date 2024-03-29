#========================================================================================
# Functions to generate messages about the phenology of the tree itself and its 
# surrounding community. These functions rely heavily on work by Bijan Seyednasrollah.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  getPhenoCamImagesAndData                variable
#   2)  checkLeafColourChange                   variable
#----------------------------------------------------------------------------------------

# function to get a phenocam image
#----------------------------------------------------------------------------------------
getPhenocamImagesAndData <- function (siteName = 'Harvard Forest', # name of phenocam sites
                                      DOWNLOAD = TRUE,             # whether to download images or not 
                                      GCC      = TRUE,             # whether to download GCC
                                      TEST     = 0) {
  # load dependencies
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('get_midday_list')) library ('phenocamapi')
  
  # get phenocam site names
  #--------------------------------------------------------------------------------------
  if (siteName == 'Harvard Forest') {
    siteNames = c ('witnesstree', 'harvardbarn') #,'harvardbarn2') # Took out the second 
                                                                   # tower camera to speed 
                                                                   # things up
  }
    
  # Check whether download of images is desired
  #--------------------------------------------------------------------------------------
  if (DOWNLOAD) {
    
    # loop over number of cameras
    #------------------------------------------------------------------------------------
    for (s in siteNames) {
      # get list of midday image names for the three cameras
      #------------------------------------------------------------------------------------
      assign (paste0 ('site_midday_',s), get_midday_list (s))
      
      # download only the last available image for each camera
      #----------------------------------------------------------------------------------
      download.file (tail (get (paste0 ('site_midday_',s)), n = 1), 
                     destfile = paste0 (path,'tmp/',s,'_PhenoCamImage.jpg'), 
                     mode = 'wb',
                     quiet = TRUE)
    }
  }

  # getting the timeseries from the phenocam server
  #----------------------------------------------------------------------------------------
  if (GCC){
    for (s in siteNames) {
      gcc_temp <- tail (get_pheno_ts (s, 
                                      vegType = 'DB', 
                                      roiID   = 1000, 
                                      type    = '3day'), 
                        n = 1)
      if (s == siteNames [1]) {
        gcc <- gcc_temp 
      } else {
        gcc <- rbind (gcc, gcc_temp)
      }
    }
  } else {
    gcc <- 0
  }

  # Return gcc values for the site's cameras
  #----------------------------------------------------------------------------------------
  return (gcc)
    
}

# function to post a phenocam image when colour change is ongoing
#----------------------------------------------------------------------------------------
checkLeafColourChange <- function (ptable, TEST = 0) {

  # Don't bother checking this during summer and winter
  #--------------------------------------------------------------------------------------
  if ((substr (Sys.Date (), 6, 10) > "03-01" | substr (Sys.Date (), 6, 10) < "06-15") | 
      (substr (Sys.Date (), 6, 10) > "09-01" | substr (Sys.Date (), 6, 10) < "11-15")) {
    
    # check season in memory
    #------------------------------------------------------------------------------------
    if (file.exists ('memory.csv')) {
      memory <- read_csv ('memory.csv', col_types = cols ())
    } else {
      memory <- tibble (numberOfPreviousVisitors = length (listOfVisitors),
                        lastResponse = format (Sys.time (), '%Y-%m-%d %H:%M'),
                        dimensionsPosted = FALSE,
                        growingSeason = TRUE)
    }
  
    # set site threshold
    #------------------------------------------------------------------------------------
    siteGCCThreshold <- 0.35 
     
    # get phenocam data
    #------------------------------------------------------------------------------------
    gcc <- getPhenocamImagesAndData (siteName = 'Harvard Forest', DOWNLOAD = TRUE)
  
    if ((!memory [['growingSeason']] & gcc [['gcc_90']] [2] > siteGCCThreshold)| TEST == 1) {
      postDetails <- getPostDetails ("checkLeafUnfolding")
      FigureName  <- 'witnesstree_PhenoCamImage'
      delay     <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
      ptable    <- add_row (ptable,
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/tmp/%s.jpg', path, FigureName),
                            message     = postDetails [["MessageText"]],
                            hashtags    = postDetails [["Hashtags"]],
                            expires     = expiresIn (delay))
      
      # update growingSeason boolean to start the season
      #----------------------------------------------------------------------------------
      memory [['growingSeason']] <- TRUE
      memory [['lastResponse']] <- format (memory [['lastResponse']], '%Y-%m-%d %H:%M')
      write_csv (memory, 'memory.csv')
      
    } else if ((memory [['growingSeason']] & gcc [['gcc_90']] [2] < siteGCCThreshold) | TEST == 2) {
      postDetails <- getPostDetails ("checkLeafColourChange - endOfSeason")
      FigureName  <- 'witnesstree_PhenoCamImage'
      delay     <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
      ptable    <- add_row (ptable,
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/tmp/%s.jpg', path, FigureName),
                            message     = postDetails [['MessageText']],
                            hashtags    = postDetails [["Hashtags"]],
                            expires     = expiresIn (delay))
      
      # update growingSeason boolean to end the season
      #-----------------------------------------------------------------------------------
      memory [['growingSeason']] <- FALSE
      memory [['lastResponse']] <- format (memory [['lastResponse']], '%Y-%m-%d %H:%M')
      write_csv (memory, 'memory.csv')
      
    }
  }
  # return table with posts
  #------------------------------------------------------------------------------------
  return (ptable)
}

# # load dependencies
# #----------------------------------------------------------------------------------------
# library ('animation')
# library ('phenocamapi')
# library ('lubridate')
# 
# 
# # set parameters
# #----------------------------------------------------------------------------------------
# site  <- 'witnesstree'  # phenocam site name
# Years <- year (Sys.Date ()) # vector of years to make the animation
# vegType <- 'DB' # vegetation type DB = deciduous broadloeaf
# roiID <- 1000  # ROI ID 
# 
# # plot the image
# #----------------------------------------------------------------------------------------
# if(class(img)!='try-error'){
#   par(mar= c(0,0,0,0))
#   plot(0:1,0:1, type='n', axes= FALSE, xlab= '', ylab = '')
#   rasterImage(img, 0, 0, 1, 1)
# }
# 
# # create a new folder to download the midday images
# #----------------------------------------------------------------------------------------
# dir.create (site, showWarnings = FALSE)
# 
# # getting the timeseries from the phenocam server
# #----------------------------------------------------------------------------------------
# gcc_ts <- get_pheno_ts (site, 
#                         vegType = vegType, 
#                         roiID   = roiID, 
#                         type    = '1day')
# 
# # organizing columns
# #----------------------------------------------------------------------------------------
# gcc_ts [, month:=month (YYYYMMDD)] # extracting month from the date
# gcc_ts [, YYYYMMDD:=as.Date (YYYYMMDD)] # convert to the right format
# gcc_ts [, midday_url:=sprintf ('https://phenocam.sr.unh.edu/data/archive/%s/%04d/%02d/%s', 
#                                site, year, month, midday_filename)] # making the URL of midday images
# 
# # organizing the data into a new data.table including the URL, date and GCC90 values
# #----------------------------------------------------------------------------------------
# gcc_file_tbl <- gcc_ts[year%in%(Years),.(midday_url, YYYYMMDD, gcc_90)] 
# 
# # creating the destination filename to download each midday image
# #----------------------------------------------------------------------------------------
# gcc_file_tbl [, midday_dest:=paste0 (site, '/', basename (midday_url))] 
# gcc_file_tbl <- na.omit (gcc_file_tbl) # removing the NA values
# 
# 
# gcc_file_tbl <- gcc_file_tbl [month (YYYYMMDD) == 5]
# 
# # downloading midday files
# #----------------------------------------------------------------------------------------
# mapply (function (x) {
#           dest <- paste0 (site, '/', basename (x))
#           if (file.exists (dest)) {
#             message (dest, ' ', 'already exists!')
#             return ()
#           }
#           try (download.file (x, dest))
#         }, gcc_file_tbl$midday_url)
# 
# # a simple function to plot midday image given an index and corresponding gcc timeseries upto that date
# #----------------------------------------------------------------------------------------
# show_midday <- function (i) {
#   
#   par (fig = c (0, 1, 0.3, 1),  mar = c (0, 0, 0, 0), bg = '#000000')  
#   plot (0:1, 0:1, type = 'n', axes = FALSE, xlab = '', ylab = '')
#   
#   img <- readJPEG (gcc_file_tbl$midday_dest [i])
#   rasterImage (img, 0, 0, 1, 1)
#   mtext ('Greenup Seasonality at Harvard Forest', col = '#51fddc')
#   
#   par (fig = c (0, 1, 0, 0.3), new = T, mar = c (2, 2, 0, 0))  
#   plot (gcc_file_tbl$YYYYMMDD [1:i], 
#         gcc_file_tbl$gcc [1:i], 
#         bty ='n', 
#         type = 'l',
#         lwd = 2,
#         cex.axis = 1.5,
#         col = '#51fddc', 
#         col.axis = '#51fddc',
#         xlim = range (gcc_file_tbl$YYYYMMDD),
#         ylim = range (gcc_file_tbl$gcc, na.rm = TRUE))
#   mtext ('Canopy Greenness', side = 2, line = 0, col = '#51fddc', cex = 2, font = 2)
#   
#   points (gcc_file_tbl$YYYYMMDD [i], 
#           gcc_file_tbl$gcc [i], 
#           pch = 19,
#           col = '#ca5f63')
# }
# 
# # dummy
# #----------------------------------------------------------------------------------------
# gcc_file_tbl [, gcc := gcc_90]
# 
# # number of image
# #----------------------------------------------------------------------------------------
# n <- nrow (gcc_file_tbl)
# 
# # make the animation using the saveVideo animation file
# #----------------------------------------------------------------------------------------
# saveVideo (interval = 0.5, # animation interval in seconds
#            ani.width = 1000, # image width in pixels
#            ani.height = 900,# image height in pixels
#            ani.res = 75, # resolution, not important here
#            video.name = paste0 (site, '.mp4'),
#           
#            for (i in seq (1, n, by = 1)){
#              cat (i, '\n')
#              show_midday (i)
#            })
#========================================================================================

