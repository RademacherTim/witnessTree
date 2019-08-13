#=======================================================================================#
# Functions to generate messages about the community around the tree.
#
#       Event                                   Date                 
#---------------------------------------------------------------------------------------#
#   1)  explainSeedDispersal                    Between 1st of Sep and 30th of Nov
#   2)  checkCommunityWildlife                  All year, when image is added to the 
#                                               directory
#---------------------------------------------------------------------------------------#

# Explain seed dispersal
#---------------------------------------------------------------------------------------#
explainSeedDispersal <- function (ptable, TEST = 0) {
  if (substring (Sys.Date (), 6, 10) > '09-01' & substring (Sys.Date (), 6, 10) < '11-30'| 
      TEST == 1) {
    postDetails <- getPostDetails ("explainSeedDispersal", gs_posts_key = gsPostsKey)
    message   <- sprintf (postDetails [["Message"]])
    expirDate <- sprintf ("%s-11-30 23:59:59 %s", format (Sys.Date (), format = '%Y'), treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]],
                          fFigure     = postDetails [['fFigure']],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  } 
  
  return (ptable)
} 

# Check for wildlife images (visitors) at the tree
#----------------------------------------------------------------------------------------
# 
# To post an image, it needs to be moved into the wildlifeCamera folder in the imagesPath
# directory and named according to the following naming convention: 
# 
# wildlifeCameraImageXXXX.jpg
# 
# , where XXXX has to be replaced by an increasing number with preceeding zeros. The 
#   first image would be named wildlifeCameraImage0001.jpg and so on. 
#----------------------------------------------------------------------------------------
checkCommunityWildlife <- function (ptable, TEST = 0) {
  
  # Check whether there is a new wildlife photo
  #--------------------------------------------------------------------------------------
  if (file.exists ('memory.csv')) {
    memory <- read_csv ('memory.csv', col_types = cols ())
  } 
  listOfVisitors <- list.files (path = sprintf ('%s/wildlifeCam/',imagesPath), 
                                pattern = '.jpg')  

  # Check that there is at least one picture in the directory
  #--------------------------------------------------------------------------------------
  if (length (listOfVisitors) > 0 | TEST >= 1) {
    
    # Check whether there is a new picture in the directory
    #------------------------------------------------------------------------------------
    if (as.numeric (substring (tail (listOfVisitors, n = 1), 20, 23)) > 
        memory [['numberOfPreviousVisitors']] | TEST >= 1) {
    
      # Get message depending on time of year
      #----------------------------------------------------------------------------------
      if (substring (Sys.Date (), 6, 10) >  '03-21' & 
          substring (Sys.Date (), 6, 10) <= '06-21' | 
          TEST == 1) { # it is spring
        postDetails <- getPostDetails ("checkCommunityWildlife - spring", gs_posts_key = gsPostsKey)
      } else if (substring (Sys.Date (), 6, 10) >  '06-21' & 
                 substring (Sys.Date (), 6, 10) <= '09-21' | 
                 TEST == 2) { # it is summer
        postDetails <- getPostDetails ("checkCommunityWildlife - summer", gs_posts_key = gsPostsKey)
      } else if (substring (Sys.Date (), 6, 10) >  '09-21' & 
                 substring (Sys.Date (), 6, 10) <= '11-21' | 
                 TEST == 3) { # it is fall
        postDetails <- getPostDetails ("checkCommunityWildlife - fall", gs_posts_key = gsPostsKey)
      } else if (substring (Sys.Date (), 6, 10) >  '11-21' & 
                 substring (Sys.Date (), 6, 10) <= '03-21' | 
                 TEST == 4) { # it is winter
        postDetails <- getPostDetails ("checkCommunityWildlife - winter", gs_posts_key = gsPostsKey)
      }
      message   <- sprintf (postDetails [["Message"]])
      delay     <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
      expirDate <- sprintf ("%s 23:59:59 %s", 
                            format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone)
      ptable    <- add_row (ptable, 
                            priority    = postDetails [["Priority"]],
                            fFigure     = postDetails [['fFigure']],
                            figureName  = sprintf ('%s/wildlifeCam/%s',imagesPath, tail (listOfVisitors, n = 1)),
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate)
    
      # Increase the wildlife counter in the memory
      #----------------------------------------------------------------------------------
      memory [['numberOfPreviousVisitors']] <- memory [['numberOfPreviousVisitors']] + 1
      write_csv (memory, 'memory.csv')
    }
  }
  
  return (ptable)
} 
#=======================================================================================#