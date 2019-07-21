#========================================================================================
# Function to select message of highest priority
#----------------------------------------------------------------------------------------
selectPost <- function (mtable) # tibble of posts with, inter alia, priorities 
{
  # Delete all messages that are empty or saying "NEEDS MESSAGE"
  #--------------------------------------------------------------------------------------
  mtable <- mtable [mtable [['message']] != 'NEEDS MESSAGE', ]
  mtable <- mtable [mtable [['message']] != '', ]
  
  # Arrange messages by descending priority
  #--------------------------------------------------------------------------------------
  mByPriority <- arrange (.data = mtable, desc (priority))
  
  # Subset only highest priority
  #--------------------------------------------------------------------------------------
  highestPriority <- mByPriority [mByPriority[['priority']] == mByPriority [['priority']] [1] &
                                  !is.na (mByPriority [['priority']]), ]
  
  # Check whether there is more than one post of highest priority
  #--------------------------------------------------------------------------------------
  if (dim (highestPriority) [1] > 1) { # there are several posts of highest priority, select one at random
    post <- sample_n (highestPriority, 1)
  } else { # there is only one post of highest priority
    post <- highestPriority
  }
  
  # Delete temporary variables
  #--------------------------------------------------------------------------------------
  rm (mByPriority, highestPriority)
  
  # Return the selected message
  #--------------------------------------------------------------------------------------
  return (post)
}

# Comments:
#---------------------------------------------------------------------------------------
# TR - Not sure selecting messages at random is the best method when there are several 
#      messages of highest priority. TBD!
#=======================================================================================

#=======================================================================================
# Function to delete message from messages tibble to avoid it being used again.
#---------------------------------------------------------------------------------------
deletePost <- function (mtable,   # tibble of posts
                        message)  # tibble of the selected post
{
  # Get line on which the message is 
  nRow <- which (mtable [['message']] == message [['message']])
  
  # Delete row with same message in messages tibble
  mtable <- mtable [-nRow, ]
  
  # Delete temporary variables
  rm (nRow)
  
  # Return the selected message
  return (mtable)
}
#=======================================================================================

#=======================================================================================
# This function checks the expiration dates of messages
#---------------------------------------------------------------------------------------
checkExpirationDatesOf <- function (mtable) 
{
 
  # Loop over messages to check expiration date
  i = 1
  while (i <= dim (mtable) [1]) {
    
    # If message is expired, delete it.
    if (mtable [['expires']] [i] <  Sys.time ()) {
      mtable <- mtable [-i, ]
    } else {
      i = i + 1
    }
  }
  
  # Return the remaining messages
  return (mtable)
} 
#=======================================================================================

#=======================================================================================
# This function re-evaluates the priority of preserved messages
#---------------------------------------------------------------------------------------
reEvaluatePriorityOf <- function (mtable) 
{
  
  # Increase priority of all messages by 1
  mtable [['priority']] <- apply (X      = cbind (as.numeric (mtable [['priority']]) + 1, 
                                                 rep (10, dim (mtable) [1])), 
                                 MARGIN = 1, 
                                 FUN    = min)
  
  # Return messages with updated priority
  return (mtable)
}

# Comments:
#---------------------------------------------------------------------------------------
# TR - maybe wee need to come up with a different way of increasing the priority of 
#      various messages instead on just increasing them by 1. 
#=======================================================================================

#=======================================================================================
# This function reads in the message text, hastags and expiration date from a central 
# spreadsheet and hands them to a specific function.
#---------------------------------------------------------------------------------------
getPostDetails <- function (fName) 
{
  
  # load dependencies
  #-------------------------------------------------------------------------------------
  if (!existsFunction ('gs_title')) library ('googlesheets')
  if (!existsFunction ('oauth2.0_token')) library ('httr')
  library ('httpuv')
  options (httr_oob_default = TRUE) 

  # remove current authentication token for google sheets
  #-------------------------------------------------------------------------------------
  file.remove ('.httr-oauth') 

  # generate new authentication token for google sheets
  #-------------------------------------------------------------------------------------
  oauth2.0_token (
    endpoint = oauth_endpoints ("google"),
    app = oauth_app (
      "google",
      key = getOption("googlesheets.client_id"),
      secret = getOption("googlesheets.client_secret")
    ),
    scope = c (
      "https://spreadsheets.google.com/feeds",
      "https://www.googleapis.com/auth/drive"),
    use_oob = TRUE,
    cache = TRUE
  )

  # get the witnessTreePosts google sheet
  #----------------------------------------------------------------------------------------
  spreadsheet <- gs_title ("witnessTreePosts")
  
  # get posts spreadsheet
  #----------------------------------------------------------------------------------------
  input <- gs_read (ss = spreadsheet, ws = "posts", col_types = cols ())
  
  # Find appropriate lines using the function name
  #-------------------------------------------------------------------------------------
  temp <- input [input [['FunctionID']] == fName &
                 !is.na (input [['FunctionID']]), ]
  
  # If there is more than one message for an event choose a message randomly
  # N.B.: To make sure that both treatments get choosen with the same probability, we 
  # need to have the same number of messages for both treatments (sober scientific data 
  # versus narrative environmental facts).
  #-------------------------------------------------------------------------------------
  if (dim (temp) [1] > 1) {
    temp <- sample_n (temp, 1)
  }
  
  # Extract post details # TR - May not need all of them, i.e. logic
  #-------------------------------------------------------------------------------------
  postDetails <- temp %>% select (-c (Status, Event))
  
  # Check whether there is a figure accompanying the post
  #-------------------------------------------------------------------------------------
  postDetails <- add_column (postDetails,
                             fFigure = ifelse (length (postDetails [["FigureName"]]) == 0 |
                                               is.na (postDetails [['FigureName']]), F, T))
  
  # Randomly decide whether we use the accompanying figure or not
  # N.B. Audience building posts are marked as such and are always posted with pictures
  #-------------------------------------------------------------------------------------
  if (postDetails [['Treatment']] != 'Audience' & !is.na (postDetails [['FigureName']])) {
    postDetails [['fFigure']] <- sample (c (TRUE, FALSE), size = 1)
  }
  
  # Add the image path to the figureName, so that the bot can actually find them
  #-------------------------------------------------------------------------------------
  if (!is.na (postDetails [['FigureName']])) {
    postDetails [['FigureName']] <- sprintf ('%s%s', imagesPath, 
                                             postDetails [['FigureName']])
  }
  
  # Return the post'd details
  #-------------------------------------------------------------------------------------
  return (postDetails)
}
#=======================================================================================