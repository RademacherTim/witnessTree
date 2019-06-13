#=======================================================================================#
# Function to select message of highest priority
#---------------------------------------------------------------------------------------#
selectMessage <- function (mtable) # tibble of messages with, inter alia, priorities 
{
  # Arrange messages by descending priority
  mByPriority <- arrange (.data = mtable, desc (priority))
  
  # Subset only highest priority
  highestPriority <- mByPriority [mByPriority[['priority']] == mByPriority [['priority']] [1], ]
  
  # Check whether there is more than one message of highest priority
  if (dim (highestPriority) [1] > 1) { # there are several messages of highest priority, select one at random
    message <- sample_n (highestPriority, 1)
  } else { # there is only one messages of highest priority
    message <- highestPriority
  }
  
  # Delete temporary variables
  rm (mByPriority, highestPriority)
  
  # Return the selected message
  return (message)
}

# Comments:
#---------------------------------------------------------------------------------------
# TR - Not sure selecting messages at random is the best method when there are several 
#      messages of highest priority. TBD!
#=======================================================================================

#=======================================================================================
# Function to delete message from messages tibble to avoid it being used again.
#---------------------------------------------------------------------------------------
deleteMessage <- function (mtable,   # tibble of messages
                           message)  # tibble of the selected message
{
  # Get line on which the message is
  nRow <- which (mtable$message == message$message)
  
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
checkExpirationDatesOf <- function (mtable) {
  
  # Loop over messages to check expiration date
  for (i in 1:dim (mtable) [1]) {
    
    # If message is expired, delete it. 
    if (mtable [['expires']] [i] <  Sys.time ()) {
      mtable <- mtable [-i, ]
    }
  }
  
  # Return the remaining messages
  return (mtable)
} 
#=======================================================================================

#=======================================================================================
# This function re-evaluates the priority of preserved messages
#---------------------------------------------------------------------------------------
reEvaluatePriorityOf <- function (mtable) {
  
  # Increase priority of all messages by 1
  mtable [['prioity']] <- apply (X      = cbind (as.numeric (mtable [['priority']]) + 1, 
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
getMessageDetails <- function (fName) {
  
  # Read in spreadsheet with message texts
  #-------------------------------------------------------------------------------------
  input <- read_csv (file = 'messagesText.csv', col_types = cols ())  
  
  # Find appropriate lines using the function name
  #-------------------------------------------------------------------------------------
  temp <- input [input [['FunctionID']] == fName &
                 !is.na (input [['FunctionID']]), ]
  
  # If there is more than one message for an event choose one randomly
  #-------------------------------------------------------------------------------------
  if (dim (temp) [1] > 1) {
    temp <- sample_n (temp, 1)
  }
  
  # Extract message details # TR - May not need all of them, i.e. logic
  #-------------------------------------------------------------------------------------
  messageDetails <- temp %>% select (-c (Status, Event))
  
  # Check whether there is a figure accompanying the post
  #-------------------------------------------------------------------------------------
  messageDetails <- add_column (messageDetails,
                                fFigure = ifelse (length (messageDetails [["FigureName"]]) == 0, T, F))
  
  return (messageDetails)
}

# Comments:
#---------------------------------------------------------------------------------------
# TR -  
#=======================================================================================