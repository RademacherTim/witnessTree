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
#---------------------------------------------------------------------------------------#
# TTR Not sure selecting messages at random is the best method when there are several 
# TTR messages of highest priority. TBD!
#=======================================================================================#

#=======================================================================================#
# Function to delete message from messages tibble to avoid it being used again.
#---------------------------------------------------------------------------------------#
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
