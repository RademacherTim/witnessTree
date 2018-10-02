# This function checks the expiration dates of messages
checkExpirationDatesOf <- function (mtable) {
  
  # Format current time
  date <- sprintf ("%s %s", format (Sys.time (), "%Y-%m-%d %H:%M:%S"), treeTimeZone)
  
  # Loop over messages to check expiration date
  for (i in 1:dim (mtable) [1]) {
    
     # If message is expired, delete it. 
     if (mtable [['expires']] [i] < as.POSIXct (date, format = "%Y-%m-%d %H:%M:%S")) {
       mtable <- mtable [-i, ]
     }
  }
  
  # Return the remaining messages
  return (mtable)
} # TTR: To-do include updating of priorities, so that older messages get a progressively higher priority, never exceeding 9.

