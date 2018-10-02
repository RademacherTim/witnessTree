# This function checks the expiration dates of messages
#---------------------------------------------------------------------------------------#
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

# This function re-evaluates the priority of preserved messages
#---------------------------------------------------------------------------------------#
reEvaluatePriorityOf <- function (mtable) {
  
  # Increase priority of all messages by 1
  mtable [['prioity']] <- apply (X      = cbind (as.numeric (mtable [['priority']]) + 1, 
                                                 rep (10, dim (mtable) [1])), 
                                 MARGIN = 1, 
                                 FUN    = min)
  
  # Return messages with updated priority
  return (mtable)
}