#========================================================================================
# Functions to generate messages depending on the physiological status of the tree.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Monthly sapflow summary                 second week of the month
#   x)  Start of sap flow                       variable
#   x)  Peak of sap flow                        variable
#   x)  End of sap flow                         variable
#   x)  Total growing season sap flow           variable
#   x)  Start of wood growth                    variable
#   x)  Peak of wood growth                     variable
#   x)  End of wood growth                      variable
#   x)  Total growing season wood growth        variable
#
#----------------------------------------------------------------------------------------


# Monthly sap flow summary
#----------------------------------------------------------------------------------------
monthlySapFlowSummary <- function (mtable, TEST = 0) {
  
  # Check whether it is the second week of the month
  if (ceiling (day (Sys.Date ()) / 7) == 2 | TEST == 1) {
  # Read in sap flow data
  # Compare the sap flow to the previous month
  #
  } 
  return (mtable)
} 

# Monthly sap flow summary
#----------------------------------------------------------------------------------------
monthlyRadGrowthSummary <- function (mtable, TEST = 0) {
  
  # Check whether it is the second week of the month
  if (ceiling (day (Sys.Date ()) / 7) == 4 | TEST == 1) {

    # Get radial growth for the last month and the month prior
    #------------------------------------------------------------------------------------
    
    
    # Compare the sap flow to the previous month
    #
  } 
  return (mtable)
} 

# Start of sap flow
#----------------------------------------------------------------------------------------
startSapFlow <- function (mtable, TEST = 0) {
  # Read in sap flow data
  # Check whether the sap flow has increased substantially ove rthe past week
  # If so, send beginning of sap flow message
  #if ( | TEST == 1) {
  #} 
  return (mtable)
} 
#========================================================================================