#=======================================================================================#
# Functions to generate messages depending on the physiological status of the tree.
#
#       Event                                   Date                 
#---------------------------------------------------------------------------------------#
#   1)  Start of sap flow                       variable
#   2)  Peak of sap flow                        variable
#   2)  End of sap flow                         variable
#   2)  Total growing season sap flow           variable
#   3)  Start of wood growth                    variable
#   4)  Peak of wood growth                     variable
#   5)  End of wood growth                      variable
#   6)  Total growing season wood growth        variable
#
#---------------------------------------------------------------------------------------#

# Start of sap flow
#---------------------------------------------------------------------------------------#
startSapFlow <- function (mtable, TEST = 0) {
  # Read in sap flow data
  # Check whether the sap flow has increased substantially ove rthe past week
  # If so, send beginning of sap flow message
  #if ( | TEST == 1) {
  #} 
  return (mtable)
} 
#=======================================================================================#