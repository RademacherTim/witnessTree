#=======================================================================================#
# This script does include some statistics of the witness tree. The values have to be 
# revised.
#---------------------------------------------------------------------------------------#
birthDay         <- as.POSIXct ("1919/04/12", format = "%Y/%m/%d") # in years C.E.    
age              <- floor (time_length (Sys.time () - birthDay, "years"))
  
# LIDAR-derived quantities from scan by Peter Boucher with LEICA instrument on 2018/ 
#---------------------------------------------------------------------------------------#
totalVolume      <- 13937                                     # L
trunkVolume      <- 5624                                      # L
branchVolume     <- 8313                                      # L
treeHeight       <- 26.6                                      # m
trunkLength      <- 25.0                                      # m
branchLength     <- 2589.0                                    # m
branchNumber     <- 2015                                      # m
maxBranchOrder   <- 9                                         # unitless
totalSurfaceArea <- 437.0                                     # m2
dbh_qsm          <- 96.5                                      # cm
dbh_cyl          <- 78.0                                      # cm

# Values from literature
#---------------------------------------------------------------------------------------#
rhoWood       <- 740  # kg/m3 TTR guesstimate which needs to be based of literature eventually.
carbonContent <- 0.47 # % TTR guesstimate which needs to be based of literature eventually.
RSRatio       <- 0.5 # unitless TTR guesstimate which needs to be based of literature eventually.
totalMass     <- (totalVolume / 1000.0 * rhoWood) / RSRatio     # kg
totalCarbon   <- totalMass * carbonContent
meanAnnualCarbonSequestration <- totalCarbon / age
#=======================================================================================#