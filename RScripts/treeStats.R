#=======================================================================================#
# This script does include some statistics of the witness tree. The values have to be 
# revised.
#---------------------------------------------------------------------------------------#
birthDay         <- as.POSIXct ("1919/04/12", format = "%Y/%m/%d") # in years C.E.    
age              <- floor (time_length (Sys.time () - birthDay, "years"))
  
# LIDAR-derived quantities from scan by Peter Boucher (PhD Candidate, School of 
# Environment, University of Massachusetts Boston) with LEICA BLK360 on 2018/07/27 
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
dbh_qsm          <- 96.5                                      # cm of cylinder at 1.3m height
dbh_cyl          <- 78.0                                      # cm mean of cylinders fitted between 1.1 and 1.5 m height

# Values measured in the field
#---------------------------------------------------------------------------------------#
cbh <- 

# Values from literature
#---------------------------------------------------------------------------------------#
rhoWood       <- 740  # kg/m3 TTR guesstimate which needs to be based of literature eventually.
carbonContent <- 0.463 # % For Quercus rubra from Lamlom & Savidge (2003)
RSRatio       <- 0.5 # unitless TTR guesstimate which needs to be based of literature eventually.
totalMass     <- (totalVolume / 1000.0 * rhoWood) / RSRatio     # kg
totalCarbon   <- totalMass * carbonContent
meanAnnualCarbonSequestration <- totalCarbon / age

# Location
#---------------------------------------------------------------------------------------#
treeLocationName <- "Harvard Forest"
treeLon <-
treeLat <-
treeTimeZone <- 'EST'
#=======================================================================================#