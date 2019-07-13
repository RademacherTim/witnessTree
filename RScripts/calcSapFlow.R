#========================================================================================
# This script contains a function to calculate sap flow from three needle sensors 
# (East 30) using the heat pulse method.
#----------------------------------------------------------------------------------------
treeName <- 'witnessTree'

calcSapflow <- function (treeName, dataPath) {

  # tree dimensions
  #--------------------------------------------------------------------------------------
  if (treeName == 'QuercusRubra1') {
    treeCBH <- 245.6
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.5
    rHeartWood <- 0.0
  } else if (treeName == 'QuercusRubra2' | treeName = 'witnessTree') {
    ifelse (treeName == 'QuercusRubra2', treeCBH <- 142.8, treeCBH <- 245.6)
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.5
    rHeartWood <- 0.0
  } else if (treeName == 'QuercusRubra3') {
    treeCBH <- 171.3
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.5
    rHeartWood <- 0.0
  } else if (treeName == 'AcerRubrum1') {
    treeCBH <- 92.0
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 0.5
    rHeartWood <- 0.0
  } else if (treeName == 'AcerRubrum2') {
    treeCBH <- 101.0
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 0.5
    rHeartWood <- 0.0
  } else if (treeName == 'AcerRubrum3') {
    treeCBH <- 70.8
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 0.5
    rHeartWood <- 0.0
  } else if (treeName == 'PinusStrobus1') {
    treeCBH <- 165.8
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 0.0
  } else if (treeName == 'PinusStrobus2') {
    treeCBH <- 152.8
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 0.0
  } else if (treeName == 'PinusStrobus3') {
    treeCBH <- 181.0
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 0.0
  }


  # Load dependencies
  #--------------------------------------------------------------------------------------
  library ('tidyverse')

  # Constants
  #--------------------------------------------------------------------------------------
  Cw <- 4.18 # volumetric heat capacity of water (MJ m-3 K-1)
  xd <- 0.6  # distance between the heated needle and downstream needle (cm)
  xu <- 0.6  # distance between the heated needle and upstream needle (cm)

  # Get and collate data
  #--------------------------------------------------------------------------------------
  temp1 <- read_csv (file = sprintf ('%s/%s(2).dat', dataPath, treeName), 
                     col_types = cols (),
                     skip = 4, 
                     col_names = c ('datetime','recordNumber','batteryVoltage',
                                    'panelTemperature','Tinitial_1','Tinitial_2',
                                    'Tinitial_3','Tinitial_4','Tinitial_5','Tinitial_6',
                                    'T60_1','T60_2','T60_3','T60_4','T60_5','T60_6'))
  temp2 <- read_csv (file = sprintf ('%s/%s(1).dat', dataPath, treeName), 
                     col_types = cols (),
                     skip = 4, 
                     col_names = c ('datetime','recordNumber','batteryVoltage',
                                    'panelTemperature','Tinitial_1','Tinitial_2',
                                    'Tinitial_3','Tinitial_4','Tinitial_5','Tinitial_6',
                                    'T60_1','T60_2','T60_3','T60_4','T60_5','T60_6'))

  temp3 <- read_csv (file = sprintf ('%s/%s.dat', dataPath, treeName), 
                     col_types = cols (),
                     skip = 4, 
                     col_names = c ('datetime','recordNumber','batteryVoltage',
                                    'panelTemperature','Tinitial_1','Tinitial_2',
                                    'Tinitial_3','Tinitial_4','Tinitial_5','Tinitial_6',
                                    'T60_1','T60_2','T60_3','T60_4','T60_5','T60_6'))
  rawData <- rbind (temp1, temp2, temp3); rm (temp1, temp2, temp3)
  Tinitial <- rawData [, c (1,5:10)]
  T60 <- rawData [, c (1,11:16)]
  rm (rawData)

  # Calculate the temperature differences
  #--------------------------------------------------------------------------------------
  deltaT <- tibble (datetime = Tinitial [['datetime']])
  for (i in 1:6) {
    deltaT <- cbind (deltaT, T60 [, i + 1] - Tinitial [, i + 1])
  }

  # Calculate the sap wood area for each zone
  #--------------------------------------------------------------------------------------
  rXylem <- rTree - bark # radius of xylem
  AXylem1p5 <- pi * (rXylem^2.0         - (rXylem-0.015)^2.0) # area of the outermost 1.5 
  # cm of xylem (m2) 
  AXylem2p5 <- pi * ((rXylem-0.015)^2.0 - (rXylem-0.025)^2.0) # area of the ring between 
  # 1.5 and 2.5 cm of xylem (m2) 
  AXylemRem <- pi * ((rXylem-0.025)^2.0 - (rXylem-rHeartWood)^2.0) # area of the ring 
  # between 2.5cm inside 
  # of the bark and the
  # heartwood

  # Calculate sap flow velocity and flux in all three zones
  #--------------------------------------------------------------------------------------
  ratio  <- tibble (datetime = Tinitial [['datetime']])
  sapVel <- tibble (datetime = Tinitial [['datetime']])
  sapFlu <- tibble (datetime = Tinitial [['datetime']])
  for (i in 1:3) {
    ratio  <- cbind (ratio,  log (deltaT [,i+1] / deltaT [, i+4]))
    sapVel <- cbind (sapVel, (ratio [,i+1] * 2.0 * k) / ((xd + xu) / Cw)) # Sap flow velocity (cm s-1) for each zone
    if (i == 1) {
      area = AXylem1p5
    } else if (i == 2) {
      area = AXylem2p5
    } else if (i == 3) {
      area = AXylemRem
    }
    sapFlu <- cbind (sapFlu, sapVel [, i+1]*3600*area) # Sap flux (cm3 hr-1)
  }

  plot (sapVel [, 1], sapVel [, 2],
        typ = 'l',
        xlab = 'time',
        ylab = 'sap velocity (cm s-1)',
        col = '#91b9a4',
        ylim = c (0, 0.8))
  lines (sapVel [, 1], sapVel [, 3],
         col = '#106470')
  lines (sapVel [, 1], sapVel [, 4],
         col = 'brown')
  plot (sapFlu [, 1], (sapFlu [, 2] + sapFlu [, 3] +  sapFlu [, 4]) / 1000.0,
        typ = 'l',
        xlab = 'time',
        ylab = 'sap flux (L h-1)',
        ylim = c (-0, 20))
  lines (sapFlu [, 1], sapFlu [, 2] / 1000.0,
         col = '#91b9a4')
  lines (sapFlu [, 1], sapFlu [, 3] / 1000.0,
         col = '#106470')
  lines (sapFlu [, 1], sapFlu [, 4] / 1000.0,
         col = 'brown')
  
  return ()
}
#========================================================================================