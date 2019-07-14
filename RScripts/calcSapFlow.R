#========================================================================================
# This script contains a function to calculate sap flow from three needle sensors 
# (East 30) using the heat pulse method.
#----------------------------------------------------------------------------------------
treeName <- 'witnessTree'
treeName <- 'PinusStrobus1'

calcSapflow <- function (treeName, dataPath, PLOT = F) {

  # dimensions of the tree
  #--------------------------------------------------------------------------------------
  if (treeName == 'QuercusRubra1') {
    treeCBH <- 245.6
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 5.0
  } else if (treeName == 'QuercusRubra2' | treeName == 'witnessTree') {
    ifelse (treeName == 'QuercusRubra2', treeCBH <- 142.8, treeCBH <- 245.6)
    treeName <- 'QuercusRubra2'
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 5.0
  } else if (treeName == 'QuercusRubra3') {
    treeCBH <- 171.3
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 5.0
  } else if (treeName == 'AcerRubrum1') {
    treeCBH <- 92.0
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 0.5
    rHeartWood <- 5.0
  } else if (treeName == 'AcerRubrum2') {
    treeCBH <- 101.0
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 0.5
    rHeartWood <- 5.0
  } else if (treeName == 'AcerRubrum3') {
    treeCBH <- 70.8
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 0.5
    rHeartWood <- 5.0
  } else if (treeName == 'PinusStrobus1') {
    treeCBH <- 165.8
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 5.0
  } else if (treeName == 'PinusStrobus2') {
    treeCBH <- 152.8
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 5.0
  } else if (treeName == 'PinusStrobus3') {
    treeCBH <- 181.0
    rTree   <- treeCBH / (2.0 * pi) 
    bark    <- 2.0
    rHeartWood <- 5.0
  }


  # Load dependencies, if not already loaded
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('tibble')) library ('tidyverse')

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
  AXylem1p5 <- pi * (rXylem^2.0       - (rXylem-1.5)^2.0) # area of the outermost 1.5 
                                                          # cm of xylem (cm2) 
  AXylem2p5 <- pi * ((rXylem-1.5)^2.0 - (rXylem-2.5)^2.0) # area of the ring between 
                                                          # 1.5 and 2.5 cm of xylem (cm2) 
  AXylemRem <- pi * ((rXylem-2.5)^2.0 - (rXylem-rHeartWood)^2.0) # area of the ring 
                                                                 # between 2.5cm inside 
                                                                 # of the bark and the
                                                                 # heartwood (cm2)

  # Calculate sap flow velocity and flux in all three zones
  #--------------------------------------------------------------------------------------
  ratio  <- tibble (datetime = Tinitial [['datetime']])
  sapVel <- tibble (datetime = Tinitial [['datetime']])
  sapFlu <- tibble (datetime = Tinitial [['datetime']])
  for (i in 1:3) {
    ratio  <- cbind (ratio,  log (deltaT [,i+1] / deltaT [, i+4]))
    temp   <- (ratio [,i+1] * 2.0 * k) / ((xd + xu) * Cw) 
    temp   <- pmax (temp, 0.0, na.rm = T) # make sure sap flow velocity is positive 
    sapVel <- cbind (sapVel, temp) # Sap flow velocity (cm s-1) for each zone
    if (i == 1) {
      area = AXylem1p5
    } else if (i == 2) {
      area = AXylem2p5
    } else if (i == 3) {
      area = AXylemRem
    }
    sapFlu <- cbind (sapFlu, sapVel [, i+1]*3600.0*area) # Sap flux (cm3 hr-1)
  }
  names (sapVel) <- c ('datetime','outer','middle','inner')
  names (sapFlu) <- c ('datetime','outer','middle','inner')

  # Sum total sap flux for the day and get the mean sapflux velocity
  #--------------------------------------------------------------------------------------
  total <- sapFlu  %>% mutate (date = date(sapFlu [['datetime']])) %>%
                       group_by (date) %>%
                       summarize (dailySapFlux = sum (outer + middle + inner))
  total [['dailySapFlux']] <- total [['dailySapFlux']] / 3.0 / 1000.0

  if (PLOT) {
    plot (total$date, total$total / 1000.0,
          typ = 'l',
          xlab = 'time',
          ylab = 'sap flux (L day-1)',
          col = '#91b9a4')
  }
  
  # Determine sap flow in the last 30 days
  #--------------------------------------------------------------------------------------
  
  
  return ()
}
#========================================================================================