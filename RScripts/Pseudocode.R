#GRAPHS 

    #Immediate Production
    1. "AirTemperature_2p0" vs "CBH_Oak1"
    2. "Ploem Temperature"  vs "CBH_Oak1"
    3. "AirTemperature_2p0" vs "Ploem Temperature"
  
    

    #For Later Production
    1. Plot "CBH_Oak1"(y1) vs "SapFlow"(y2) vs Time(x)
    2. Rate of tree respiration with automated respiration 
       chambers and create a graph in flux puppy
    3. Stem water evaporation rate using flux puppy
    4. Comparison graph for growing season ploem 

#BACKGROUND STATISTICS
1. Calculate daily max and min CBH "CBH_range <- max(data$CBH_Oak1) - min(data$CBH_Oak1)"
2. Calculate daily max and min air temperature "AirTemp <- max(data$AirTemperature_2p0) - min(data$AirTemperature_2p0)"
    a. Compare max and min air temperature to phloem temperature
        1a. During hottest summer day and coldest winter day-> 
            need to calculate the max and min temperatures for the year
    b. Record maximum daily temperature fluxuation for the seasons,
       report at end of season (ex. start of winter to end of winter) 
       accompanied by explanation of what can happen to the organism 
       with these drastic daily fluxuations
    
3. Range in CBH for growing season
4. Calculating airtemperature in Fahrenheit and show the formula to teach those who do not know the conversion to Celcius
      " airtemperature_F <-airtemperature*1.8+32, formula for Fahrenheit: Temp(Celcius) * 9/5 + 32, 
        formula for Celcius: Temp(Fahrenheit) * 5/9 - 32"

