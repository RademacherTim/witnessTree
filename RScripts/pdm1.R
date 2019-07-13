## PDM-1 Point Dendrometer functions
# D.Basler 2019
#

library (dplyr)
#
#EXAMPLE:
#'@examples
#'\dontrun{
#'  #include this file 
#'  source("./pdm1.R",local=T) # provide path to file
#'  # Define path for calibration files
#'  pdm_calibration_path<-"~/dendrometer_calibration/cal"
#'  # import calibrations
#'  cal_data<-get_calibration_data(pdm_calibration_path)
#'  # Define the excitation voltage used
#'  v_excitation<-2 # excitation Voltage used [V]
#'  # Create some exemplary data
#'  logdata<-(data.frame(time=rep(1:10,2),sensor_id=rep(c(1,2),each=10), voltage= c((c(1:10)/12+runif(10)/10)*v_excitation,(c(1:10)/12+runif(10)/10)*v_excitation)))
#'  # Add calulated values to the table
#'  logdata<-cbind(logdata,voltage_to_position(logdata$sensor_id,logdata$voltage,v_excitation,cal_data))
#'  # The 'voltage_to_position' function returns
#'  #  resistance: calculated resistance
#'  #  pos_lm:   Absolute position [mm] calculated using a simple linear interpolation
#'  #  pos_poly: Absolute position [mm] calculated using the non-lonearity adjustmet from clalibration
#'  #  max_pos:  maximal distance the sensor is able to measure (con be used to see if sensor needs repositioning soon)
#'}



read_calfile<-function(filename){
  ####
  # read and parse a single calibration file
  ####
  conn <- file(filename, open="r")
  linn <-readLines(conn)
  close(conn)
  sensor <- strsplit(linn[[1]],'\t')[[1]][2]
  if (sensor=='PDM-1'){
    id <- strsplit(linn[[2]],'\t')[[1]][2]
    resistance <- strsplit(linn[[4]],'\t')[[1]][2]
    travel <- strsplit(linn[[5]],'\t')[[1]][2]
    lin<-strsplit(linn[[6]],'\t')[[1]][c(2,3)]
    poly<-strsplit(linn[[7]],'\t')[[1]][c(2:11)]
    cal_data<-c(sensor,id,resistance,travel,lin,poly)
    return(cal_data)
  }else { stop ('unkonwn sensor type')}
  
}

get_calibration_data<-function(pdm_calibration_path){
  ####
  # read all calibration files from folder and prepare a data.frame
  ####
  
  # get calibration file names
  cal_files<-list.files(pdm_calibration_path,  pattern = '*.cal')
  if (length(cal_files)==0) stop('No claibration files found. Check path')
  # read all .cal files and make a data.frame
  cal_data<-lapply(file.path(pdm_calibration_path,cal_files),read_calfile)
  cal_data<-do.call(rbind.data.frame, cal_data)
  names(cal_data)<-c('sensor_type', 'sensor_id', 'resistance', 'travel', 'lin_a', 'lin_b', 'p1','p2','p3','p4','p5','p6','p7','p8','p9','p10')
  cal_data[,1]<-as.character(cal_data[,1])
  cal_data[,c(2:16)] <- apply(cal_data[,c(2:16)], 2, function(x) as.numeric(as.character(x)));
  cal_data<-cal_data[!is.na(cal_data$sensor_id),] # remove invalid sensor ids
  print (sprintf('imported %i calibration files',nrow(cal_data)))
  return (cal_data)
  }


polynomial <- function(a,x){
  ####
  # calulate a polynom using coefficients a (highest power last)
  ####
  if (length(x)>1){
    rowSums(sapply(seq_along(a), function(ii) a[ii] * (x ) ^ (ii - 1L)),dims=1)
  }else{
    sum(sapply(seq_along(a), function(ii) a[ii] * (x ) ^ (ii - 1L)))
  }
}

voltage_to_position<-function(sensor_id,v_signal,v_excitation,cal_data){
  ###
  # Caluclates the absolute distance from logged voltages using the parameters from sensor calibration
  ### 
  #prepare calibration parameters for selected sensor_id 
  calibration_parameters<-merge(data.frame(sensor_id=sensor_id),cal_data,by.x='sensor_id', all.x = TRUE)
  # check if calibration is available
  if(NA %in% calibration_parameters$resistance){
    warning(sprintf('No calibration data available for sensor(s) [%s]. Using default values', paste(calibration_parameters$sensor_id[is.na(calibration_parameters$resistance)],collapse=', ') ))
    ## #assign default values
    default_values<-c(10,11,1.1,rep(0,9),1.1,0)
    calibration_parameters[is.na(calibration_parameters$resistance),3:16]<- rep(default_values,each=sum(is.na(calibration_parameters$resistance)))
  }
  # calculate Resistance from logged voltage
  # The potentimeter is a volatge divider so Vout=Vin*(R1/(R1+R2)) with R1+R2=Rtot
  r_tot<-calibration_parameters$resistance # [kOhm]

  # if hooked up correctly: -> retraction  GND (yellow) --R2---  SIGNAL(red) ---R1---VEXT (green)
  # calibration refers to R2
  r_signal<- r_tot*(v_signal/v_excitation)
  
  # Convert resistance to position
  pos_lm<-r_signal*calibration_parameters$lin_a    # [mm]
  #Account for non-linearity of the sensor
  a<-calibration_parameters[,16:7]#Polynomial coefficients,reversed as .cal file is stored highest power first
  pos_poly<-rep(NA,nrow(a))
  for (i in c(1:nrow(a))) {pos_poly[i]<-polynomial(as.numeric(a[i,]),r_signal[i])}
  #TO DO  : Temperature compensation
  
  # Add maximum travel distance
  travel<-calibration_parameters$travel    # [mm]
  return(data.frame(resistance=r_signal, pos_lm,pos_poly,max_pos=travel))
}
