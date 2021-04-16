rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## packages
library(devtools)
library(glmtools) 
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(zoo)


source('functions_driver.R')

# Words of caution: This script assumes that in 'Driver_Data/extdata' multiple folders for each 
# lake site exist that are filled with 
# i) hypsographic information about the lake (provided by the GLM configuration file, i.e., *.nml)
# ii) simulated water temperature data on a evenly space time step with an even grid over the
# vertical axis, i.e., from GLM3 (Read, J.S., Jia, X., Willard, J., Appling, A.P., Zwart, J.A., 
# Oliver, S.K., Karpatne, A., Hansen, G.J.A., Hanson,655P.C., Watkins, W., Steinbach, M., Kumar, V., 
# 2019. Process-Guided Deep Learning Predictions of Lake WaterTemperature.  Water Resources Research 
# 55, 9173–9190.  URL:https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR024922, 
# doi:https://doi.org/10.1029/2019WR024922)
# iii) long-term dissolved oxygen data, i.e., from NTL-LTER (Magnuson,   J.,   Carpenter,   S.,   
# Stanley,   E.,   2020b.North   Temperate   Lakes   LTER:   Physical   Limnology   of   Primary   
# Study   Lakes   1981   -   current   ver   27.
# URL:https://doi.org/10.6073/pasta/c120b223f80c63982457a2e1e76f6038)
# iv) evenly spaced meteorological data of air temperature and wind speed, i.e., from NLDS-2 (Xia, 
# Y., Mitchell, K., Ek, M., Sheffield, J., Cosgrove, B., Wood, E., Luo, L., Alonge, C., Wei, H., 
# Meng, J.,Livneh, B., Lettenmaier, D., Koren, V., Duan, Q., Mo, K., Fan, Y., Mocko, D., 2012. 
# Continental-scale waterand  energy  flux  analysis  and  validation  for  the  North  American  
# Land  Data  Assimilation  System  projectphase 2 (NLDAS-2):  1. Intercomparison and application 
# of model products:  WATER AND ENERGY FLUXANALYSIS.  Journal of Geophysical Research:  Atmospheres 
# 117, n/a–n/a.  URL:http://doi.wiley.com/72010.1029/2011JD016048, doi:10.1029/2011JD016048.)

lks <- list.dirs(path = '../Driver_Data/extdata/', full.names = TRUE, recursive = F)

for (ii in lks){
  print(paste0('Running ',ii))
  data <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'pball', include.dirs = T)))
  meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'NLDAS', include.dirs = T)))
  
  chidx <- match(as.POSIXct(data$date),as.POSIXct(meteo$time))
  wind <- meteo$WindSpeed[chidx]
  airtemp <- meteo$AirTemp[chidx]
  
  if (length( list.files(ii, pattern = 'wq_data', include.dirs = T)) > 0){
    wq_data<- paste0(ii,'/', list.files(ii, pattern = 'wq_data', include.dirs = T))
    obs <- NULL
    
    for(jj in wq_data){
      raw_obs <- read.csv(jj)
      if ('ActivityDepthHeighMeasure.MeasureValue' %in% colnames(raw_obs)){
        wq <- raw_obs %>%
          dplyr::filter(CharacteristicName== "Dissolved oxygen (DO)") %>%
          dplyr::select(c('ActivityStartDate', 'ActivityDepthHeighMeasure.MeasureValue', 'ResultMeasureValue'))
        wq <- rename(wq, 'ActivityDepthHeightMeasure.MeasureValue' = 'ActivityDepthHeighMeasure.MeasureValue')
      } else {
        wq <- raw_obs %>%
          dplyr::filter(CharacteristicName== "Dissolved oxygen (DO)") %>%
          dplyr::select(c('ActivityStartDate', 'ActivityDepthHeightMeasure.MeasureValue', 'ResultMeasureValue'))
      }
      
      if (length(wq_data) == 1 | is.null(obs)){
        obs <- wq
      } else {
        obs <- rbind(obs, wq)
      }
    }
    obs$ActivityStartDate<-as.POSIXct(obs$ActivityStartDate)
  }
  
  if (is.factor(obs$ActivityDepthHeightMeasure.MeasureValue)){
    obs$ActivityDepthHeightMeasure.MeasureValue <-  as.numeric(as.character(obs$ActivityDepthHeightMeasure.MeasureValue))
  }
  if (is.factor(obs$ResultMeasureValue)){
    obs$ResultMeasureValue <-  as.numeric(as.character(obs$ResultMeasureValue))
  }
  
  # outlier detection
  outlier_values <- boxplot.stats(obs$ResultMeasureValue)$out 
  uvx <- match(outlier_values, obs$ResultMeasureValue)
  obs$ResultMeasureValue[uvx] <- NA
  
  eg_nml <- read_nml(paste0(ii,'/', list.files(ii, pattern = 'nml', include.dirs = T)))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
  A <- eg_nml$morphometry$A
  
  input.values <- input(wtemp = data, H = H, A = A)
  
  input.values$year <- year(input.values$datetime)
  input.values$doy <- yday(input.values$datetime)
  input.values$max.d <- max(H)
  
  input.values$wind = wind
  input.values$airtemp = airtemp
  write.table(input.values, paste0('../Driver_Data/Processed/input_',sub("\\).*", "", sub(".*\\(", "", ii)) ,'.txt'), append = FALSE, sep = ",", dec = ".",
              row.names = FALSE, col.names = FALSE)
  
  # proc.obs <- preprocess_obs(obs,input.values = input.values, H, A)
  w.obs <- weigh_obs(obs,input.values = input.values, H, A)
  obs_long <- w.obs[[1]]
  obs_weigh <- w.obs[[2]]
  
  write.table(obs_weigh,paste0('../Driver_Data/Processed/oxy_observed_',sub("\\).*", "", sub(".*\\(", "", ii)) ,'.txt'), append = FALSE, sep = " ", dec = ".",
              row.names = FALSE, col.names = FALSE)
  
}



