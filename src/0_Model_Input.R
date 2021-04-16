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



