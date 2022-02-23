## First, check estimated parameters to identify N-S differences
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')

library(tidyverse)
library(patchwork)

# input <- readr::read_csv(
#   './input.txt',
#   col_names=c('datetime', 'thermocline_depth', 'temperature_epi', 
#               'temperature_hypo', 'temperature_total', 'volume_total', 
#               'volume_epi', 'volume_hypo', 'area_thermocline', 'area_surface', 
#               'upper_meta', 'lower_meta', 'year', 'day_of_year', 'max.d', 'wind', 'airtemp'),
#   col_types=cols(datetime=col_datetime(), year=col_integer(), day_of_year=col_integer(), .default=col_double()))

## load simulated data
allequash = read_csv(paste0('Processed_Output/allequash_fluxes.csv'))
bigmuskellunge = read_csv(paste0('Processed_Output/bigmuskellunge_fluxes.csv'))
crystal = read_csv(paste0('Processed_Output/crystal_fluxes.csv'))
fish = read_csv(paste0('Processed_Output/fish_fluxes.csv'))
mendota = read_csv(paste0('Processed_Output/mendota_fluxes.csv'))
monona = read_csv(paste0('Processed_Output/monona_fluxes.csv'))
sparkling = read_csv(paste0('Processed_Output/sparkling_fluxes.csv'))
trout = read_csv(paste0('Processed_Output/trout_fluxes.csv'))

df.fluxes <- data.frame('NEP_epi' = c(allequash$Fnep, bigmuskellunge$Fnep, crystal$Fnep,
                                      fish$Fnep, mendota$Fnep, monona$Fnep,
                                      sparkling$Fnep, trout$Fnep),
                        'NEP_hypo' = c(allequash$Fmineral, bigmuskellunge$Fmineral, crystal$Fmineral,
                                       fish$Fmineral, mendota$Fmineral, monona$Fmineral,
                                       sparkling$Fmineral, trout$Fmineral),
                        'SED' = (-1) * c(allequash$Fsed, bigmuskellunge$Fsed, crystal$Fsed,
                                       fish$Fsed, mendota$Fsed, monona$Fsed,
                                       sparkling$Fsed, trout$Fsed),
                        'lakeid' = c(rep('AL', nrow(allequash)), rep('BM', nrow(bigmuskellunge)),
                                     rep('CR', nrow(crystal)), rep('FI', nrow(fish)),
                                     rep('ME', nrow(mendota)), rep('MO', nrow(monona)),
                                     rep('SP', nrow(sparkling)), rep('TR', nrow(trout))))

df.fluxes <- reshape2::melt(df.fluxes, id = 'lakeid')
g.param <- ggplot(df.fluxes) +
  geom_boxplot(aes(x = lakeid, y = value)) +
  facet_wrap(~ variable,scales = 'free') +
  xlab('') +
  # ylab('Modeled Fluxes: NEP in mg/m3/d; SED in mg/m2/d')+
  ylab(expression(atop("Estimated flux parameters", paste("NEP in mg/m3/d, SED in mg/m2/d")))) +
  theme_minimal() +theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
                         axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
                         legend.position = 'bottom')

## Check Livingstone for southern lakes
## packages
library(devtools)
library(glmtools) 
library(pracma)
library(lubridate)
library(LakeMetabolizer)
library(zoo)


source('src/functions_driver.R')

df.livingstone = data.frame('year' = NULL, 
                            'depth' = NULL,
                            'jz' = NULL,
                            'alphaz' = NULL,
                            'NEP' = NULL,
                            'SED' = NULL,
                            'SEDmgm3' = NULL,
                            'SED_wo'  = NULL,
                            'id' = NULL)
coeff = data.frame('year' = NULL, 
                   'Jz' = NULL,
                   'Jv' = NULL,
                   'Ja' = NULL,
                   'NEP' = NULL,
                   'SED' = NULL,
                   'SEDmgm3' =NULL,
                   'SED_wo' = NULL,
                   'id' = NULL)

lks <- list.dirs(path = 'Driver_Data/extdata/', full.names = TRUE, recursive = F)

for (ii in lks[1:3]){
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
  
  # proc.obs <- preprocess_obs(obs,input.values = input.values, H, A)
  # w.obs <- weigh_obs(obs,input.values = input.values, H, A)
  # obs_long <- w.obs[[1]]
  # obs_weigh <- w.obs[[2]]
  

    # load observed data
    data_long <- obs %>% arrange(ActivityStartDate)
    # check if obs max depth is > max hypso depth
    if (max(data_long$ActivityDepthHeightMeasure.MeasureValue) > max(H)){
      H <- c(max(data_long$ActivityDepthHeightMeasure.MeasureValue), H)
      A <- c(min(A), A)
    }
    # add area to depth in data_long
    data_long$Area <- approx(H, A, data_long$ActivityDepthHeightMeasure.MeasureValue)$y
    # calculate the q factor for alpha
    if (length(A) <= 2){
      areas <- approx(H, A, seq(max(H), min(A),-1))$y
      depths <- seq(max(H), min(H),-1)
    } else {
      areas = A
      depths = H
    }
    
    
    fit_q <- function(x, areas, depths){
      
      pred_areas <- max(areas) * (1 - depths/max(depths))^x
      fit <- sqrt(sum((areas - pred_areas)^2)/length(areas))
      return(fit)
    }
    # use Brent method to fit q
    q <- optim(par = 0.1, fn = fit_q, areas = areas, depths = depths, method = 'Brent', lower = 0.5, upper = 2.0)
    
    # visual check
    plot(depths, areas)
    lines(depths, max(areas) * (1 - depths/max(depths))^q$par, col = 'red')
    
    idx <- match(zoo::as.Date(data_long$ActivityStartDate), zoo::as.Date(input.values$datetime))
    data_long$Layer <- data_long$ActivityDepthHeightMeasure.MeasureValue <= input.values$td.depth[idx]
    
    data_long$Layer[which(data_long$Layer == TRUE)] = 'EPILIMNION'
    data_long$Layer[which(data_long$Layer == FALSE)] = 'HYPOLIMNION'
    data_long$Layer[which(is.na(data_long$Layer))] = 'TOTAL'
    
    if (ii == lks[1]){
      fluxes <- fish
      lake.id = 'Fish'
    } else if (ii == lks[2]){
      fluxes <- mendota
      lake.id = 'Mendota'
    } else if (ii == lks[3]){
      fluxes <- monona
      lake.id = 'Monona'
    }

    
    for (id.year in unique(year(data_long$ActivityStartDate))){
      sim <- fluxes %>%
        filter(lubridate::year(datetime) == id.year)
      strat.date <- min(sim$datetime[which(sim$stratified == 1)])
      thermocline.z = ceiling(mean(sim$tddepth[which(sim$stratified == 1)])) #+ floor(sd(sim$tddepth[which(sim$stratified == 1)]))
      
      obs <- data_long %>%
        filter(year(ActivityStartDate) == id.year & Layer == 'HYPOLIMNION')

      ggplot(obs %>% filter(ActivityStartDate >=  strat.date)) +
        geom_point(aes(ActivityStartDate, ResultMeasureValue, col = as.factor(ActivityDepthHeightMeasure.MeasureValue))) +
        geom_line(aes(ActivityStartDate, ResultMeasureValue, col = as.factor(ActivityDepthHeightMeasure.MeasureValue)))
      
      for (id.z in unique(floor(unique(obs$ActivityDepthHeightMeasure.MeasureValue)))){
        # print(id.z)
        
        obs.z <- obs %>%
          filter(ActivityDepthHeightMeasure.MeasureValue == id.z)
        
        if (nrow(obs.z) > 1){
        
        id.start <- (which.min(abs(lubridate::yday(obs.z$ActivityStartDate) - lubridate::yday(strat.date))))
        
        if (any(obs.z$ResultMeasureValue <= 2)){
        id.end <- which(obs.z$ResultMeasureValue <=2) [which(which(obs.z$ResultMeasureValue <=2) > id.start)][1]
        
        do.df <- data.frame('time' = lubridate::yday(obs.z$ActivityStartDate[id.start:id.end]) - min(lubridate::yday(obs.z$ActivityStartDate[id.start:id.end]))+1,
                            'o2' = obs.z$ResultMeasureValue[id.start:id.end],
                            'area' = obs.z$Area[id.start:id.end],
                            'realdate' = lubridate::yday(obs.z$ActivityStartDate[id.start:id.end]),
                            'datetime' =obs.z$ActivityStartDate[id.start:id.end])
        
        interp.df <- spline(do.df$time, do.df$o2, n = max(do.df$time))
        id.enddoy <- which(interp.df$y <=2) [which(which(interp.df$y <=2) > id.start)][1]
        
        id.end <-  (do.df$datetime[1]+ 24*3600*id.enddoy-1*24*3600)
        
        j <- (obs.z$ResultMeasureValue[id.start] - interp.df$y[id.enddoy]) / 
          ((do.df$realdate[1]+ id.enddoy-1) - lubridate::yday(obs.z$ActivityStartDate[id.start]))
        
        flag = 0
        
        area.z = approx(depths, areas, id.z)$y
        area.zprior = approx(depths, areas, id.z-1)$y
        dAdz <- (area.zprior - area.z) / (id.z - (id.z-1))
        
        sim$Fsed_corr <- NA
        sim$Fsed_corrmgm3 = NA
        sim$Fsed_corr_wo = NA
        for (p in 2:nrow(sim)){
          sim$Fsed_corr[p] <- sim$Fsed[p]  * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1) -
            sim$Fentr2[p] * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1)  -
            sim$fdiffex_middle[p] * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1)
          sim$Fsed_corrmgm3[p] <- sim$Fsed[p] -
            sim$Fentr2[p]   -
            sim$fdiffex_middle[p]
          sim$Fsed_corr_wo[p] <- sim$Fsed[p]  * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1) 
        }
        
        if (!is.na(id.end)){
          if ( flag == 0){
            NEP <- mean(sim$Fmineral[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(as.Date(id.end),as.Date(sim$datetime))])
            SED <- mean(sim$Fsed_corr[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(as.Date(id.end),as.Date(sim$datetime))])
            SEDmgm3 <- mean(sim$Fsed_corrmgm3[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(as.Date(id.end),as.Date(sim$datetime))])
            SED_wo <- mean(sim$Fsed_corr_wo[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(as.Date(id.end),as.Date(sim$datetime))])
          } else {
            NEP <- mean(sim$Fmineral[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(id.end,as.Date(sim$datetime))])
            SED <- mean(sim$Fsed_corr[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(id.end,as.Date(sim$datetime))])
            SEDmgm3 <- mean(sim$Fsed_corrmgm3[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(id.end,as.Date(sim$datetime))])
            SED_wo <- mean(sim$Fsed_corr_wo[match(as.Date(obs.z$ActivityStartDate[id.start]),as.Date(sim$datetime)) : match(id.end,as.Date(sim$datetime))])
          }
          
        } else {
          NEP = NA
          SED = NA
          SEDmgm3 = NA
          SED_wo = NA
        }
        if (id.z == max(depths)){
          id.z = id.z-0.1
        }
        df.livingstone <- rbind(df.livingstone, 
                                data.frame('year' = id.year,
                                           'depth' =  id.z,
                                           'jz' = j,
                                           'alphaz' = q$par/(max(depths) - id.z), #1/area.z * dAdz 
                                           'NEP' = NEP / 1000,
                                           'SED' = SED / 1000,
                                           'SEDmgm3' = SEDmgm3 / 1000,
                                           'SED_wo' = SED_wo / 1000,
                                           'id' = lake.id))
      }
        }
      }
    }


    

for (ii in na.omit(unique(df.livingstone$year))){
  idx <- which(as.character(df.livingstone$year) %in% ii)
  dat = df.livingstone[na.omit(idx),]
  dat = dat %>% filter(alphaz < 0.4)
  for (jj in unique(df.livingstone$id)){
    dat.id = dat %>% filter(id == jj)
    if(all(is.na(dat.id$jz))){
      
      coeff <- rbind(coeff, 
                     data.frame('year' = ii, 
                                'Jz' = NA,
                                'Jv' = NA,
                                'Ja' = NA,
                                'NEP' = (mean(df.livingstone$NEP[idx], na.rm = T)),
                                'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                'SEDmgm3' =  mean(df.livingstone$SEDmgm3[idx], na.rm = T),
                                'SED_wo' = mean(df.livingstone$SED_wo[idx], na.rm = T),
                                'id' = lake.id))
  
    } else {
      if (length(na.omit((dat.id$jz))) > 1){
        mod <- lm(jz ~ alphaz, dat.id)
        sum.mod <- summary(mod)
        p  <- pf(sum.mod$fstatistic[1], sum.mod$fstatistic[2], sum.mod$fstatistic[3], lower.tail=F)
        if (!is.na(p) && p <= 0.05){
          coeff <- rbind(coeff, 
                         data.frame('year' = ii,
                                    'Jz' = mean(dat.id$jz, na.rm = T),
                                    'Jv' = mod$coefficients[1],
                                    'Ja' = mod$coefficients[2],
                                    'NEP' = (mean(df.livingstone$NEP[idx], na.rm = T)),
                                    'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                    'SEDmgm3' = mean(df.livingstone$SEDmgm3[idx], na.rm = T),
                                    'SED_wo' = mean(df.livingstone$SED_wo[idx], na.rm = T),
                                    'id' = lake.id))
          
        } else {
          coeff <- rbind(coeff, 
                         data.frame('year' = ii, 
                                    'Jz' = mean(dat.id$jz, na.rm = T),
                                    'Jv' = NA,
                                    'Ja' = NA,
                                    'NEP' = (mean(df.livingstone$NEP[idx], na.rm = T)),
                                    'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                    'SEDmgm3' = mean(df.livingstone$SEDmgm3[idx], na.rm = T),
                                    'SED_wo' = mean(df.livingstone$SED_wo[idx], na.rm = T),
                                    'id' = lake.id))
          
        }
      } else {
        coeff <- rbind(coeff, 
                       data.frame('year' = ii, 
                                  'Jz' = mean(dat.id$jz, na.rm = T),
                                  'Jv' = NA,
                                  'Ja' = NA,
                                  'NEP' = (mean(df.livingstone$NEP[idx], na.rm = T)),
                                  'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                  'SEDmgm3' = mean(df.livingstone$SEDmgm3[idx], na.rm = T),
                                  'SED_wo' = mean(df.livingstone$SED_wo[idx], na.rm = T),
                                  'id' = lake.id))
        
      }
      
      

    }
  }
  
  
}
}

ggplot(df.livingstone ) +
  geom_point(aes(alphaz, jz, col = id))+
  facet_wrap(~ year, scales = 'free')

g1 <-ggplot(coeff ) +
  geom_density(aes(Jv, fill = 'Livingstone'), alpha = 0.1) +
  geom_density(aes((-1)*NEP,  fill = 'This study'), alpha = 0.1) +
  ggtitle('Volumetric DO consumption') +
  facet_wrap(~ id, ncol=1, scales = 'free')+
  xlab(expression(atop("Jv (Livingstone) against", paste("average hypo. NEP (g/m3/d)")))) +
  ylab('Density')+
  # xlab('')+
  theme_bw() +
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))

g2 <- ggplot(coeff ) +
  geom_density(aes(Ja, fill = 'Livingstone'), alpha = 0.1) +
  geom_density(aes(SED,  fill = 'This study'), alpha = 0.1) +
  ggtitle('Areal DO consumption') +
  facet_wrap(~ id, ncol = 1, scales = 'free')+
  xlab(expression(atop("Ja (Livingstone) against", paste("average hypo. SED+ENTR+DIFF (g/m2/d)")))) +
  ylab('Density')+
  # xlab('Ja (Livingstone) against average hypolimnetic SED (g/m2/d)')+
  theme_bw()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))

g4 <- ggplot(coeff ) +
  geom_density(aes(Ja, fill = 'Livingstone'), alpha = 0.1) +
  geom_density(aes(SED_wo,  fill = 'This study'), alpha = 0.1) +
  ggtitle('Areal DO consumption') +
  facet_wrap(~ id, ncol = 1, scales = 'free')+
  xlab(expression(atop("Ja (Livingstone) against", paste("average hypo. SED (g/m2/d)")))) +
  ylab('Density')+
  # xlab('Ja (Livingstone) against average hypolimnetic SED (g/m2/d)')+
  theme_bw()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))


g3 <-ggplot(coeff ) +
  geom_density(aes(Jz, fill = 'Livingstone'), alpha = 0.1) +
  geom_density(aes((-1) * NEP + SEDmgm3,  fill = 'This study'), alpha = 0.1) +
  ggtitle('Total average DO consumption') +
  facet_wrap(~ id, ncol =1, scales = 'free')+
  xlab(expression(atop("Jz (Livingstone) against", paste("average hypo. consumption (g/m3/d)")))) +
  ylab('Density')+
  # xlab('average Jz (Livingstone) against average hypolimnetic consumption (g/m3/d)')+
  theme_bw()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))



g.living <- g1 + g4 + g2 + g3 +  plot_layout(nrow = 1, guides = 'collect')
g.living
ggsave(file = paste0('Figures/southernLakes_livingstone.png'), g.living, dpi = 300, width =300, height = 200,
                units='mm')

library(patchwork)

# setwd('/home/robert/Projects/DSI/metabolism_phenology/')


# Package ID: knb-lter-ntl.112.6 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER: Primary Production - Trout Lake Area 1986 - 2007.
# Data set creator:    - Center for Limnology 
# Data set creator:    - NTL LTER 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    - LNO Information Manager LTER Network Office  - 
# Contact:    - NTL LTER Information Manager University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Contact:    - NTL LTER Lead PI Center for Limnology  - leadpi@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.112.6
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/112/6/086d78e2b3455eaab0cb8564ba283cb2" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "sampledate",     
                 "daynum",     
                 "sample_time",     
                 "par",     
                 "pp_epi_hw_m2",     
                 "pp_met_hw_m2",     
                 "pp_hyp_hw_m2",     
                 "pp_epi_hw_m3",     
                 "pp_met_hw_m3",     
                 "pp_hyp_hw_m3",     
                 "pp_epi_nhw_m2",     
                 "pp_met_nhw_m2",     
                 "pp_hyp_nhw_m2",     
                 "pp_epi_nhw_m3",     
                 "pp_met_nhw_m3",     
                 "pp_hyp_nhw_m3",     
                 "flag_par",     
                 "flag_pp_epi_hw_m2",     
                 "flag_pp_met_hw_m2",     
                 "flag_pp_hyp_hw_m2",     
                 "flag_pp_epi_hw_m3",     
                 "flag_pp_met_hw_m3",     
                 "flag_pp_hyp_hw_m3",     
                 "flag_pp_epi_nhw_m2",     
                 "flag_pp_met_nhw_m2",     
                 "flag_pp_hyp_nhw_m2",     
                 "flag_pp_epi_nhw_m3",     
                 "flag_pp_met_nhw_m3",     
                 "flag_pp_hyp_nhw_m3"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)
if (class(dt1$par)=="factor") dt1$par <-as.numeric(levels(dt1$par))[as.integer(dt1$par) ]               
if (class(dt1$par)=="character") dt1$par <-as.numeric(dt1$par)
if (class(dt1$pp_epi_hw_m2)=="factor") dt1$pp_epi_hw_m2 <-as.numeric(levels(dt1$pp_epi_hw_m2))[as.integer(dt1$pp_epi_hw_m2) ]               
if (class(dt1$pp_epi_hw_m2)=="character") dt1$pp_epi_hw_m2 <-as.numeric(dt1$pp_epi_hw_m2)
if (class(dt1$pp_met_hw_m2)=="factor") dt1$pp_met_hw_m2 <-as.numeric(levels(dt1$pp_met_hw_m2))[as.integer(dt1$pp_met_hw_m2) ]               
if (class(dt1$pp_met_hw_m2)=="character") dt1$pp_met_hw_m2 <-as.numeric(dt1$pp_met_hw_m2)
if (class(dt1$pp_hyp_hw_m2)=="factor") dt1$pp_hyp_hw_m2 <-as.numeric(levels(dt1$pp_hyp_hw_m2))[as.integer(dt1$pp_hyp_hw_m2) ]               
if (class(dt1$pp_hyp_hw_m2)=="character") dt1$pp_hyp_hw_m2 <-as.numeric(dt1$pp_hyp_hw_m2)
if (class(dt1$pp_epi_hw_m3)=="factor") dt1$pp_epi_hw_m3 <-as.numeric(levels(dt1$pp_epi_hw_m3))[as.integer(dt1$pp_epi_hw_m3) ]               
if (class(dt1$pp_epi_hw_m3)=="character") dt1$pp_epi_hw_m3 <-as.numeric(dt1$pp_epi_hw_m3)
if (class(dt1$pp_met_hw_m3)=="factor") dt1$pp_met_hw_m3 <-as.numeric(levels(dt1$pp_met_hw_m3))[as.integer(dt1$pp_met_hw_m3) ]               
if (class(dt1$pp_met_hw_m3)=="character") dt1$pp_met_hw_m3 <-as.numeric(dt1$pp_met_hw_m3)
if (class(dt1$pp_hyp_hw_m3)=="factor") dt1$pp_hyp_hw_m3 <-as.numeric(levels(dt1$pp_hyp_hw_m3))[as.integer(dt1$pp_hyp_hw_m3) ]               
if (class(dt1$pp_hyp_hw_m3)=="character") dt1$pp_hyp_hw_m3 <-as.numeric(dt1$pp_hyp_hw_m3)
if (class(dt1$pp_epi_nhw_m2)=="factor") dt1$pp_epi_nhw_m2 <-as.numeric(levels(dt1$pp_epi_nhw_m2))[as.integer(dt1$pp_epi_nhw_m2) ]               
if (class(dt1$pp_epi_nhw_m2)=="character") dt1$pp_epi_nhw_m2 <-as.numeric(dt1$pp_epi_nhw_m2)
if (class(dt1$pp_met_nhw_m2)=="factor") dt1$pp_met_nhw_m2 <-as.numeric(levels(dt1$pp_met_nhw_m2))[as.integer(dt1$pp_met_nhw_m2) ]               
if (class(dt1$pp_met_nhw_m2)=="character") dt1$pp_met_nhw_m2 <-as.numeric(dt1$pp_met_nhw_m2)
if (class(dt1$pp_hyp_nhw_m2)=="factor") dt1$pp_hyp_nhw_m2 <-as.numeric(levels(dt1$pp_hyp_nhw_m2))[as.integer(dt1$pp_hyp_nhw_m2) ]               
if (class(dt1$pp_hyp_nhw_m2)=="character") dt1$pp_hyp_nhw_m2 <-as.numeric(dt1$pp_hyp_nhw_m2)
if (class(dt1$pp_epi_nhw_m3)=="factor") dt1$pp_epi_nhw_m3 <-as.numeric(levels(dt1$pp_epi_nhw_m3))[as.integer(dt1$pp_epi_nhw_m3) ]               
if (class(dt1$pp_epi_nhw_m3)=="character") dt1$pp_epi_nhw_m3 <-as.numeric(dt1$pp_epi_nhw_m3)
if (class(dt1$pp_met_nhw_m3)=="factor") dt1$pp_met_nhw_m3 <-as.numeric(levels(dt1$pp_met_nhw_m3))[as.integer(dt1$pp_met_nhw_m3) ]               
if (class(dt1$pp_met_nhw_m3)=="character") dt1$pp_met_nhw_m3 <-as.numeric(dt1$pp_met_nhw_m3)
if (class(dt1$pp_hyp_nhw_m3)=="factor") dt1$pp_hyp_nhw_m3 <-as.numeric(levels(dt1$pp_hyp_nhw_m3))[as.integer(dt1$pp_hyp_nhw_m3) ]               
if (class(dt1$pp_hyp_nhw_m3)=="character") dt1$pp_hyp_nhw_m3 <-as.numeric(dt1$pp_hyp_nhw_m3)
if (class(dt1$flag_par)!="factor") dt1$flag_par<- as.factor(dt1$flag_par)
if (class(dt1$flag_pp_epi_hw_m2)!="factor") dt1$flag_pp_epi_hw_m2<- as.factor(dt1$flag_pp_epi_hw_m2)
if (class(dt1$flag_pp_met_hw_m2)!="factor") dt1$flag_pp_met_hw_m2<- as.factor(dt1$flag_pp_met_hw_m2)
if (class(dt1$flag_pp_hyp_hw_m2)!="factor") dt1$flag_pp_hyp_hw_m2<- as.factor(dt1$flag_pp_hyp_hw_m2)
if (class(dt1$flag_pp_epi_hw_m3)!="factor") dt1$flag_pp_epi_hw_m3<- as.factor(dt1$flag_pp_epi_hw_m3)
if (class(dt1$flag_pp_met_hw_m3)!="factor") dt1$flag_pp_met_hw_m3<- as.factor(dt1$flag_pp_met_hw_m3)
if (class(dt1$flag_pp_hyp_hw_m3)!="factor") dt1$flag_pp_hyp_hw_m3<- as.factor(dt1$flag_pp_hyp_hw_m3)
if (class(dt1$flag_pp_epi_nhw_m2)!="factor") dt1$flag_pp_epi_nhw_m2<- as.factor(dt1$flag_pp_epi_nhw_m2)
if (class(dt1$flag_pp_met_nhw_m2)!="factor") dt1$flag_pp_met_nhw_m2<- as.factor(dt1$flag_pp_met_nhw_m2)
if (class(dt1$flag_pp_hyp_nhw_m2)!="factor") dt1$flag_pp_hyp_nhw_m2<- as.factor(dt1$flag_pp_hyp_nhw_m2)
if (class(dt1$flag_pp_epi_nhw_m3)!="factor") dt1$flag_pp_epi_nhw_m3<- as.factor(dt1$flag_pp_epi_nhw_m3)
if (class(dt1$flag_pp_met_nhw_m3)!="factor") dt1$flag_pp_met_nhw_m3<- as.factor(dt1$flag_pp_met_nhw_m3)
if (class(dt1$flag_pp_hyp_nhw_m3)!="factor") dt1$flag_pp_hyp_nhw_m3<- as.factor(dt1$flag_pp_hyp_nhw_m3)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(sampledate)
summary(daynum)
summary(sample_time)
summary(par)
summary(pp_epi_hw_m2)
summary(pp_met_hw_m2)
summary(pp_hyp_hw_m2)
summary(pp_epi_hw_m3)
summary(pp_met_hw_m3)
summary(pp_hyp_hw_m3)
summary(pp_epi_nhw_m2)
summary(pp_met_nhw_m2)
summary(pp_hyp_nhw_m2)
summary(pp_epi_nhw_m3)
summary(pp_met_nhw_m3)
summary(pp_hyp_nhw_m3)
summary(flag_par)
summary(flag_pp_epi_hw_m2)
summary(flag_pp_met_hw_m2)
summary(flag_pp_hyp_hw_m2)
summary(flag_pp_epi_hw_m3)
summary(flag_pp_met_hw_m3)
summary(flag_pp_hyp_hw_m3)
summary(flag_pp_epi_nhw_m2)
summary(flag_pp_met_nhw_m2)
summary(flag_pp_hyp_nhw_m2)
summary(flag_pp_epi_nhw_m3)
summary(flag_pp_met_nhw_m3)
summary(flag_pp_hyp_nhw_m3) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$flag_par)) 
summary(as.factor(dt1$flag_pp_epi_hw_m2)) 
summary(as.factor(dt1$flag_pp_met_hw_m2)) 
summary(as.factor(dt1$flag_pp_hyp_hw_m2)) 
summary(as.factor(dt1$flag_pp_epi_hw_m3)) 
summary(as.factor(dt1$flag_pp_met_hw_m3)) 
summary(as.factor(dt1$flag_pp_hyp_hw_m3)) 
summary(as.factor(dt1$flag_pp_epi_nhw_m2)) 
summary(as.factor(dt1$flag_pp_met_nhw_m2)) 
summary(as.factor(dt1$flag_pp_hyp_nhw_m2)) 
summary(as.factor(dt1$flag_pp_epi_nhw_m3)) 
summary(as.factor(dt1$flag_pp_met_nhw_m3)) 
summary(as.factor(dt1$flag_pp_hyp_nhw_m3))
detach(dt1)               

library(naniar)
library(tidyverse)
library(patchwork)
library(ggplot2)

str(dt1)

## replace -999 with NaN
df <- dt1 %>%
  replace_with_na(replace = list(pp_epi_hw_m3 = -999, pp_met_hw_m3 = -999, pp_hyp_hw_m3 = -999))

## load simulated data
cr.df = read_csv(paste0('Processed_Output/crystal_fluxes.csv'))
sp.df = read_csv(paste0('Processed_Output/sparkling_fluxes.csv'))
tr.df = read_csv(paste0('Processed_Output/trout_fluxes.csv'))

model.df = data.frame('NEP_epi' = c(cr.df$fnep_middle, sp.df$fnep_middle, tr.df$fnep_middle),
                      'NEP_hypo' = c(cr.df$fmineral_middle, sp.df$fmineral_middle, tr.df$fmineral_middle),
                      'lakeid' = c(rep('CR', nrow(cr.df)), rep('SP', nrow(sp.df)), rep('TR', nrow(tr.df))),
                      'datetime' = as.Date(c(cr.df$datetime, sp.df$datetime, tr.df$datetime)),
                      'Total_NEP_epi' = c(cr.df$fnep_middle, sp.df$fnep_middle, tr.df$fnep_middle),
                      'Total_NEP_hypo' = c(cr.df$fmineral_middle-cr.df$fsed2_middle, sp.df$fmineral_middle-sp.df$fsed2_middle, tr.df$fmineral_middle-tr.df$fsed2_middle))

g1 <- ggplot(df) +
  geom_point(aes(sampledate, pp_epi_hw_m3 * 44/12 * 32/44 * 1/1000 * 24, col = '14C')) +
  geom_line(data= model.df, aes(datetime, NEP_epi/1000, col = 'This study')) +
  # geom_point(aes(sampledate, pp_met_hw_m3, col = 'meta')) + 
  # geom_point(aes(sampledate, pp_hyp_hw_m3, col = 'hypo')) + 
  ylab('Production in g O2 per m3 per day') + 
  ylim(0,1) +
  ggtitle('Epilimnion')+
  facet_wrap(~lakeid, ncol=1)+ theme_minimal()

g2 <- ggplot(df) +
  # geom_point(aes(sampledate, pp_epi_hw_m3, col = 'epi')) +
  geom_point(aes(sampledate, pp_met_hw_m3  * 44/12 * 32/44  * 1/1000 * 24, col = '14C')) +
  # geom_line(data= model.df, aes(datetime, NEP_epi, col = 'PB')) +
  # geom_point(aes(sampledate, pp_hyp_hw_m3, col = 'hypo')) + 
  ylab('Production in g O2 per m3 per day') +
  ggtitle('Metalimnion')+
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1)+ theme_minimal()

g3 <- ggplot(df) +
  # geom_point(aes(sampledate, pp_epi_hw_m3, col = 'epi')) +
  # geom_point(aes(sampledate, pp_met_hw_m3, col = 'meta')) +
  geom_point(aes(sampledate, pp_hyp_hw_m3  * 44/12 * 32/44  * 1/1000 * 24, col = '14C')) +
  geom_line(data= model.df, aes(datetime, NEP_hypo/1000, col = 'This study')) +
  ylab('Production in g O2 per m3 per day') +
  ggtitle('Hypolimnion')+
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1)+ theme_minimal()


g4 <- ggplot(df) +
  # geom_point(aes(sampledate, pp_epi_hw_m3, col = 'epi')) +
  # geom_point(aes(sampledate, pp_met_hw_m3, col = 'meta')) +
  geom_point(aes(sampledate, (pp_hyp_hw_m3 + pp_met_hw_m3)  * 44/12 * 32/44  * 1/1000 * 24, col = '14C')) +
  geom_line(data= model.df, aes(datetime, NEP_hypo / 1000, col = 'This study')) +
  ylab('Production in g O2 per m3 per day') +
  ggtitle('Metalimnon + Hypolimnion')+
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1) + theme_minimal()

g <-g1 + g3 +  g4 +  plot_layout(nrow = 1, guides = 'collect'); g
ggsave(file = paste0('Figures/northernLakes_C14.png'), g, dpi = 300, width =300, height = 200,
       units='mm')


p1 <- ggplot(df) +
  geom_boxplot(aes(x = '14C', pp_epi_hw_m3 * 44/12 * 32/44 * 1/1000 * 24*1.25, fill = '14C'), alpha = 0.1) +
  geom_boxplot(data = model.df, aes(x = 'This study', NEP_epi/1000,  fill = 'This study'), alpha = 0.1) +
  ylab('Production in g O2 per m3 per day') +xlab('')+
  ggtitle('Epilimnion')+
  ylim(0,1) +
  facet_wrap(~lakeid, ncol=1, scales = 'free')+ theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))

p2 <- ggplot(df) +
  geom_boxplot(aes(x = '14C', pp_hyp_hw_m3  * 44/12 * 32/44  * 1/1000 * 24 *1.25, fill = '14C'), alpha = 0.1) +
  geom_boxplot(data = model.df, aes(x = 'This study', NEP_hypo/1000,  fill = 'This study'), alpha = 0.1) +
  ylab('Production in g O2 per m3 per day') +xlab('')+
  ggtitle('Hypolimnion')+
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1, scales = 'free')+ theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))

p3 <- ggplot(df) +
  geom_boxplot(aes(x = '14C', (pp_hyp_hw_m3 + pp_met_hw_m3)  * 44/12 * 32/44  * 1/1000 * 24 * 1.25, fill = '14C'), alpha = 0.1) +
  geom_boxplot(data = model.df, aes(x = 'This study', NEP_hypo/1000,  fill = 'This study'), alpha = 0.1) +
  ylab('Production in g O2 per m3 per day') + xlab('')+ylim(0,1) +
  ggtitle('Metalimnion + Hypolimnion')+
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1, scales = 'free')+ theme_minimal() + 
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))

p4 <- ggplot(df) +
  geom_boxplot(aes(x = '14C', pp_hyp_hw_m3  * 44/12 * 32/44  * 1/1000 * 24 *1.27, fill = '14C'), alpha = 0.1) +
  geom_boxplot(data = model.df, aes(x = 'This study', Total_NEP_hypo/1000,  fill = 'This study'), alpha = 0.1) +
  ylab('Production in g O2 per m3 per day') +xlab('')+
  ggtitle('Hypolimnion')+
  ylim(-0.1, 0.5) +
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1, scales = 'free')+ theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))


p4p5 <- ggplot(df) +
  geom_boxplot(aes(x = '14C', (pp_hyp_hw_m3 + pp_met_hw_m3)  * 44/12 * 32/44  * 1/1000 * 24 * 1.27, fill = '14C'), alpha = 0.1) +
  geom_boxplot(data = model.df, aes(x = 'This study', Total_NEP_hypo/1000,  fill = 'This study'), alpha = 0.1) +
  ylab('Production in g O2 per m3 per day') + xlab('')+ylim(0,1) +
  ggtitle('Metalimnion + Hypolimnion')+
  # ylim(0,130) +
  facet_wrap(~lakeid, ncol=1, scales = 'free')+ theme_minimal() + 
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20))


p <- p1 + p2 + p3 + plot_layout(nrow = 1, guides = 'collect'); p
ggsave(file = paste0('Figures/northernLakes_C14_boxplots.png'), p, dpi = 300, width =300, height = 200,
       units='mm')



v <- g.param / g.living / p + plot_annotation(tag_levels = 'A') ;v

ggsave(file = 'Figures/Fig_8.png', v, dpi = 600, width = 27, height = 20.5, units='in')
ggsave(file = 'Figures/Fig_8.pdf', v, dpi = 600, width = 27, height = 20.5, units='in')
