#### Verify hypolimentic metabolic fluxes ####
library(tidyverse)
library(patchwork)
library(pracma)
library(glmtools)

custom.theme = theme_minimal() + 
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 11), axis.text.x= element_text(size = 18), plot.title = element_text(size = 18),
        axis.text.y= element_text(size = 18), text = element_text(size = 18), legend.title = element_blank(), strip.text =element_text(size = 18),
        legend.position = 'bottom', 
        legend.margin=margin(t = -0.3, unit='cm'),
        plot.margin = unit(c(0,0.2,0,0.2), "cm"))

# load NTL-LTER field data
# Package ID: knb-lter-ntl.29.8 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:    - Center for Limnology 
# Data set creator:    - NTL LTER 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - NTL LTER Information Manager University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Contact:    - NTL LTER Lead PI Center for Limnology  - leadpi@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.29.8
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/8/1932bb71889c8e25cb216c8dc0db33d5" 
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
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)!="factor") dt1$frlight<- as.factor(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Convert Missing Values to NA for non-dates
# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
detach(dt1)               

check_lakes <- data.frame('flux.name' = c('allequash',
                                          'bigmuskellunge',
                                          'crystal',
                                          'fish',
                                          'mendota',
                                          'monona',
                                          'sparkling',
                                          'trout'), 'folder.name' = c('Allequash',
                                                                      'BigMuskellunge',
                                                                      'Crystal',
                                                                      'Fish',
                                                                      'Mendota',
                                                                      'Monona',
                                                                      'Sparkling',
                                                                      'Trout'), 'short.name' = c('AL',
                                                                                                 'BM',
                                                                                                 'CR',
                                                                                                 'FI',
                                                                                                 'ME',
                                                                                                 'MO',
                                                                                                 'SP',
                                                                                                 'TR'))

df.livingstone = data.frame('year' = NA, 
                            'depth' = NA,
                            'jz' = NA,
                            'alphaz' = NA,
                            'NEP' = NA,
                            'SED' = NA,
                            'id' = NA)
coeff = data.frame('year' = NA, 
                   'Jv' = NA,
                   'Ja' = NA,
                   'NEP' = NA,
                   'SED' = NA,
                   'id' = NA)
for (loop in 1:nrow(check_lakes)){
  
  fluxes = read_csv(paste0('Processed_Output/',check_lakes$flux.name[loop],'_fluxes.csv'))
  hypsography = read_nml(paste0(check_lakes$folder.name[loop],'/config.nml'))
  areas <- hypsography$morphometry$A
  depths <- max(hypsography$morphometry$H) - hypsography$morphometry$H
  years <- unique(lubridate::year(fluxes$datetime))
  
  
  if (length(areas) <= 2){
    areas <- approx(depths, areas, seq(max(depths), min(depths),-1))$y
    depths <- seq(max(depths), min(depths),-1)
  }
  
  fit_q <- function(x, areas, depths){

    pred_areas <- max(areas) * (1 - depths/max(depths))^x
    fit <- sqrt(sum((areas - pred_areas)^2)/length(areas))
    return(fit)
  }
  
  q <- optim(par = 0.1, fn = fit_q, areas = areas, depths = depths, method = 'Brent', lower = 0.5, upper = 2.0)
  
  plot(depths, areas)
  points(depths, max(areas) * (1 - depths/max(depths))^q$par, col = 'red')
  
  lake.id = check_lakes$short.name[loop]

  for (id.year in years[which(years %in% dt1$year4)]){
    sim <- fluxes %>%
      filter(lubridate::year(datetime) == id.year)
    strat.date <- min(sim$datetime[which(sim$stratified == 1)])
    thermocline.z = ceiling(mean(sim$tddepth[which(sim$stratified == 1)])) #+ floor(sd(sim$tddepth[which(sim$stratified == 1)]))
    
    obs <- dt1 %>%
      filter(lakeid == lake.id & year4 == id.year & depth >= floor(thermocline.z))
    
    ggplot(obs %>% filter(sampledate >=  strat.date)) +
      geom_point(aes(sampledate, o2, col = as.factor(depth))) +
      geom_line(aes(sampledate, o2, col = as.factor(depth)))
    
    for (id.z in unique(floor(unique(obs$depth)))){
      # print(id.z)
      
      obs.z <- obs %>%
        filter(depth == id.z)
      
      id.start <- (which.min(abs(lubridate::yday(obs.z$sampledate) - lubridate::yday(strat.date))))
      id.end <- which(obs.z$o2 <=2) [which(which(obs.z$o2 <=2) > id.start)][1]
      
      j <- (obs.z$o2[id.start] - obs.z$o2[id.end]) / 
        (lubridate::yday(obs.z$sampledate[id.end]) - lubridate::yday(obs.z$sampledate[id.start]))
      
      flag = 0
      
      # if (!is.na(id.end)){
      #   if (length(obs.z$o2[id.start:id.end][-which(is.na(obs.z$o2[id.start:id.end]))]) > 2){
      #     interp.seq <- seq(lubridate::yday(obs.z$sampledate[id.start]),
      #                       lubridate::yday(obs.z$sampledate[id.end]),1)
      #     interp.dates <- seq((obs.z$sampledate[id.start]),
      #                       (obs.z$sampledate[id.end]),1)
      #     interp.do <- interp1(lubridate::yday(obs.z$sampledate[id.start:id.end][-which(is.na(obs.z$o2[id.start:id.end]))]), obs.z$o2[id.start:id.end][-which(is.na(obs.z$o2[id.start:id.end]))], 
      #                          interp.seq, method = "spline")
      #     
      #     crit.do <- interp.do[which(interp.do <=2)][1]
      #     
      #     id.end <- interp.dates[which(interp.do <=2)][1]
      #     
      #     j <- (obs.z$o2[id.start] - crit.do) / 
      #       (lubridate::yday(id.end) - lubridate::yday(obs.z$sampledate[id.start]))
      #     
      #     flag = 1 
      #   }
      # 
      # }

      
      
      
      # print((obs.z$o2[id.start] - obs.z$o2[id.end]))
      # print( (lubridate::yday(obs.z$sampledate[id.end]) - lubridate::yday(obs.z$sampledate[id.start])))
      
      # print(paste(id.z, round((obs.z$o2[id.start] - obs.z$o2[id.end]),1), 
      #             round((lubridate::yday(obs.z$sampledate[id.end]) - lubridate::yday(obs.z$sampledate[id.start])),1)))
      
      area.z = approx(depths, areas, id.z)$y
      area.zprior = approx(depths, areas, id.z-1)$y
      dAdz <- (area.zprior - area.z) / (id.z - (id.z-1))
      
      sim$Fsed_corr <- NA
      for (p in 2:nrow(sim)){
        sim$Fsed_corr[p] <- sim$Fsed[p]  * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1)  
      }
      
      if (!is.na(id.end)){
        if ( flag == 0){
          NEP <- mean(sim$Fmineral[match(obs.z$sampledate[id.start],as.Date(sim$datetime)) : match(obs.z$sampledate[id.end],as.Date(sim$datetime))])
          SED <- mean(sim$Fsed_corr[match(obs.z$sampledate[id.start],as.Date(sim$datetime)) : match(obs.z$sampledate[id.end],as.Date(sim$datetime))])
          
        } else {
          NEP <- mean(sim$Fmineral[match(obs.z$sampledate[id.start],as.Date(sim$datetime)) : match(id.end,as.Date(sim$datetime))])
          SED <- mean(sim$Fsed_corr[match(obs.z$sampledate[id.start],as.Date(sim$datetime)) : match(id.end,as.Date(sim$datetime))])
          
        }
       
      } else {
        NEP = NA
        SED = NA
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
                                         'id' = lake.id))
    }
    
  }
  
  
  library(ggpmisc)
  my.formula <- y ~ x
  
  for (idxx in na.omit(unique(df.livingstone$id))){
    g1 <- ggplot(df.livingstone %>% filter(id == idxx), aes(alphaz, jz))  +
      geom_point(aes(alphaz, jz)) +
      geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
      stat_poly_eq(formula = my.formula,
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                   parse = TRUE,size = rel(4.5),
                   label.y = 0.05,
                   label.x = 0.1) +
      facet_wrap(~year,scales = "free") +
      theme_bw() +
      labs(y= expression("Diss. oxygen depletion rate J(z) [g"*~m^{-3}*""*~d^{-1}*"]"),
           x= expression("\u03b1(z) ["~m^{2}*""*~m^{-3}*"]"))+
      theme(text = element_text(size=20),
            legend.position = "none",
            axis.text.x = element_text(angle=0, hjust=1));g1
    ggsave(file = paste0('Figures/',idxx,'_livingstone.png'), g1, dpi = 300, width =500, height = 400,
           units='mm')
  }
  
  
  # coeff <- matrix(NA, nrow = na.omit(length(unique(df.livingstone$year))), ncol = 6)
  
  
  for (ii in na.omit(unique(df.livingstone$year))){
    idx <- which(as.character(df.livingstone$year) %in% ii)
    dat = df.livingstone[na.omit(idx),]
    for (jj in unique(check_lakes$short.name)){
      dat.id = dat %>% filter(id == jj)
      if(all(is.na(dat.id$jz))){
        
        coeff <- rbind(coeff, 
                       data.frame('year' = ii, 
                                  'Jv' = NA,
                                  'Ja' = NA,
                                  'NEP' = abs(mean(df.livingstone$NEP[idx], na.rm = T)),
                                  'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                  'id' = lake.id))
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),1] <- ii
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),2] <- NA
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),3] <- NA
        # 
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),4] <- abs(mean(df.livingstone$NEP[idx], na.rm = T))
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),5] <- mean(df.livingstone$SED[idx], na.rm = T)
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),6] <- lake.id
        
      } else {
        if (length(na.omit((dat.id$jz))) > 1){
          mod <- lm(jz ~ alphaz, dat.id)
          sum.mod <- summary(mod)
          p  <- pf(sum.mod$fstatistic[1], sum.mod$fstatistic[2], sum.mod$fstatistic[3], lower.tail=F)
          if (!is.na(p) && p <= 0.05){
            coeff <- rbind(coeff, 
                           data.frame('year' = ii, 
                                      'Jv' = mod$coefficients[1],
                                      'Ja' = mod$coefficients[2],
                                      'NEP' = abs(mean(df.livingstone$NEP[idx], na.rm = T)),
                                      'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                      'id' = lake.id))
          } else {
            coeff <- rbind(coeff, 
                           data.frame('year' = ii, 
                                      'Jv' = NA,
                                      'Ja' = NA,
                                      'NEP' = abs(mean(df.livingstone$NEP[idx], na.rm = T)),
                                      'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                      'id' = lake.id))
          }
        } else {
          coeff <- rbind(coeff, 
                         data.frame('year' = ii, 
                                    'Jv' = NA,
                                    'Ja' = NA,
                                    'NEP' = abs(mean(df.livingstone$NEP[idx], na.rm = T)),
                                    'SED' = mean(df.livingstone$SED[idx], na.rm = T),
                                    'id' = lake.id))
        }
        
        
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),1] <- ii
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),2] <- mod$coefficients[1]
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),3] <- mod$coefficients[2]
        # 
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),4] <- abs(mean(df.livingstone$NEP[idx], na.rm = T))
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),5] <- mean(df.livingstone$SED[idx], na.rm = T)
        # coeff[which(unique(as.character(df.livingstone$year)) %in% ii),6] <- lake.id
      }
    }
    

  }
}
  # coeff <- as.data.frame(coeff)
  # colnames(coeff) = c('year', 'Jv', 'Ja', 'NEP', 'SED')

ggplot(coeff %>% filter(id == 'CR') ) +
  geom_density(aes(Jv, col = id, fill = 'Livingstone'), alpha = 0.1) +
  geom_density(aes(NEP, col = id, fill = 'ODEM'), alpha = 0.1) +
  ggtitle('Volumetric Fluxes') +
  # geom_point(aes(Jv, NEP, col = as.factor(year))) +
  # ylim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  # xlim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  theme_bw()

ggplot(coeff %>% filter(id == 'CR') ) +
  geom_density(aes(Ja, col = id, fill = 'Livingstone'), alpha = 0.1) +
  geom_density(aes(SED, col = id, fill = 'ODEM'), alpha = 0.1) +
  ggtitle('Areal Fluxes') +
  # geom_point(aes(Jv, NEP, col = as.factor(year))) +
  # ylim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  # xlim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  theme_bw()
  
  df.coeff <- coeff
  df.coeff$Jv[  df.coeff$Jv < 0] <- NA
  df.coeff$Ja[  df.coeff$Ja < 0] <- NA
  
  plot.list <- list()
  for (plot.list.id in na.omit(unique(df.coeff$id))){
    index.str = match(plot.list.id, na.omit(unique(df.coeff$id)))
    plot.list[[(2*index.str)-1]] <- ggplot(df.coeff %>% filter(id == plot.list.id)) +
      geom_density(aes(Jv,fill = 'Livingstone'), alpha = 0.1) +
      geom_density(aes(NEP,  fill = 'ODEM'), alpha = 0.1) +
      ggtitle(paste0(plot.list.id,': Volumetric Fluxes')) +
      # geom_point(aes(Jv, NEP, col = as.factor(year))) +
      # ylim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
      # xlim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
      theme_bw()+ custom.theme
    plot.list[[(2*index.str)]] <- ggplot(df.coeff %>% filter(id == plot.list.id)) +
      geom_density(aes(Ja,  fill = 'Livingstone'), alpha = 0.1) +
      geom_density(aes(SED, fill = 'ODEM'), alpha = 0.1) +
      ggtitle(paste0(plot.list.id,': Areal Fluxes')) +
      # geom_point(aes(Jv, NEP, col = as.factor(year))) +
      # ylim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
      # xlim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
      theme_bw()+ custom.theme
  }
  # g2 <- ggplot(df.coeff ) +
  #   geom_density(aes(Jv, col = id, fill = 'Livingstone'), alpha = 0.1) +
  #   geom_density(aes(NEP, col = id, fill = 'ODEM'), alpha = 0.1) +
  #   ggtitle('Volumetric Fluxes') +
  #   # geom_point(aes(Jv, NEP, col = as.factor(year))) +
  #   # ylim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  #   # xlim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  #   theme_bw()+ custom.theme
  # 
  # g3 <- ggplot(df.coeff ) +
  #   geom_density(aes(Ja, col = id, fill = 'Livingstone'), alpha = 0.1) +
  #   geom_density(aes(SED, col = id, fill = 'ODEM'), alpha = 0.1) +
  #   ggtitle('Areal Fluxes') +
  #   # geom_point(aes(Jv, NEP, col = as.factor(year))) +
  #   # ylim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  #   # xlim(min(c(coeff$Jv, coeff$NEP), na.rm = T),max(c(coeff$Jv, coeff$NEP), na.rm = T)) +
  #   theme_bw()+ custom.theme
  
  # g4 <- ggplot(df.coeff, aes (Jv, NEP, group = id, col = id) ) +
  #   geom_point(aes(Jv, NEP, col = id, fill = id)) +
  #   ggtitle('Volumetric Fluxes') +
  #   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x, col = id) +
  #   # geom_point(aes(Jv, NEP, col = as.factor(year))) +
  #   ylim(min(c(df.coeff$Jv, df.coeff$NEP), na.rm = T),max(c(df.coeff$Jv, df.coeff$NEP), na.rm = T)) +
  #   xlim(min(c(df.coeff$Jv, df.coeff$NEP), na.rm = T),max(c(df.coeff$Jv, df.coeff$NEP), na.rm = T)) +
  #   xlab('Vol. Flux Livingstone') + ylab('NEP term ODEM') +
  #   theme_bw()+ custom.theme
  # 
  # g5 <- ggplot(df.coeff, aes (Ja, SED, group = id, col = id) ) +
  #   geom_point(aes(Ja, SED, col = id, fill = id)) +
  #   ggtitle('Areal Fluxes') +
  #   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x, col = id) +
  #   # geom_point(aes(Jv, NEP, col = as.factor(year))) +
  #   ylim(min(c(df.coeff$Ja, df.coeff$SED), na.rm = T),max(c(df.coeff$Ja, df.coeff$SED), na.rm = T)) +
  #   xlim(min(c(df.coeff$Ja, df.coeff$SED), na.rm = T),max(c(df.coeff$Ja, df.coeff$SED), na.rm = T)) +
  #   xlab('Areal Flux Livingstone') + ylab('SED term ODEM') +
  #   theme_bw()+ custom.theme
  # 
  
  # (g2 + g3)  / (g4 + g5) 
  g <-  (plot.list[[1]] + plot.list[[2]]) / 
    (plot.list[[3]] + plot.list[[4]]) / 
    (plot.list[[5]] + plot.list[[6]]) / 
    (plot.list[[7]] + plot.list[[8]]) / 
    (plot.list[[9]] + plot.list[[10]]) / 
    (plot.list[[11]] + plot.list[[12]]) / 
    (plot.list[[13]] + plot.list[[14]]) / 
    (plot.list[[15]] + plot.list[[16]]) +
   plot_layout(guides = 'collect') &
    theme(legend.position='bottom');g
  ggsave(file = paste0('Figures/fluxVerification_Livingstone.png'), g, dpi = 300, width =500, height = 900,
         units='mm')
  

