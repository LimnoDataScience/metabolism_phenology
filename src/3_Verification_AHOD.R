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

df.AHOD = data.frame('year' = NULL, 
                            'AHOD' = NULL,
                            'SED' = NULL,
                     'NEP' = NULL,
                            'id' = NULL)

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
  
  # input = read_csv(paste0(check_lakes$folder.name[loop],'input.txt'))
  
  lake.id = check_lakes$short.name[loop]
  
  for (id.year in years[which(years %in% dt1$year4)]){
    sim <- fluxes %>%
      filter(lubridate::year(datetime) == id.year)
    strat.date <- min(sim$datetime[which(sim$stratified == 1)])
    thermocline.z = ceiling(mean(sim$tddepth[which(sim$stratified == 1)])) #+ floor(sd(sim$tddepth[which(sim$stratified == 1)]))
    
    sim$Fsed_corr <- NA
    sim$Fnep_corr <- NA
    for (p in 2:nrow(sim)){
      sim$Fsed_corr[p] <- sim$Fsed[p]  * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1)  
      sim$Fnep_corr[p] <- sim$Fmineral[p]  * max(sim$volume_hyp[p-1]/(sim$area_hyp[p-1]),1)  
    }
    
    obs <- dt1 %>%
      filter(lakeid == lake.id & year4 == id.year & depth >= floor(thermocline.z))
    
    if (any(obs$flago2sat == 'U')){
      # obs <- obs[-which(obs$flago2sat == 'U'), ]
      obs <- obs[-which(obs$flago2sat != ""), ]
    }
    if (any(obs$flago2 == 'U')){
      # obs <- obs[-which(obs$flago2 == 'U'), ]
      obs <- obs[-which(obs$flago2 != ""), ]
    }
    

    
    ggplot(obs %>% filter(sampledate >=  strat.date)) +
      geom_point(aes(sampledate, o2, col = as.factor(depth))) +
      geom_line(aes(sampledate, o2, col = as.factor(depth)))
    
    mass.do <- data.frame('datetime' = NULL, 'DOmass' = NULL)
    for (id.dates in unique(obs$sampledate)){
      df <- obs %>% filter(sampledate == id.dates)
      if (length(na.omit(df$o2)) < 2){
        mass.do <- rbind(mass.do, data.frame('datetime' = as.Date(id.dates),
                                             'DOmass' = NA,
                                             'Area' = sim$area_hyp[match(as.Date(id.dates),as.Date(sim$datetime))]))
      } else {
      if (any(is.na(df$o2))){
        df <- df[-which(is.na(df$o2)),]
      }
      
      do.values <- approx(df$depth, df$o2, seq(min(df$depth), max(df$depth), 1))$y
      area.values <- approx(depths, areas, seq(min(df$depth), max(df$depth), 1))$y
      
      if (max(depths) < max(max(df$depth))){
        area.values[which(seq(min(df$depth), max(df$depth), 1) > max(depths))] <- 1e-1
      }
      
      mass.do <- rbind(mass.do, data.frame('datetime' = as.Date(id.dates),
                                            'DOmass' = trapz(seq(min(df$depth), max(df$depth), 1),
            area.values * do.values),
            'Area' = sim$area_hyp[match(as.Date(id.dates),as.Date(sim$datetime))]))
      }
      
    }
    
    # plot(mass.do$datetime, mass.do$DOmass/mean(mass.do$Area))
    
    id.start <- which(
      lubridate::yday(mass.do$datetime) >= lubridate::yday(strat.date)
    )[(which.min(abs(lubridate::yday(mass.do$datetime[which(
      lubridate::yday(mass.do$datetime) >= lubridate::yday(strat.date)
    )]) - lubridate::yday(strat.date))))]
    id.end <- which(
      lubridate::yday(mass.do$datetime) >= lubridate::yday(strat.date)
    )[ which.min(mass.do$DOmass[which(
       lubridate::yday(mass.do$datetime) >= lubridate::yday(strat.date)
      )])]
    if (mass.do$DOmass[id.start] < mass.do$DOmass[id.start + 1] && !is.na(mass.do$DOmass[id.start + 1])){
      for (k in id.start:(length(mass.do$datetime))){
        if (mass.do$DOmass[k] >= mass.do$DOmass[k+1] && !is.na(mass.do$DOmass[k]) && !is.na(mass.do$DOmass[k + 1])){
          id.start = k
          break
        }
      }
      id.end <- which(
        lubridate::yday(mass.do$datetime) >= lubridate::yday(mass.do$datetime[id.start])
      )[ which.min(mass.do$DOmass[which(
        lubridate::yday(mass.do$datetime) >= lubridate::yday(mass.do$datetime[id.start])
      )])]
    }

    
    plot(mass.do$datetime[id.start:id.end], mass.do$DOmass[id.start:id.end]/mean(mass.do$Area[id.start:id.end]))
    
    mod <- lm(DOmass[id.start:id.end]/mean(Area[id.start:id.end]) ~ datetime[id.start:id.end], mass.do)
    sum.mod <- summary(mod)
    p  <- pf(sum.mod$fstatistic[1], sum.mod$fstatistic[2], sum.mod$fstatistic[3], lower.tail=F)
    
    AHOD <- mod$coefficients[2]
    SED <- mean(sim$Fsed_corr[match(mass.do$datetime[id.start],as.Date(sim$datetime)) : match(mass.do$datetime[id.end],as.Date(sim$datetime))], na.rm = T) / 1000
    NEP <- mean(sim$Fnep_corr[match(mass.do$datetime[id.start],as.Date(sim$datetime)) : match(mass.do$datetime[id.end],as.Date(sim$datetime))], na.rm = T) / 1000
    
    if (!is.na(p) && p <= 0.05){
      df.AHOD <- rbind(df.AHOD, 
                              data.frame('year' = id.year,
                                         'AHOD' = AHOD,
                                         'SED' = SED ,
                                         'NEP' = NEP ,
                                         'id' = lake.id))
    } else {
      df.AHOD <- rbind(df.AHOD, 
                              data.frame('year' = id.year,
                                         'AHOD' = NA,
                                         'SED' = SED ,
                                         'NEP' = NEP ,
                                         'id' = lake.id))
    }
  }
}
   
# coeff <- as.data.frame(coeff)
# colnames(coeff) = c('year', 'Jv', 'Ja', 'NEP', 'SED')

ggplot(df.AHOD) +
  geom_density(aes(AHOD *(-1),  fill = 'AHOD'), alpha = 0.1) +
  geom_density(aes(SED ,  fill = 'ODEM'), alpha = 0.1) +
  facet_wrap(~id)+
  ggtitle('Sediment depletion in g/m2/d') + xlab('areal hypolimnetic oxygen deﬁcit (AHOD) and sediment oxygen demand (SOD) in g/m2/d') +
  theme_bw()

ggplot(df.AHOD) +
  geom_point(aes(AHOD *(-1), SED+NEP, col = id)) +
  ggtitle('Fluxes') +
  xlab('AHOD') + ylab('SED+NEP')+
  theme_bw()

ggsave(file = paste0('Figures/fluxVerification.png'), g, dpi = 300, width =500, height = 900,
       units='mm')


