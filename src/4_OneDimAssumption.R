# setwd('/home/robert/Projects/DSI/metabolism_phenology/')
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')


library(tidyverse)
library(patchwork)

## packages
library(devtools)
library(glmtools) 
library(pracma)
library(lubridate)
library(LakeMetabolizer)
library(zoo)
library(MASS)
library(scales)

source('src/functions_driver.R')

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

summary(lakeid)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth)
summary(rep)
summary(sta)
summary(event)
summary(wtemp)
summary(o2)
summary(o2sat)
summary(deck)
summary(light)
summary(frlight)
summary(flagdepth)
summary(flagwtemp)
summary(flago2)
summary(flago2sat)
summary(flagdeck)
summary(flaglight)
summary(flagfrlight) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$rep)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$event)) 
summary(as.factor(dt1$frlight)) 
summary(as.factor(dt1$flagdepth)) 
summary(as.factor(dt1$flagwtemp)) 
summary(as.factor(dt1$flago2)) 
summary(as.factor(dt1$flago2sat)) 
summary(as.factor(dt1$flagdeck)) 
summary(as.factor(dt1$flaglight)) 
summary(as.factor(dt1$flagfrlight))
detach(dt1)               


lks <- list.dirs(path = 'Driver_Data/extdata/', full.names = TRUE, recursive = F)
id <- c('FI','ME', 'MO', 'AL', 'TR','BM', 'SP', 'CR')

t = list()
t1 = list()
for (ii in lks){
  # print(paste0('Running ',ii))
  data <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'pball', include.dirs = T)))
  meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'NLDAS', include.dirs = T)))
  
  wind <- meteo$WindSpeed
  airtemp <- meteo$AirTemp
  
  eg_nml <- read_nml(paste0(ii,'/', list.files(ii, pattern = 'nml', include.dirs = T)))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
  A <- eg_nml$morphometry$A
  width =  eg_nml$morphometry$bsn_wid
  
  # summerDate <- dt1$sampledate[which.min(abs(dt1$sampledate - as.Date('1999-08-01')))]
  
  wtr.profile <- dt1 %>%
    filter(lakeid == id[match(ii, lks)])#
  
  # summerDate <- wtr.profile$sampledate[which.min(abs(wtr.profile$sampledate - as.Date('1999-08-01')))]
  
  imp.dep <- as.factor(wtr.profile$depth)[which(summary(as.factor(wtr.profile$depth)) >mean(summary(as.factor(wtr.profile$depth))))]
  imp.dep <- sort(as.numeric(as.character(imp.dep)))
  interp.profile = c()
  for (i in unique(wtr.profile$sampledate)){
    dat =  wtr.profile %>%
      filter(sampledate == i) %>%
      arrange(depth)
    if(all(is.na(dat$wtemp))){
      next 
    }
    

    d = data.frame(matrix(NA, nrow = 1, ncol = length(seq(min(imp.dep),max(imp.dep),1))+1))
    colnames(d) = c('datetime', paste0('wtr_',seq(min(imp.dep),max(imp.dep),1)))
    d[2:ncol(d)] = approx(dat$depth, dat$wtemp, seq(min(imp.dep),max(imp.dep),1))$y
    d$datetime = as.POSIXct(unique(dat$sampledate))
    if (is.na(d[ncol(d)])){
      d[ncol(d)] = d[max(which(!is.na(d[2:ncol(d)])))]
    }
    if (is.na(d[2])){
      d[2] = d[min(which(!is.na(d[2:ncol(d)]))+1)]
    }

    d[2:ncol(d)] <- na.approx(d[2:ncol(d)])
    interp.profile <- rbind(interp.profile,d)
  }
  # wtr.profile <- wtr.profile %>%
  #   # filter(sampledate == summerDate) %>%
  #   arrange(sampledate, depth) %>%
  #   group_by(sampledate) %>%
  #   mutate(temp = approx(depth, wtemp, seq(1,max(imp.dep),1))$y,
  #          dep = seq(1,max(imp.dep),1)) %>%
  #   select(sampledate, dep, temp)
  # 
  # na.id = which(is.na(wtr.profile$wtemp))
  # 
  # wtr.profile <- wtr.profile[- na.id, ]
  
  # wnd = meteo$WindSpeed[match(summerDate, as.Date(meteo$time))]
  wnd = meteo$WindSpeed[match(as.Date(meteo$time), as.Date(interp.profile$datetime))]
  
  if (max(H) < max( seq(min(imp.dep),max(imp.dep),1))){
    H = c(max( seq(min(imp.dep),max(imp.dep),1)), H)
    A = c(min(A), A)
  }
  area = approx(rev(H), rev(A),  seq(min(imp.dep),max(imp.dep),1))
  
  # df <- reshape(wtr.profile, idvar = "sampledate", timevar = "depth", direction = "wide")
  
  # mT = meta.depths(wtr = wtr.profile$wtemp, depths = wtr.profile$depth, slope = 0.1, seasonal = TRUE, mixed.cutoff = 1)
  mT = ts.meta.depths(wtr = interp.profile)
  
  # LN = lake.number(bthA = area$y, bthD = area$x, 
                   # uStar = uStar(wndSpeed = na.omit(wnd), wndHeight = 2, averageEpiDense = water.density(wtr.profile$wtemp[1])) 
  
  LN = ts.lake.number(wtr = interp.profile, 
                      wnd = data.frame('datetime' = interp.profile$datetime,
                                       'wind' = na.omit(wnd)),
                      wnd.height = 10,
                      bathy = data.frame('depths' =  seq(min(imp.dep),max(imp.dep),1),
                                         'areas' = area$y))
  

  
  # St = schmidt.stability(wtr = wtr.profile$wtemp, depths = wtr.profile$depth, bthA = area$y, bthD = area$x, sal = 0), 
  # metaT = mT[1], metaB = mT[2], averageHypoDense = water.density(wtr.profile$wtemp[nrow(wtr.profile)]))
  # 
  # g = 9.81 * ( water.density(wtr.profile$wtemp[nrow(wtr.profile)]) - water.density(wtr.profile$wtemp[1]))/998.2
  g = 9.81 * ( water.density(interp.profile[,ncol(interp.profile)]) - water.density(interp.profile[,2]))/998.2
  
  Ri = (g * mT[,2])^(1/2) /  (1 * 10^(-4)) # s-1
  R = Ri/width
  
  df.R = data.frame('datetime' = LN$datetime, 'Radius' = R)
  
  stats = LN %>%
    mutate(year = year(datetime), ln = ifelse(lake.number > 1, 'above', 'below'),
           doy = ifelse(lake.number > 1, yday(datetime), NA),
           month = ifelse(lake.number > 1, month(datetime), NA)) %>%
    group_by(year) %>%
    summarise('duration' = max(doy, na.rm=T) - min(doy, na.rm = T),
              'percentage' = (length(na.omit(doy))*100)/ length(ln), 
              'start' = min(month, na.rm = T), 
              'end' = max(month, na.rm=T))
  
  g1 <- ggplot(subset(LN, datetime >= '2013-01-01 00:00:00')) +
    geom_line(aes(datetime, lake.number)) +
    geom_point(aes(datetime, lake.number)) +
    xlab('') + ylab('Lake Number (-)') +
    # scale_y_continuous(trans='log10') +
    scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), labels =
                  trans_format('log10', function(x) 10^x)) +
    # coord_trans(y='log10')+
    geom_hline(yintercept=c(1), linetype='dashed', color=c('red')) +
    ggtitle(paste0(id[match(ii, lks)],": mean annual period of LN > 1 (",round(mean(stats$percentage),0), '% of data',") from ",month.abb[mean(stats$start)],' to ', month.abb[mean(stats$end)],
                   ' for ',round(mean(stats$duration),0), ' days')) +
    theme_minimal() 
  g2 <- ggplot(df.R) +
    geom_line(aes(datetime, Radius)) +
    geom_point(aes(datetime, Radius)) +
    xlab('') + ylab('Rossby Radius (m)') +
    geom_hline(yintercept=c(1), linetype='dashed', color=c('red')) +
    ggtitle(paste0(id[match(ii, lks)],': mean R = ',round(mean(df.R$Radius,na.rm =T),1),' m')) +
    theme_minimal() 
  ggsave(paste0('Figures/',id[match(ii, lks)],'_oneD.png'), g1/g2, width =5, height = 8, unit = 'in' )
  
  t[[match(ii, lks)]] <- g1#/g2
  t1[[match(ii, lks)]] <- g2
  # W = wedderburn.number(delta_rho =  ( water.density(wtr.profile$wtemp[nrow(wtr.profile)]) - water.density(wtr.profile$wtemp[1])), 
  #                                      metaT = mT[1], uSt =  uStar(wndSpeed = wnd, wndHeight = 2, averageEpiDense = water.density(wtr.profile$wtemp[1])),
  #                       Ao = max(A), AvHyp_rho =  water.density(wtr.profile$wtemp[nrow(wtr.profile)]))
  # 
  # print(paste0(id[match(ii, lks)],': LN = ',round(LN,1),', R = ', round(R,2), ', W = ', round(W,1)))
}
  
g <- (t[[4]] | t[[6]] )/ (t[[8]] | t[[7]]) / (t[[5]] | t[[1]]) / (t[[2]] | t[[3]])+ plot_annotation(tag_levels = 'A');g
g1 <- (t1[[4]] | t1[[6]] )/ (t1[[8]] | t1[[7]]) / (t1[[5]] | t1[[1]]) / (t1[[2]] | t1[[3]])+ plot_annotation(tag_levels = 'A');g
  
  
ggsave(paste0('Figures/LN_specific_oneD.png'), g, width =15, height = 15, unit = 'in' )
ggsave(paste0('Figures/RR_oneD.png'), g1, width =15, height = 15, unit = 'in' )

  
  