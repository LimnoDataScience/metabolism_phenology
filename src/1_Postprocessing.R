cat('\f')
rm(list= ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
library(tidyverse)
library(LakeMetabolizer)
library(zoo)
library(lazyeval)
library(broom)
library(purrr)

# Custom Functions 
calc_fit <- function(mod_data, obs_data){
  obs <- obs_data 
  mod <- mod_data 
  return (sqrt(mean((obs-mod)**2,na.rm = TRUE))) # RMSE
}
calc_nse <- function(mod_data, obs_data){
  obs <- obs_data 
  mod <- mod_data 
  return (1-mean((mod-obs)**2,na.rm = TRUE)/mean((obs-mean(obs, na.rm=TRUE))**2,na.rm = TRUE)) # RMSE
}

lake.list <- c('Allequash', 'BigMuskellunge', 'Crystal', 'Fish', 'Mendota',
               'Monona', 'Sparkling', 'Trout')
lake.list.Abr <- c('Allequash' = 'AL', 'BigMuskellunge' = 'BM', 'Crystal' = 'CR', 
                   'Fish' = 'FI', 'Mendota' = 'ME',
                   'Monona' = 'MO', 'Sparkling' = 'SP', 'Trout' = 'TR')

df.estimations <- list()
all.sigma <- c()
list.rhat <- list()
for (lake.id in lake.list){
  if (match(lake.id, lake.list) == 1){
    setwd(paste0('../',lake.id))
  } else {
    setwd(paste0(lake.id))
  }


fiveyear <- 1979:2019

input <- readr::read_csv(
  './input.txt',
  col_names=c('datetime', 'thermocline_depth', 'temperature_epi', 
              'temperature_hypo', 'temperature_total', 'volume_total', 
              'volume_epi', 'volume_hypo', 'area_thermocline', 'area_surface', 
              'upper_meta', 'lower_meta', 'year', 'day_of_year', 'max.d', 'wind', 'airtemp'),
  col_types=cols(datetime=col_datetime(), year=col_integer(), day_of_year=col_integer(), .default=col_double()))

in5yr <- filter(input, year %in% fiveyear)


library(zoo)
obs <- read.table(
  './observed.txt',
  header=FALSE,
  sep=' ',
  as.is=TRUE) %>%
  t() %>%
  as_tibble(.name_repair='minimal') %>%
  setNames(., nm=c('dateint', 'DO_tot', 'DO_epi', 'DO_hypo')) %>%
  mutate(date = zoo::as.Date(dateint, origin='1979-04-01')) %>% 
  dplyr::select(date, everything())

obs5yr <- filter(obs, lubridate::year(date) %in% fiveyear)


in1yr= in5yr
obs1yr = obs5yr


idx1 = which(!is.na(in1yr$thermocline_depth))[1]
idx2 = rev(which(!is.na(in1yr$thermocline_depth)))[1]
in1yr = in1yr 
in1yr$strat <- ifelse(is.na(in1yr$thermocline_depth),0,1)
strat.pos <- c()
for (ii in 1:length(in1yr$strat)){
  if (in1yr$strat[ii] == 1 && in1yr$strat[ii-1] == 0){
    strat.pos <- append(strat.pos, ii)
  }
}

idy = match(obs1yr$date,zoo::as.Date(in1yr$datetime))

idx = idy[!is.na(obs1yr$DO_tot)]
idxx = idy[!is.na(obs1yr$DO_epi)]
# idz = which(!is.na(idx))
idz = match(idx, idy)
idzz =match(idxx, idy)
DO_obs_epi = rep(NA, length(in1yr$datetime))
DO_obs_epi[idxx] = obs1yr$DO_epi[idzz]
DO_obs_hyp = rep(NA, length(in1yr$datetime))
DO_obs_hyp[idxx] = obs1yr$DO_hypo[idzz]
DO_obs_tot = rep(NA, length(in1yr$datetime))
DO_obs_tot[idx] = obs1yr$DO_tot[idz]


in1yr <- in1yr[,-c(1)] %>% mutate_each( funs_( interp( ~replace(., is.na(.),0) ) ) )

simdata <- tibble(
  DO_obs_epi = DO_obs_epi * 1000,
  DO_obs_hyp = DO_obs_hyp * 1000,
  DO_obs_tot = DO_obs_tot * 1000,
  day = seq(1, nrow(in1yr))
)

WhenToEstimateParams = sort(c(idxx, idx, strat.pos)) # strat.pos

nParamEstimates = length(WhenToEstimateParams)
ParamIndex = rep(nParamEstimates,nrow(simdata))
ParamIndex[1:WhenToEstimateParams[1]-1] = 1 # use the first parameter value
for (i in 2:nParamEstimates){
  iCurrent = WhenToEstimateParams[i-1]:(WhenToEstimateParams[i]-1)
  ParamIndex[iCurrent] = i
}


dummyinput <- list(
  NEP_mu_min = 0,
  NEP_mu_max = 0.5,
  NEP_sigma = 1e-32, 
  SED1_mu_min = 0,
  SED1_mu_max = 1,
  SED1_sigma = 1e-32, 
  MIN1_mu_min = 0,
  MIN1_mu_max = 1,
  MIN1_sigma = 1e-32, 
  SED2_mu_min = 0,
  SED2_mu_max = 1,
  SED2_sigma = 1e-32, 
  theta0 = 1.08^(in1yr$temperature_total - 20),
  theta1 = 1.08^(in1yr$temperature_epi - 20),
  theta2 = 1.08^(in1yr$temperature_hypo - 20),
  k600t = k600.2.kGAS.base(k.cole.base(in1yr$wind),temperature = in1yr$temperature_total, gas = "O2"),
  o2satt = o2.at.sat.base(temp = in1yr$temperature_total, altitude = 450) * 1000,
  k600 = k600.2.kGAS.base(k.cole.base(in1yr$wind),temperature = in1yr$temperature_epi, gas = "O2"),
  o2sat = o2.at.sat.base(temp = in1yr$temperature_epi, altitude = 450) * 1000,
  volume_epi = in1yr$volume_epi,
  volume_tot = in1yr$volume_total,
  area_epi = in1yr$area_surface,
  volume_hyp = in1yr$volume_hypo,
  area_hyp = in1yr$area_thermocline,
  tddepth = in1yr$thermocline_depth,
  ii_obs = idxx,
  ii_obs_mix = idx,
  wtr_epi = in1yr$temperature_epi,
  wtr_hyp = in1yr$temperature_hypo,
  wtr_tot = in1yr$temperature_total,
  khalf = 500, 
  err_sigma = 0.0003,
  d = nrow(simdata),
  DO_epi_init = 15 * 1000, 
  DO_hyp_init = 15 * 1000,
  DO_tot_init = 15 * 1000,
  stratified = in1yr$strat,
  strat_pos = strat.pos,
  len_strat_pos = length(strat.pos),
  d_strat_pos = length(strat.pos),
  i_Param = ParamIndex,  # n_ParamEst indeces within d
  n_ParamEst = nParamEstimates, # number of times params are estimated
  airtemp = in1yr$airtemp,
  delvol_epi = c(diff(in1yr$volume_epi),0)/c(in1yr$volume_epi),
  delvol_hyp =  c(diff(in1yr$volume_hypo),0)/c(in1yr$volume_hypo)
)

dummyinput$delvol_epi[strat.pos] = 0
dummyinput$delvol_hyp[strat.pos] = 0
dummyinput$delvol_epi[is.na(dummyinput$delvol_epi)] = 0
dummyinput$delvol_hyp[is.na(dummyinput$delvol_hyp)] = 0
dummyinput$DO_obs_epi = simdata$DO_obs_epi[idxx]
dummyinput$DO_obs_hyp = simdata$DO_obs_hyp[idxx]
dummyinput$DO_obs_tot = simdata$DO_obs_tot[idx]
dummyinput$N_obs = length(dummyinput$ii_obs)
dummyinput$N_obs_mix = length(idx)
dummyinput$k600t[which(in1yr$airtemp <= 0 & in1yr$temperature_total <= 4)] = 1e-5

# chains <- 3
# iter <-2000
# warmup <- 500
# adapt_delta <- 0.85
# max_treedepth <- 15
# thin <- 1

fit_csv <- readr::read_csv(
  './fit_summary.csv')

## GET STAN OUTPUT for MCMC check
# fit <- readRDS(file = paste0('fit.rds'))
# source("../src/stan_utility.R")
# check_rhat(fit)
# check_n_eff(fit)
# check_div(fit)
# check_energy(fit)
# check_treedepth(fit,max_depth = max_treedepth)


fit_clean <- fit_csv %>%
  rename(lower = '2.5%', middle = '50%',upper = '97.5%')  %>%
  mutate(name = strsplit(var, "\\[|\\]|,") %>% map_chr(~.x[1]),
         index = strsplit(var, "\\[|\\]|,") %>% map_int(~as.integer(.x[2])))

outvars <- c("o2_epi","o2_hyp","fatm","fnep","fsed2","fmineral","fentr_epi","fentr_hyp","NEP_mgm3d","SED_mgm2d","MIN_mgm3d")

dat <- fit_clean %>% filter(name %in% outvars) %>% 
  dplyr::select(mean,
         sd,
         lower,
         middle,
         upper,
         name,
         index)

dat_sigma <- fit_clean %>% filter(name %in% c('smooth_sigma')) %>% 
  dplyr::select(mean,
                sd,
                lower,
                middle,
                upper,
                name,
                index)

multi_spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

odem_stan <- dat %>% multi_spread(key = name,c(mean,sd,lower,middle,upper))
alldmmydata = data.frame(volume_epi = in1yr$volume_epi,
                  volume_tot = in1yr$volume_total,
                  area_epi = in1yr$area_surface,
                  volume_hyp = in1yr$volume_hypo,
                  area_hyp = in1yr$area_thermocline,
                  tddepth = in1yr$thermocline_depth,
                  wtr_epi = in1yr$temperature_epi,
                  wtr_hyp = in1yr$temperature_hypo,
                  wtr_tot = in1yr$temperature_total,
                  stratified=in1yr$strat)
odem_stan <- cbind(in5yr[,1],odem_stan,simdata, alldmmydata)
odem_stan <- odem_stan %>% 
  rename(DO_epi = o2_epi_mean,
         DO_hyp = o2_hyp_mean,
         Fatm = fatm_mean,
         Fnep = fnep_mean,
         Fmineral = fmineral_mean,
         Fsed = fsed2_mean,
         Fentr1 = fentr_epi_mean,
         Fentr2 = fentr_hyp_mean,
         NEP = NEP_mgm3d_mean,
         SED = SED_mgm2d_mean,
         MIN = MIN_mgm3d_mean)


library(lubridate)

odem_stan$doy =  yday(odem_stan$datetime); odem_stan$year = year(odem_stan$datetime)
simdata$doy =  in1yr$day_of_year; simdata$year = in1yr$year

calc_fit(mod_data = odem_stan$o2_epi_middle, obs_data = simdata$DO_obs_epi)/1000
calc_fit(mod_data = odem_stan$o2_hyp_middle, obs_data = simdata$DO_obs_hyp)/1000
calc_fit(mod_data = cbind(odem_stan$o2_epi_middle,odem_stan$o2_hyp_middle), obs_data = cbind(simdata$DO_obs_epi, simdata$DO_obs_hyp))/1000
rmse <- round(calc_fit(mod_data = cbind(odem_stan$o2_epi_middle,odem_stan$o2_hyp_middle), obs_data = cbind(simdata$DO_obs_epi, simdata$DO_obs_hyp))/1000
              ,2)

nse <- round(calc_nse(mod_data = cbind(odem_stan$o2_epi_middle, odem_stan$o2_hyp_middle), obs_data = cbind(simdata$DO_obs_epi, simdata$DO_obs_hyp))
             ,2)
odem_stan$rmse = rmse
odem_stan$nse = nse

save(odem_stan, file = paste0(lake.id,'_mineral.Rda'))


# fair_cols <- c("#38170B","#BF1B0B", "#FFC465", "#66ADE5", "#252A52")

df.estimations[[match(lake.id, lake.list)]] <-ParamIndex

sigma.df =  dat_sigma[which(dat_sigma$name == 'smooth_sigma'),]
sigma.df$index = lake.id
sigma.df$neff = fit_clean$n_eff[which( fit_clean$name == 'smooth_sigma')]
sigma.df$rhat = fit_clean$Rhat[which( fit_clean$name == 'smooth_sigma')]
all.sigma <- rbind(all.sigma, sigma.df)

list.rhat[[match(lake.id, lake.list)]] = unique(fit_clean$name[which(fit_clean$Rhat > 1.1)])

setwd('../')
}

#### Loading of output (as loop) ####
fit.data = list()
for (lake.id in lake.list){
  load(paste0(lake.id,'/',lake.id,'_mineral.Rda'))
  mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
  maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
  odem_stan_filt = odem_stan[mindata:maxdata,]
  find.lake <- match(lake.id, lake.list)
  estimat.lake <- df.estimations[[find.lake]]
  estimat.lake <- estimat.lake[mindata:maxdata]
  pos.lake <- which(duplicated(estimat.lake)== FALSE)
  
  lake.odem = odem_stan_filt
  lake.odem[,c('SED','MIN','NEP')] = NA

  lake.odem$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
  lake.odem$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
  lake.odem$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]
  # New columns
  lake.odem[,c('NEP_max','NEP_min','SED_max','SED_min','MIN_max','MIN_min')] = NA
  lake.odem$NEP_max[pos.lake] <- odem_stan$NEP_mgm3d_upper[min(estimat.lake):max(estimat.lake)]
  lake.odem$NEP_min[pos.lake] <- odem_stan$NEP_mgm3d_lower[min(estimat.lake):max(estimat.lake)]
  lake.odem$SED_max[pos.lake] <- odem_stan$SED_mgm2d_upper[min(estimat.lake):max(estimat.lake)]
  lake.odem$SED_min[pos.lake] <- odem_stan$SED_mgm2d_lower[min(estimat.lake):max(estimat.lake)]
  lake.odem$MIN_max[pos.lake] <- odem_stan$MIN_mgm3d_upper[min(estimat.lake):max(estimat.lake)]
  lake.odem$MIN_min[pos.lake] <- odem_stan$MIN_mgm3d_lower[min(estimat.lake):max(estimat.lake)]
  assign(tolower(lake.id), lake.odem)
  write_csv(x = lake.odem, file = paste0('Processed_Output/',tolower(lake.id),'_fluxes.csv'),col_names = T)
  
  # export fit data to list
  fit.data[[lake.id]]  = data.frame(obsdata = lake.odem$DO_obs_epi, simdata = lake.odem$DO_epi, id = lake.id, type = 'Epilimnion') %>% 
    bind_rows(data.frame(obsdata = lake.odem$DO_obs_hyp, simdata = lake.odem$DO_hyp, id = lake.id, type = 'Hypolimnion')) %>% 
    na.omit()
}
fitdata = bind_rows(fit.data)
write_csv(fitdata, 'Processed_Output/fitdata.csv')


### Compute annual, cumulative, and total fluxes ####
annual.flux = list()
cum.flux = list()
cum.flux_max = list()
cum.flux_min = list()
odem.flux = list()
total.flux= list()

for (lake.id in lake.list){
  
  load(paste0(lake.id,'/',lake.id,'_mineral.Rda'))
  
  mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
  minyear = year(odem_stan$datetime[mindata])
  minyear = 1996
  maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
  maxyear = year(odem_stan$datetime[maxdata])
  
  BeginYear = minyear #1979
  EndYear = maxyear # 2019
  
  epiV = odem_stan$volume_epi # m3
  hypoV = odem_stan$volume_hyp # m3
  totalV = odem_stan$volume_tot # m3
  Stratified = odem_stan$strat
  LakeArea = odem_stan$area_epi # m2
  Year = odem_stan$year
  DayOfYear = odem_stan$doy
  
  iStrat = which(Stratified==1)
  iNotStrat = which(Stratified==0)
  
  #####################
  # Convert fluxes to areal (lake area)
  
  areal.fluxes <- function(name, layerV) {
    vec = rep(NA,dim(odem_stan)[1])
    vec[iStrat] = odem_stan[,name][iStrat]/1000*layerV[iStrat]/LakeArea[iStrat] # Stratified
    vec[iNotStrat] = odem_stan[,name][iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat] # Mixed 
    return(vec)
  }
  
  Fatm = areal.fluxes('Fatm', epiV)
  Fnep = areal.fluxes('Fnep', epiV)
  FentrEpi = areal.fluxes('Fentr1', epiV)
  Fmin = areal.fluxes('Fmineral', hypoV)
  Fsed = areal.fluxes('Fsed', hypoV)
  FentrHypo = areal.fluxes('Fentr2', hypoV)

  #max
  Fatm_max = areal.fluxes('fatm_upper', epiV)
  Fnep_max = areal.fluxes('fnep_upper', epiV)
  FentrEpi_max = areal.fluxes('fentr_epi_upper', epiV)
  Fmin_max = areal.fluxes('fmineral_upper', hypoV)
  Fsed_max = areal.fluxes('fsed2_upper', hypoV)
  FentrHypo_max = areal.fluxes('fentr_hyp_upper', hypoV)
  
  # min
  Fatm_min = areal.fluxes('fatm_lower', epiV)
  Fnep_min = areal.fluxes('fnep_lower', epiV)
  FentrEpi_min = areal.fluxes('fentr_epi_lower', epiV)
  Fmin_min = areal.fluxes('fmineral_lower', hypoV)
  Fsed_min = areal.fluxes('fsed2_lower', hypoV)
  FentrHypo_min = areal.fluxes('fentr_hyp_lower', hypoV)
  
  plot(odem_stan$Fentr2 * odem_stan$volume_hyp+odem_stan$Fentr1 * odem_stan$volume_epi);grid()
  #####################
  # Calculate cumulatives
  
  # Calculate the yearly cumulative fluxes
  uYears = unique(Year)
  
  annflux <- function(thisYear, vec) { # For individual years sum up fluxes 
    iYear = which(Year == thisYear)
    return(sum(vec[iYear], na.rm = T))
  }
  
  yFatm = sapply(X = uYears, FUN = annflux, vec = Fatm)
  yFnep = sapply(X = uYears, FUN = annflux, vec = Fnep)
  yFentrEpi = sapply(X = uYears, FUN = annflux, vec = FentrEpi)
  yFmin = sapply(X = uYears, FUN = annflux, vec = Fmin)
  yFsed = sapply(X = uYears, FUN = annflux, vec = Fsed)
  yFentrHypo = sapply(X = uYears, FUN = annflux, vec = FentrHypo)
  yFnepTot = yFnep + yFmin + yFsed
  
  yFatm_max = sapply(X = uYears, FUN = annflux, vec = Fatm_max)
  yFnep_max = sapply(X = uYears, FUN = annflux, vec = Fnep_max)
  yFentrEpi_max = sapply(X = uYears, FUN = annflux, vec = FentrEpi_max)
  yFmin_max = sapply(X = uYears, FUN = annflux, vec = Fmin_max)
  yFsed_max = sapply(X = uYears, FUN = annflux, vec = Fsed_max)
  yFentrHypo_max = sapply(X = uYears, FUN = annflux, vec = FentrHypo_max)
  yFnepTot_max = yFnep_max + yFmin_max + yFsed_max
  
  yFatm_min = sapply(X = uYears, FUN = annflux, vec = Fatm_min)
  yFnep_min = sapply(X = uYears, FUN = annflux, vec = Fnep_min)
  yFentrEpi_min = sapply(X = uYears, FUN = annflux, vec = FentrEpi_min)
  yFmin_min = sapply(X = uYears, FUN = annflux, vec = Fmin_min)
  yFsed_min = sapply(X = uYears, FUN = annflux, vec = Fsed_min)
  yFentrHypo_min = sapply(X = uYears, FUN = annflux, vec = FentrHypo_min)
  yFnepTot_min = yFnep_min + yFmin_min + yFsed_min
  
  # Subset data by year indices
  iYears = which(Year>=BeginYear & Year<=EndYear) # For all days in all years
  iYearsOnly = which(uYears>=BeginYear & uYears<=EndYear) # Just the years
  
  # Cumulative fluxes
  FatmCum = cumsum(Fatm[iYears])
  FnepCum = cumsum(Fnep[iYears])
  FminCum = cumsum(Fmin[iYears])
  FsedCum = cumsum(Fsed[iYears])
  FAllCum = FatmCum+FnepCum+FminCum+FsedCum
  FnepTotCum = FnepCum+FminCum-FsedCum
  
  FatmCum_max = cumsum(Fatm_max[iYears])
  FnepCum_max = cumsum(Fnep_max[iYears])
  FminCum_max = cumsum(Fmin_max[iYears])
  FsedCum_max = cumsum(Fsed_max[iYears])
  FAllCum_max = FatmCum_max+FnepCum_max+FminCum_max+FsedCum_max
  FnepTotCum_max = FnepCum_max+FminCum_max-FsedCum_max
  
  FatmCum_min = cumsum(Fatm_min[iYears])
  FnepCum_min = cumsum(Fnep_min[iYears])
  FminCum_min = cumsum(Fmin_min[iYears])
  FsedCum_min = cumsum(Fsed_min[iYears])
  FAllCum_min = FatmCum_min+FnepCum_min+FminCum_min+FsedCum_min
  FnepTotCum_min = FnepCum_min+FminCum_min-FsedCum_min
  
  YearFrac = Year+(DayOfYear/366)
  
  # Convert to annual carbon units
  TYears = EndYear-BeginYear+1
  Oxygen2Carbon = 1/32 * 12/1
  meanFatmC = -FatmCum[length(FatmCum)]/TYears * Oxygen2Carbon # gC/m2/y
  meanFnepC = FnepCum[length(FnepCum)]/TYears * Oxygen2Carbon # gC/m2/y
  meanFnepTotC = FnepTotCum[length(FnepTotCum)]/TYears * Oxygen2Carbon # gC/m2/y
  meanFminC = FminCum[length(FminCum)]/TYears * Oxygen2Carbon # gC/m2/y
  meanFsedC = FsedCum[length(FsedCum)]/TYears * Oxygen2Carbon # gC/m2/y
  print('*************')
  print('Mean annual C units')
  print(paste('Fatm: ',signif(meanFatmC,3),' (gC/m2/y)',sep=""))
  print(paste('Fnep: ',signif(meanFnepC,3),' (gC/m2/y), epilimnion',sep=""))
  print(paste('Fmin: ',signif(meanFminC,3),' (gC/m2/y)',sep=""))
  print(paste('Fsed: ',signif(meanFsedC,3),' (gC/m2/y)',sep=""))
  print('-------------')
  print(paste('Fnep total: ',signif(meanFnepTotC,3),' (gC/m2/y), fate as burial(+) +export(-)',sep=""))
  print('If we assume approximate steady state for water column OC,')
  print('then FnepTotal is ~ contribution of allocthony to total burial.')
  print('Note that total burial+export is greater than FnepTotal,')
  print('because burial+export also includes allocthony not mineralized.')
  
  rmse <- round(calc_fit(mod_data = cbind(odem_stan$DO_epi, odem_stan$DO_hyp), obs_data = cbind(odem_stan$DO_obs_epi, odem_stan$DO_obs_hyp))/1000,2)
  nse <- round(calc_nse(mod_data = cbind(odem_stan$DO_epi, odem_stan$DO_hyp), obs_data = cbind(odem_stan$DO_obs_epi, odem_stan$DO_obs_hyp)),2)
  
  
  TDepth = odem_stan$tddepth
  iZeros = which(TDepth == 0)
  TDepth[iZeros] = max(TDepth) # set zeros to maximum TDepth
  myK = 0.8 # m/d # piston velocity for atm exchange
  Scaler = 1/8 # 1/m # mixed layer depth
  EpiT = odem_stan$wtr_epi
  EpiT[which(EpiT==0)] = odem_stan$wtr_tot[which(EpiT==0)]
  o2sat = o2.at.sat.base(temp = EpiT, altitude = 480) * 1000
  o2def = myK * (o2sat - odem_stan$DO_epi) * 1/TDepth / 1000 # convert to mg/L
  
  o2defObs = myK * (o2sat - odem_stan$DO_obs_epi) * 1/TDepth / 1000

  epiMod = odem_stan$DO_epi
  epiObs = odem_stan$DO_obs_epi
  epiObs[!is.na(odem_stan$DO_obs_tot)] = odem_stan$DO_obs_tot[!is.na(odem_stan$DO_obs_tot)]
  hypoMod = odem_stan$DO_hyp
  hypoObs = odem_stan$DO_obs_hyp
  
  df.dots <- data.frame('datetime' = date_decimal(YearFrac[iYears]),
                        'simO2' = epiMod[iYears],
                        'satO2' = o2sat[iYears],
                        'obsO2' = epiObs[iYears],
                        'simFatm' =  odem_stan$Fatm[iYears]/1000,
                        'o2def' = o2def[iYears],
                        'obso2def' = o2defObs[iYears])
  
  df.dots$year = year(df.dots$datetime)
  df.dots$doy = yday(df.dots$datetime)
  

  
  o2defObs = myK * (o2sat - odem_stan$DO_obs_epi) * 1/TDepth / 1000

  odem.flux[[lake.id]] = data.frame(date = as.Date(odem_stan$datetime),
                                      'Atm' = Fatm,
                                      'Nep' = Fnep,
                                      'Min' = Fmin,
                                      'Sed' = Fsed,
                                      'FnepTot' = Fnep + Fmin - Fsed,
                                      'FAll' = Fatm + Fnep + Fmin + Fsed,
                                      'id' = lake.id,
                                      'lake' = lake.list.Abr[lake.id])[mindata:maxdata,]
  
  annual.flux[[lake.id]] = data.frame('year' = uYears[iYearsOnly],
                           'Atm' = yFatm[iYearsOnly],
                           'Nep' = yFnep[iYearsOnly],
                           'Min' = yFmin[iYearsOnly],
                           'Sed' = yFsed[iYearsOnly],
                           'Neptot' = yFnepTot[iYearsOnly],
                           'EntrEpi' = yFentrEpi[iYearsOnly],
                           'EntrHyp' = yFentrHypo[iYearsOnly],
                           'id' = lake.id)
  
  cum.flux[[lake.id]] = data.frame('time' = YearFrac[iYears],
                        'Atm' = FatmCum,
                        'Nep' = FnepCum,
                        'Min' = FminCum,
                        'Sed' = FsedCum,
                        'NepTot' = FnepTotCum,
                        'id' = lake.id)
  
  cum.flux_max[[lake.id]] = data.frame('time' = YearFrac[iYears],
                        'Atm_max' = FatmCum_max,
                        'Nep_max' = FnepCum_max,
                        'Min_max' = FminCum_max,
                        'Sed_max' = FsedCum_max,
                        'NepTot_max' = FnepTotCum_max,
                        'id' = lake.id)
  
  cum.flux_min[[lake.id]] = data.frame('time' = YearFrac[iYears],
                            'Atm_min' = FatmCum_min,
                            'Nep_min' = FnepCum_min,
                            'Min_min' = FminCum_min,
                            'Sed_min' = FsedCum_min,
                            'NepTot_min' = FnepTotCum_min,
                            'id' = lake.id)
  
  total.flux[[lake.id]] = data.frame('Atm' = signif(meanFatmC,3),
                          'Nep' = signif(meanFnepC,3),
                          'Min' = signif(meanFminC,3),
                          'Sed' = signif(meanFsedC,3),
                          'NepTot' = signif(meanFnepTotC,3),
                          'fit' = rmse,
                          'id' = lake.id,
                          'nse' =nse)
}

annual.flux = bind_rows(annual.flux)
cum.flux = bind_rows(cum.flux)
cum.flux_max = bind_rows(cum.flux_max)
cum.flux_min = bind_rows(cum.flux_min)
odem.flux = bind_rows(odem.flux)
total.flux= bind_rows(total.flux)

write_csv(odem.flux, 'Processed_Output/Fluxes_dailyFluxes.csv')
write_csv(cum.flux, 'Processed_Output/Fluxes_cumFlux.csv')
write_csv(cum.flux_min, 'Processed_Output/Fluxes_cumFluxmin.csv')
write_csv(cum.flux_max, 'Processed_Output/Fluxes_cumFluxmax.csv')
write_csv(annual.flux, 'Processed_Output/Fluxes_annualFluxes.csv')

### Decompose time series of nep ####
decompose.nep <- function(df.nep, name) {
  # Three decomposed objects: NEP, NEP_max, and NEP_min
  assign(paste0(name,'.nep'), decompose(ts(df.nep$Fnep - df.nep$Fsed + df.nep$Fmin, frequency = 365)), envir = .GlobalEnv)
  assign(paste0(name,'.nep_max'), decompose(ts(df.nep$fnep_upper - df.nep$fsed2_upper + df.nep$fmineral_upper, frequency = 365)), envir = .GlobalEnv)
  assign(paste0(name,'.nep_min'), decompose(ts(df.nep$fnep_lower - df.nep$fsed2_lower + df.nep$fmineral_lower, frequency = 365)), envir = .GlobalEnv)
}
decompose.nep(allequash, 'allequash')
decompose.nep(bigmuskellunge, 'bigmuskellunge')
decompose.nep(crystal, 'crystal')
decompose.nep(fish, 'fish')
decompose.nep(mendota, 'mendota')
decompose.nep(monona, 'monona')
decompose.nep(sparkling, 'sparkling')
decompose.nep(trout, 'trout')

# Create data frame of seasonal patterns 
seasonal.df = list()
cum.seasonal.df = list()
for (lake.id in lake.list) {
  name = tolower(lake.id)
  seasonal.df[[lake.id]] = data.frame(id = lake.id, NEP = get(paste0(name,'.nep'))$seasonal[1:366], 
             NEP_max = get(paste0(name,'.nep_max'))$seasonal[1:366], 
             NEP_min = get(paste0(name,'.nep_min'))$seasonal[1:366], 
             time = get(name)$datetime[1:366], volume = get(name)$volume_tot[1:366], area = get(name)$area_epi[1:366]) %>%
    mutate(yday = yday(time)) %>% 
    mutate(movavg = rollmean(NEP, k = 7, na.pad = TRUE)) %>% 
    mutate(movavg_max = rollmean(NEP_max, k = 7, na.pad = TRUE)) %>% 
    mutate(movavg_min = rollmean(NEP_min, k = 7, na.pad = TRUE)) %>% 
    mutate(loc = if_else(lake.id %in% c('Fish','Mendota','Monona','Wingra'), 'South', 'North'))
  
  day.df = seasonal.df[[lake.id]] %>% arrange(id, yday) # arrange from start of the year
  cum.seasonal.df[[lake.id]] = seasonal.df[[lake.id]] %>% arrange(id, yday) %>% # arrange from start of the year
    group_by(id) %>% 
    mutate(NEP = cumsum(NEP), NEP_max = cumsum(NEP_max), NEP_min = cumsum(NEP_min)) %>% 
    mutate(movavg = rollmean(NEP, k = 7, na.pad = TRUE)) %>% 
    mutate(movavg_max = rollmean(NEP_max, k = 7, na.pad = TRUE)) %>% 
    mutate(movavg_min = rollmean(NEP_min, k = 7, na.pad = TRUE))
}

seasonal.df = bind_rows(seasonal.df)
seasonal.df$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

cum.seasonal.df = bind_rows(cum.seasonal.df)
cum.seasonal.df$id <- factor(cum.seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

seasonal.df = seasonal.df %>%
  mutate(var_mean = movavg * volume/area/1000,
         var_max = movavg_max * volume/area/1000,
         var_min = movavg_min * volume/area/1000)

write_csv(seasonal.df, 'Processed_Output/NEP_seasonal.csv')
write_csv(cum.seasonal.df, 'Processed_Output/NEP_seasonal_cumulative.csv')

