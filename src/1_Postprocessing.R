cat('\f')
rm(list= ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(lubridate)
library(ggdark)
library(shades)
library(LakeMetabolizer)
library(RColorBrewer)
library(zoo)
library(lazyeval)
library(rstan)
library(LakeMetabolizer)
library(corrplot)
library(trend)
library(broom)
library(purrr)


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

interpolate_nearest.neighbor <- function(data){
  for (i in 1:ncol(data)){
    idx <- which(!is.na(data[,i]))
    for (j in seq(1, length(data[,i]))[!(seq(1, length(data[,i])) %in% idx)]){
      find.min <- which.min((idx-j)^2)
      data[j,i] = data[idx[find.min], i]
    }
  }
  return(data)
}


lake.list <- c('Allequash', 'BigMuskellunge', 'Crystal', 'Fish', 'Mendota',
               'Monona', 'Sparkling', 'Trout')

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
  NEP_mu_max = 0.5,#10000,
  NEP_sigma = 1e-32, #1000,#0.000001,
  SED1_mu_min = 0,
  SED1_mu_max = 1,#1500,
  SED1_sigma = 1e-32, #5000,
  MIN1_mu_min = 0,
  MIN1_mu_max = 1,#10000,
  MIN1_sigma = 1e-32, #1000,#0.000001,
  SED2_mu_min = 0,
  SED2_mu_max = 1,#1500,
  SED2_sigma = 1e-32, #5000,
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
  khalf = 500, # New, was 3000
  err_sigma = 0.0003,
  d = nrow(simdata),
  DO_epi_init = 15 * 1000, #simdata$DO_obs[1],
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






fair_cols <- c("#38170B","#BF1B0B", "#FFC465", "#66ADE5", "#252A52")



df.estimations[[match(lake.id, lake.list)]] <-ParamIndex

sigma.df =  dat_sigma[which(dat_sigma$name == 'smooth_sigma'),]
sigma.df$index = lake.id
sigma.df$neff = fit_clean$n_eff[which( fit_clean$name == 'smooth_sigma')]
sigma.df$rhat = fit_clean$Rhat[which( fit_clean$name == 'smooth_sigma')]
all.sigma <- rbind(all.sigma, sigma.df)

list.rhat[[match(lake.id, lake.list)]] = unique(fit_clean$name[which(fit_clean$Rhat > 1.1)])

setwd('../')
}


# brute-force loading of output

load('Allequash/Allequash_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
allequash <- odem_stan_filt
find.lake <- match('Allequash', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
allequash$SED <- NA
allequash$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
allequash$MIN <- NA
allequash$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
allequash$NEP <- NA
allequash$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]
write_csv(x = allequash, file = 'Processed_Output/allequash_fluxes.csv',col_names = T)

load('BigMuskellunge/BigMuskellunge_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
bigmuskellunge <- odem_stan_filt
find.lake <- match('BigMuskellunge', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
bigmuskellunge$SED <- NA
bigmuskellunge$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
bigmuskellunge$MIN <- NA
bigmuskellunge$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
bigmuskellunge$NEP <- NA
bigmuskellunge$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]

load('Crystal/Crystal_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
crystal <- odem_stan_filt
find.lake <- match('Crystal', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
crystal$SED <- NA
crystal$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
crystal$MIN <- NA
crystal$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
crystal$NEP <- NA
crystal$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]

load('Fish/Fish_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
fish <- odem_stan_filt
find.lake <- match('Fish', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
fish$SED <- NA
fish$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
fish$MIN <- NA
fish$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
fish$NEP <- NA
fish$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]


load('Mendota/Mendota_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
mendota = odem_stan_filt
find.lake <- match('Mendota', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
mendota$SED <- NA
mendota$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
mendota$MIN <- NA
mendota$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
mendota$NEP <- NA
mendota$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]
mendota$NEP_max <- NA
mendota$NEP_max[pos.lake] <- odem_stan$NEP_mgm3d_upper[min(estimat.lake):max(estimat.lake)]
mendota$NEP_min <- NA
mendota$NEP_min[pos.lake] <- odem_stan$NEP_mgm3d_lower[min(estimat.lake):max(estimat.lake)]
mendota$SED_max <- NA
mendota$SED_max[pos.lake] <- odem_stan$SED_mgm2d_upper[min(estimat.lake):max(estimat.lake)]
mendota$SED_min <- NA
mendota$SED_min[pos.lake] <- odem_stan$SED_mgm2d_lower[min(estimat.lake):max(estimat.lake)]
mendota$MIN_max <- NA
mendota$MIN_max[pos.lake] <- odem_stan$MIN_mgm3d_upper[min(estimat.lake):max(estimat.lake)]
mendota$MIN_min <- NA
mendota$MIN_min[pos.lake] <- odem_stan$MIN_mgm3d_lower[min(estimat.lake):max(estimat.lake)]



load('Monona/Monona_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
monona <- odem_stan_filt
find.lake <- match('Monona', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
monona$SED <- NA
monona$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
monona$MIN <- NA
monona$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
monona$NEP <- NA
monona$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]

load('Sparkling/Sparkling_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
sparkling <- odem_stan_filt
find.lake <- match('Sparkling', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
sparkling$SED <- NA
sparkling$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
sparkling$MIN <- NA
sparkling$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
sparkling$NEP <- NA
sparkling$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]

load('Trout/Trout_mineral.Rda')
mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
odem_stan_filt = odem_stan[mindata:maxdata,]
trout <- odem_stan_filt
find.lake <- match('Trout', lake.list)
estimat.lake <- df.estimations[[find.lake]]
estimat.lake <- estimat.lake[mindata:maxdata]
pos.lake <- which(duplicated(estimat.lake)== FALSE)
trout$SED <- NA
trout$SED[pos.lake] <- odem_stan$SED[min(estimat.lake):max(estimat.lake)]
trout$MIN <- NA
trout$MIN[pos.lake] <- odem_stan$MIN[min(estimat.lake):max(estimat.lake)]
trout$NEP <- NA
trout$NEP[pos.lake] <- odem_stan$NEP[min(estimat.lake):max(estimat.lake)]

write_csv(x = allequash, file = 'Processed_Output/allequash_fluxes.csv',col_names = T)
write_csv(x = bigmuskellunge, file = 'Processed_Output/bigmuskellunge_fluxes.csv',col_names = T)
write_csv(x = crystal, file = 'Processed_Output/crystal_fluxes.csv',col_names = T)
write_csv(x = fish, file = 'Processed_Output/fish_fluxes.csv',col_names = T)
write_csv(x = mendota, file = 'Processed_Output/mendota_fluxes.csv',col_names = T)
write_csv(x = monona, file = 'Processed_Output/monona_fluxes.csv',col_names = T)
write_csv(x = trout, file = 'Processed_Output/trout_fluxes.csv',col_names = T)
write_csv(x = sparkling, file = 'Processed_Output/sparkling_fluxes.csv',col_names = T)

fitdata = data.frame(obsdata <- as.numeric(c(allequash$DO_obs_epi[which(!is.na(allequash$DO_obs_epi))], allequash$DO_obs_hyp[which(!is.na(allequash$DO_obs_hyp))],
                                             bigmuskellunge$DO_obs_epi[which(!is.na(bigmuskellunge$DO_obs_epi))], bigmuskellunge$DO_obs_hyp[which(!is.na(bigmuskellunge$DO_obs_hyp))],
                                             crystal$DO_obs_epi[which(!is.na(crystal$DO_obs_epi))], crystal$DO_obs_hyp[which(!is.na(crystal$DO_obs_hyp))],
                                             fish$DO_obs_epi[which(!is.na(fish$DO_obs_epi))], fish$DO_obs_hyp[which(!is.na(fish$DO_obs_hyp))],
                                             mendota$DO_obs_epi[which(!is.na(mendota$DO_obs_epi))], mendota$DO_obs_hyp[which(!is.na(mendota$DO_obs_hyp))],
                                             monona$DO_obs_epi[which(!is.na(monona$DO_obs_epi))], monona$DO_obs_hyp[which(!is.na(monona$DO_obs_hyp))],
                                             sparkling$DO_obs_epi[which(!is.na(sparkling$DO_obs_epi))], sparkling$DO_obs_hyp[which(!is.na(sparkling$DO_obs_hyp))],
                                             trout$DO_obs_epi[which(!is.na(trout$DO_obs_epi))], trout$DO_obs_hyp[which(!is.na(trout$DO_obs_hyp))])),
                     moddata <- as.numeric(c(allequash$DO_epi[which(!is.na(allequash$DO_obs_epi))], allequash$DO_hyp[which(!is.na(allequash$DO_obs_hyp))],
                                             bigmuskellunge$DO_epi[which(!is.na(bigmuskellunge$DO_obs_epi))], bigmuskellunge$DO_hyp[which(!is.na(bigmuskellunge$DO_obs_hyp))],
                                             crystal$DO_epi[which(!is.na(crystal$DO_obs_epi))], crystal$DO_hyp[which(!is.na(crystal$DO_obs_hyp))],
                                             fish$DO_epi[which(!is.na(fish$DO_obs_epi))], fish$DO_hyp[which(!is.na(fish$DO_obs_hyp))],
                                             mendota$DO_epi[which(!is.na(mendota$DO_obs_epi))], mendota$DO_hyp[which(!is.na(mendota$DO_obs_hyp))],
                                             monona$DO_epi[which(!is.na(monona$DO_obs_epi))], monona$DO_hyp[which(!is.na(monona$DO_obs_hyp))],
                                             sparkling$DO_epi[which(!is.na(sparkling$DO_obs_epi))], sparkling$DO_hyp[which(!is.na(sparkling$DO_obs_hyp))],
                                             trout$DO_epi[which(!is.na(trout$DO_obs_epi))], trout$DO_hyp[which(!is.na(trout$DO_obs_hyp))])),
                     iddata <- c(rep('Allequash',length(c(allequash$DO_epi[which(!is.na(allequash$DO_obs_epi))], allequash$DO_hyp[which(!is.na(allequash$DO_obs_hyp))]))),
                                 rep('BigMuskellunge',length(c(bigmuskellunge$DO_epi[which(!is.na(bigmuskellunge$DO_obs_epi))], bigmuskellunge$DO_hyp[which(!is.na(bigmuskellunge$DO_obs_hyp))]))),
                                 rep('Crystal',length(c(crystal$DO_epi[which(!is.na(crystal$DO_obs_epi))], crystal$DO_hyp[which(!is.na(crystal$DO_obs_hyp))]))),
                                 rep('Fish',length(c(fish$DO_epi[which(!is.na(fish$DO_obs_epi))], fish$DO_hyp[which(!is.na(fish$DO_obs_hyp))]))),
                                 rep('Mendota',length(c(mendota$DO_epi[which(!is.na(mendota$DO_obs_epi))], mendota$DO_hyp[which(!is.na(mendota$DO_obs_hyp))]))),
                                 rep('Monona',length(c(monona$DO_epi[which(!is.na(monona$DO_obs_epi))], monona$DO_hyp[which(!is.na(monona$DO_obs_hyp))]))),
                                 rep('Sparkling',length(c(sparkling$DO_epi[which(!is.na(sparkling$DO_obs_epi))], sparkling$DO_hyp[which(!is.na(sparkling$DO_obs_hyp))]))),
                                 rep('Trout',length(c(trout$DO_epi[which(!is.na(trout$DO_obs_epi))], trout$DO_hyp[which(!is.na(trout$DO_obs_hyp))])))),
                     type <- c(rep('Epilimnion',length(c(allequash$DO_epi[which(!is.na(allequash$DO_obs_epi))]))),rep('Hypolimnion',length(allequash$DO_hyp[which(!is.na(allequash$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(bigmuskellunge$DO_epi[which(!is.na(bigmuskellunge$DO_obs_epi))]))),rep('Hypolimnion',length( bigmuskellunge$DO_hyp[which(!is.na(bigmuskellunge$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(crystal$DO_epi[which(!is.na(crystal$DO_obs_epi))]))),rep('Hypolimnion',length( crystal$DO_hyp[which(!is.na(crystal$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(fish$DO_epi[which(!is.na(fish$DO_obs_epi))]))),rep('Hypolimnion',length( fish$DO_hyp[which(!is.na(fish$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(mendota$DO_epi[which(!is.na(mendota$DO_obs_epi))]))),rep('Hypolimnion',length( mendota$DO_hyp[which(!is.na(mendota$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(monona$DO_epi[which(!is.na(monona$DO_obs_epi))]))),rep('Hypolimnion',length( monona$DO_hyp[which(!is.na(monona$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(sparkling$DO_epi[which(!is.na(sparkling$DO_obs_epi))]))),rep('Hypolimnion',length( sparkling$DO_hyp[which(!is.na(sparkling$DO_obs_hyp))])),
                               rep('Epilimnion',length(c(trout$DO_epi[which(!is.na(trout$DO_obs_epi))]))),rep('Hypolimnion',length( trout$DO_hyp[which(!is.na(trout$DO_obs_hyp))])))
)
colnames(fitdata) <-c('obsdata', 'simdata', 'id', 'type')

g.fits <- setNames(data.frame(matrix(ncol = 5, nrow = 8)), c('id', 'MAE', 'NSE', 'RMSE', 'R2'))
g.fits$id = unique(fitdata$id)
RMSE = fitdata %>%
  group_by(id) %>%
  summarise(RMSE = sqrt(sum((obsdata-simdata)**2)/length(obsdata)) * 1/1000)
MAE = fitdata %>%
  group_by(id) %>%
  summarise(MAE = sum(abs(obsdata-simdata))/length(obsdata) * 1/1000)
NSE = fitdata %>%
  group_by(id) %>%
  summarise(NSE = 1- sum((obsdata-simdata)**2)/sum((obsdata-mean(obsdata))**2))

R2 = fitdata %>%
  group_by(id) %>%
  summarise(r2 = cor(obsdata,simdata, method = 'pearson'))
g.fits$RMSE = RMSE$RMSE
g.fits$MAE = MAE$MAE
g.fits$NSE = NSE$NSE
g.fits$R2 = R2$r2
print(g.fits)

fitdata$RMSE =NA; fitdata$MAE =NA; fitdata$NSE =NA; fitdata$R2 =NA
for (i in unique(fitdata$id)){
  idx = which(i == fitdata$id)
  idy = which(i == g.fits$id)
  fitdata$RMSE[idx] = rep(g.fits$RMSE[idy], length(idx))
  fitdata$MAE[idx] = rep(g.fits$MAE[idy], length(idx))
  fitdata$NSE[idx] = rep(g.fits$NSE[idy], length(idx))
  fitdata$R2[idx] = rep(g.fits$R2[idy], length(idx))
}
fitdata=fitdata %>% 
  arrange(factor(id, levels = (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","","Fish","Mendota","Monona"))))
g.fits$id <- factor(g.fits$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))
fitdata$id <- factor(fitdata$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","","Fish","Mendota","Monona")))

g1 <- ggplot(fitdata, aes(x=obsdata/1000, y=simdata/1000, col = type))+
  geom_point(alpha = 0.5) +
  ylab(expression("Simulated DO conc. [g DO"*~m^{-3}*"]")) +
  xlab(expression("Observed DO conc. [g DO"*~m^{-3}*"]")) +
  # scale_color_brewer(palette="Dark2") +
  scale_color_manual(values = rep(c('red1','lightblue3'),1)) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 0.1, y = 12, label = paste0('MAE: ',round((MAE),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 4.3, y = 12, label =  paste0(', RMSE: ',round((RMSE),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 0.1, y = 10, label =  paste0('NSE: ',round((NSE),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 4.3, y = 10, label =  paste0(', R2: ',round((R2),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  facet_wrap(.~id, ncol = 3, drop = FALSE)+
  xlim(0,15.5)+ ylim(0,15.5)+
  theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom');g1

library(gridExtra)
library(grid)
g <- ggplotGrob(g1)
## remove empty panels
g$grobs[names(g$grobs) %in% c("panel3", "panel9", "strip_t3", "strip_t9")] <- NULL
## remove them from the layout
g$layout <- g$layout[!(g$layout$name %in% c("panel-3", "panel-9", 
                                            "strip_t-3", "strip_t-9")),]
## move axis closer to panel
g$layout[g$layout$name == "axis_b-9", c("t", "b")] = c(9,9)
grid.newpage()
grid.draw(g)

ggsave(file = 'Figures/Fig_3.png', g1, dpi = 300, width =250, height = 250,
       units='mm')


a=1
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
  Fatm = rep(NA,dim(odem_stan)[1])
  Fnep = rep(NA,dim(odem_stan)[1])
  FentrEpi = rep(NA,dim(odem_stan)[1])
  FentrHypo = rep(NA,dim(odem_stan)[1])
  Fmin = rep(NA,dim(odem_stan)[1])
  Fsed = rep(NA,dim(odem_stan)[1])
  
  Fatm_max = rep(NA,dim(odem_stan)[1])
  Fnep_max = rep(NA,dim(odem_stan)[1])
  FentrEpi_max = rep(NA,dim(odem_stan)[1])
  FentrHypo_max = rep(NA,dim(odem_stan)[1])
  Fmin_max = rep(NA,dim(odem_stan)[1])
  Fsed_max = rep(NA,dim(odem_stan)[1])
  
  Fatm_min = rep(NA,dim(odem_stan)[1])
  Fnep_min = rep(NA,dim(odem_stan)[1])
  FentrEpi_min = rep(NA,dim(odem_stan)[1])
  FentrHypo_min = rep(NA,dim(odem_stan)[1])
  Fmin_min = rep(NA,dim(odem_stan)[1])
  Fsed_min = rep(NA,dim(odem_stan)[1])
  
  #####################
  # Convert fluxes to areal (lake area)
  
  Fatm[iStrat] = odem_stan$Fatm[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  Fatm[iNotStrat] = odem_stan$Fatm[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  Fnep[iStrat] = odem_stan$Fnep[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  Fnep[iNotStrat] = odem_stan$Fnep[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  FentrEpi[iStrat] = odem_stan$Fentr1[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  FentrEpi[iNotStrat] = odem_stan$Fentr1[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  
  Fmin[iStrat] = odem_stan$Fmineral[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  Fmin[iNotStrat] = odem_stan$Fmineral[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  Fsed[iStrat] = odem_stan$Fsed[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  Fsed[iNotStrat] = odem_stan$Fsed[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  FentrHypo[iStrat] = odem_stan$Fentr2[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  FentrHypo[iNotStrat] = odem_stan$Fentr2[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  
  #max
  Fatm_max[iStrat] = odem_stan$fatm_upper[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  Fatm_max[iNotStrat] = odem_stan$fatm_upper[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  Fnep_max[iStrat] = odem_stan$fnep_upper[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  Fnep_max[iNotStrat] = odem_stan$fnep_upper[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  FentrEpi_max[iStrat] = odem_stan$fentr_epi_upper[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  FentrEpi_max[iNotStrat] = odem_stan$fentr_epi_upper[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  
  Fmin_max[iStrat] = odem_stan$fmineral_upper[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  Fmin_max[iNotStrat] = odem_stan$fmineral_upper[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  Fsed_max[iStrat] = odem_stan$fsed2_upper[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  Fsed_max[iNotStrat] = odem_stan$fsed2_upper[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  FentrHypo_max[iStrat] = odem_stan$fentr_hyp_upper[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  FentrHypo_max[iNotStrat] = odem_stan$fentr_hyp_upper[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  
  # min
  Fatm_min[iStrat] = odem_stan$fatm_lower[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  Fatm_min[iNotStrat] = odem_stan$fatm_lower[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  Fnep_min[iStrat] = odem_stan$fnep_lower[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  Fnep_min[iNotStrat] = odem_stan$fnep_lower[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  FentrEpi_min[iStrat] = odem_stan$fentr_epi_lower[iStrat]/1000*epiV[iStrat]/LakeArea[iStrat]
  FentrEpi_min[iNotStrat] = odem_stan$fentr_epi_lower[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  
  Fmin_min[iStrat] = odem_stan$fmineral_lower[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  Fmin_min[iNotStrat] = odem_stan$fmineral_lower[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  Fsed_min[iStrat] = odem_stan$fsed2_lower[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  Fsed_min[iNotStrat] = odem_stan$fsed2_lower[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  FentrHypo_min[iStrat] = odem_stan$fentr_hyp_lower[iStrat]/1000*hypoV[iStrat]/LakeArea[iStrat]
  FentrHypo_min[iNotStrat] = odem_stan$fentr_hyp_lower[iNotStrat]/1000*totalV[iNotStrat]/LakeArea[iNotStrat]
  
  plot(odem_stan$Fentr2 * odem_stan$volume_hyp+odem_stan$Fentr1 * odem_stan$volume_epi);grid()
  #####################
  # Calculate cumulatives
  
  # Calculate the yearly cumulative fluxes
  uYears = unique(Year)
  yFatm = rep(NA,length(uYears))
  yFnep = rep(NA,length(uYears))
  yFentrEpi = rep(NA,length(uYears))
  yFmin = rep(NA,length(uYears))
  yFsed = rep(NA,length(uYears))
  yFentrHypo = rep(NA,length(uYears))
  yFnepTot = rep(NA,length(uYears))

  yFatm_max = rep(NA,length(uYears))
  yFnep_max = rep(NA,length(uYears))
  yFentrEpi_max = rep(NA,length(uYears))
  yFmin_max = rep(NA,length(uYears))
  yFsed_max = rep(NA,length(uYears))
  yFentrHypo_max = rep(NA,length(uYears))
  yFnepTot_max = rep(NA,length(uYears))
  
  yFatm_min = rep(NA,length(uYears))
  yFnep_min = rep(NA,length(uYears))
  yFentrEpi_min = rep(NA,length(uYears))
  yFmin_min = rep(NA,length(uYears))
  yFsed_min = rep(NA,length(uYears))
  yFentrHypo_min = rep(NA,length(uYears))
  yFnepTot_min = rep(NA,length(uYears))
  for (i in 1:length(uYears)){
    thisYear = uYears[i]
    iYear = which(Year==thisYear)
    yFatm[i] = sum(Fatm[iYear])
    yFnep[i] =  sum(Fnep[iYear])
    yFentrEpi[i] =  sum(FentrEpi[iYear], na.rm = TRUE)
    yFmin[i] =  sum(Fmin[iYear])
    yFsed[i] =  sum(Fsed[iYear])
    yFentrHypo[i] =  sum(FentrHypo[iYear], na.rm = TRUE)
    yFnepTot[i] = yFnep[i]+yFmin[i]+yFsed[i]
    
    yFatm_max[i] = sum(Fatm_max[iYear])
    yFnep_max[i] =  sum(Fnep_max[iYear])
    yFentrEpi_max[i] =  sum(FentrEpi_max[iYear], na.rm = TRUE)
    yFmin_max[i] =  sum(Fmin_max[iYear])
    yFsed_max[i] =  sum(Fsed_max[iYear])
    yFentrHypo_max[i] =  sum(FentrHypo_max[iYear], na.rm = TRUE)
    yFnepTot_max[i] = yFnep_max[i]+yFmin_max[i]+yFsed_max[i]
    
    yFatm_min[i] = sum(Fatm_min[iYear])
    yFnep_min[i] =  sum(Fnep_min[iYear])
    yFentrEpi_min[i] =  sum(FentrEpi_min[iYear], na.rm = TRUE)
    yFmin_min[i] =  sum(Fmin_min[iYear])
    yFsed_min[i] =  sum(Fsed_min[iYear])
    yFentrHypo_min[i] =  sum(FentrHypo_min[iYear], na.rm = TRUE)
    yFnepTot_min[i] = yFnep_min[i]+yFmin_min[i]+yFsed_min[i]
  }
  
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
  
  #####################
  # Plots
  
  # For plotting, setup year fractions
  YearFrac = Year+(DayOfYear/366)
  
  # Plot Fatm and NEP time series
  plot(YearFrac[iYears],Fatm[iYears],type='l',
       xlab = 'Year',ylab = 'Flux (g/m2/d)')
  abline(h=0,lty=2)
  lines(YearFrac[iYears],Fnep[iYears],col='green',)
  lText = c("Fatm","epiNEP")
  legend('topleft',lText,cex=0.7,lty=c(1,1,1,1,1),lwd=c(1,1,1,1,1),
         col=c('black','green','red','blue','grey'))
  
  # Plot Fatm against NEP
  plot(Fnep[iYears],-Fatm[iYears],
       xlab = 'Fnep (g/m2/d)',ylab = '(-) Fatm (g/m2/d)')
  abline(a=0,b=1,lty=2)
  abline(lm(-Fatm[iYears]~Fnep[iYears]), col="red") # regression line (y~x)
  
  # Plot yearly total fluxes
  maxY = max(c(yFatm,yFnep,yFmin))
  myYLim = c(-maxY,maxY)
  plot(uYears[iYearsOnly],yFatm[iYearsOnly],type='l',ylim = myYLim,
       xlab = 'Year',ylab = 'Total yearly flux (g/m2/y)')
  abline(h=0,lty=2)
  lines(uYears[iYearsOnly],yFnep[iYearsOnly],col='green')
  lines(uYears[iYearsOnly],yFmin[iYearsOnly],col='red')
  lines(uYears[iYearsOnly],yFsed[iYearsOnly],col='blue')
  lines(uYears[iYearsOnly],yFnepTot[iYearsOnly],col='grey',lwd=2)
  lines(uYears[iYearsOnly],yFentrEpi[iYearsOnly],col='cyan',lwd=2)
  lines(uYears[iYearsOnly],yFentrHypo[iYearsOnly],col='violet',lwd=2)
  
  lText = c("Fatm","epiNEP","Min","Sed","totNEP",'Entr1','Entr2')
  legend('topleft',lText,cex=0.7,lty=c(1,1,1,1,1),lwd=c(1,1,1,1,1),
         col=c('black','green','red','blue','grey','cyan','violet'))
  
  # Plot total cumulative fluxes
  maxY = max(c(FatmCum,FnepCum,FminCum,FsedCum))
  myYLim = c(-maxY,maxY)
  plot(YearFrac[iYears],FatmCum,type='l',ylim = myYLim,
       xlab = 'Year',ylab = 'Cumultative flux (g/m2)')
  abline(h=0,lty=2)
  lines(YearFrac[iYears],FnepCum,col='green')
  lines(YearFrac[iYears],FminCum,col='red')
  lines(YearFrac[iYears],FsedCum,col='blue')
  lines(YearFrac[iYears],FnepTotCum,col='grey',lwd=2)
  
  grid(NA, 5, lwd = 1)
  
  lText = c("Fatm","epiNEP","Min","Sed","totNEP")
  legend('topleft',lText,cex=0.7,lty=c(1,1,1,1,1),lwd=c(1,1,1,1,1),
         col=c('black','green','red','blue','grey'))
  
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
  
  calc_fit <- function(mod_data, obs_data){
    obs <- obs_data #cbind(obs_data[3,], obs_data[4,])
    mod <- mod_data #cbind(input.values$o2_epil[proc.obs[1,]]/1000,
    #     input.values$o2_hypo[proc.obs[1,]]/1000)
    return (sqrt(mean((obs-mod)**2,na.rm = TRUE))) # RMSE
  }
  
  calc_nse <- function(mod_data, obs_data){
    obs <- obs_data #cbind(obs_data[3,], obs_data[4,])
    mod <- mod_data #cbind(input.values$o2_epil[proc.obs[1,]]/1000,
    #     input.values$o2_hypo[proc.obs[1,]]/1000)
    return (1-mean((mod-obs)**2,na.rm = TRUE)/mean((obs-mean(obs, na.rm=TRUE))**2,na.rm = TRUE)) # RMSE
  }
  
  rmse <- round(calc_fit(mod_data = cbind(odem_stan$DO_epi, odem_stan$DO_hyp), obs_data = cbind(odem_stan$DO_obs_epi, odem_stan$DO_obs_hyp))/1000
                ,2)
  
  nse <- round(calc_nse(mod_data = cbind(odem_stan$DO_epi, odem_stan$DO_hyp), obs_data = cbind(odem_stan$DO_obs_epi, odem_stan$DO_obs_hyp))
               ,2)
  
  
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
  plot(YearFrac[iYears], odem_stan$Fatm[iYears]/1000,type='l',ylim=c(-1,1),
       xlab='Day of sim',ylab='Atmospheric flux (mgO2/L/d)',main="")
  lines(YearFrac[iYears],o2def[iYears],col='red')
  points(YearFrac[iYears],o2defObs[iYears],col='blue')
  abline(h=0)
  legend('topright',col=c('black','red','blue'),lty=c(1,1,1),c('Modeled Fatm','Fatm, modeled O2 def scaled','Fatm, observed O2 def scaled'))
  
  
  # Plot observations and fits in model output units
  epiMod = odem_stan$DO_epi
  epiObs = odem_stan$DO_obs_epi
  epiObs[!is.na(odem_stan$DO_obs_tot)] = odem_stan$DO_obs_tot[!is.na(odem_stan$DO_obs_tot)]
  hypoMod = odem_stan$DO_hyp
  hypoObs = odem_stan$DO_obs_hyp
  
  plot(YearFrac[iYears],epiMod[iYears],type='l',col='red',
       xlab = 'Year fraction',ylab='DO (mg/m3)')
  points(YearFrac[iYears],epiObs[iYears],col='red')
  lines(YearFrac[iYears],hypoMod[iYears],col='green')
  points(YearFrac[iYears],hypoObs[iYears],col='green')
  legend('topright',col=c('red','green'),lty=c(1,1,1),c('Epi O2','Hypo O2'))
  
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
  plot(YearFrac[iYears], odem_stan$Fatm[iYears]/1000,type='l',ylim=c(-1,1),
       xlab='Day of sim',ylab='Atmospheric flux (mgO2/L/d)',main="")
  lines(YearFrac[iYears],o2def[iYears],col='red')
  points(YearFrac[iYears],o2defObs[iYears],col='blue')
  abline(h=0)
  legend('topright',col=c('black','red','blue'),lty=c(1,1,1),c('Modeled Fatm','Fatm, modeled O2 def scaled','Fatm, observed O2 def scaled'))
  
  
  # Plot time series of modeled, observed, and saturated epi DO
  plot(YearFrac[iYears],epiMod[iYears],ylim=c(6000,13000),type='l',col='red',
       xlab = 'Year fraction',ylab='DO (mg/m3)')
  lines(YearFrac[iYears],o2sat[iYears],type='l',col='blue')
  points(YearFrac[iYears],epiObs[iYears],col='red')
  legend('topright',col=c('red','red','blue'),lty=c(1,NaN,1),pch=c(NaN,21,NaN),c('Modeled O2','Obs O2','O2sat'))
  
  
  
  if (a==1){
    annual.flux = data.frame('year' = uYears[iYearsOnly],
                             'Atm' = yFatm[iYearsOnly],
                             'Nep' = yFnep[iYearsOnly],
                             'Min' = yFmin[iYearsOnly],
                             'Sed' = yFsed[iYearsOnly],
                             'Neptot' = yFnepTot[iYearsOnly],
                             'EntrEpi' = yFentrEpi[iYearsOnly],
                             'EntrHyp' = yFentrHypo[iYearsOnly],
                             'id' = lake.id)
    cum.flux = data.frame('time' = YearFrac[iYears],
                          'Atm' = FatmCum,
                          'Nep' = FnepCum,
                          'Min' = FminCum,
                          'Sed' = FsedCum,
                          'NepTot' = FnepTotCum,
                          'id' = lake.id)
    
    cum.flux_max = data.frame('time' = YearFrac[iYears],
                          'Atm_max' = FatmCum_max,
                          'Nep_max' = FnepCum_max,
                          'Min_max' = FminCum_max,
                          'Sed_max' = FsedCum_max,
                          'NepTot_max' = FnepTotCum_max,
                          'id' = lake.id)
    
    cum.flux_min = data.frame('time' = YearFrac[iYears],
                              'Atm_min' = FatmCum_min,
                              'Nep_min' = FnepCum_min,
                              'Min_min' = FminCum_min,
                              'Sed_min' = FsedCum_min,
                              'NepTot_min' = FnepTotCum_min,
                              'id' = lake.id)
    
    total.flux = data.frame('Atm' = signif(meanFatmC,3),
                            'Nep' = signif(meanFnepC,3),
                            'Min' = signif(meanFminC,3),
                            'Sed' = signif(meanFsedC,3),
                            'NepTot' = signif(meanFnepTotC,3),
                            'fit' = rmse,
                            'id' = lake.id,
                            'nse' =nse)
  } else {
    annual.flux = rbind(annual.flux, data.frame('year' = uYears[iYearsOnly],
                                                'Atm' = yFatm[iYearsOnly],
                                                'Nep' = yFnep[iYearsOnly],
                                                'Min' = yFmin[iYearsOnly],
                                                'Sed' = yFsed[iYearsOnly],
                                                'Neptot' = yFnepTot[iYearsOnly],
                                                'EntrEpi' = yFentrEpi[iYearsOnly],
                                                'EntrHyp' = yFentrHypo[iYearsOnly],
                                                'id' = lake.id))
    cum.flux = rbind(cum.flux, data.frame('time' = YearFrac[iYears],
                                          'Atm' = FatmCum,
                                          'Nep' = FnepCum,
                                          'Min' = FminCum,
                                          'Sed' = FsedCum,
                                          'NepTot' = FnepTotCum,
                                          'id' = lake.id))
    
    cum.flux_max = rbind(cum.flux_max, data.frame('time' = YearFrac[iYears],
                                                  'Atm_max' = FatmCum_max,
                                                  'Nep_max' = FnepCum_max,
                                                  'Min_max' = FminCum_max,
                                                  'Sed_max' = FsedCum_max,
                                                  'NepTot_max' = FnepTotCum_max,
                                                  'id' = lake.id))
    
    cum.flux_min = rbind(cum.flux_min, data.frame('time' = YearFrac[iYears],
                                                  'Atm_min' = FatmCum_min,
                                                  'Nep_min' = FnepCum_min,
                                                  'Min_min' = FminCum_min,
                                                  'Sed_min' = FsedCum_min,
                                                  'NepTot_min' = FnepTotCum_min,
                                                  'id' = lake.id))
    
    total.flux = rbind(total.flux, data.frame('Atm' = signif(meanFatmC,3),
                                              'Nep' = signif(meanFnepC,3),
                                              'Min' = signif(meanFminC,3),
                                              'Sed' = signif(meanFsedC,3),
                                              'NepTot' = signif(meanFnepTotC,3),
                                              'fit' = rmse,
                                              'id' = lake.id,
                                              'nse' = nse))
  }
  a=a+1
}


library(patchwork)
total.flux$TP = c(15.7, 8.6, 5.6, 22.4, 109.5, 73.5, 7.2, 6.9)#, 40.3)

name.chane.annual.flux <- annual.flux
colnames(name.chane.annual.flux) <- c('Year', 'ATM', 'NEP_epi', 'NEP_hypo', 'SED', 'NEP_tot', 'ENTR_epi',
                                      'ENTR_hypo', 'ID')


colnames(cum.flux) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
cum.flux$Sed = cum.flux$Sed * (-1)
m.cum.flux =reshape2::melt(cum.flux[,-c(1)])
m.cum.flux$time = cum.flux$time

colnames(cum.flux_max) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
cum.flux_max$Sed = cum.flux_max$Sed * (-1)
m.cum.flux_max =reshape2::melt(cum.flux_max[,-c(1)])
m.cum.flux_max$time = cum.flux_max$time

colnames(cum.flux_min) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
cum.flux_min$Sed = cum.flux_min$Sed * (-1)
m.cum.flux_min =reshape2::melt(cum.flux_min[,-c(1)])
m.cum.flux_min$time = cum.flux_min$time



m.cum.flux.extr = m.cum.flux_min
colnames(m.cum.flux.extr) = c('id', 'variable', 'min', 'time')
m.cum.flux.extr$max = m.cum.flux_max$value
m.cum.flux.extr$value = m.cum.flux$value

idx.year = c()
for (i in seq(1996,2019,1)){
  idy = which(m.cum.flux.extr$time > i)
  idz = match(m.cum.flux.extr$time[idy[which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i)))]], 
              m.cum.flux.extr$time)
  idx.year = append(idx.year,
                    idy[which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i)))])
  print(which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i))))
  if (length(which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i)))) != 40){
    print(i)
  }
}
red.m.cum.flux.extr = m.cum.flux.extr[idx.year,]

g1.north=ggplot(subset(m.cum.flux, id == c('Allequash', 
                                           'BigMuskellunge',
                                           'Crystal',
                                           'Sparkling',
                                           'Trout')), aes(time, value/1000, col = variable, linetype = variable)) +
  geom_line(size=1.5) +

  ylim(c(-8,10))+
  # ylim(c(-20,20))+
  facet_wrap(~id) +
  scale_linetype_manual(values = c(1,1,1,1,2))+
  ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  xlab('') +
  scale_color_brewer(palette="Set1") +
  theme_minimal()+
  theme(legend.text = element_text(size = 30), axis.text.x= element_text(size = 30, angle = 90, vjust = 0.5, hjust=1), plot.title = element_text(size = 30),
        axis.text.y= element_text(size = 30), text = element_text(size = 30), legend.title = element_blank(), strip.text =element_text(size = 30),
        legend.position = 'none');g1.north
g1.south=ggplot(subset(m.cum.flux, id == c('Fish',
                                           'Mendota',
                                           'Monona')), aes(time, value/1000, col = variable, linetype = variable)) +
  geom_line(size=1.5) +
  facet_wrap(~id) +
 ylim(c(-8,10))+
  # ylim(c(-25,25))+
  # ylim(c(-8,10))+
  scale_linetype_manual(values = c(1,1,1,1,2))+
  ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  xlab('') +
  scale_color_brewer(palette="Set1") +
  theme_minimal()+
  theme(legend.text = element_text(size = 30), axis.text.x= element_text(size = 30, angle = 90, vjust = 0.5, hjust=1), plot.title = element_text(size = 30),
        axis.text.y= element_text(size = 30), text = element_text(size = 30), legend.title = element_blank(), strip.text =element_text(size = 30),
        legend.position = 'bottom');g1.south

g1 <- g1.north / g1.south+
  plot_annotation(tag_levels = 'A')+
  plot_layout(heights = (c(2, 1)));g1
ggsave(file = 'Figures/Fig_4.png', g1, dpi = 300, width =450, height = 350,
       units='mm')




allequash.nep = ts(allequash$Fnep -allequash$Fsed +allequash$Fmin, frequency = 365)
allequash.nep = decompose(allequash.nep)
bigmuskellunge.nep = ts(bigmuskellunge$Fnep- bigmuskellunge$Fsed +bigmuskellunge$Fmin, frequency = 365)
bigmuskellunge.nep = decompose(bigmuskellunge.nep)
crystal.nep = ts(crystal$Fnep -crystal$Fsed +crystal$Fmin, frequency = 365)
crystal.nep = decompose(crystal.nep)
fish.nep = ts(fish$Fnep-fish$Fsed +fish$Fmin, frequency = 365)
fish.nep = decompose(fish.nep)
mendota.nep = ts(mendota$Fnep- mendota$Fsed +mendota$Fmin, frequency = 365)
mendota.nep = decompose(mendota.nep)
monona.nep = ts(monona$Fnep -monona$Fsed +monona$Fmin, frequency = 365)
monona.nep = decompose(monona.nep)
sparkling.nep = ts(sparkling$Fnep -sparkling$Fsed +sparkling$Fmin, frequency = 365)
sparkling.nep = decompose(sparkling.nep)
trout.nep = ts(trout$Fnep - trout$Fsed +trout$Fmin, frequency = 365)
trout.nep = decompose(trout.nep)

allequash.nep_max = ts(allequash$fnep_upper -allequash$fsed2_upper +allequash$fmineral_upper, frequency = 365)
allequash.nep_max = decompose(allequash.nep_max)
bigmuskellunge.nep_max = ts(bigmuskellunge$fnep_upper- bigmuskellunge$fsed2_upper +bigmuskellunge$fmineral_upper, frequency = 365)
bigmuskellunge.nep_max = decompose(bigmuskellunge.nep_max)
crystal.nep_max = ts(crystal$fnep_upper -crystal$fsed2_upper +crystal$fmineral_upper, frequency = 365)
crystal.nep_max = decompose(crystal.nep_max)
fish.nep_max = ts(fish$fnep_upper-fish$fsed2_upper +fish$fmineral_upper, frequency = 365)
fish.nep_max = decompose(fish.nep_max)
mendota.nep_max = ts(mendota$fnep_upper- mendota$fsed2_upper +mendota$fmineral_upper, frequency = 365)
mendota.nep_max = decompose(mendota.nep_max)
monona.nep_max = ts(monona$fnep_upper -monona$fsed2_upper +monona$fmineral_upper, frequency = 365)
monona.nep_max = decompose(monona.nep_max)
sparkling.nep_max = ts(sparkling$fnep_upper -sparkling$fsed2_upper +sparkling$fmineral_upper, frequency = 365)
sparkling.nep_max = decompose(sparkling.nep_max)
trout.nep_max = ts(trout$fnep_upper - trout$fsed2_upper +trout$fmineral_upper, frequency = 365)
trout.nep_max = decompose(trout.nep_max)

allequash.nep_min = ts(allequash$fnep_lower -allequash$fsed2_lower +allequash$fmineral_lower, frequency = 365)
allequash.nep_min = decompose(allequash.nep_min)
bigmuskellunge.nep_min = ts(bigmuskellunge$fnep_lower- bigmuskellunge$fsed2_lower +bigmuskellunge$fmineral_lower, frequency = 365)
bigmuskellunge.nep_min = decompose(bigmuskellunge.nep_min)
crystal.nep_min = ts(crystal$fnep_lower -crystal$fsed2_lower +crystal$fmineral_lower, frequency = 365)
crystal.nep_min = decompose(crystal.nep_min)
fish.nep_min = ts(fish$fnep_lower-fish$fsed2_lower +fish$fmineral_lower, frequency = 365)
fish.nep_min = decompose(fish.nep_min)
mendota.nep_min = ts(mendota$fnep_lower- mendota$fsed2_lower +mendota$fmineral_lower, frequency = 365)
mendota.nep_min = decompose(mendota.nep_min)
monona.nep_min = ts(monona$fnep_lower -monona$fsed2_lower +monona$fmineral_lower, frequency = 365)
monona.nep_min = decompose(monona.nep_min)
sparkling.nep_min = ts(sparkling$fnep_lower -sparkling$fsed2_lower +sparkling$fmineral_lower, frequency = 365)
sparkling.nep_min = decompose(sparkling.nep_min)
trout.nep_min = ts(trout$fnep_lower - trout$fsed2_lower +trout$fmineral_lower, frequency = 365)
trout.nep_min = decompose(trout.nep_min)
# wingra.nep = ts(wingra$Fnep + wingra$Fsed +wingra$Fmin, frequency = 365)
# wingra.nep = decompose(wingra.nep)
seasonal.df = data.frame('id' = rep(lake.list,each=366), 'NEP' = c(allequash.nep$seasonal[1:366],
                                                                   bigmuskellunge.nep$seasonal[1:366],crystal.nep$seasonal[1:366],fish.nep$seasonal[1:366],
                                                                   mendota.nep$seasonal[1:366],monona.nep$seasonal[1:366],sparkling.nep$seasonal[1:366],
                                                                   trout.nep$seasonal[1:366]),#wingra.nep$seasonal[1:366]),
                         'NEP_max' = c(allequash.nep_max$seasonal[1:366],
                                   bigmuskellunge.nep_max$seasonal[1:366],crystal.nep_max$seasonal[1:366],fish.nep_max$seasonal[1:366],
                                   mendota.nep_max$seasonal[1:366],monona.nep_max$seasonal[1:366],sparkling.nep_max$seasonal[1:366],
                                   trout.nep_max$seasonal[1:366]),#wingra.nep_max$seasonal[1:366]),
                         'NEP_min' = c(allequash.nep_min$seasonal[1:366],
                                   bigmuskellunge.nep_min$seasonal[1:366],crystal.nep_min$seasonal[1:366],fish.nep_min$seasonal[1:366],
                                   mendota.nep_min$seasonal[1:366],monona.nep_min$seasonal[1:366],sparkling.nep_min$seasonal[1:366],
                                   trout.nep_min$seasonal[1:366]),#wingra.nep_min$seasonal[1:366]),
                         'time' = c(yday(allequash$datetime[1:366]),yday(bigmuskellunge$datetime[1:366]),yday(crystal$datetime[1:366]),
                                    yday(fish$datetime[1:366]),yday(mendota$datetime[1:366]),yday(monona$datetime[1:366]),
                                    yday(sparkling$datetime[1:366]),yday(trout$datetime[1:366])),#yday(wingra$datetime[1:366])),
                         'volume' = rep(c(allequash$volume_tot[1],bigmuskellunge$volume_tot[1],crystal$volume_tot[1],
                                          fish$volume_tot[1],mendota$volume_tot[1],monona$volume_tot[1],
                                          sparkling$volume_tot[1],trout$volume_tot[1]),each=366),#,wingra$volume_total[1]
                         'area' = rep(c(allequash$area_epi[1],bigmuskellunge$area_epi[1],crystal$area_epi[1],
                                        fish$area_epi[1],mendota$area_epi[1],monona$area_epi[1],
                                        sparkling$area_epi[1],trout$area_epi[1]),each=366),#,wingra$area_surface[1]
                         'movavg' =c(rollmean(allequash.nep$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(bigmuskellunge.nep$seasonal[1:366], 7, na.pad=TRUE),rollmean(crystal.nep$seasonal[1:366], 7, na.pad=TRUE),rollmean(fish.nep$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(mendota.nep$seasonal[1:366], 7, na.pad=TRUE),rollmean(monona.nep$seasonal[1:366], 7, na.pad=TRUE),rollmean(sparkling.nep$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(trout.nep$seasonal[1:366], 7, na.pad = TRUE)),
                         'movavg_max' =c(rollmean(allequash.nep_max$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(bigmuskellunge.nep_max$seasonal[1:366], 7, na.pad=TRUE),rollmean(crystal.nep_max$seasonal[1:366], 7, na.pad=TRUE),rollmean(fish.nep_max$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(mendota.nep_max$seasonal[1:366], 7, na.pad=TRUE),rollmean(monona.nep_max$seasonal[1:366], 7, na.pad=TRUE),rollmean(sparkling.nep_max$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(trout.nep_max$seasonal[1:366], 7, na.pad = TRUE)),
                         'movavg_min' =c(rollmean(allequash.nep_min$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(bigmuskellunge.nep_min$seasonal[1:366], 7, na.pad=TRUE),rollmean(crystal.nep_min$seasonal[1:366], 7, na.pad=TRUE),rollmean(fish.nep_min$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(mendota.nep_min$seasonal[1:366], 7, na.pad=TRUE),rollmean(monona.nep_min$seasonal[1:366], 7, na.pad=TRUE),rollmean(sparkling.nep_min$seasonal[1:366], 7, na.pad=TRUE),
                                     rollmean(trout.nep_min$seasonal[1:366], 7, na.pad = TRUE))
                         )#,rollmean(wingra.nep_min$seasonal[1:366], 7, na.pad=TRUE)))#rep(seq(1,366),9))
seasonal.df$loc = 'North'
seasonal.df$loc[seasonal.df$id %in% c('Fish','Mendota','Monona','Wingra')] = 'South'


seasonal.df$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))
seasonal.df = seasonal.df %>%
  mutate(var_mean = movavg*volume/area/1000,
         var_max = movavg_max * volume/area/1000,
         var_min = movavg_min * volume/area/1000)

display.brewer.pal(name = 8, name = 'Dark2')
g1=ggplot(seasonal.df[!is.na(seasonal.df$movavg),], aes(time, (movavg*volume/area/1000), col = id)) +
  geom_ribbon(aes(x=time, ymin = movavg_min * volume/area/1000, ymax = movavg_max * volume/area/1000, col = id), size = 0.3, alpha = 0.1)+
  geom_line(aes(linetype=id, col = id), size = 1)+
 # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression("Filtered seasonal total NEP [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  xlab('Day of the year') +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,2,2,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides();g1#linetype = FALSE



cum.seasonal.df = data.frame('id' = rep(lake.list,each=366), 'NEP' = c(cumsum(allequash.nep$seasonal[1:366][order(yday(allequash$datetime[1:366]))]),
                                                                       cumsum(bigmuskellunge.nep$seasonal[1:366][order(yday(bigmuskellunge$datetime[1:366]))]),cumsum(crystal.nep$seasonal[1:366][order(yday(crystal$datetime[1:366]))]),
                                                                       cumsum(fish.nep$seasonal[1:366][order(yday(fish$datetime[1:366]))]),
                                                                              cumsum(mendota.nep$seasonal[1:366][order(yday(mendota$datetime[1:366]))]),cumsum(monona.nep$seasonal[1:366][order(yday(monona$datetime[1:366]))]),
                                                                       cumsum(sparkling.nep$seasonal[1:366][order(yday(sparkling$datetime[1:366]))]),
                                                                   trout.nep$seasonal[1:366][order(yday(trout$datetime[1:366]))]),#wingra.nep$seasonal[1:366]),
                             'NEP_max' = c(cumsum(allequash.nep_max$seasonal[1:366][order(yday(allequash$datetime[1:366]))]),
                                       cumsum(bigmuskellunge.nep_max$seasonal[1:366][order(yday(bigmuskellunge$datetime[1:366]))]),cumsum(crystal.nep_max$seasonal[1:366][order(yday(crystal$datetime[1:366]))]),
                                       cumsum(fish.nep_max$seasonal[1:366][order(yday(fish$datetime[1:366]))]),
                                       cumsum(mendota.nep_max$seasonal[1:366][order(yday(mendota$datetime[1:366]))]),cumsum(monona.nep_max$seasonal[1:366][order(yday(monona$datetime[1:366]))]),
                                       cumsum(sparkling.nep_max$seasonal[1:366][order(yday(sparkling$datetime[1:366]))]),
                                       trout.nep_max$seasonal[1:366][order(yday(trout$datetime[1:366]))]),
                             'NEP_min' = c(cumsum(allequash.nep_min$seasonal[1:366][order(yday(allequash$datetime[1:366]))]),
                                           cumsum(bigmuskellunge.nep_min$seasonal[1:366][order(yday(bigmuskellunge$datetime[1:366]))]),cumsum(crystal.nep_min$seasonal[1:366][order(yday(crystal$datetime[1:366]))]),
                                           cumsum(fish.nep_min$seasonal[1:366][order(yday(fish$datetime[1:366]))]),
                                           cumsum(mendota.nep_min$seasonal[1:366][order(yday(mendota$datetime[1:366]))]),cumsum(monona.nep_min$seasonal[1:366][order(yday(monona$datetime[1:366]))]),
                                           cumsum(sparkling.nep_min$seasonal[1:366][order(yday(sparkling$datetime[1:366]))]),
                                           trout.nep_min$seasonal[1:366][order(yday(trout$datetime[1:366]))]),
                             
                         'time' = c(yday(allequash$datetime[1:366][order(yday(allequash$datetime[1:366]))]),
                                    yday(bigmuskellunge$datetime[1:366][order(yday(bigmuskellunge$datetime[1:366]))]),
                                    yday(crystal$datetime[1:366][order(yday(crystal$datetime[1:366]))]),
                                    yday(fish$datetime[1:366][order(yday(fish$datetime[1:366]))])
                                    ,yday(mendota$datetime[1:366][order(yday(mendota$datetime[1:366]))]),
                                    yday(monona$datetime[1:366][order(yday(monona$datetime[1:366]))]),
                                    yday(sparkling$datetime[1:366][order(yday(sparkling$datetime[1:366]))]),
                                    yday(trout$datetime[1:366][order(yday(trout$datetime[1:366]))])),#yday(wingra$datetime[1:366])),
                         'volume' = rep(c(allequash$volume_tot[1],bigmuskellunge$volume_tot[1],crystal$volume_tot[1],
                                          fish$volume_tot[1],mendota$volume_tot[1],monona$volume_tot[1],
                                          sparkling$volume_tot[1],trout$volume_tot[1]),each=366),#,wingra$volume_total[1]
                         'area' = rep(c(allequash$area_epi[1],bigmuskellunge$area_epi[1],crystal$area_epi[1],
                                        fish$area_epi[1],mendota$area_epi[1],monona$area_epi[1],
                                        sparkling$area_epi[1],trout$area_epi[1]),each=366),#,wingra$area_surface[1]
                         'movavg' =c(rollmean(cumsum(allequash.nep$seasonal[1:366][order(yday(allequash$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(bigmuskellunge.nep$seasonal[1:366][order(yday(bigmuskellunge$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(crystal.nep$seasonal[1:366][order(yday(crystal$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(fish.nep$seasonal[1:366][order(yday(fish$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(mendota.nep$seasonal[1:366][order(yday(mendota$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(monona.nep$seasonal[1:366][order(yday(monona$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(sparkling.nep$seasonal[1:366][order(yday(sparkling$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(trout.nep$seasonal[1:366][order(yday(trout$datetime[1:366]))]), 7, na.pad = TRUE)),
                         'movavg_max' =c(rollmean(cumsum(allequash.nep_max$seasonal[1:366][order(yday(allequash$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(bigmuskellunge.nep_max$seasonal[1:366][order(yday(bigmuskellunge$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(crystal.nep_max$seasonal[1:366][order(yday(crystal$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(fish.nep_max$seasonal[1:366][order(yday(fish$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(mendota.nep_max$seasonal[1:366][order(yday(mendota$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(monona.nep_max$seasonal[1:366][order(yday(monona$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(sparkling.nep_max$seasonal[1:366][order(yday(sparkling$datetime[1:366]))]), 7, na.pad=TRUE),
                                     rollmean(cumsum(trout.nep_max$seasonal[1:366][order(yday(trout$datetime[1:366]))]), 7, na.pad = TRUE)),
                         'movavg_min' =c(rollmean(cumsum(allequash.nep_min$seasonal[1:366][order(yday(allequash$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(bigmuskellunge.nep_min$seasonal[1:366][order(yday(bigmuskellunge$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(crystal.nep_min$seasonal[1:366][order(yday(crystal$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(fish.nep_min$seasonal[1:366][order(yday(fish$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(mendota.nep_min$seasonal[1:366][order(yday(mendota$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(monona.nep_min$seasonal[1:366][order(yday(monona$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(sparkling.nep_min$seasonal[1:366][order(yday(sparkling$datetime[1:366]))]), 7, na.pad=TRUE),
                                         rollmean(cumsum(trout.nep_min$seasonal[1:366][order(yday(trout$datetime[1:366]))]), 7, na.pad = TRUE))
                         )#,rollmean(wingra.nep_max$seasonal[1:366], 7, na.pad=TRUE)))#rep(seq(1,366),9))
cum.seasonal.df$loc = 'North'
cum.seasonal.df$loc[seasonal.df$id %in% c('Fish','Mendota','Monona','Wingra')] = 'South'
cum.seasonal.df$id <- factor(cum.seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g2=ggplot(cum.seasonal.df, aes(time, movavg*volume/area/1000, col = id)) +
  geom_ribbon(aes(x=time, ymin = movavg_min * volume/area/1000, ymax = movavg_max * volume/area/1000, col = id), size = 0.3, alpha = 0.1)+
  geom_line(aes(linetype=id, col = id), size = 1)+
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +

  ylab(expression("Filtered cum. sum total NEP [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  xlab('Day of the year') +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,2,2,2))+#scale_linetype_manual(values = c(1,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides();g2


g3 = g1 / g2   + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect') &
  theme(legend.position='bottom');g3
ggsave(file = 'Figures/Fig_5.png', g3, dpi = 300, width =300, height = 250,
       units='mm')

# Clustering

anoxDym <- list()
anoxLab <- c()
loc = factor(c('north','north','north','south','south','south','north','north','south'))
a=1
for (i in lake.list){
  load(paste0(i,'/',i,'_mineral.Rda'))# load('Allequash/Allequash.Rda')
  mindata = min(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
  maxdata = max(which(!is.na(odem_stan$DO_obs_tot)),which(!is.na(odem_stan$DO_obs_epi)))
  odem_stan = odem_stan[mindata:maxdata,]
  data <- odem_stan
  if (loc[which(i %in% lake.list)] == 'north'){
    elev = 450
  } else {
    elev = 300
  }
  
  for (an in unique(data$year)){
    dataAnn = data[ which(data$year %in% an),]
    dataStrat = dataAnn[which(dataAnn$strat == 1),]
    dataStrat$Sat_hypo <- o2.at.sat.base(temp = dataStrat$wtr_hyp, altitude = elev) * 1000
    dataStrat$dist <- (dataStrat$doy - min(dataStrat$doy)) / (max(dataStrat$doy) - min(dataStrat$doy))
    dataStrat$normalDO <- dataStrat$DO_hyp / dataStrat$Sat_hypo
    scaledDOdiff <- approx(dataStrat$dist, dataStrat$normalDO , seq(0,1,0.01))
    
    anoxDym[[a]] = scaledDOdiff$y
    a=a+1
    anoxLab <- append(anoxLab, paste0(i,'_',an))
  }
}

library(dtw)
anoxDym2 = lapply(anoxDym,rollmean,k = 10)

mydata = (as.matrix(as.data.frame(anoxDym2)))
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type='b', xlab='Number of Clusters',
     ylab='Within groups sum of squares')
distMatrix <- dist(anoxDym2, method= 'euclidean')
# hc <- hclust(distMatrix, method='average')
hc <- hclust(distMatrix, method='ward.D')
plot(hc, main='')
groups <- cutree(hc, k=5) #k=5) # cut tree into 7 clusters
# draw dendogram with red borders around the 7 clusters
rect.hclust(hc, k=5,border='red')#k=7
indMycl = unique(groups)
dataGroups = list()
idz = as.numeric(table(groups))
for (i in 1:length(indMycl)){
  idy = which(groups == i)
  data = anoxDym2[idy]
  
  data.df = as.data.frame(do.call(cbind, data)) 
  dataGroups[[i]] = apply(data.df,1, mean)
  
  data.long = data.df %>% 
    mutate(x = 1:92) %>% 
    pivot_longer(-x) 
  
  # Individual Cluster Plots
  p1 = ggplot(data.long, aes(x, value, colour=name)) +
    geom_line() + 
    theme(legend.position = "none") +
    labs(title = paste0('Cluster = ',i,' n = ',idz[i]))
  print(p1)
}

df = as.data.frame(dataGroups)
names(df) = c('Anoxic','Convex','Hypoxic','Linear','Concave')

nameVec = names(df)
df$depth = seq(1,nrow(df))
table(groups)
lakeinv <- nameVec[unique(which(table(groups) > 0))]# >5

table(groups)[1]

# Pivot wide for plotting 
df.long = df %>% 
  dplyr::select(lakeinv, depth) %>% 
  pivot_longer(lakeinv) %>% 
  # mutate(name = fct_relevel(name,'Anoxic','Hypoxic','Linear','Convex','Concave')) 
  mutate(name = fct_relevel(name,'Concave','Linear','Convex','Hypoxic','Anoxic')) 
# mutate(name = fct_relevel(name,'Concave','Linear','Convex'))#,'Hypoxic','Anoxic')) 

# Cluster lables 
cluster.labels = NA
# order = match(lakeinv, c('Hypoxic','Linear','Convex')) # Order in the same way as 
order = match(lakeinv, c('Concave','Linear','Convex','Hypoxic','Anoxic')) # Order in the same way as
for (i in 1:5){
  j = order[i]
  cluster.labels[j] = paste0(lakeinv[i],' (n = ',table(groups)[i],')')
}

# Cluster plotting 
g.cluster = ggplot(df.long) +
  geom_line(aes(depth, value, color = name)) +
  scale_color_manual(values = c('lightblue3','lightblue1','gold','red1','red4'), name = 'Cluster',
                     labels = cluster.labels) +
  xlab('Stratification duration [%]') +
  ylab('Hypo/Sat DO conc. [-]') +
  theme_minimal(base_size = 8); g.cluster

# Grid Plot
df.grd <-  setNames(data.frame(matrix(ncol = 1+length(seq(1979, 2018,1)), nrow = 8)), c('lake',
                                                                                        as.character(seq(1979,2018,1))))
df.grd$lake <- lake.list

for (i in 1:5) {
  dff = anoxLab[which(groups == i)]
  name = lakeinv[i]
  dmlst <- lake.list[!is.na(match(lake.list, unlist(strsplit(dff, '_'))))]
  for (j in dmlst){
    xrow = which(df.grd$lake == j)
    whpatt = grep(j, dff)
    whyrs = gsub(".*_","",dff[grep(j, dff)])
    df.grd[xrow, !is.na(match(colnames(df.grd),whyrs))] <- name
  }
}

m.df.grd <- reshape2::melt(df.grd, id.vars = 'lake')
m.df.grd$lake <- factor(m.df.grd$lake, levels= rev(c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))
g1 <- ggplot(m.df.grd, aes(x = variable, y = lake, fill = as.factor(value))) + 
  scale_fill_manual(values = c('lightblue3','lightblue1','gold','red1','red4'), name = 'Cluster', 
                    breaks = c('Concave','Linear','Convex','Hypoxic','Anoxic')) +
  geom_tile(color = 'black', width = 0.8, height = 0.8, size = 0.5) +
  labs(x = 'Time', y = '') +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank()); g1

g <- g.cluster / g1 + plot_layout(heights = c(1.5,2))  + plot_annotation(tag_levels = 'A');g
# ggsave('cluster_hd.png', width = 6, height = 4, dpi = 500)

# g <- g.cluster / g1 + plot_annotation(tag_levels = 'A') + plot_layout(heights = c(1.5,2)); g
# ggsave('cluster_hd.png', width = 6, height = 6, dpi = 500)
ggsave(file = 'Figures/Fig_7.png', g, dpi = 500, width =6, height = 4)


### example plot

g.conc <- ggplot(subset(mendota, year > 2002 & year < 2006)) + 
  geom_line(aes(x=datetime, y=o2_epi_middle/1000, col = 'Epilimnion sim.')) +
  geom_ribbon(aes(x=datetime, ymin=o2_epi_lower/1000, ymax=o2_epi_upper/1000, col = 'Epilimnion sim.'),linetype =2, alpha=0.2) +
  geom_point(aes(x=datetime, y=DO_obs_epi/1000, col = 'Epilimnion obs.'), size =2) +
  geom_line(aes(x=datetime, y=o2_hyp_middle/1000, col ='Hypolimnion sim.')) +
  geom_ribbon(aes(x=datetime, ymin=o2_hyp_lower/1000, ymax=o2_hyp_upper/1000, col = 'Hypolimnion sim.'),linetype =2, alpha=0.2) +
  geom_point(aes(x=datetime, y=DO_obs_hyp/1000, col= 'Hypolimnion obs.'), size =2) +
  geom_point(aes(x=datetime, y=DO_obs_tot/1000, col = 'Total obs.'), size=2) +
  # facet_wrap(~ year) +
  ylab(expression("Conc. [g DO"*~m^{-3}*"]")) +
  scale_color_manual(values = c('red1','red4','lightblue3','lightblue1','gold')) +
  xlab('') +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  ylim(c(-2,20)) +  theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom');g.conc

g.flux <- ggplot(subset(mendota, year > 2002 & year < 2006)) + 
  geom_line(aes(x=datetime, y=fnep_middle, col = 'Fnep,epi')) +
  geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper, col = 'Fnep,epi'), linetype =2,alpha=0.2) +
  geom_line(aes(x=datetime, y=fmineral_middle, col = 'Fnep,hypo')) +
  geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper, col = 'Fnep,hypo'), linetype =2, alpha=0.2) +
  geom_line(aes(x=datetime, y=(-1)*fsed2_middle, col = 'Fsed')) +
  geom_ribbon(aes(x=datetime, ymin=(-1)*fsed2_lower, ymax=(-1)*fsed2_upper, col = 'Fsed'), linetype =2,alpha=0.2) +
  # geom_line(aes(x=datetime, y=fatm_middle, col = 'Fatm')) +
  # geom_ribbon(aes(x=datetime, ymin=fatm_lower, ymax=fatm_upper, col = 'Fatm'), alpha=0.2) +
  ylab(expression("Sim. fluxes [g DO"*~m^{-3}*~d^{-1}*"]")) +
  xlab('') +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom');g.flux

j.m <- mendota
j.m$nNEP <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP, col = 1))
j.m$nNEP_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP_max, col = 1))
j.m$nNEP_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP_min, col = 1))
j.m$nMIN <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN, col = 1))
j.m$nSED <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED, col = 1))
j.m$nMIN_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN_max, col = 1))
j.m$nMIN_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN_min, col = 1))
j.m$nSED_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED_max, col = 1))
j.m$nSED_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED_min, col = 1))
g.param <- ggplot(subset(j.m, year > 2002 & year < 2006)) + 
  geom_line(aes(x=datetime, y=(nNEP), col ='NEP,epi20'),) +
  geom_ribbon(aes(x=datetime, ymin=nNEP_min, ymax=nNEP_max, col = 'NEP,epi20'), linetype =2,alpha=0.2) +
  # geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper), alpha=0.2) +
  geom_line(aes(x=datetime, y=(nMIN), col ='NEP,hypo20')) +
  geom_ribbon(aes(x=datetime, ymin=nMIN_min, ymax=nMIN_max, col = 'NEP,hypo20'), linetype =2,alpha=0.2) +
  ylab(expression("NEP20 [g DO"*~m^{-3}*~d^{-1}*"]")) +
  # geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper), alpha=0.2) +
  geom_line(aes(x=datetime, y=(-1) * (nSED)/6, col = 'SED20')) +
  geom_ribbon(aes(x=datetime, ymin=-1 * nSED_min/6, ymax= -1 *nSED_max/6, col = 'SED20'), linetype =2,alpha=0.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*6, name = 
                                           expression("SED20 [g DO"*~m^{-2}*~d^{-1}*"]"))       )+
  xlab('') +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom');g.param

g <-g.param /g.flux / g.conc + plot_annotation(tag_levels = 'A'); g
ggsave(file = paste0('Figures/Fig_2.png'), g, dpi = 300, width =266, height = 300,
       units='mm')


scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

subchain <- . %>% 
  select(lakeid, year4, mean.var) %>% 
  left_join(do.decompose(.))  %>% 
  mutate(month = month(year4), year = year(year4), date = as.Date(year4)) %>% 
  group_by(lakeid, year, month) %>% 
  summarise(mean.var = mean(.trend), date = first(date)) %>% 
  left_join(ntl.monthly) %>% 
  group_by(lakeid) %>% 
  mutate_if(is.numeric, scale2) #scale data

do.decompose <- function(df) {
  df2 = df %>% 
    group_by(lakeid, year = year(year4)) %>% mutate(n = n()) %>% 
    filter(n >= 365) 
  
  trend = df2 %>% 
    nest(data = -lakeid) %>% 
    dplyr::mutate(
      ts =  purrr::map(data, ~ ts(.x$mean.var, start = c(1982,1), frequency = 365)),
      decomp = purrr::map(ts, ~ decompose(.x, type = 'additive')),
      tidied = purrr::map(decomp, augment)
    ) %>% 
    unnest(tidied) %>% ungroup() %>% 
    mutate(year4 = df2$year4) %>% 
    select(lakeid, year4, .trend)
  return(trend)
}

fluxes = read_csv('Processed_Output/odemFluxes.csv')
ntl.monthly = read_csv('Processed_Output/LTERdecomposeNutrients.csv')


test.df <- ntl.monthly
test.df = test.df %>%
  mutate(date = as.yearmon(paste0(year,'-',month))) %>%
  filter(lakeid %in% c('AL','SP','CR',"BM","TR"))
idx <- which(test.df$month == 5) 
ggplot(test.df) +
  # geom_point(aes(x = date, y = mean.var, col = lakeid), pch = 16, size = 0.3) +
  geom_path(aes(x = date, y = chlor), size = 0.3) +
  scale_color_brewer(palette = 'Spectral') +
  facet_wrap(~lakeid, nrow = 5) +
  theme_bw(base_size = 8) +
  ylab('chlorophyll-a') +
  geom_vline(xintercept=as.numeric(test.df$date[idx]),
             linetype=4, colour="black") +
  theme(axis.title.x = element_blank(), 
        legend.position = 'bottom')

flux.nepTot = fluxes %>% rename(year4 = date, mean.var = FnepTot) %>% subchain() %>% 
  ungroup() %>% 
  mutate(Lake = factor(lakeid, levels = c('AL','BM', 'CR', 'SP', 'TR','ME','MO','FI'))) %>%
  mutate(region = as.factor(if_else(lakeid %in% c('BM', 'TR', 'CR', 'SP', 'AL', 'N','S'), 'North','South')))

# Plot 
p1 = ggplot(flux.nepTot) +
  # geom_point(aes(x = date, y = mean.var, col = lakeid), pch = 16, size = 0.3) +
  geom_path(aes(x = date, y = mean.var, col = Lake), size = 0.5) +
  scale_color_brewer(palette = 'Spectral') +
  facet_wrap(~region, nrow = 2) +
  theme_bw(base_size = 8) +
  ylab('Scaled trend signal of total NEP flux') +
  # theme(axis.title.x = element_blank(), 
  #       legend.position = 'bottom',
  #       legend.title=element_blank(),
  #       legend.margin = margin(1, 1, 1, 1),
  #       legend.key.size = unit(0.3, "cm")) 
  theme(axis.title.x = element_blank(), 
        legend.box.background = element_rect(colour = "black"),
        legend.key.width =unit(0.3,"cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position=c(.1,.2),
        #legend.position = 'bottom',
        # legend.title=element_blank());p1
  );p1


ggsave(p1, filename = 'Figures/fluxnep.pdf',width = 4, height = 4, dpi = 500)


flux.wide = flux.nepTot %>% select(lakeid, date, mean.var) %>% 
  pivot_wider(names_from = lakeid, values_from = mean.var, values_fn = mean) %>% 
  select(-date) %>% 
  select('AL','BM', 'CR', 'SP', 'TR','ME','MO','FI')
# Compute a correlation matrix
cor.mat <- flux.wide %>% cor(use = 'complete.obs')
round(cor.mat,2)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(flux.wide)
head(p.mat[, 1:5])


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

dd <- dist(cor.mat, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

library("ggdendro")
g2 <- ggdendrogram(hc) +
  theme_bw(base_size = 8) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab('') + ylab(''); g2
ggsave(g2, filename = 'Figures/fluxnep_dendro.pdf',width = 3, height = 1, dpi = 500)


pdf(file = "Figures/fluxCorr.pdf", width = 3, height = 3)
p.corr = corrplot(cor.mat, method = 'color', col = col(200),
                  type="full", order="hclust",
                  hclust.method = 'ward.D',
                  addCoef.col = "black", # Add coefficient of correlation
                  tl.col="black", tl.srt=45, #Text label color and rotation
                  tl.cex = 0.7, cl.cex = 0.5,
                  number.cex = 0.5,
                  # Combine with significance
                  p.mat = p.mat, sig.level = 0.01, insig = "blank",
                  # hide correlation coefficient on the principal diagonal
                  diag=TRUE, addrect = 3)
dev.off()

