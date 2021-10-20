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
library(patchwork)


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

lake.list <- c('Mendota')
lake.list.Abr <- c('Mendota' = 'ME')


# lake.list <- c('Allequash', 'BigMuskellunge', 'Crystal', 'Fish', 'Mendota',
#                'Monona', 'Sparkling', 'Trout')
# lake.list.Abr <- c('Allequash' = 'AL', 'BigMuskellunge' = 'BM', 'Crystal' = 'CR', 
#                    'Fish' = 'FI', 'Mendota' = 'ME',
                   # 'Monona' = 'MO', 'Sparkling' = 'SP', 'Trout' = 'TR')

df.estimations <- list()
all.sigma <- c()
list.rhat <- list()
for (lake.id in lake.list){
  if (match(lake.id, lake.list) == 1){
    setwd(paste0('../',lake.id))
  } else {
    setwd(paste0(lake.id))
  }
  
  
  fiveyear <- 1979:1999
  
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
  
  get_dens <- function(temp, salt){
    dens = 999.842594 + (6.793952 * 10^-2 * temp) - (9.095290 * 10^-3 * temp^2) +
      (1.001685 * 10^-4 * temp^3) - (1.120083 * 10^-6 * temp^4) + (6.536336 * 10^-9 * temp^5) +
      (8.24493 * 10^-1 -4.0899 * 10^-3 * temp+ 7.6438 * 10^-5 * temp^2 - 8.2467 * 10^-7 * temp^3 + 
         5.3875 * 10^-9* temp^4) * salt+
      (-5.72466 *  10^-3 + 1.0227 * 10^-4 * temp -1.6546 * 10^-6 * temp^2) * salt^(3/2) +
      (4.8314*  10^-4 ) * salt
    return(dens)
  }
  
  
  
  buoy.freq <- sqrt((get_dens(in1yr$temperature_hypo,0) - get_dens(in1yr$temperature_epi,0))/(in1yr$lower_meta - in1yr$upper_meta)* 9.81/998.2)
  buoy.freq[which(buoy.freq<7e-5)] = 7e-5
  kz = 0.00706 * (mean(in1yr$area_surface))^(0.56) * buoy.freq^(-0.43)
  kz[which(is.na(kz))] <- 7e-5
  
  # End new code
  ##################
  
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
    # k600t = k600.2.kGAS.base(k.cole.base(in1yr$wind),temperature = in1yr$temperature_total, gas = "O2"),
    k600t = k600.2.kGAS.base(k.vachon.base(wnd = in1yr$wind,
                                           lake.area = mean(in1yr$area_surface)),temperature = in1yr$temperature_total, gas = "O2"),
    o2satt = o2.at.sat.base(temp = in1yr$temperature_total, altitude = 450) * 1000,
    # k600 = k600.2.kGAS.base(k.cole.base(in1yr$wind),temperature = in1yr$temperature_epi, gas = "O2"),
    k600 = k600.2.kGAS.base(k.vachon.base(wnd = in1yr$wind,
                                          lake.area = mean(in1yr$area_surface)),temperature = in1yr$temperature_epi, gas = "O2"),
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
    ##################
    # New code by Paul
    i_Param = ParamIndex,  # n_ParamEst indeces within d
    n_ParamEst = nParamEstimates, # number of times params are estimated
    airtemp = in1yr$airtemp,
    delvol_epi = c(diff(in1yr$volume_epi),0)/c(in1yr$volume_epi),
    delvol_hyp =  c(diff(in1yr$volume_hypo),0)/c(in1yr$volume_hypo),
    diff_mol = 1.08 *10e-4,
    z_dbl = 1/1000,
    diff_eddy = kz
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
  
  outvars <- c("o2_epi","o2_hyp","fatm","fnep","fsed2",'fsed_monod','fsed_first',"fmineral","fentr_epi","fentr_hyp","fdiffex","NEP_mgm3d","SED_mgm2d","MIN_mgm3d")
  
  dat <- fit_clean %>% filter(name %in% outvars) %>% 
    dplyr::select(mean,
                  sd,
                  lower,
                  middle,
                  upper,
                  name,
                  index) %>%
    mutate(mean = as.numeric(mean))
  
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

str(lake.odem)


custom.theme = theme_minimal() + 
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 11), axis.text.x= element_text(size = 18), plot.title = element_text(size = 18),
        axis.text.y= element_text(size = 18), text = element_text(size = 18), legend.title = element_blank(), strip.text =element_text(size = 18),
        legend.position = 'bottom', 
        legend.margin=margin(t = -0.3, unit='cm'),
        plot.margin = unit(c(0,0.2,0,0.2), "cm"))


j.m <- lake.odem
j.m$nNEP <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP, col = 1))
j.m$nNEP_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP_max, col = 1))
j.m$nNEP_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP_min, col = 1))
j.m$nMIN <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN, col = 1))
j.m$nSED <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED, col = 1))
j.m$nMIN_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN_max, col = 1))
j.m$nMIN_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN_min, col = 1))
j.m$nSED_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED_max, col = 1))
j.m$nSED_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED_min, col = 1))
g.param <- ggplot(j.m) + 
  geom_line(aes(x=datetime, y=(nNEP/1000), col ='Xnep,epi'),) +
  geom_ribbon(aes(x=datetime, ymin=nNEP_min/1000, ymax=nNEP_max/1000, col = 'Xnep,epi'), linetype =2,alpha=0.2) +
  # geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper), alpha=0.2) +
  geom_line(aes(x=datetime, y=(nMIN/1000), col ='Xnep,hypo')) +
  geom_ribbon(aes(x=datetime, ymin=nMIN_min/1000, ymax=nMIN_max/1000, col = 'Xnep,hypo'), linetype =2,alpha=0.2) +
  ylab(expression("Xnep [g DO"*~m^{-3}*~d^{-1}*"]")) +
  # geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper), alpha=0.2) +
  geom_line(aes(x=datetime, y=(-1) * (nSED/1000)*2, col = 'Xsed')) +
  geom_ribbon(aes(x=datetime, ymin=-1 * nSED_min/1000*2, ymax= -1 *nSED_max/1000*2, col = 'Xsed'), linetype =2,alpha=0.2) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = 
                                           expression("Xsed [g DO"*~m^{-2}*~d^{-1}*"]"))       )+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  custom.theme; g.param

g.conc <- ggplot(lake.odem) + 
  geom_line(aes(x=doy, y=o2_epi_middle/1000, col = 'Epilimnion sim.')) +
  geom_ribbon(aes(x=doy, ymin=o2_epi_lower/1000, ymax=o2_epi_upper/1000, col = 'Epilimnion sim.'),linetype =2, alpha=0.2) +
  geom_point(aes(x=doy, y=DO_obs_epi/1000, col = 'Epilimnion obs.'), size = 2) +
  geom_line(aes(x=doy, y=o2_hyp_middle/1000, col ='Hypolimnion sim.')) +
  geom_ribbon(aes(x=doy, ymin=o2_hyp_lower/1000, ymax=o2_hyp_upper/1000, col = 'Hypolimnion sim.'),linetype =2, alpha=0.2) +
  geom_point(aes(x=doy, y=DO_obs_hyp/1000, col= 'Hypolimnion obs.'), size = 2) +
  geom_point(aes(x=doy, y=DO_obs_tot/1000, col = 'Total obs.'), size=2) +
  facet_wrap(~ year,nrow=1) +
  ylab(expression("Conc. [g DO"*~m^{-3}*"]")) +
  scale_color_manual(values = c('red1','red4','lightblue3','lightblue1','gold'),
                     guide = guide_legend(override.aes = list(
                       linetype = c('blank', 'solid', 'blank', 'solid', 'blank'),
                       shape = c(16, NA, 16, NA, 16)))) +
  xlab('') +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  ylim(c(0,25)) +
  custom.theme; g.conc

mu = mean(lake.odem$DO_obs_epi, na.rm = T)
tau= sd(lake.odem$DO_obs_epi, na.rm =T)
lake.odem$do_trans = (lake.odem$o2_hyp_middle -mu)/tau

sed.flux = rep(NA, nrow(lake.odem))
nep.flux = rep(NA, nrow(lake.odem))
sed_first =  rep(NA, nrow(lake.odem))
sed_monod = rep(NA, nrow(lake.odem))
sed_trans = rep(NA, nrow(lake.odem))
sed_red_first = rep(NA, nrow(lake.odem))
sed_red_first_one = rep(NA, nrow(lake.odem))
sed_red_first_two = rep(NA, nrow(lake.odem))
sed_firstorder = rep(NA, nrow(lake.odem))
for (ii in 2:nrow(lake.odem)){
  sed.flux[ii] = (j.m$nSED[ii-1] *  (lake.odem$o2_hyp_middle[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1]) * dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) + 
    ((lake.odem$o2_hyp_middle[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1]) * lake.odem$o2_hyp_middle[ii-1] * dummyinput$diff_mol/dummyinput$z_dbl* dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) 
  nep.flux[ii] = j.m$nMIN[ii-1] * dummyinput$theta2[ii-1]
  sed_monod[ii] = (j.m$nSED[ii-1] *  (lake.odem$o2_hyp_middle[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1]) * dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1])
  sed_first[ii] = (lake.odem$o2_hyp_middle[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1]) *(lake.odem$o2_hyp_middle[ii-1] * dummyinput$diff_mol/dummyinput$z_dbl* dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) 
  sed_red_first[ii] = (lake.odem$o2_hyp_middle[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1]) *( dummyinput$diff_mol/dummyinput$z_dbl* dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) 
  sed_red_first_one[ii]=  (lake.odem$o2_hyp_middle[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1])
  sed_red_first_two[ii] =( dummyinput$diff_mol/dummyinput$z_dbl* dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) 
  sed_trans[ii] = (j.m$nSED[ii-1] *  (lake.odem$do_trans[ii-1])/(dummyinput$khalf + lake.odem$o2_hyp_middle[ii-1]) * dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) + 
    (lake.odem$do_trans[ii-1] * dummyinput$diff_mol/dummyinput$z_dbl* dummyinput$theta2[ii-1]) / (dummyinput$volume_hyp[ii-1]/dummyinput$area_hyp[ii-1]) 
  
  sed_firstorder[ii] = max(max((lake.odem$o2_hyp_middle[i-1]),1e-06)/(dummyinput$khalf + max((lake.odem$o2_hyp_middle[i-1]),1e-06)),1e-06) *  (max((lake.odem$o2_hyp_middle[i-1]),1e-06) * dummyinput$diff_mol/dummyinput$z_dbl* dummyinput$theta2[i-1]) / max((dummyinput$volume_hyp[i-1]/dummyinput$area_hyp[i-1]),1);
  }

lake.odem$fsed2_custom <- sed.flux * (-1)

ggplot(lake.odem) + 
  geom_line(aes(x=doy, y=fnep_middle/1000, col = 'Fnep,epi')) +
  # geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper, col = 'Fnep,epi'), linetype =2,alpha=0.2) +
  geom_line(aes(x=doy, y=fmineral_middle/1000, col = 'Fnep,hypo')) +
  # geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper, col = 'Fnep,hypo'), linetype =2, alpha=0.2) +
  geom_line(aes(x=doy, y=(-1)*fsed2_middle/1000, col = 'Fsed')) +
  # geom_ribbon(aes(x=datetime, ymin=(-1)*fsed2_lower, ymax=(-1)*fsed2_upper, col = 'Fsed'), linetype =2,alpha=0.2) +
  # geom_line(aes(x=datetime, y=fdiffex_middle/1000, col = 'Fdiff')) +
  # geom_ribbon(aes(x=datetime, ymin=(-1)*fdiffex_lower, ymax=(-1)*fdiffex_upper, col = 'Fdiff'), linetype =2,alpha=0.2) +
  geom_line(aes(x=doy, y=(-1)*fsed_monod_middle/1000, col = 'Fzero')) +
  geom_line(aes(x=doy, y=(-1)*fsed_first_middle/1000, col = 'Ffirst')) +
  # geom_line(aes(x=datetime, y=fatm_middle/1000, col = 'Fatm')) +
  # geom_ribbon(aes(x=datetime, ymin=fatm_lower, ymax=fatm_upper, col = 'Fatm'), linetype =2, alpha=0.2) +
  # geom_line(aes(x=datetime, y=Fentr1/1000  , col = 'Fentr')) +
  # geom_line(aes(x=datetime, y=fsed2_custom, col = 'Fsed_custom')) +
  ylab(expression("Sim. fluxes [g DO"*~m^{-3}*~d^{-1}*"]")) +
  facet_wrap(~year)+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "magenta", 'lightblue', "green", 'cyan','purple')) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  custom.theme

g.flux <- ggplot(lake.odem) + 
  geom_line(aes(x=datetime, y=fnep_middle/1000, col = 'Fnep,epi')) +
  # geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper, col = 'Fnep,epi'), linetype =2,alpha=0.2) +
  geom_line(aes(x=datetime, y=fmineral_middle/1000, col = 'Fnep,hypo')) +
  # geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper, col = 'Fnep,hypo'), linetype =2, alpha=0.2) +
  geom_line(aes(x=datetime, y=(-1)*fsed2_middle/1000, col = 'Fsed')) +
  # geom_ribbon(aes(x=datetime, ymin=(-1)*fsed2_lower, ymax=(-1)*fsed2_upper, col = 'Fsed'), linetype =2,alpha=0.2) +
  geom_line(aes(x=datetime, y=fdiffex_middle/1000, col = 'Fdiff')) +
  # geom_ribbon(aes(x=datetime, ymin=(-1)*fdiffex_lower, ymax=(-1)*fdiffex_upper, col = 'Fdiff'), linetype =2,alpha=0.2) +
  # geom_line(aes(x=datetime, y=fsed_monod_middle/1000, col = 'Fzero')) +
  # geom_line(aes(x=datetime, y=fsed_first_middle/1000, col = 'Ffirst')) +
  geom_line(aes(x=datetime, y=fatm_middle/1000, col = 'Fatm')) +
  # geom_ribbon(aes(x=datetime, ymin=fatm_lower, ymax=fatm_upper, col = 'Fatm'), linetype =2, alpha=0.2) +
  geom_line(aes(x=datetime, y=Fentr1/1000  , col = 'Fentr')) +
  # geom_line(aes(x=datetime, y=fsed2_custom, col = 'Fsed_custom')) +
  ylab(expression("Sim. fluxes [g DO"*~m^{-3}*~d^{-1}*"]")) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "magenta", 'lightblue', "green", 'cyan','purple')) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  custom.theme; g.flux

ggplot(lake.odem)+
  geom_line(aes(datetime, (fsed2_middle), col= 'total')) +
  geom_point(aes(datetime, (fsed_monod_middle), col = 'monod kinetics', shape = factor(stratified))) +
  geom_point(aes(datetime, (fsed_first_middle), col = 'first-order', shape = factor(stratified))) +
  geom_point(aes(x=datetime, y=(fentr_epi_middle)  , col = 'entrainment')) +
  geom_point(aes(x=datetime, y=(fdiffex_middle)  , col = 'diffusion')) +
  # geom_point(aes(x=datetime, y=(o2_hyp_middle)/1000  , col = 'o2'))

plot(lake.odem$o2_hyp_middle, lake.odem$fsed_first_middle)


g <-g.param /g.flux / g.conc + plot_annotation(tag_levels = 'A'); g


# 
# start = 0
# end= 30
# par(mfrow = c(3, 2)) 
# plot(lake.odem$o2_epi_middle/1000,xlim=c(start,end), ylim=c(-10000,1300), ylab=c('O2_epi'))
# plot(lake.odem$o2_hyp_middle/1000,xlim=c(start,end), ylim=c(-10000,1300), ylab=c('O2_hyp'))
# plot(lake.odem$fentr_epi_middle,xlim=c(start,end), ylim=c(-10,500), ylab=c('entrainment'))
# plot(lake.odem$fsed2_middle,xlim=c(start,end), ylim=c(-1000,1000000), ylab=c('total sed'))
# plot(lake.odem$fsed_first_middle,xlim=c(start,end), ylim=c(-1000,1000000), ylab=c('1st order'))
# plot(lake.odem$fsed_monod_middle,xlim=c(start,end), ylim=c(-10,90), ylab=c('pure monod'))
df = lake.odem %>% filter(stratified == 1)
mean(df$fmineral_middle) - mean(df$fsed2_middle)
(mean(df$fmineral_middle) - mean(df$fsed2_middle)) * mean(df$volume_hyp)/mean(df$area_hyp)
(median(df$fmineral_middle) - median(df$fsed2_middle)) * median(df$volume_hyp)/median(df$area_hyp)
mean(df$fmineral_middle)  * mean(df$volume_hyp)/mean(df$area_hyp)/1000
mean(df$fsed2_middle) * mean(df$volume_hyp)/mean(df$area_hyp)/1000

(sum(df$fmineral_middle) - sum(df$fsed2_middle)) * median(df$volume_hyp)/median(df$area_hyp)
