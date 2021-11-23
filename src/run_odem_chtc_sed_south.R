cat('\f')
rm(list= ls())


library(tidyverse)

#oneyear <- 2008
#twoyear <- 2007:2008
fiveyear <- 1979:2019
# max.d = 8 # lake max depth

get_dens <- function(temp, salt){
  dens = 999.842594 + (6.793952 * 10^-2 * temp) - (9.095290 * 10^-3 * temp^2) +
    (1.001685 * 10^-4 * temp^3) - (1.120083 * 10^-6 * temp^4) + (6.536336 * 10^-9 * temp^5) +
    (8.24493 * 10^-1 -4.0899 * 10^-3 * temp+ 7.6438 * 10^-5 * temp^2 - 8.2467 * 10^-7 * temp^3 + 
       5.3875 * 10^-9* temp^4) * salt+
    (-5.72466 *  10^-3 + 1.0227 * 10^-4 * temp -1.6546 * 10^-6 * temp^2) * salt^(3/2) +
    (4.8314*  10^-4 ) * salt
  return(dens)
}

input <- readr::read_csv(
  './input.txt',
  col_names=c('datetime', 'thermocline_depth', 'temperature_epi', 'temperature_hypo', 'temperature_total', 'volume_total', 'volume_epi', 'volume_hypo', 'area_thermocline', 'area_surface', 'upper_meta', 'lower_meta', 'year', 'day_of_year', 'max.d', 'wind', 'airtemp'),
  col_types=cols(datetime=col_datetime(), year=col_integer(), day_of_year=col_integer(), .default=col_double()))
input <- input %>% mutate(thermocline_depth = ifelse(thermocline_depth>(max.d-1),(max.d-1),thermocline_depth)) #ensure we don't get SED blowing up with shallow hypo

#in1yr <- filter(input, year %in% oneyear)
#in2yr <- filter(input, year %in% twoyear)
in5yr <- filter(input, year %in% fiveyear)


library(zoo)
#  time stamp in the first row, then area weighted average total oxygen in the second row, area weighted average epilimnion oxygen in the third row, and area weighted average hypolimnion oxygen in the fourth row
obs <- read.table(
  './observed.txt',
  header=FALSE,
  sep=' ',
  as.is=TRUE) %>%
  t() %>%
  as_tibble(.name_repair='minimal') %>%
  setNames(., nm=c('dateint', 'DO_tot', 'DO_epi', 'DO_hypo')) %>%
  mutate(date = zoo::as.Date(dateint, origin='1979-04-01')) %>% # just guessing at origin and therefore at dates
  select(date, everything())
#obs1yr <- filter(obs, lubridate::year(date) %in% oneyear)
#obs2yr <- filter(obs, lubridate::year(date) %in% twoyear)
obs5yr <- filter(obs, lubridate::year(date) %in% fiveyear)


in1yr= in5yr
obs1yr = obs5yr
#obs1yr = obs1yr[((round(nrow(obs5yr) * 1/3))):nrow(obs5yr),]

idx1 = which(!is.na(in1yr$thermocline_depth))[1]
idx2 = rev(which(!is.na(in1yr$thermocline_depth)))[1]
in1yr = in1yr #[idx1:idx2,]
in1yr$strat <- ifelse(is.na(in1yr$thermocline_depth),0,1)
strat.pos <- c()
for (ii in 1:length(in1yr$strat)){
  if (in1yr$strat[ii] == 1 && in1yr$strat[ii-1] == 0){
    strat.pos <- append(strat.pos, ii)
  }
}

idy = match(obs1yr$date,zoo::as.Date(in1yr$datetime))
# idx = idy[!is.na(idy)]
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

library(lazyeval)
in1yr <- in1yr[,-c(1)] %>% mutate_each( funs_( interp( ~replace(., is.na(.),0) ) ) )

library(rstan)
# options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(LakeMetabolizer)
# we started with 10 days but I've played with 200 to get more "data"

simdata <- tibble(
  DO_obs_epi = DO_obs_epi * 1000,
  DO_obs_hyp = DO_obs_hyp * 1000,
  DO_obs_tot = DO_obs_tot * 1000,
  day = seq(1, nrow(in1yr))
  # DO_obs_tot_true = 10 * exp(-0.04*day) + rnorm(length(day), 0, 0.0002),
  # have_obs = ifelse(day == 1, 0, round(runif(10))),
  # DO_obs_tot = ifelse(have_obs == 1, DO_obs_tot_true, NA)
)

WhenToEstimateParams = sort(c(idxx, idx, strat.pos)) # strat.pos

nParamEstimates = length(WhenToEstimateParams)
ParamIndex = rep(nParamEstimates,nrow(simdata))
ParamIndex[1:WhenToEstimateParams[1]-1] = 1 # use the first parameter value
for (i in 2:nParamEstimates){
  iCurrent = WhenToEstimateParams[i-1]:(WhenToEstimateParams[i]-1)
  ParamIndex[iCurrent] = i
}


buoy.freq <- sqrt((get_dens(in1yr$temperature_hypo,0) - get_dens(in1yr$temperature_epi,0))/(in1yr$lower_meta - in1yr$upper_meta)* 9.81/998.2)
buoy.freq[which(buoy.freq<7e-5)] = 7e-5
kz = 0.00706 * (mean(in1yr$area_surface))^(0.56) * buoy.freq^(-0.43)
kz[which(is.na(kz))] = 1e-10

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
  o2satt = o2.at.sat.base(temp = in1yr$temperature_total, altitude = 300) * 1000,
  # k600 = k600.2.kGAS.base(k.cole.base(in1yr$wind),temperature = in1yr$temperature_epi, gas = "O2"),
  k600 = k600.2.kGAS.base(k.vachon.base(wnd = in1yr$wind,
                                         lake.area = mean(in1yr$area_surface)),temperature = in1yr$temperature_epi, gas = "O2"),
  o2sat = o2.at.sat.base(temp = in1yr$temperature_epi, altitude = 300) * 1000,
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
  diff_mol = 10^(-4.41+773.8/(in1yr$temperature_hypo+273.15)-(506.4/(in1yr$temperature_hypo+273.15))^2)*86400/10000, 
  z_dbl = 1/1000,
  diff_eddy = kz
)

dummyinput$delvol_epi[strat.pos] = 0
dummyinput$delvol_hyp[strat.pos] = 0
dummyinput$delvol_epi[is.na(dummyinput$delvol_epi)] = 0
dummyinput$delvol_hyp[is.na(dummyinput$delvol_hyp)] = 0
# simdata$DO_obs_epi = simdata$DO_obs_epi * 1.5
dummyinput$DO_obs_epi = simdata$DO_obs_epi[idxx]
dummyinput$DO_obs_hyp = simdata$DO_obs_hyp[idxx]
dummyinput$DO_obs_tot = simdata$DO_obs_tot[idx]
dummyinput$N_obs = length(dummyinput$ii_obs)
dummyinput$N_obs_mix = length(idx)
dummyinput$k600t[which(in1yr$airtemp <= 0 & in1yr$temperature_total <= 4)] = 1e-5
#dummyinput$theta0[which(in1yr$airtemp <= 0 & in1yr$temperature_total <= 4)] = 1e-5
dummyinput$nep_lim = 2000; #3000 2000
dummyinput$sed_lim = 6000; #4000
dummyinput$smooth_sigma=0.2;#0.1 ALL EXCEPT TROUT, 0.2 FOR TROUT; feb21
dummyinput$max_depth = mean(input$max.d);

chains = 3
iter <-2000
warmup <- 500
adapt_delta <- 0.8
max_treedepth <- 15
thin <- 1

sm <- stan_model(file = "odem_min_indexed_sed.stan")

fit <- sampling(sm, data = dummyinput, chains = chains, cores = chains, iter = iter, warmup = min(iter*0.5,warmup),
                include=FALSE,pars=c("SED2","NEP","MIN","DO_hyp","DO_epi","DO_epi0","DO_hyp0","DO_tot0","nep0","sed20","nu0","mineral0"),
                control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth), 
                seed=194838,thin = thin,save_warmup=FALSE)

fit_summary <- summary(fit, probs=c(0.025,0.5,0.975))$summary %>% 
  {as_tibble(.) %>%
      mutate(var = rownames(summary(fit)$summary))}

write_csv(fit_summary,"fit_summary.csv")

# Check transitions that ended with a divergence
check_div <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n = sum(divergent)
  N = length(divergent)
  # print(sprintf('%s of %s iterations ended with a divergence (%s%%)',
                # n, N, 100 * n / N))
  cat("/*",n," of ",N,"  ended with a divergence (",
      100 * n/N, ") */\n")
  if (n > 0)
    # print('  Try running with larger adapt_delta to remove the divergences')
  cat("/* Try running with larger adapt_delta to remove the divergences */\n")
}
# Check transitions that ended prematurely due to maximum tree depth limit
check_treedepth <- function(fit, max_depth) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n = length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  N = length(treedepths)
  # print(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)',
                # n, N, max_depth, 100 * n / N))
  cat("/*",n," of ",N," iterations saturated the maximum tree depth of ", max_depth,"(",
      100 * n/N, ") */\n")
  if (n > 0)
    # print('  Run again with max_depth set to a larger value to avoid saturation')
  cat("/*   Run again with max_depth set to a larger value to avoid saturation */\n")
}
# Checks the energy Bayesian fraction of missing information (E-BFMI)
check_energy <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  no_warning <- TRUE
  for (n in 1:length(sampler_params)) {
    energies = sampler_params[n][[1]][,'energy__']
    numer = sum(diff(energies)**2) / length(energies)
    denom = var(energies)
    if (numer / denom < 0.2) {
      # print(sprintf('Chain %s: E-BFMI = %s', n, numer / denom))
      cat("/* Chain", n,": E-BFMI = ", numer / denom, "*/\n")
      no_warning <- FALSE
    }
  }
  if (no_warning)
    # print('E-BFMI indicated no pathological behavior')
    cat("/* E-BFMI indicated no pathological behavior */\n")
  else
    # print('  E-BFMI below 0.2 indicates you may need to reparameterize your model')
    cat("/*  E-BFMI below 0.2 indicates you may need to reparameterize your model */\n")
}
# Checks the effective sample size per iteration
check_n_eff <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  iter <- dim(extract(fit)[[1]])[[1]]
  no_warning <- TRUE
  for (n in 1:N) {
    ratio <- fit_summary[,5][n] / iter
    if (ratio < 0.001) {
      # print(sprintf('n_eff / iter for parameter %s is %s!',
                    # rownames(fit_summary)[n], ratio))
      cat("/* n_eff / iter for parameter", rownames(fit_summary)[n]," is ", ratio, "*/\n")
      no_warning <- FALSE
    }
  }
  if (no_warning)
    # print('n_eff / iter looks reasonable for all parameters')
  cat("/* n_eff / iter looks reasonable for all parameters */\n")
  else
    # print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')
    cat("/* n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated */\n")
}
# Checks the potential scale reduction factors
check_rhat <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      # print(sprintf('Rhat for parameter %s is %s!',
      #               rownames(fit_summary)[n], rhat))
      cat("/* Rhat for parameter",rownames(fit_summary)[n]," is ", rhat, "*/\n")
      no_warning <- FALSE
    }
  }
  if (no_warning)
    # print('Rhat looks reasonable for all parameters')
    cat("/* Rhat looks reasonable for all parameters */\n")
  else
    # print('  Rhat above 1.1 indicates that the chains very likely have not mixed')
    cat("/* Rhat above 1.1 indicates that the chains very likely have not mixeds */\n")
}
check_all_diagnostics <- function(fit) {
  check_n_eff(fit)
  check_rhat(fit)
  check_div(fit)
  check_treedepth(fit, max_depth)
  check_energy(fit)
}
max_depth = max_treedepth
file.create("log")
sink(file = "log", type = "output")
cat("/* File created on", date(), "*/\n")
check_all_diagnostics(fit)
sink()
