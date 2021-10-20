data {

  // Data dimensions
  int<lower=1> d; // number of dates
  int N_obs; // number of dates with observations
  int N_obs_mix;
  int<lower=1, upper=d> ii_obs[N_obs];
  int<lower=1, upper=d> ii_obs_mix[N_obs_mix];
  int<lower=1> d_strat_pos;
  int<lower=1> i_Param[d]; // New Paul
  int<lower=1> n_ParamEst; // New Paul

  // Data
  real khalf;
  real max_depth;
  real theta0[d];
  real theta1[d];
  real theta2[d];
  real volume_tot[d];
  real volume_epi[d];
  real area_epi[d];
  real volume_hyp[d];
  real area_hyp[d];
  real k600[d];
  real o2sat[d];
  real k600t[d];
  real o2satt[d];
  real tddepth[d];
  real DO_obs_epi[N_obs]; // how do we accept missing data? https://mc-stan.org/docs/2_23/stan-users-guide/sliced-missing-data.html
  real DO_obs_hyp[N_obs];
  real DO_obs_tot[N_obs_mix];
  real stratified[d];
  real strat_pos[d_strat_pos];
  int len_strat_pos;
  //real delvol_epi[d]; // not used
  //real delvol_hyp[d]; // not used
  real nep_lim; // limits for NEP (mg/m3/d)
  real sed_lim; // upper limit for sediment oxygen demand (mg/m2/d)
  real<lower=0> smooth_sigma;
  real diff_mol;
  real z_dbl;
  real diff_eddy[d];
}

transformed data {
  real<lower=0> mu; // mean of DO for entire time series
  real<lower=0> tau; // sd of DO for entire time series
  real x_obs_epi[N_obs]; // scaled oxygen concentrations in epi
  real x_obs_hyp[N_obs]; // scaled oxygen concentrations in hyp
  real x_obs_tot[N_obs_mix]; //scaled oxygen concentrations for entire water column
  real x_eq[d]; // scaled oxygen saturation values in epi when lake is stratified
  real x_eqt[d]; // scaled oxygen saturation values when lake is mixed
  real neplim; // scaled limits for NEP (mg/m3/d)
  real sedlim; // scaled upper limit for sediment oxygen demand (mg/m2/d)
  real mean_depth; //mean depth of lake (m)
  real delvol[d]; //change in volume between time steps for entrainment

  mu = mean(DO_obs_epi);
  tau = sd(DO_obs_epi);
  neplim = nep_lim/tau;
  sedlim = sed_lim/tau;
  mean_depth = volume_tot[1]/area_epi[1];

  for(i in 1:d){
    x_eq[i] = (o2sat[i]-mu)/tau;
    x_eqt[i] = (o2satt[i]-mu)/tau;
  }

  delvol[1] = 0;
  for(i in 2:d) {
    delvol[i] = volume_epi[i]-volume_epi[i-1];
  }

  for(n in 1:N_obs){
    x_obs_epi[n] = (DO_obs_epi[n]-mu)/tau;
    x_obs_hyp[n] = (DO_obs_hyp[n]-mu)/tau;
  }
  for(n in 1:N_obs_mix){
    x_obs_tot[n] = (DO_obs_tot[n]-mu)/tau;
  }
}

parameters {
  // real<lower=0> smooth_sigma;
  real<lower=0> sigma; // sd of oxygen state process error
  // real<lower=0> sig_nep; // sd of oxygen state process error
  // real<lower=0> sig_sed; // sd of oxygen state process error
  real DO_epi0; //initial scaled epi DO concentration
  real DO_hyp0; // initical scaled hyp DO concentration
  real DO_tot0; // initial scaled tot DO concentration
  real nep0; // initial nep estimate
  real sed20; // initial sed estimate
  real sed_monod0;
  real sed_first0;
  real mineral0; // initial min estimate
  real nu0; // initial gas flux estimate
  real diffex0; // initial eddy diffusivity
  real<lower = -neplim,upper=neplim> NEP[n_ParamEst]; // NEP at reference temperature
  real<lower=0,upper=sedlim> SED2[n_ParamEst]; //sediment oxygen demand at reference temperature
  real<lower=-neplim,upper=0> MIN[n_ParamEst]; //sediment oxygen demand at reference temperature
}

transformed parameters {
  real DO_epi[d]; //inferred epi DO concentration
  real DO_tot[d]; //inferred total water column do concentration
  real DO_hyp[d]; // inferred hyp do concentration
  real nep[d]; //nep flux
  real sed2[d]; //sediment oxygen demand flux
  real sed_monod[d]; //sediment oxygen demand flux: monod component
  real sed_first[d]; //sediment oxygen demand flux: first-order component
  real mineral[d]; //mineralization flux
  real nu[d]; //gas flux
  real diffex[d]; //vertical diffusivity flux
  { // create a block so discreate values can be created that aren't treated as estimated parameters
    real x_do1;
    real x_do2;
    int first_day;
    first_day = 0;

    //Initial Values
    DO_epi[1] = DO_epi0;
    DO_hyp[1] = DO_hyp0;
    DO_tot[1] = DO_tot0;
    nep[1] = nep0;
    sed2[1] = sed20;
    sed_monod[1] = sed_monod0;
    sed_first[1] = sed_first0;
    mineral[1] = mineral0;
    nu[1] = nu0;
    diffex[1] = diffex0;

    for(i in 2:d) { //loop through days

      first_day = 0; //deterimine if it is first of stratification change
      for (k in 1:len_strat_pos){
        if (strat_pos[k] == i){
          first_day = 1;
        }
      }

      if (stratified[i] == 0) {
        nep[i] = NEP[i_Param[i]] * theta0[i-1];
        mineral[i] = MIN[i_Param[i]] * theta0[i-1];
	      //sed2[i] = SED2[i_Param[i]] *  (fmax((DO_tot[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_tot[i-1]*tau+mu),1e-06))) * theta0[i-1] / mean_depth + DO_tot[i-1] * diff_mol/z_dbl * theta0[i-1] / mean_depth;
        sed_monod[i] = (fmax((DO_tot[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_tot[i-1]*tau+mu),1e-06)) * SED2[i_Param[i]] *  theta0[i-1] )/ mean_depth; //fmax(fmax((DO_tot[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_tot[i-1]*tau+mu),1e-06)),1e-06) *
        // sed_first[i] = fmax(fmax((DO_tot[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_tot[i-1]*tau+mu),1e-06)),1e-06) * (fmax((DO_tot[i-1]*tau + mu),1e-06) * diff_mol/z_dbl * theta0[i-1]) / fmax((volume_tot[i-1]/area_epi[i-1]),1);
        sed_first[i] = ((fmax((DO_tot[i-1]*tau + mu),1e-06) * diff_mol/(z_dbl * max_depth) * theta0[i-1]))/tau;
        sed2[i] =   sed_monod[i] + sed_first[i];
        nu[i] =  k600t[i-1]  *  (x_eqt[i-1] - DO_epi[i-1]) / mean_depth;
        diffex[i] = 1e-09;
        DO_tot[i] =  DO_tot[i-1] + nep[i] + mineral[i] - sed2[i] + nu[i];
        DO_hyp[i] = DO_tot[i];
        DO_epi[i] = DO_tot[i];

      } else if (stratified[i] == 1){

        if(delvol[i]>0) {
          x_do1 = DO_hyp[i-1];
        } else {
          x_do1 = DO_epi[i-1];
        }

        if(first_day == 1){
        	nep[i] =  nep[i-1];
        	nu[i] = nu[i-1];
        	DO_epi[i] =  (DO_epi[i-1]);
        	diffex[i] = diffex[i-1];
        } else {
        	nep[i] =  NEP[i_Param[i]] *theta1[i-1];
        	nu[i] = k600[i-1] *  (x_eq[i-1] - DO_epi[i-1]) / tddepth[i-1];
          diffex[i] = diff_eddy[i-1]/area_hyp[i-1] * (DO_hyp[i-1] - DO_epi[i-1]);
        	DO_epi[i] =  ((DO_epi[i-1] + nep[i] + nu[i] + diffex[i])*volume_epi[i-1] + (delvol[i]*x_do1))/volume_epi[i];
        }

    	if(first_day == 1) {
    		sed_monod[i] = sed_monod[i-1];
		sed_first[i] = sed_first[i-1];
  		sed2[i] = sed2[i-1];
    		mineral[i] = mineral[i-1];
    		DO_hyp[i] = DO_hyp[i-1];
    	} else {
    	 mineral[i] = MIN[i_Param[i]] * theta2[i-1];
	     //sed2[i] = (SED2[i_Param[i]] *  (fmax((DO_hyp[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_hyp[i-1]*tau+mu),1e-06))) * theta2[i-1]) / fmax((volume_hyp[i-1]/area_hyp[i-1]),1) + (DO_hyp[i-1] * diff_mol/z_dbl* theta2[i-1]) / fmax((volume_hyp[i-1]/area_hyp[i-1]),1);
       sed_monod[i] = (fmax((DO_hyp[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_hyp[i-1]*tau+mu),1e-06)) * SED2[i_Param[i]] *  theta2[i-1]) / fmax((volume_hyp[i-1]/area_hyp[i-1]),1);// fmax(fmax((DO_hyp[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_hyp[i-1]*tau+mu),1e-06)),1e-06) *
       // sed_first[i] =fmax(fmax((DO_hyp[i-1]*tau+mu),1e-06)/(khalf + fmax((DO_hyp[i-1]*tau+mu),1e-06)),1e-06) *  (fmax((DO_hyp[i-1]*tau + mu),1e-06) * diff_mol/z_dbl* theta2[i-1]) / fmax((volume_hyp[i-1]/area_hyp[i-1]),1);
       sed_first[i] =  ((fmax((DO_hyp[i-1]*tau + mu),1e-06) * diff_mol/(fmax((volume_hyp[i-1]/area_hyp[i-1]),1)*z_dbl) * theta2[i-1]) ) /tau;
       sed2[i] = sed_monod[i] + sed_first[i];
       // DO_hyp[i] =  ((DO_hyp[i-1] - fmin(sed2[i] + diffex[i], DO_hyp[i-1]))*volume_hyp[i-1] - (delvol[i]*x_do1))/volume_hyp[i];
       DO_hyp[i] =  ((DO_hyp[i-1] + mineral[i] - sed2[i] - diffex[i])*volume_hyp[i-1] - (delvol[i]*x_do1))/volume_hyp[i];
    	}
      DO_tot[i] = (DO_epi[i] *volume_epi[i] + DO_hyp[i] *volume_hyp[i])/volume_tot[i];
      }
    }
  } // end block
}
model {
//priors
DO_epi0 ~ normal(x_obs_epi[1],10);
DO_hyp0 ~ normal(x_obs_hyp[1],10);
DO_tot0 ~ normal(x_obs_tot[1],10);
nep0 ~ normal(0,0.01);
sed20 ~ normal(0,0.01);
sed_monod0 ~ normal(0,0.01);
sed_first0 ~ normal(0,0.01);
nu0 ~ normal(0,0.01);
mineral0 ~ normal(0,0.01);
sigma ~ cauchy(0, 5);
diffex0 ~ normal(0,0.01);
// smooth_sigma ~ cauchy(0, 5);
// sig_nep~ cauchy(0, 5);
// sig_sed~ cauchy(0, 5);

//parameter smoothing
NEP[1] ~ normal(0,10);
SED2[1] ~ normal(0,10);
MIN[1] ~ normal(0,10);
NEP[2:n_ParamEst] ~ normal(NEP[1:(n_ParamEst-1)],smooth_sigma);
SED2[2:n_ParamEst] ~ normal(SED2[1:(n_ParamEst-1)],smooth_sigma);//2200 3200
MIN[2:n_ParamEst] ~ normal(MIN[1:(n_ParamEst-1)],smooth_sigma);

//likelihood
  x_obs_epi[1:N_obs] ~ normal(DO_epi[ii_obs[1:(N_obs)]], sigma); // error in "i", sigma of 1 way too low
  x_obs_hyp[1:N_obs] ~ normal(DO_hyp[ii_obs[1:(N_obs)]], sigma); // error in "i", sigma of 1 way too low
  x_obs_tot[1:N_obs_mix] ~ normal(DO_tot[ii_obs_mix[1:(N_obs_mix)]], sigma); // error in "i", sigma of 1 way too low
}

generated quantities {
   real o2_epi[d]; // inferred epi oxygen state [mg m^-3]
   real o2_hyp[d]; // inferred hyp oxygen state [mg m^-3]
   real o2_tot[d]; // inferred tot oxygen state [mg m^-3]
   real NEP_mgm3d[n_ParamEst]; //NEP transformed back to original scale mg/m3/d
   real SED_mgm2d[n_ParamEst]; //sediment oxygen demand transformed back to original scale mg/m3/d
   real MIN_mgm3d[n_ParamEst]; //mineralization transformed back to original scale mg/m3/d
   real fnep[d]; //NEP flux mg/m3/d
   real fmineral[d]; //mineralization flux mg/m3/d
   real fsed2[d]; //sediment oxygen flux mg/m3/d
   real fatm[d]; //atmospheric flux mg/m3/d
   real fentr_epi[d]; //epi entrainment flux mg/m3/d
   real fentr_hyp[d]; //hyp entrainment flux mg/m3/d
   real fdiffex[d]; //vertical diffusivity flux mg/m3/d
   real fsed_monod[d]; //sediment oxygen flux mg/m3/d
   real fsed_first[d]; //sediment oxygen flux mg/m3/d
   for(n in 1:d){
    o2_epi[n] = tau*DO_epi[n] + mu;
    o2_hyp[n] = tau*DO_hyp[n] + mu;
    o2_tot[n] = tau*DO_tot[n] + mu;
    fnep[n] = tau*nep[n];
    fmineral[n] = tau*mineral[n];
    fsed2[n] = tau*sed2[n];
    fatm[n] = tau*nu[n];
    fdiffex[n] = tau*diffex[n];
    fsed_monod[n] = tau*sed_monod[n];
    fsed_first[n] = tau*sed_first[n];
   }
   for(n in 1:n_ParamEst){
    NEP_mgm3d[n] = tau*NEP[n];
    MIN_mgm3d[n] = tau*MIN[n];
    SED_mgm2d[n] = tau*SED2[n];
   }
   for(i in 2:d){

     if(stratified[i] == 0){
       fentr_epi[i] = 0.00001*DO_tot[i-1]*tau;
       fentr_hyp[i] = 0.00001*DO_tot[i-1]*tau*(-1);
     } else{

      if(delvol[i]>0) {
          fentr_epi[i] = (delvol[i]*DO_hyp[i-1]/volume_epi[i-1])*tau;
          fentr_hyp[i] = (delvol[i]*DO_hyp[i-1]/volume_hyp[i-1])*tau*(-1);
        } else {
          fentr_epi[i] = (delvol[i]*DO_epi[i-1]/volume_epi[i-1])*tau;
          fentr_hyp[i] = (delvol[i]*DO_epi[i-1]/volume_hyp[i-1])*tau*(-1);
        }
     }
      for (k in 1:len_strat_pos){
        if (strat_pos[k] == i){
          fentr_epi[i] = fentr_epi[i-1]*tau;
          fentr_hyp[i] = fentr_hyp[i-1]*tau*(-1);
        }
      }
   }
  fentr_epi[1] = fentr_epi[2];
  fentr_hyp[1] = fentr_hyp[2];
}
