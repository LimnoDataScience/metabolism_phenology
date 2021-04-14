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
