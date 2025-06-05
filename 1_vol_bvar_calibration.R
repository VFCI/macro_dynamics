#-------------------------------------------------------------------------------
# 1. Specifying the data input into the BVAR and IRFs
#-------------------------------------------------------------------------------

fig_width <- 5 # in inches
fig_height <- fig_width / 1.618

#Variables
if (type == "baseline" | type == "pre_crisis" | type == "100k" | type == "1M" | grepl("mn", type)) {
  vars_in_system <- c('lgdp','lpce','vfci','fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "Fed Funds shock")
  yaxis_vfci_shock     <- list(c(-0.01,0.002),c(-0.005,0.005),c(-0.04,0.2),c(-0.5,0.05))
  yaxis_vfci_response  <- list(c(-0.1,0.1),c(-0.1,0.1),c(-0.05,0.2),c(-0.05,0.15))
  
} else if (type == "regimes") {
  vars_in_system <- c('lgdp','lpce','vfci','fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "Fed Funds shock")
  yaxis_vfci_shock  <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.7,0.1))
  yaxis_vfci_response  <- list(c(-0.1,0.1),c(-0.1,0.1),c(-0.05,0.3),c(-0.05,0.3))
  
} else if (type == "normal") {
  vars_in_system <- c('lgdp','lpce','vfci','fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "Fed Funds shock")
  yaxis_vfci_shock     <- list(c(-0.02,0.002),c(-0.01,0.01),c(-0.05,0.3),c(-0.1,0.1))
  yaxis_vfci_response  <- list(c(-0.1,0.1),c(-0.1,0.1),c(-0.05,0.3),c(-0.05,0.25))
  
} else if (type == "stationary") {
  vars_in_system <- c('ygr','infl_pce','vfci','fedfunds') 
  var_names      <- c("GDP Growth", "PCE Inflation", "VFCI", "Fed Funds")
  shock_names    <- c("GDP Growth shock", "PCE inflation shock", "VFCI shock", "Fed Funds shock")
  yaxis_vfci_shock     <- list(c(-0.01,0.002),c(-0.005,0.005),c(-0.04,0.2),c(-0.5,0.05))
  yaxis_vfci_response  <- list(c(-0.1,0.1),c(-0.1,0.1),c(-0.05,0.2),c(-0.05,0.15))
  
} else if (type == "vfci_lev") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci_lev','fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "Fed Funds shock")
  yaxis_vfci_shock     <- list(c(-0.01,0.002),c(-0.005,0.005),c(-0.04,0.2),c(-0.5,0.05))
  yaxis_vfci_response  <- list(c(-0.1,0.1),c(-0.1,0.1),c(-0.05,0.2),c(-0.05,0.15))
  
} else if (type == "horserace_ecy") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci', 'ecy', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "ECY", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "ECY shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.04,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.04,0.3),c(-0.1,0.3),c(-0.15,0.15)) 
  
} else if (type == "horserace_gz") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci', 'gz', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "GZ", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "GZ shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.04,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.04,0.3),c(-0.1,0.3),c(-0.15,0.15)) 
  
}  else if (type == "horserace_tedr") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci', 'tedr', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "TEDR", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "TEDR shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.04,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.04,0.3),c(-0.1,0.3),c(-0.15,0.15)) 
  
} else if (type == "horserace_nfci") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci', 'nfci', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "NFCI", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "NFCI shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.04,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.04,0.3),c(-0.1,0.3),c(-0.15,0.15)) 
  
} else if (type == "horserace_gsfci") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci', 'gsfci', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "GSFCI", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "GSFCI shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.04,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.04,0.3),c(-0.1,0.3),c(-0.15,0.15)) 
  
} else if (type == "horserace_gz_tedr") {
  vars_in_system <- c('lgdp', 'lpce', 'vfci', 'gz', 'tedr', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "VFCI", "GZ", "TEDR", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "VFCI shock", "GZ shock", "TEDR shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.04,0.3),c(-0.6,0.05),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.1,0.3),c(-0.1,0.3),c(-0.1,0.3),c(-0.15,0.15)) 
  
} else if (type == "horserace_no_vfci_yes_gz") {
  vars_in_system <- c('lgdp', 'lpce', 'gz', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "GZ", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "GZ shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.04,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.1,0.3),c(-0.15,0.15)) 
  
} else if (type == "horserace_no_vfci_yes_tedr") {
  vars_in_system <- c('lgdp', 'lpce', 'tedr', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "TEDR", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "TEDR shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.010),c(-0.010,0.010),c(-0.04,0.6),c(-0.75,0.5))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.25,0.10),c(-0.1,0.6),c(-0.15,0.15)) 
  
} else if (type == "horserace_no_vfci_yes_gz_tedr") {
  vars_in_system <- c('lgdp', 'lpce', 'gz', 'tedr', 'fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "GZ",  "TEDR", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock","GZ shock", "TEDR shock", "Fed Funds shock")
  yaxis_vfci_shock    <- list(c(-0.015,0.002),c(-0.005,0.005),c(-0.1,0.3),c(-0.1,0.3),c(-0.5,0.05))
  yaxis_vfci_response <- list(c(-0.1,0.1),c(-0.06,0.06),c(-0.1,0.3),c(-0.1,0.3),c(-0.15,0.15)) 

} else if (type == "total_log_vol") {
  vars_in_system <- c('lgdp','lpce','total_log_vol','fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "Tot LV", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "Total Log Vol shock", "Fed Funds shock")
  yaxis_vfci_shock     <- list(c(NA,NA), c(NA,NA), c(NA,NA), c(NA,NA)) #list(c(-0.005,0.01),c(-0.005,0.005),c(-0.2,0.2),c(-0.2,0.5))
  yaxis_vfci_response  <- list(c(NA,NA), c(NA,NA), c(NA,NA), c(NA,NA)) #list(c(-0.5,0.5),c(-0.5,0.5),c(-0.5,0.5),c(-0.5,0.5))
} else if (type == "resid_log_vol") {
  vars_in_system <- c('lgdp','lpce','resid_log_vol','fedfunds') 
  var_names      <- c("Log Real GDP", "Log Core PCE", "Res LV", "Fed Funds")
  shock_names    <- c("Real GDP shock", "Core PCE shock", "Resid Log Vol shock", "Fed Funds shock")
  yaxis_vfci_shock     <- list(c(NA,NA), c(NA,NA), c(NA,NA), c(NA,NA)) #list(c(-0.005,0.01),c(-0.005,0.005),c(-0.2,0.2),c(-0.2,0.5))
  yaxis_vfci_response  <- list(c(NA,NA), c(NA,NA), c(NA,NA), c(NA,NA)) #list(c(-0.5,0.5),c(-0.5,0.5),c(-0.5,0.5),c(-0.5,0.5))
}

input_in_var <- as.data.frame(vfci_data[, c("date", vars_in_system)])

#-------------------------------------------------------------------------------
# 2. Specifying average or regime-specific IRFs
#-------------------------------------------------------------------------------

if (type != "regimes") {
  avg_regime   <<- 1
} else {
  avg_regime   <<- 0
  regime_calib <<- 7
}

#-------------------------------------------------------------------------------
#3. Calibration for posterior mode
#-------------------------------------------------------------------------------

vars                  <- vars_in_system
if (length(vars) == 4) {
  log_trans           <<- c(0,0,0,0)
} else if (length(vars) == 5) {
  log_trans           <<- c(0,0,0,0,0)
} else if (length(vars) == 6) {
  log_trans           <<- c(0,0,0,0,0,0)
}

# Baseline time period
startdate_calibration <<- as.Date('1962-01-01') 
enddate_calibration   <<- as.Date('2022-07-01')
regime_dates          <<- as.Date(c('1979-10-01','1983-01-01','1990-01-01','2008-01-01','2011-01-01', '2020-01-01'))

# Alternate time periods (robustness)

if (type == "pre_crisis") {
  startdate_calibration <<- as.Date('1962-01-01') 
  enddate_calibration   <<- as.Date('2007-10-01')
  regime_dates          <<- as.Date(c('1979-10-01','1983-01-01','1990-01-01'))
}

if (type == "horserace_ecy" | type == "horserace_gz" | type == "horserace_no_vfci_yes_gz" | type == "horserace_nfci") {
  startdate_calibration <<- as.Date('1973-04-01')      # Common starting date for horseraces depending on data availability
  enddate_calibration   <<- as.Date('2022-07-01')
  regime_dates          <<- as.Date(c('1979-10-01','1983-01-01','1990-01-01','2008-01-01','2011-01-01', '2020-01-01'))
}

if (type == "horserace_tedr" | type == "horserace_gz_tedr" | type == "horserace_no_vfci_yes_tedr"| type ==  "horserace_no_vfci_yes_gz_tedr") {
  startdate_calibration <<- as.Date('1973-04-01')
  enddate_calibration   <<- as.Date('2021-10-01')      # Series discontinued from 2022Q1
  regime_dates          <<- as.Date(c('1979-10-01','1983-01-01','1990-01-01','2008-01-01','2011-01-01', '2020-01-01'))
}

if (type == "horserace_gsfci") {
  startdate_calibration <<- as.Date('1982-10-01')      # Series starts in 1982Q4
  enddate_calibration   <<- as.Date('2022-07-01')      
  regime_dates          <<- as.Date(c('1983-01-01','1990-01-01','2008-01-01','2011-01-01', '2020-01-01'))
}

irf_steps <<- 20
nvar                  <- length(vars) ## number of variables
lcA0                  <- matrix(TRUE, nvar, nvar) ## no restrictions on A0
nlags_calibration     <- 4

mn_tight_calib <- 3
mn_decay_calib <- 0.5

if (type == "mn_tight_low") {
  mn_tight_calib <- 1
} else if (type == "mn_tight_med") {
  mn_tight_calib <- 2
} else if (type == "mn_tight_high") {
  mn_tight_calib <- 5
} else if (type == "mn_decay_low") {
  mn_decay_calib <- 0.3
} else if (type == "mn_decay_high") {
  mn_decay_calib <- 0.7
}


#-------------------------------------------------------------------------------
#4. Calibration for MCMC algorithm
#-------------------------------------------------------------------------------

# Models
model_choices = c('gaussian','t')
my_choice = model_choices[2] ## by default, draw from the t model
if (type == "normal") {
  my_choice = model_choices[1] 
}

# Tuning parameters
hessian_scaling = 0.05 ## scaling of inverse hessian as covariance matrix

disperse = TRUE ## if TRUE, disperse the starting point using inverse hessian as covariance matrix
## For some of the robustness checks, the inverse hessian matrix doesn't work, so we set disperse to FALSE
if (type == "horserace_gz" | type == "horserace_tedr" | type =="horserace_ecy" | type == "horserace_gsfci" | type == "horserace_nfci" | type == "horserace_no_vfci_yes_gz" | type == "horserace_gz_tedr" | type == "horserace_no_vfci_yes_tedr"| type ==  "horserace_no_vfci_yes_gz_tedr") {
  disperse <- FALSE
}

# MCMC Chain
total_draws <- 10000
if (type == "100k") {
  total_draws <- 100000
} else if (type == "1M") {
  total_draws <- 1000000
}
draws_step <- 100
ndraw = total_draws ## total draws to record 
nsep = 1    ## number of draws per recorded draws

## Note that while the program `gdraw` will print out a message every 100 draws,
## it only saves to the disk on the savepots
if (type == "1M") {
  savespots = seq(min(draws_step,ndraw),ndraw,length.out=100) ## draws at which to save output
} else {
  savespots = seq(min(draws_step,ndraw),ndraw,length.out=10) ## draws at which to save output
}

# Number of cores to use in parallelization
ncore = 1   

# Output file
filename = paste0("output/mcmc_out_",type)
if (type == "baseline") {
  filename_overleaf = paste0("output/baseline/figures/mcmc_out_",type)
} else {
  filename_overleaf = paste0("output/appendix/figures/mcmc_out_",type)
}

#-------------------------------------------------------------------------------