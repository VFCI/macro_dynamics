## Load functions and packages
renv::restore()
source('library_packages.R')
source('load_functions.R')
load_functions() 

source('create_vfci.R')
source('create_figures.R')
source('create_tables.R')

# Load Data --------------------------------------------------------------------
base::load("variables.RData")
vfci_data <- variables
vfci_data$date = seq.Date(as.Date('1962-01-01'),as.Date('2022-07-01'),by = 'quarter')

#Set lags for all models
nlags    <- 4

# Main results -----------------------------------------------------------------
## Baseline results ------------------------------------------------------------
type <- "baseline"

### Volatility BVAR ------------------------------------------------------------
source('1_vol_bvar_calibration.R')
source('1_vol_bvar_estimation.R')
source('1_vol_bvar_output.R')

### SVAR-IV, LP-IV, Cholesky, Sign Restrictions --------------------------------
source('2_svariv_lpiv_chol_sn_calibration.R')
source('2_svariv_lpiv_chol_sn_estimation.R')
source('2_svariv_lpiv_chol_sn_output.R')

### Export Residuals when running baseline  ------------------------------------
if(type  == "baseline"){
  source("3_create_residual_mat.R")
}

## Test if the residual shocks are normally distributed
source("test_for_normality.R")

### Panel: All models ----------------------------------------------------------
ff_y <- c("ff", "y")
for (i in ff_y) {
  vfci_pair = i
  source('3_panel_all_models.R')
}

### Internal VFCI --------------------------------------------------------------
source('4_internal_vfci_var_calibration.R')
source('4_internal_vfci_var_estimation.R')
source('4_internal_vfci_var_output.R')

source('4b_internal_vfci_var_figures.R')


# Robustness of the five identification schemes --------------------------------

#Robustness
  # 1. Models specified in stationary terms
  # 2. Models specified with exponential VFCI

five_model_robustness <- c("stationary", "vfci_lev")
for (i in five_model_robustness) {
  type = i
  
  ## Volatility BVAR -----------------------------------------------------------
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
  
  ## SVAR-IV, LP-IV, Cholesky, Sign Restrictions -------------------------------
  source('2_svariv_lpiv_chol_sn_calibration.R')
  source('2_svariv_lpiv_chol_sn_estimation.R')
  source('2_svariv_lpiv_chol_sn_output.R')
  
  ## Panel: All models ---------------------------------------------------------
  ff_y <- c("ff", "y")
  for (i in ff_y) {
    vfci_pair = i
    source('3_panel_all_models.R')
  }
}

# Robustness of the heteroskedastic BVAR ---------------------------------------
#Robustness
# 3. Regime-specific IRFs across the 7 regimes
# 4. Data sample before 2008-10 global financial crisis
# 5. Normal distribution of errors in BVAR
# 6. Horserace with ECY
# 7. Horserace with GZ
# 8. Horserace with TEDR
# 9. Horserace with NFCI
# 10. 100,000 draws in the MCMC chain
# 11. 1 million draws in the MCMC chain

vol_bvar_robustness_a <- c("regimes", "pre_crisis", "normal", "100k", "1M")  
for (i in vol_bvar_robustness_a) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}

## MDD -------------------------------------------------------------------------
 
vol_bvar_robustness_b <- c("horserace_gz", "horserace_tedr", "horserace_ecy", "horserace_nfci")  
for (i in vol_bvar_robustness_b) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}

# Other robustness checks (not in Appendix) ------------------------------------
#Robustness
# 12. Remove VFCI and add GZ
# 13. Remove VFCI and add TED
# 14. Remove VFCI and add both GZ and TED
# 15. Add both GZ and TED

vol_bvar_robustness_c <- c("horserace_no_vfci_yes_gz","horserace_no_vfci_yes_tedr", "horserace_no_vfci_yes_gz_tedr","horserace_gz_tedr")
for (i in vol_bvar_robustness_c) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}

## Robustness
# 16. Replace VFCI with Total Log Vol
# 17. Replace VFCI with Residual Log Vol

vol_bvar_robustness_d <- c("total_log_vol", "resid_log_vol")
for (i in vol_bvar_robustness_d) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}

## Robustness
# 18. Baseline, but lower minnesota tightness parameter
# 19. Baseline, but higher minnesota tightness parameter
# 20. Baseline, but lower minnesota decay parameter
# 21. Baseline, but higher minnesota decay parameter


vol_bvar_robustness_e <- c("mn_tight_low", "mn_tight_med", "mn_tight_high", "mn_decay_low", "mn_decay_high")
for (i in vol_bvar_robustness_e) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}


## Robustness of VFCI Variations
vol_bvar_robustness_f <- c(
  "variation_vfci_pce",
  "variation_vfci_lev",
  "variation_vfci_pc3",
  "variation_vfci_pc5",
  "variation_vfci_ind",
  "variation_vfci_lags",
  "variaton_vfci_yields",
  "variation_vfci_no_rvol",
  "variation_vfci_stocks",
  "variation_vfci_ret"
)
for (i in vol_bvar_robustness_f) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}
