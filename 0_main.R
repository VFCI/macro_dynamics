#-------------------------------------------------------------------------------
# Paper: The Market Price of Risk and Macro-Financial Dynamics
# Purpose: This code replicates the results from the Adrian, Duarte, Iyer (2023) paper
# Author: Tara Iyer
# Date: Feb 2023
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Instructions
# To replicate all the results, change the directory in "path" and run this code
# All the tables and IRFs in the paper and appendix will be outputted into the Output folder
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#0. Preliminary
#-------------------------------------------------------------------------------

# Clear workspace and set path
rm(list=ls(all = TRUE))
path <- "//data4/users2/TIyer/My Documents/vfci_feb2023"
setwd(path)

# Load functions and packages
source('load_functions.R')
load_functions() 

#-------------------------------------------------------------------------------
#1. Load Data
#-------------------------------------------------------------------------------

vfci_data <- openxlsx::readWorkbook("dataset_vfci_feb2023.xlsx")
vfci_data$date = seq.Date(as.Date('1962-01-01'),as.Date('2022-07-01'),by = 'quarter')

#-------------------------------------------------------------------------------
#2. Results in Paper
#-------------------------------------------------------------------------------

# Main results of paper 
type <- "baseline"

# Volatility BVAR
source('1_vol_bvar_calibration.R')
source('1_vol_bvar_estimation.R')
source('1_vol_bvar_output.R')

# SVAR-IV, LP-IV, Cholesky, Sign Restrictions
source('2_svariv_lpiv_chol_sn_calibration.R')
source('2_svariv_lpiv_chol_sn_estimation.R')
source('2_svariv_lpiv_chol_sn_output.R')

# Panel: All models 
ff_y <- c("ff", "y")
for (i in ff_y) {
  vfci_pair = i
  source('3_panel_all_models.R')
}


#-------------------------------------------------------------------------------
#3. Robustness of the five identification schemes [Internet Appendix]
#-------------------------------------------------------------------------------

#Robustness
  # 1. Models specified in stationary terms
  # 2. Models specified with exponential VFCI

five_model_robustness <- c("stationary", "vfci_lev")
for (i in five_model_robustness) {
  type = i
  
  # Volatility BVAR
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
  
  # SVAR-IV, LP-IV, Cholesky, Sign Restrictions
  source('2_svariv_lpiv_chol_sn_calibration.R')
  source('2_svariv_lpiv_chol_sn_estimation.R')
  source('2_svariv_lpiv_chol_sn_output.R')
  
  # Panel: All models 
  ff_y <- c("ff", "y")
  for (i in ff_y) {
    vfci_pair = i
    source('3_panel_all_models.R')
  }
}

#-------------------------------------------------------------------------------
#4.  Robustness of the heteroskedastic BVAR [Internet Appendix]
#-------------------------------------------------------------------------------

#Robustness
# 3. Regime-specific IRFs across the 7 regimes
# 4. Data sample before 2008-10 global financial crisis
# 5. Normal distribution of errors in BVAR
# 6. Horserace with ECY
# 7. Horserace with GZ
# 8. Horserace with TEDR
# 9. Horserace with NFCI
# 10. 100,000 draws in the MCMC chain

vol_bvar_robustness_a <- c("regimes", "pre_crisis", "normal", "100k")  
for (i in vol_bvar_robustness_a) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}
 
vol_bvar_robustness_b <- c("horserace_gz", "horserace_tedr","horserace_ecy", "horserace_nfci")  
for (i in vol_bvar_robustness_b) {
  type = i
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}

# Other robustness checks conducted but not reported in the appendix include:
  # alternate specifications of the tightness and decay parameters
  # alternate versions of VFCI with alternate PCs and with original variables instead of the PCs
  # alternate draws of the MCMC chain
# These additional checks also corroborate the baseline results.

#-------------------------------------------------------------------------------

# NOTE: In case '1_vol_bvar_estimation.R' results in non-convergence (rare but possible due to randomness in the optimizer algorithm),
#       then do not change anything in the code but just run it again. Specifically, run:
#
#       type = "[insert type of model here]" 
#       source('1_vol_bvar_calibration.R')
#       source('1_vol_bvar_estimation.R')
#       source('1_vol_bvar_output.R')
#
# "[insert type of model here]" to be replaced by one or more of the following options where convergence fails
# "baseline", "regimes", "stationary", "vfci_lev", "pre_crisis", "normal", "100k", "horserace_gz", "horserace_tedr","horserace_ecy", "horserace_nfci"