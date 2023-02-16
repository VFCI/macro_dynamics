# macro_dynamics

**A. How to run this:**

To generate all the IRFs in the paper and appendix, open "0_Main.R", change the directory in line 20, and run the script.

It will run through the models and output the required output for the paper in the folder called "Output", which can then be uploaded to Overleaf.



**B: General workflow:**

* A number of scenarios assigned to "type" are found in 0_Main.R. 
* For several of these model types, the following seven 7 scripts are called (those cases where a panel chart with all five models is generated in the end). 
* For other model types, only the scripts with 1_.. are called (those cases where only the BPSS BVAR is used)


**1_vol_bvar_calibration.R:** Provides the calibration for all scenarios (variable in the BPSS BVAR, Bayesian parameters, configuration for the IRFs etc)

**1_vol_bvar_estimation.R:** Calculates the posterior mode and runs the MCMC algorithm using functions in the Functions folder

**1_vol_bvar_output.R:** Generates the IRF panels using functions in the Functions folder


**2_svar_lpiv_chol_sn_calibration.R:** Provides the calibration for all scenarios (for all the four other identification schemes: SVAR-IV, LP-IV, Cholesky, Sign Restrictions)

**2_svar_lpiv_chol_sn_estimation.R:** Estimates the four other identification schemes

**2_svar_lpiv_chol_sn_output.R:** Outputs the results from the four other identification schemes


**3_panel_all_models.R:** Pulls in the rsults from the five identification schemes and creates the panel graphs for Fed Funds<-->VFCI and Output<-->VFCI
