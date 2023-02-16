#-------------------------------------------------------------------------------
# Paper: The Market Price of Risk and Macro-Financial Dynamics
# Purpose: Saves the output from the following four identification schemes for VFCI
  # 1. SVAR-IV
  # 2. LP-IV
  # 3. Cholesky
  # 4. Sign restrictions
# Author: Tara Iyer
# Date: Feb 2023
#-------------------------------------------------------------------------------

save(df_irf_ff, df_irf_vfci, df_irf_y, results_lin_iv_ff, results_lin_iv_vfci, 
     results_lin_iv_y, chol_irf_vfci, irfs_mp, irfs_vfci, 
     file=paste0(path,"/output","/svariv_lpiv_chol_sn_",type,".Rdata"))

#-------------------------------------------------------------------------------