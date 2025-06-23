
#-------------------------------------------------------------------------------

save(df_irf_ff, df_irf_vfci, df_irf_y, results_lin_iv_ff, results_lin_iv_vfci, results_lin_vfci,
     results_lin_iv_y, chol_irf_vfci, chol_irf_vfci_first, irfs_mp, irfs_vfci, 
     file=paste0("output/svariv_lpiv_chol_sn_",type,".Rdata"))


#-------------------------------------------------------------------------------