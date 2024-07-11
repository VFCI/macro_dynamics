
#-------------------------------------------------------------------------------

save(df_irf_ff, df_irf_vfci, df_irf_y, results_lin_iv_ff, results_lin_iv_vfci, 
     results_lin_iv_y, chol_irf_vfci, irfs_mp, irfs_vfci, 
     file=here::here("output",paste0("svariv_lpiv_chol_sn_",type,".Rdata")))


#-------------------------------------------------------------------------------