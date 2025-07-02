#-------------------------------------------------------------------------------
# Estimate reduced form model
#-------------------------------------------------------------------------------
dat_VAR  <- vfci_data[,vars_in_system]
dat_VAR  <- dat_VAR[apply(dat_VAR,1,function(x) sum(is.na(x)))==0,]
rf_model <- vars::VAR(dat_VAR,p = nlags)

#-------------------------------------------------------------------------------
# Calculate posterior
#-------------------------------------------------------------------------------

set.seed(1234)

optout         = TvvDir(input_in_var, 
    freq       = 'quarterly',
    vars       = vars,
    logseries  = log_trans,
    nStep      = irf_steps, 
    nLags      = nlags,
    lcA0       = lcA0,            # no restrictions
    lmdblock   = NULL,            # default option: all variances change in all periods
    strTsigbrk = regime_dates,
    vprior     = 0,               # default option, weight 0.01 on each row
    mntight    = mn_tight_calib,  
    mndecay    = mn_decay_calib, 
    startdate  = startdate_calibration,
    enddate    = enddate_calibration,
    verbose    = TRUE
    )

n_var          <<- length(vars)             # number of variables
n_reg          <<- length(regime_dates) + 1 # number of variance regimes 

#############################################################

# Save output
save(optout,file = 'output/analysis_data/gmode.Rdata')

#-------------------------------------------------------------------------------
# Gibbs sampling
#-------------------------------------------------------------------------------

# MCMC starting point
if (disperse){
    optout$x = optout$x + MASS::mvrnorm(n = 1, mu = rep(0,length(optout$x)), Sigma = optout$opt$H)
}

if (my_choice == 'gaussian'){
    mcmc_output = gdraw(optout,
                        ndraw = ndraw,
                        nburn = 1000,
                        nsep = nsep,
                        filename = paste(filename,'.Rdata',sep=''),
                        savespots = savespots,
                        hsnfactor = hessian_scaling,
                        alpha = 1,
                        k = 1,
                        drawa = TRUE,  # if TRUE, save the A_{1:nlag}
                        dscore = TRUE, # if TRUE, save the log inverse gamma densities, which is useful for MDD calculation
                        drawe = TRUE   # If TRUE, save the structural residuals
    )
    
    ## COMPUTE MARGINAL DATA DENSITY 
    ## Outputs are mout[1]: uncorrected mdd
   	##			   mout[2]: corrected mdd using the log density weights in efac
   	##			   cv     : covariance matrix
   	
    mdd_output <- get_mdd(t(mcmc_output$xout), mcmc_output$lhout,
    					efac  = mcmc_output$dsout,      # density component from non-normal adj parameters
    					trunc = 0.95)
    						    
    
} else if (my_choice == 't'){
    
    df = 2.51928019                    # For VFCI with 10,000 draws
    scale = 1;                         # normalization; variances are free to adjust
    
    mcmc_output = gdraw(optout,
                        ndraw = ndraw,
                        nburn = 1000,
                        nsep = nsep,
                        filename = paste(filename,'.Rdata',sep=''),
                        savespots = savespots,
                        hsnfactor = hessian_scaling,
                        tparam  = df / 2,   # this is the right parameter to feed in for t(df)
                        tscale = scale,     # scale of the t distribution. So the draw is from IG(tparam,tscale^2 * tparam)
                        drawa = TRUE,       # if TRUE, save the A_{1:nlag}
                        dscore = TRUE,      # if TRUE, save the log inverse gamma densities, which is useful for MDD calculation
                        drawe = TRUE        # if TRUE, save the structural residuals
    )
    
    ## COMPUTE MARGINAL DATA DENSITY
    mdd_output <- get_mdd(t(mcmc_output$xout), mcmc_output$lhout,
    					efac  = mcmc_output$dsout,      # density component from non-normal adj parameters
    					trunc = 0.95)
      
}

# Save output
save(mdd_output,file = paste0('output/analysis_data/gibbs_out_',type,'.Rdata'))


#-------------------------------------------------------------------------------