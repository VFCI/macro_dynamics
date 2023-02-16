#-------------------------------------------------------------------------------
# Paper: The Market Price of Risk and Macro-Financial Dynamics 
# Purpose: Estimation of the time-varying heteroskedastic BVAR
# Adapted from BPSS (AER, 2021)
# Author: Tara Iyer
# Date: Feb 2023
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Calculate posterior
#-------------------------------------------------------------------------------

optout         = TvvDir(input_in_var, 
    freq       = 'quarterly',
    vars       = vars,
    logseries  = log_trans,
    nStep      = irf_steps, 
    nLags      = nlags_calibration,
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

# Save output
save(optout,file = 'output/gmode.Rdata')

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
                        nburn = 0,
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
} else if (my_choice == 't'){
    
    df = 2.51928019                    # For VFCI with 10,000 draws
    scale = 1;                         # normalization; variances are free to adjust
    
    mcmc_output = gdraw(optout,
                        ndraw = ndraw,
                        nburn = 0,
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
} 

#-------------------------------------------------------------------------------