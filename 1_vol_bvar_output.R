#-------------------------------------------------------------------------------
# Relative variances per regime (for baseline model)
#-------------------------------------------------------------------------------
if (avg_regime == 1 & type == "baseline") {
    lambda = array(0,c(n_var,n_reg,ndraw)) 
    lambda[,1:(n_reg-1),] = array(mcmc_output$xout[(n_var^2 + (n_reg-1)*n_var - n_var*(n_reg-1) + 1):(n_var^2 + (n_reg-1)*n_var),],c(n_var,n_reg-1,ndraw))
    lambda[,n_reg,] = n_reg - apply(lambda[,1:(n_reg-1),], c(1,3), sum) # retrieving the normalized variance
    
    lambda_median = apply(lambda, c(1,2), median); # taking the median across draws
    lambda_median <- format(round(lambda_median, 2))
    table_rel_variance <- as.data.frame(lambda_median)
    row.names(table_rel_variance) <- c("Log GDP", "Log PCE", "VFCI", "Fed Funds")
    colnames(table_rel_variance) <- c("1962Q1-1979Q3", "1979Q3-1982Q4", "1983Q1-1989Q4", "1990Q1-2007Q4", "2008Q1-2010Q4", "2011Q1-2019Q4", "2020Q1-2022Q3")

    #Export to TeX
    library("xtable")
    tab<-xtable(table_rel_variance,  align=c("l","c","c","c","c","c","c","c"))
    print(tab,file="output/baseline/tables/table_rel_var.tex",append=F)
}

#-------------------------------------------------------------------------------
# Impulse Response Functions (IRFs)
#-------------------------------------------------------------------------------

thin = seq(1,dim(mcmc_output$xout)[2],by=1) 

if (avg_regime == 1) {

	mcmc_output$xout[(n_var^2 + (n_reg-1)*n_var - n_var*(n_reg-1) + 1):(n_var^2 + (n_reg-1)*n_var - n_var*(n_reg-1) + n_var),] = 1
	r = 1 # to scale ir to an "average" regime, set the variances in the first period to 1 as in BPSS (2021)
	ir = McmcIr(t(mcmc_output$xout)[thin,],
	            optout, 
	            lrange = r, 
	            cores = ncore, 
	            aplus = mcmc_output$aout[,,thin]
	)
	
	save(ir,file = paste(filename,'_ir.Rdata',sep=''))

    #--------------------------------------------------
    #VFCI shock
    #--------------------------------------------------

	medir <- apply(ir$ir[,,,1,],c(1:3),median)
	
    #1-GDP; 2-PCE, 3-VFCI, 4-FF
    if (length(vars) == 4) {
   		
        impulseplots_subset(
          ir = medir,
          irdraws = ir$ir[,,,1,],
          varind = 3, # Index of the variable for which the plot is specified
          conf = c(.68,.90), # Confidence bands
          findvar = c(1,4,2,3), 
          y_axis_custom = yaxis_vfci_shock,
          sstruct = "TO",
          color = c(0,0.6,0), ##Color of IRFs
          nsteps = irf_steps,
          varnames = var_names,
          filename = paste(filename_overleaf,'_irplot_all_shocks',sep=''),
          width = fig_width, height = fig_height,savedata = FALSE,newplot = TRUE)
        
        #--------------------------------------------------
        #VFCI response
        #--------------------------------------------------
        
        #1-GDP; 2-PCE, 3-VFCI, 4-FF
        
        impulseplots_subset(
          ir = medir,
          irdraws = ir$ir[,,,1,],
          varind = 3, # Index of the variable for which the plot is specified
          conf = c(.68,.90), # Confidence bands
          findvar = c(1,4,2,3), 
          y_axis_custom = yaxis_vfci_response,
          sstruct = "FROM",
          color = c(0,0.6,0), 
          nsteps = irf_steps,
          varnames = var_names,
          filename = paste(filename_overleaf,'_irplot_all_shocks',sep=''),
          width = fig_width, height = fig_height,savedata = FALSE,newplot = TRUE)
   
		## VARIANCE DECOMPOSITION
	    vd <- ir$ir[,,,1,]
	
		for (iv in 1:dim(vd)[4]){
		    vd[,,,iv] <- vdecomp(ir$ir[,,,1,iv])
		}
		
		blocks = list(list(x=c(1:4),y=c(1:4)))
		
		impulseplots(
		    ir = apply(vd,c(1:3),median),
		    irdraws = vd,
		    shocknames = var_names,
		    blocks = blocks,
		    filename = paste(filename_overleaf,'_vd',sep=''),
		    varnames = var_names,
		    width = 8, height = 5,nsteps = 20,ylims = c(0,1))


 } else if (length(vars == 5)) {
   
   #--------------------------------------------------
   #VFCI shock [horserace]
   #--------------------------------------------------
   
   impulseplots_subset(
     ir = medir,
     irdraws = ir$ir[,,,1,],
     varind = 3, ## Index of the variable for which the plot is specified
     conf = c(.68,.90), ## what confidence bands to put on the plot
     findvar = c(1,5,2,3,4), ##orders the variables in the IRFs
     y_axis_custom = yaxis_vfci_shock,
     sstruct = "TO",
     color = c(0,0.6,0), ##Color of IRFs
     nsteps = irf_steps,
     varnames = var_names,
     filename = paste(filename_overleaf,'_irplot_all_shocks',sep=''),
     format = 'svg',#'r_plot',
     width = fig_width, height = fig_height,savedata = FALSE,newplot = TRUE)
   
   #--------------------------------------------------
   #VFCI response [horserace]
   #--------------------------------------------------
   impulseplots_subset(
     ir = medir,
     irdraws = ir$ir[,,,1,],
     varind = 3, ## Index of the variable for which the plot is specified
     conf = c(.68,.90), ## what confidence bands to put on the plot
     findvar = c(1,5,2,3,4), ##orders the variables in the IRFs
     y_axis_custom = yaxis_vfci_response,
     sstruct = "FROM",
     color = c(0,0.6,0), ##Color of IRFs
     nsteps = irf_steps,
     varnames = var_names,
     filename = paste(filename_overleaf,'_irplot_all_shocks',sep=''),
     width = fig_width, height = fig_height,savedata = FALSE,newplot = TRUE)
 }

#--------------------------------------------------
#All shocks and all variables
#--------------------------------------------------

	if (type == "baseline") {
	    blocks = list()
	    if (length(vars) == 4) {
	      blocks[[1]] = list(x = 1:4,y=1:4)
	    } else if (length(vars) == 5) {
	      blocks[[1]] = list(x = 1:5,y=1:5)
	    }
	    impulseplots(
	      ir = medir,
	      irdraws = ir$ir[,,,1,],
	      conf = c(0.68,0.90), ## posterior uncertainty bands
	      color = c(0,0.6,0), ##Color of IRFs
	      nsteps = irf_steps,
	      varnames = var_names,
	      shocknames = shock_names,
	      filename = paste(filename_overleaf,'_irplot_all_shocks',sep=''),
	      blocks = blocks,
	      width = 6, height = 7)
	  }

} else if (avg_regime == 0) {
  for (r in 1:regime_calib) {
    ir = McmcIr(t(mcmc_output$xout)[thin,],
                optout, ## this is the posterior mode file. as written, the function extracts the names of the variables and a few other things from here (not actually used in the calculation)
                lrange = r, ## which variance regimes ("lambdas") for which to report IR. this was useful when I was graphing the IR separately for each regime --- if the goal is to look at an "average" regime with the new rescaling, there's no reason not to just 1
                cores = ncore, 
                ##oweights = mcmcout$dout ## these are the extra weights (the variance shock draws) that you need to take account of to get the correct reduced form residuals
                aplus = mcmc_output$aout[,,thin])
    
    medir <- apply(ir$ir[,,,1,],c(1:3),median)
  #------------------------------------------------------
  ### VFCI shock [regime-specific]
  #------------------------------------------------------
    
    #1-GDP; 2-PCE, 3-VFCI, 4-FF
    impulseplots_subset(
      irdraws = ir$ir[,,,1,],
      ir = medir,
      varind = 3, ## Index of the variable for which the plot is specified
      conf = c(.68,.90), ## what confidence bands to put on the plot
      findvar = c(1,4,2,3), ##orders the variables in the IRFs
      y_axis_custom = yaxis_vfci_shock,
      sstruct = "TO",
      color = c(0,0.6,0), ##Color of IRFs
      nsteps = irf_steps,
      varnames = var_names,
      filename = paste('output/appendix/figures/irplot_all_shocks','_reg_',r,sep=''),
      format = 'svg',#'r_plot',
      width = fig_width, height = fig_height,savedata = FALSE,newplot = TRUE)
    
  #------------------------------------------------------
  # VFCI response [regime-specific]
  #------------------------------------------------------
    
    impulseplots_subset(
      irdraws = ir$ir[,,,1,],
      ir = medir,
      varind = 3, ## Index of the variable for which the plot is specified
      conf = c(.68,.90), ## what confidence bands to put on the plot
      findvar = c(1,4,2,3), ##orders the variables in the IRFs
      y_axis_custom = yaxis_vfci_response,
      sstruct = "FROM",
      color = c(0,0.6,0), ##Color of IRFs
      nsteps = irf_steps,
      varnames = var_names,
      filename = paste('output/appendix/figures/irplot_all_shocks','_reg',r,sep=''),
      width = fig_width, height = fig_height,savedata = FALSE,newplot = TRUE)
    
     print(r)

  }
}

#-------------------------------------------------------------------------------
