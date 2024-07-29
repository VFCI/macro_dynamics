
#-------------------------------------------------------------------------------

# Load data
filename_mc = paste0("output/","mcmc_out_",type,"_ir",".Rdata")
filename_iv = paste0("output/","svariv_lpiv_chol_sn_",type,".Rdata")
base::load(filename_mc)
base::load(filename_iv)

#-------------------------------------------------------------------------------
# Note - There are 4 configurable parameters 
#-------------------------------------------------------------------------------

# I. General Configuration

# VFCI with MP or Y?
#vfci_pair = "ff"  #choose 'ff' or 'y' ##### CONFIG 1 #####

# Color of plots (don't change this)
if (vfci_pair == "ff") {
  rgb_col <<- rgb(1,0,.5,0.2) #Pink IRF shade (global var since exported to another script)
} else {
  rgb_col <<- rgb(0,0,1,0.2)  #Blue IRF shade (global var since exported to another script)
}

#-------------------------------------------------------------------------------
# II. This section generalizes the code to capture either the FF or Y IRFs
#      For baseline as well as for robustness

if (vfci_pair == "ff") {
  col1 <- "Fed Funds shock to VFCI"
  col2 <- "VFCI shock to Fed Funds" 
  vol_bvar_second_var <- "Fed Funds"
  vol_bvar_type_vfci <- var_names[3]
  GDP_FF_FOR_IV <- "fedfunds"
  GDP_FF_FOR_CHOL <- "fedfunds"
  SVAR_GDP_OR_FF <- df_irf_ff
  CHOL_GDP_OR_FF <- chol_irf_vfci
  LP_GDP_OR_FF  <-  results_lin_iv_ff
  vfci_type <- "vfci"
  lp_data_vfci_shock <- c("lgdp", "lpce", "fedfunds", "vfci")
  lp_vfci_shock_on <- which(lp_data_vfci_shock == "fedfunds")
      if (type == "stationary") {
      lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci")
      }
      if (type == "vfci_lev") {
        vfci_type <- "vfci_lev"
        lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci_lev")
      }
  } else {
  col1 <- "Output shock to VFCI"
  col2 <- "VFCI shock to Output" 
  vol_bvar_second_var <- var_names[1] #GDP
  vol_bvar_type_vfci <- var_names[3]
  GDP_FF_FOR_IV <- "lgdp"
  GDP_FF_FOR_CHOL <- "lgdp"
  SVAR_GDP_OR_FF <- df_irf_y
  CHOL_GDP_OR_FF <- chol_irf_vfci 
  LP_GDP_OR_FF  <-  results_lin_iv_y
  vfci_type <- "vfci"
  lp_data_vfci_shock <- c("lgdp", "lpce", "fedfunds", "vfci")
  lp_vfci_shock_on <- which(lp_data_vfci_shock == "lgdp")
      if (type == "stationary") {
      GDP_FF_FOR_IV <- "ygr"
      GDP_FF_FOR_CHOL <- "ygr"
      lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci")
      lp_vfci_shock_on <- which(lp_data_vfci_shock == "ygr")
      }
      if (type == "vfci_lev") {
        vfci_type <- "vfci_lev"
        lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci_lev")
      }
  }

#-------------------------------------------------------------------------------

# III. Export panel to pdf or visualize in plot window
export_pdf <- 1                  ##### CONFIG 4 #####

if (export_pdf == 1) {
  if (type == "baseline") {
    fname <- here::here(paste0("output/baseline/figures/",vfci_pair,'-vfci-',type,'.pdf', sep = ''))
  } else {
    fname <- here::here(paste0("output/appendix/figures/",vfci_pair,'-vfci-',type,'.pdf', sep = ''))
  }
  if (vfci_pair == "ff") {
      pdf(fname, width = 8, height = 11)
  } else {
      pdf(fname, width = 8, height = 11*(4/5))
  }
}

#-------------------------------------------------------------------------------

# IV. Plot layout
# Don't change anything in this section
if (vfci_pair == "ff") {
layout.mat <- matrix(seq(1,5*3,1),
                     nrow = 5, ncol = 3,
                     byrow = TRUE)
} else {
  layout.mat <- matrix(seq(1,4*3,1),
                       nrow = 4, ncol = 3,
                       byrow = TRUE)
}

layout(mat = layout.mat,widths = c(1,2.5,2.5))
par(col.lab="black",col.main="black", 
    mar=c(2,1,2,4), oma=c(1,1,1,1), tcl=0.1, mgp=c(3,1,0))

xticks <- seq(1,20,1) #Limit for plots so they are in the same scale
x_tick_same <- c(1,5,10,15,20)
xlimff <- c(min(xticks),max(xticks))

#--------------------------------------------------------------------
#Model 1: Vol-BVAR
#--------------------------------------------------------------------

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
mtext("Vol-BVAR", side = 3, line = 0, cex = .85,col="blue4",at = 0.25)

impulseplots_panel(
  ir = apply(ir$ir[,,,1,],c(1:3),median),
  irdraws = ir$ir[,,,1,],
  varind = which(var_names == vol_bvar_type_vfci), ## Index of the variable for which the plot is specified
  findvar = which(var_names == vol_bvar_second_var), ##Shock
  conf = c(.68,.90), ## what confidence bands to put on the plot
  sstruct = "FROM",
  color = NA, ##Color of IRFs
  nsteps = irf_steps,
  varnames = var_names,
  savedata = FALSE,newplot =FALSE,plotyaxis = TRUE,plottitle = FALSE)
mtext(col1, side = 3, line = 0, cex = 1.1)

impulseplots_panel(
  ir = apply(ir$ir[,,,1,],c(1:3),median),
  irdraws = ir$ir[,,,1,],
  varind = which(var_names == vol_bvar_second_var), ## Index of the variable for which the plot is specified
  findvar = which(var_names == vol_bvar_type_vfci),
  conf = c(.68,.90), ## what confidence bands to put on the plot
  sstruct = "FROM",
  color = NA, ##Color of IRFs
  nsteps = irf_steps,
  varnames = var_names,
  savedata = FALSE,newplot =FALSE,plotyaxis = TRUE,plottitle = FALSE)
mtext(col2, side = 3, line = 0, cex = 1.1)


#--------------------------------------------------------------------
#Model 2: SVAR-IV
#--------------------------------------------------------------------
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
mtext("SVAR-IV", side = 3, line = 0, cex = .85,col="blue4",at = 0.25)

ylimff <- c(min(SVAR_GDP_OR_FF$response.lower[SVAR_GDP_OR_FF$target == vfci_type])
            ,max(SVAR_GDP_OR_FF$response.upper[SVAR_GDP_OR_FF$target == vfci_type]))
ytick <- pretty(ylimff,4)
plot(xticks,SVAR_GDP_OR_FF$response[SVAR_GDP_OR_FF$target == vfci_type],type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimff)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at=ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at =c(1,5,10,15,20))
lines(xticks,SVAR_GDP_OR_FF$response.upper[SVAR_GDP_OR_FF$target == vfci_type],col=NA,lty=2,lwd=2)
lines(xticks,SVAR_GDP_OR_FF$response.lower[SVAR_GDP_OR_FF$target == vfci_type],col=NA,lty=2,lwd=2)
polygon(c(xticks, rev(xticks)), c(SVAR_GDP_OR_FF$response.lower[SVAR_GDP_OR_FF$target == vfci_type],
      rev(SVAR_GDP_OR_FF$response.upper[SVAR_GDP_OR_FF$target == vfci_type])), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)), c(SVAR_GDP_OR_FF$response.lower.68[SVAR_GDP_OR_FF$target == vfci_type],
                                  rev(SVAR_GDP_OR_FF$response.upper.68[SVAR_GDP_OR_FF$target == vfci_type])), col=rgb_col,border=NA)

ylimvfci <- c(min(df_irf_vfci$response.lower[df_irf_vfci$target == GDP_FF_FOR_IV])
              ,max(df_irf_vfci$response.upper[df_irf_vfci$target == GDP_FF_FOR_IV]))
ytick <- pretty(ylimvfci,4)
plot(xticks,df_irf_vfci$response[df_irf_vfci$target == GDP_FF_FOR_IV],type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimvfci)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1)
axis(side = 1,cex.axis = 1.25,las = 1,at =c(1,5,10,15,20))
lines(xticks,df_irf_vfci$response.upper[df_irf_vfci$target == GDP_FF_FOR_IV],col=NA,lty=2,lwd=2)
lines(xticks,df_irf_vfci$response.lower[df_irf_vfci$target == GDP_FF_FOR_IV],col=NA,lty=2,lwd=2)
polygon(c(xticks, rev(xticks)), c(df_irf_vfci$response.lower[df_irf_vfci$target == GDP_FF_FOR_IV],
                                  rev(df_irf_vfci$response.upper[df_irf_vfci$target == GDP_FF_FOR_IV])), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)), c(df_irf_vfci$response.lower.68[df_irf_vfci$target == GDP_FF_FOR_IV],
                                  rev(df_irf_vfci$response.upper.68[df_irf_vfci$target == GDP_FF_FOR_IV])), col=rgb_col,border=NA)

#--------------------------------------------------------------------
#Model 3: LP-IV
#--------------------------------------------------------------------
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
mtext("LP-IV", side = 3, line = 0, cex = .85,col="blue4",at = 0.25)

ylimff <- c(min(LP_GDP_OR_FF$irf_lin_low[3,])
            ,max(LP_GDP_OR_FF$irf_lin_up[3,]))
ytick <- pretty(ylimff,4)
plot(xticks,LP_GDP_OR_FF$irf_lin_mean[3,],type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimff)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at=ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at =c(1,5,10,15,20))
lines(xticks,LP_GDP_OR_FF$irf_lin_up[3,],col=NA,lty=2,lwd=2)
lines(xticks,LP_GDP_OR_FF$irf_lin_low[3,],col=NA,lty=2,lwd=2)
polygon(c(xticks, rev(xticks)), c(LP_GDP_OR_FF$irf_lin_low[3,], rev(LP_GDP_OR_FF$irf_lin_up[3,])), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)), c(LP_GDP_OR_FF$irf_lin_low.68[3,], rev(LP_GDP_OR_FF$irf_lin_up.68[3,])), col=rgb_col,border=NA)


ylimvfci <- c(min(results_lin_iv_vfci$irf_lin_low[lp_vfci_shock_on,])
              ,max(results_lin_iv_vfci$irf_lin_up[lp_vfci_shock_on,]))
ytick <- pretty(ylimvfci,4)
plot(xticks,results_lin_iv_vfci$irf_lin_mean[lp_vfci_shock_on,],type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimvfci)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at=ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at =c(1,5,10,15,20))
lines(xticks,results_lin_iv_vfci$irf_lin_up[lp_vfci_shock_on,],col=NA,lty=2,lwd=2)
lines(xticks,results_lin_iv_vfci$irf_lin_low[lp_vfci_shock_on,],col=NA,lty=2,lwd=2)
polygon(c(xticks, rev(xticks)),c(results_lin_iv_vfci$irf_lin_low[lp_vfci_shock_on,], rev(results_lin_iv_vfci$irf_lin_up[lp_vfci_shock_on,])), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)),c(results_lin_iv_vfci$irf_lin_low.68[lp_vfci_shock_on,], rev(results_lin_iv_vfci$irf_lin_up.68[lp_vfci_shock_on,])), col=rgb_col,border=NA)


#--------------------------------------------------------------------
#Model 4: Cholesky 
#--------------------------------------------------------------------
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
mtext("Cholesky", side = 3, line = 0, cex = .85,col="blue4",at = 0.25)

ylimff <- c(min(CHOL_GDP_OR_FF$response.lower[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL])
            ,max(CHOL_GDP_OR_FF$response.upper[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL]))
ytick <- pretty(ylimff,4)
plot(xticks,CHOL_GDP_OR_FF$response[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL],type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimff)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at=ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at =c(1,5,10,15,20))
polygon(c(xticks, rev(xticks)),c(CHOL_GDP_OR_FF$response.lower[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL],
                                 rev(CHOL_GDP_OR_FF$response.upper[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL])), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)),c(CHOL_GDP_OR_FF$response.lower.68[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL],
                                 rev(CHOL_GDP_OR_FF$response.upper.68[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL])), col=rgb_col,border=NA)


ylimvfci <- c(min(chol_irf_vfci$response.lower[chol_irf_vfci$target == GDP_FF_FOR_CHOL & chol_irf_vfci$shock == vfci_type])
              ,max(chol_irf_vfci$response.upper[chol_irf_vfci$target == GDP_FF_FOR_CHOL & chol_irf_vfci$shock == vfci_type]))
ytick <- pretty(ylimvfci,4)
plot(xticks,chol_irf_vfci$response[chol_irf_vfci$target == GDP_FF_FOR_CHOL & chol_irf_vfci$shock == vfci_type],type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimvfci)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at = ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at =c(1,5,10,15,20))
polygon(c(xticks, rev(xticks)),c(chol_irf_vfci$response.upper[chol_irf_vfci$target == GDP_FF_FOR_CHOL& chol_irf_vfci$shock == vfci_type],
                                 rev(chol_irf_vfci$response.lower[chol_irf_vfci$target == GDP_FF_FOR_CHOL& chol_irf_vfci$shock == vfci_type])), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)),c(chol_irf_vfci$response.upper.68[chol_irf_vfci$target == GDP_FF_FOR_CHOL& chol_irf_vfci$shock == vfci_type],
                                 rev(chol_irf_vfci$response.lower.68[chol_irf_vfci$target == GDP_FF_FOR_CHOL& chol_irf_vfci$shock == vfci_type])), col=rgb_col,border=NA)

#--------------------------------------------------------------------
#Model 5: Sign Restrictions 
#--------------------------------------------------------------------

if (vfci_pair == "ff") {
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
mtext("Sign Restr", side = 3, line = 0, cex = .85,col="blue4",at = 0.25)

ylimff <- c(min(apply(irfs_mp[, ,3],2,function(x) quantile(x,0.05)))
            ,max(apply(irfs_mp[, ,3],2,function(x) quantile(x,0.95))))
ytick <- pretty(ylimff,4)
plot(xticks,apply(irfs_mp[, ,3],2,median),type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = range(ytick))
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at=ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at = c(1,5,10,15,20))
polygon(c(xticks, rev(xticks)), c(apply(irfs_mp[, ,3],2,function(x) quantile(x,0.16)), rev(apply(irfs_mp[, ,3],2,function(x) quantile(x,0.84)))), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)), c(apply(irfs_mp[, ,3],2,function(x) quantile(x,0.05)), rev(apply(irfs_mp[, ,3],2,function(x) quantile(x,0.95)))), col=rgb_col,border=NA)

ylimvfci <- c(min(apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.05)))
              ,max(apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.95))))
ytick <- pretty(ylimvfci,4)
plot(xticks,apply(irfs_vfci[, ,4],2,median),type='l',col= "blue4",lty= 1,lwd = 2,
     xlab = '',ylab = '',yaxt  = 'n',xaxt = 'n',xlim = xlimff, xaxs = 'i',ylim = ylimvfci)
abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
abline(v = x_tick_same, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
abline(a=0,b=0,lwd=0.75)
axis(side = 2, cex.axis = 1.25, las = 1,at=ytick)
axis(side = 1,cex.axis = 1.25,las = 1,at = c(1,5,10,15,20))
polygon(c(xticks, rev(xticks)), c(apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.16)),rev(apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.84)))), col=rgb_col,border=NA)
polygon(c(xticks, rev(xticks)), c(apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.05)),rev(apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.95)))), col=rgb_col,border=NA)
}

if (export_pdf == 1) {
  dev.off()
}
