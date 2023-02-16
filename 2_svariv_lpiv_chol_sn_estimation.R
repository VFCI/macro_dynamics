#-------------------------------------------------------------------------------
# Paper: The Market Price of Risk and Macro-Financial Dynamics
# Purpose: Estimates the following four identification schemes for VFCI
  # 1. SVAR-IV
  # 2. LP-IV
  # 3. Cholesky
  # 4. Sign restrictions
# Author: Tara Iyer
# Date: Feb 2023
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------
#Model 1: SVAR-IV 
#--------------------------------------------------------------------

#Impact of the FF shock on all variables
if (type == "baseline") {
iv_var_ff <- sovereign::VAR(data = cbind(vfci_data_mp["date"],vfci_data_mp[,c(vars_in_system_baseline)],vfci_data_mp[mp_instrument]), 
                         p = 3, 
                         horizon = 20,
                         freq = 'quarter',
                         structure = 'IV',
                         instrument = mp_instrument, 
                         instrumented = "fedfunds")
} else if (type == "stationary") {
    iv_var_ff <- sovereign::VAR(data = cbind(vfci_data_mp["date"],vfci_data_mp[,c(vars_in_system_stationary)],vfci_data_mp[mp_instrument]), 
                                p = 3, 
                                horizon = 20,
                                freq = 'quarter',
                                structure = 'IV',
                                instrument = mp_instrument, 
                                instrumented = "fedfunds")
} else if (type == "vfci_lev") {
  iv_var_ff <- sovereign::VAR(data = cbind(vfci_data_mp["date"],vfci_data_mp[,c(vars_in_system_vfci_lev)],vfci_data_mp[mp_instrument]), 
                              p = 3, 
                              horizon = 20,
                              freq = 'quarter',
                              structure = 'IV',
                              instrument = mp_instrument, 
                              instrumented = "fedfunds")
}
df_irf_ff <- sovereign::var_irf(iv_var_ff, 
                                horizon = 20,
                                CI = c(0.05, 0.95))
df_irf_ff <- as.data.frame(df_irf_ff)

df_irf_ff_68 <- sovereign::var_irf(iv_var_ff, 
                                horizon = 20,
                                CI = c(0.16, 0.84))
df_irf_ff_68 <- as.data.frame(df_irf_ff_68)

df_irf_ff$response.lower.68 <- df_irf_ff_68$response.lower
df_irf_ff$response.upper.68 <- df_irf_ff_68$response.upper


#Impact of the VFCI shock on all variables
if (type == "baseline") {
iv_var_vfci <- sovereign::VAR(data = cbind(vfci_data["date"],vfci_data[,c(vars_in_system_baseline)],vfci_data[vfci_instrument]), 
                         p = 3, 
                         horizon = 20,
                         freq = 'quarter',
                         structure = 'IV',
                         instrument = vfci_instrument, 
                         instrumented = "vfci")
} else if (type == "stationary") {
  iv_var_vfci <- sovereign::VAR(data = cbind(vfci_data["date"],vfci_data[,c(vars_in_system_stationary)],vfci_data[vfci_instrument]), 
                                p = 3, 
                                horizon = 20,
                                freq = 'quarter',
                                structure = 'IV',
                                instrument = vfci_instrument, 
                                instrumented = "vfci")
} else if (type == "vfci_lev") {
  iv_var_vfci <- sovereign::VAR(data = cbind(vfci_data["date"],vfci_data[,c(vars_in_system_vfci_lev)],vfci_data[vfci_instrument]), 
                                p = 3, 
                                horizon = 20,
                                freq = 'quarter',
                                structure = 'IV',
                                instrument = vfci_instrument, 
                                instrumented = "vfci_lev")
}
df_irf_vfci <- sovereign::var_irf(iv_var_vfci, 
                                horizon = 20,
                                CI = c(0.05, 0.95))
df_irf_vfci <- as.data.frame(df_irf_vfci)

df_irf_vfci_68 <- sovereign::var_irf(iv_var_vfci, 
                                  horizon = 20,
                                  CI = c(0.16, 0.84))
df_irf_vfci_68 <- as.data.frame(df_irf_vfci_68)

df_irf_vfci$response.lower.68 <- df_irf_vfci_68$response.lower
df_irf_vfci$response.upper.68 <- df_irf_vfci_68$response.upper

#Impact of the growth shock on all variables
if (type == "baseline") {
iv_var_y <- sovereign::VAR(data = cbind(vfci_data_y["date"],vfci_data_y[,c(vars_in_system_stationary)],vfci_data_y[y_instrument]), 
                             p = 2, 
                             horizon = 20,
                             freq = 'quarter',
                             structure = 'IV',
                             instrument = y_instrument, 
                             instrumented = "ygr")
} else if (type == "stationary") {
  iv_var_y <- sovereign::VAR(data = cbind(vfci_data_y["date"],vfci_data_y[,c(vars_in_system_stationary)],vfci_data_y[y_instrument]), 
                             p = 2, 
                             horizon = 20,
                             freq = 'quarter',
                             structure = 'IV',
                             instrument = y_instrument, 
                             instrumented = "ygr")
} else if (type == "vfci_lev") {
  iv_var_y <- sovereign::VAR(data = cbind(vfci_data_y["date"],vfci_data_y[,c(vars_in_system_vfci_lev_stationary)],vfci_data_y[y_instrument]), 
                             p = 2, 
                             horizon = 20,
                             freq = 'quarter',
                             structure = 'IV',
                             instrument = y_instrument, 
                             instrumented = "ygr")
}
df_irf_y <- sovereign::var_irf(iv_var_y, 
                                horizon = 20,
                                CI = c(0.05, 0.95))
df_irf_y <- as.data.frame(df_irf_y)

df_irf_y_68 <- sovereign::var_irf(iv_var_y, 
                                   horizon = 20,
                                   CI = c(0.16, 0.84))
df_irf_y_68 <- as.data.frame(df_irf_y_68)

df_irf_y$response.lower.68 <- df_irf_y_68$response.lower
df_irf_y$response.upper.68 <- df_irf_y_68$response.upper

#For a 2x2 IRF panel
if (plot_within_this_code == 1) {
df_irf_svar_iv <- df_irf_y #df_irf_ff, df_irf_vfci, df_irf_y
df_irf_svar_iv <- df_irf_svar_iv[,c(-2,-7,-8)]  
df_irf_svar_iv %>% tidyr::pivot_longer(-c(target,horizon)) %>%
  ggplot(aes(x=horizon, y=value, color= name,group=name))+
  geom_line() + geom_hline(yintercept=0) + theme_minimal() +
  scale_color_manual(values = c("blue", "gray", "gray")) +
  labs(title = "SVAR-IV: Impact of Output shock",
       caption = "90% Confidence Bands") +
  theme(axis.title.y    = element_blank(),
        axis.title.x    = element_blank(),
        axis.text.x     = element_text(angle = 0),
        legend.title    = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=12),
        plot.caption = element_text(size=9)) +
  facet_wrap(~target, scales = "free")
}

#--------------------------------------------------------------------
#Model 2: LP-IV 
#--------------------------------------------------------------------

#Impact of the FF shock on all variables
if (type == "baseline") {
endog_data <- vfci_data_mp[, vars_in_system_baseline]
} else if (type == "stationary") {
  endog_data <- vfci_data_mp[, vars_in_system_stationary]
} else if (type == "vfci_lev") {
  endog_data <- vfci_data_mp[, vars_in_system_vfci_lev]
}
shock_ff <- vfci_data_mp[, mp_instrument]
shock_ff <- as.data.frame(shock_ff)
# Estimate linear model
results_lin_iv_ff <- lpirfs::lp_lin_iv(endog_data,
                              lags_endog_lin = 4,
                              shock = shock_ff,
                              trend = 2,
                              confint = 1.65,
                              adjust_se = TRUE,
                              use_nw = TRUE,
                              hor = 20)

results_lin_iv_ff.68 <- lpirfs::lp_lin_iv(endog_data,
                                 lags_endog_lin = 4,
                                 shock = shock_ff,
                                 trend = 2,
                                 confint = 1,
                                 adjust_se = TRUE,
                                 use_nw = TRUE,
                                 hor = 20)

results_lin_iv_ff$irf_lin_low.68 <- results_lin_iv_ff.68$irf_lin_low
results_lin_iv_ff$irf_lin_up.68 <- results_lin_iv_ff.68$irf_lin_up

#Impact of the VFCI shock on all variables
if (type == "baseline") {
endog_data <- vfci_data[, vars_vfci_last_baseline]
} else if (type == "stationary") {
  endog_data <- vfci_data[, vars_vfci_last_stationary]
} else if (type == "vfci_lev") {
  endog_data <- vfci_data[, vars_vfci_last_vfci_lev]
}
shock_vfci <- vfci_data[, vfci_instrument]
shock_vfci <- as.data.frame(shock_vfci)
# Estimate linear model
results_lin_iv_vfci <- lpirfs::lp_lin_iv(endog_data,
                              lags_endog_lin = 4,
                              shock = shock_vfci,
                              trend = 2,
                              confint = 1.65,
                              adjust_se = TRUE,
                              use_nw = TRUE,
                              hor = 20)

results_lin_iv_vfci.68 <- lpirfs::lp_lin_iv(endog_data,
                                   lags_endog_lin = 4,
                                   shock = shock_vfci,
                                   trend = 2,
                                   confint = 1,
                                   adjust_se = TRUE,
                                   use_nw = TRUE,
                                   hor = 20)

results_lin_iv_vfci$irf_lin_low.68 <- results_lin_iv_vfci.68$irf_lin_low
results_lin_iv_vfci$irf_lin_up.68 <- results_lin_iv_vfci.68$irf_lin_up

#Impact of the Y shock on all variables
if (type == "baseline") {
  endog_data2 <- vfci_data[, vars_in_system_stationary]
} else if (type == "stationary") {
  endog_data2 <- vfci_data[, vars_in_system_stationary]
} else if (type == "vfci_lev") {
  endog_data2 <- vfci_data[, vars_in_system_vfci_lev_stationary]
}
shock_y <- vfci_data[, y_instrument]
shock_y <- as.data.frame(shock_y)
# Estimate linear model
results_lin_iv_y <- lpirfs::lp_lin_iv(endog_data2,
                            lags_endog_lin = 4,
                            shock = shock_y,
                            trend = 2,
                            confint = 1.65,
                            adjust_se = TRUE,
                            use_nw = TRUE,
                            hor = 20)

results_lin_iv_y.68 <- lpirfs::lp_lin_iv(endog_data2,
                                lags_endog_lin = 4,
                                shock = shock_y,
                                trend = 2,
                                confint = 1,
                                adjust_se = TRUE,
                                use_nw = TRUE,
                                hor = 20)

results_lin_iv_y$irf_lin_low.68 <- results_lin_iv_y.68$irf_lin_low
results_lin_iv_y$irf_lin_up.68 <- results_lin_iv_y.68$irf_lin_up


if (plot_within_this_code == 1) {
  irf_df <- as.data.frame(results_lin_iv_ff$irf_lin_mean)
  plot(results_lin_iv_ff)
  plot(results_lin_iv_vfci)
  plot(results_lin_iv_y)
  }

#--------------------------------------------------------------------
#Model 3: Cholesky
#--------------------------------------------------------------------

# VFCI ordered last
if (type == "baseline") {
chol_var_vfci <- sovereign::VAR(data = cbind(vfci_data_mp["date"],vfci_data_mp[,c(vars_vfci_last_baseline)]), 
                              p = 3, 
                              horizon = 19,
                              freq = 'quarter',
                              structure = 'short')
} else if (type == "stationary") {
  chol_var_vfci <- sovereign::VAR(data = cbind(vfci_data["date"],vfci_data[,c(vars_vfci_last_stationary)]), 
                                  p = 3, 
                                  horizon = 19,
                                  freq = 'quarter',
                                  structure = 'short')
} else if (type == "vfci_lev") {
  chol_var_vfci <- sovereign::VAR(data = cbind(vfci_data["date"],vfci_data[,c(vars_vfci_last_vfci_lev)]), 
                                  p = 3, 
                                  horizon = 19,
                                  freq = 'quarter',
                                  structure = 'short')
}
chol_irf_vfci <- sovereign::var_irf(chol_var_vfci, 
                                  horizon = 19,
                                  CI = c(0.05,0.95))

chol_irf_vfci.68 <- sovereign::var_irf(chol_var_vfci, 
                                    horizon = 19,
                                    CI = c(0.16,0.84))
chol_irf_vfci$response.lower.68 <- chol_irf_vfci.68$response.lower
chol_irf_vfci$response.upper.68 <- chol_irf_vfci.68$response.upper

if (plot_within_this_code == 1) {
plot_irf(chol_irf_vfci)
}
#--------------------------------------------------------------------
#Model 4: Sign Restrictions 
#--------------------------------------------------------------------

if (type == "baseline") {
df_sn <- cbind(vfci_data["date"],vfci_data[,c(vars_in_system_baseline)])
} else if (type == "stationary") {
  df_sn <- cbind(vfci_data["date"],vfci_data[,c(vars_in_system_stationary)])
} else if (type == "vfci_lev") {
  df_sn <- cbind(vfci_data["date"],vfci_data[,c(vars_in_system_vfci_lev)])
}
df_ts <- as.ts(x = df_sn[, -1], order.by = df_sn$date) #convert to ts object
lab <- c("Log GDP","Log PCE","VFCI","Fed Funds Rate")

#Impact of the FF shock on all variables
constr_mp <- c(+4,-2,-1) #first element is the shock; all other elements are optional
sn_var_mp <- VARsignR::uhlig.penalty(Y=df_ts, nlags=4, draws=10000, subdraws=200, nkeep=1000, KMIN=1,
                          KMAX=6, constrained=constr_mp, constant=FALSE, steps=20)
irfs_mp <- sn_var_mp$IRFS

#Impact of the VFCI shock on all variables
constr_vfci <- c(+3,-2) #-2 is restriction on negative price level
sn_var_vfci <- VARsignR::uhlig.penalty(Y=df_ts, nlags=4, draws=10000, subdraws=200, nkeep=1000, KMIN=1,
                            KMAX=6, constrained=constr_vfci, constant=FALSE, steps=20)
irfs_vfci <- sn_var_vfci$IRFS

if (plot_within_this_code == 1) {
  irfplot(irfdraws=irfs_mp, type="median", labels=lab, save=FALSE, bands=c(0.05,0.95),grid=TRUE, bw=FALSE)
  irfplot(irfdraws=irfs_vfci, type="median", labels=lab, save=FALSE, bands=c(0.16, 0.84),grid=TRUE, bw=FALSE)
}
#-------------------------------------------------------------------------------