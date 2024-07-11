## COLLECT ALL RESIDUALS IN A DATAFRAME
# rf (reduced form) and sf (structural form)
# reduced form residuals are the same for all models
 
# Structural form residuals are named in the following format (model)_(var)
# For instance rf_bvar_GDP
suffix <- gsub(" ","_",var_names)
mod_names <- c("bvar","svariv_ff","svariv_vfci","svariv_y","chol_vfci","sign_res")

#-------------------------------------------------------------------------------
# Instruments
#-------------------------------------------------------------------------------
vfci_ins      <- data.frame(vfci_data[,c(mp_instrument,vfci_instrument,y_instrument)])

#-------------------------------------------------------------------------------
# Reduced-form residuals
#-------------------------------------------------------------------------------
rf_residuals <- scale(residuals(rf_model),center=FALSE,scale = TRUE)

#-------------------------------------------------------------------------------
# Structural residuals
#-------------------------------------------------------------------------------

#---------------------------------------
# BVAR
# Structural residuals are saved in the mcmc_output object as ``eout"
# To compute the structural residuals, extract estimate of the objects
# and then compute the posterior median for each time period
lmdndx <- rep(1:7, diff(c(optout$Tsigbrk,dim(vfci_data)[1])))
lambda_scaling <- sqrt(lambda[,lmdndx[(nlags+1):dim(vfci_data)[1]],])
lambda_scaling <- aperm(lambda_scaling,c(2,1,3))

bvar_sf_res   <- scale(data.frame(apply(mcmc_output$eout * lambda_scaling,c(1,2),median)))
	
#---------------------------------------
# SVAR-IV 
svariv_sf_res_ff <- full_join(data.frame(date = vfci_data$date),iv_var_ff$residuals$H_1) 
svariv_sf_res_ff[,3:dim(svariv_sf_res_ff)[2]] <- scale(svariv_sf_res_ff[,3:dim(svariv_sf_res_ff)[2]])

svariv_sf_res_vfci <- full_join(data.frame(date = vfci_data$date),iv_var_vfci$residuals$H_1)
svariv_sf_res_vfci[,3:dim(svariv_sf_res_vfci)[2]] <- scale(svariv_sf_res_vfci[,3:dim(svariv_sf_res_vfci)[2]])

svariv_sf_res_y <- full_join(data.frame(date = vfci_data$date),iv_var_y$residuals$H_1)
svariv_sf_res_y[,3:dim(svariv_sf_res_y)[2]] <- scale(svariv_sf_res_y[,3:dim(svariv_sf_res_y)[2]])

#---------------------------------------
# Cholesky
chol_sf_res_vfci <- full_join(data.frame(date = vfci_data$date),chol_var_vfci$residuals$H_1)
chol_sf_res_vfci[,3:dim(chol_sf_res_vfci)[2]] <- scale(chol_sf_res_vfci[,3:dim(chol_sf_res_vfci)[2]])

#---------------------------------------
# Local Projections
# Local projections do not have a ``structural representation" they only serve to estimate the impulse response
# function under minimal assumptions 
# https://www.frbsf.org/wp-content/uploads/sites/4/wp2023-16.pdf 

#---------------------------------------
# Sign-restricted VAR
df_sn <- cbind(vfci_data["date"],vfci_data[,c(vars_in_system_baseline)])
df_ts <- as.ts(x = df_sn[, -1], order.by = df_sn$date) #convert to ts object

# FF shock 
constr_mp <- c(+4,-2,-1) # first element is the MP shock; second and third constrain Y and P to decrease
sn_var_mp <- VARsignR::uhlig.penalty(Y=df_ts, nlags=4, draws=10000, subdraws=200, nkeep=1000, KMIN=1,
                                     KMAX=6, constrained=constr_mp, constant=FALSE, steps=20)
shocks_penalty_baseline <- sn_var_mp$SHOCKS
ss_penalty <- ts(t(apply(shocks_penalty_baseline,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=4, start=c(1963,1))
mp_shock <- as.data.frame(ss_penalty[,1]) %>% setNames(c("mp_shock"))
mp_shock$date <- tsibble::yearquarter(seq.Date(as.Date('1963-01-01'),as.Date('2022-07-01'),by = 'quarter'))

# VFCI shock 
constr_vfci <- c(+3,-2) # first element is the VFCI shock; second constrains P to decrease 
sn_var_vfci <- VARsignR::uhlig.penalty(Y=df_ts, nlags=4, draws=10000, subdraws=200, nkeep=1000, KMIN=1,
                                       KMAX=6, constrained=constr_vfci, constant=FALSE, steps=20)
shocks_penalty_baseline <- sn_var_vfci$SHOCKS
ss_penalty <- ts(t(apply(shocks_penalty_baseline,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=4, start=c(1963,1))
vfci_shock <- as.data.frame(ss_penalty[,1]) %>% setNames(c("vfci_shock"))
vfci_shock$date <- tsibble::yearquarter(seq.Date(as.Date('1963-01-01'),as.Date('2022-07-01'),by = 'quarter'))

# Price level shock 
constr_p <- c(+2,+4) # first element is the price shock; second constrains FF to increase
sn_var_p <- VARsignR::uhlig.penalty(Y=df_ts, nlags=4, draws=10000, subdraws=200, nkeep=1000, KMIN=1,
                                       KMAX=6, constrained=constr_vfci, constant=FALSE, steps=20)
shocks_penalty_baseline <- sn_var_p$SHOCKS
ss_penalty <- ts(t(apply(shocks_penalty_baseline,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=4, start=c(1963,1))
p_shock <- as.data.frame(ss_penalty[,1]) %>% setNames(c("p_shock"))
p_shock$date <- tsibble::yearquarter(seq.Date(as.Date('1963-01-01'),as.Date('2022-07-01'),by = 'quarter'))

# Output shock
constr_y <- c(+1,+4) # first element is the output shock; second constrains FF to increase
sn_var_y <- VARsignR::uhlig.penalty(Y=df_ts, nlags=4, draws=10000, subdraws=200, nkeep=1000, KMIN=1,
                                    KMAX=6, constrained=constr_vfci, constant=FALSE, steps=20)
shocks_penalty_baseline <- sn_var_y$SHOCKS
ss_penalty <- ts(t(apply(shocks_penalty_baseline,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=4, start=c(1963,1))
y_shock <- as.data.frame(ss_penalty[,1]) %>% setNames(c("y_shock"))
y_shock$date <- tsibble::yearquarter(seq.Date(as.Date('1963-01-01'),as.Date('2022-07-01'),by = 'quarter'))

#-------------------------------------------------------------------------------
# Standardizing the shocks
#-------------------------------------------------------------------------------
sign_restr_shocks <- rbind(matrix(NA,4,4),scale(data.frame(c(y_shock[,1]),c(p_shock[,1]),c(vfci_shock[,1]),c(mp_shock[,1]))))

# Merge data and save
df_res_names_rf <- paste("rf","res",suffix,sep="_")
df_res_names_sf <- c(t(sapply(suffix,function(x) paste("sf",mod_names,x,sep="_"))))
colnames_data_res <- c("date",df_res_names_rf,df_res_names_sf,colnames(vfci_ins))

data_res <- data.frame(vfci_data$date,rbind(matrix(NA,nvar,2*nvar),cbind(rf_residuals,bvar_sf_res)),svariv_sf_res_ff[,3:(3+nvar-1)],svariv_sf_res_vfci[,3:(3+nvar-1)],svariv_sf_res_y[,3:(3+nvar-1)],chol_sf_res_vfci[,3:(3+nvar-1)],sign_restr_shocks,vfci_ins)
names(data_res) <- colnames_data_res

rm(suffix,mod_names,vfci_ins,rf_residuals,bvar_sf_res,svariv_sf_res_ff,svariv_sf_res_vfci,svariv_sf_res_y,chol_sf_res_vfci,df_res_names_rf,df_res_names_sf,colnames_data_res)

save(data_res,file = 'output/res_ins_data.Rdata')

#-------------------------------------------------------------------------------
# 1. Correlation matrix of BVAR structural shocks
#-------------------------------------------------------------------------------
shock_type <- grep("^sf_bvar", names(data_res), value = TRUE)

vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1963-01-01"))) %>%
  select(c(all_of(shock_type)))
colnames(vars_for_corr) <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

corr_plot <- here::here(paste0("output/appendix/figures/","bvar_shock_corr",'.pdf', sep = ''))
pdf(corr_plot, width = 4, height = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, diag = TRUE,
         type = "lower", col = col(200), addCoef.col = "black", cl.pos = "n")
dev.off()

#-------------------------------------------------------------------------------
# 2. Correlation matrix of reduced-form shocks
#-------------------------------------------------------------------------------
shock_type <- grep("^rf_res", names(data_res), value = TRUE)

vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1963-01-01"))) %>%
  select(c(all_of(shock_type)))
colnames(vars_for_corr) <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

corr_plot <- here::here(paste0("output/appendix/figures/","rf_res_corr",'.pdf', sep = ''))
pdf(corr_plot, width = 4, height = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, diag = TRUE,
         type = "lower", col = col(200), addCoef.col = "black", cl.pos = "n")
dev.off()

#-------------------------------------------------------------------------------
# 3. Correlation matrix of Fed Funds structural shocks from all models and instruments
#-------------------------------------------------------------------------------
shock_type <- c("sf_bvar_Fed_Funds", "std_mp_shock_int_rr_ns", "sf_sign_res_Fed_Funds", "sf_chol_vfci_Fed_Funds")

vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1970-01-01"))) %>%
  select(c(all_of(shock_type)))
colnames(vars_for_corr) <- c("Vol-BVAR", "FF Instrument", "Sign Res", "Cholesky")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

corr_plot <- here::here(paste0("output/appendix/figures/","sf_all_models_corr_ff",'.pdf', sep = ''))
pdf(corr_plot, width = 4, height = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, diag = TRUE,
         type = "lower", col = col(200), addCoef.col = "black", cl.pos = "n")
dev.off()

#-------------------------------------------------------------------------------
# 4. Correlation matrix of VFCI structural shocks from all models and instruments
#-------------------------------------------------------------------------------
shock_type <- c("sf_bvar_VFCI", "std_vfci_shock_penalty", "sf_sign_res_VFCI", "sf_chol_vfci_VFCI")

vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1970-01-01"))) %>%
  select(c(all_of(shock_type))) 
colnames(vars_for_corr) <- c("Vol-BVAR", " VFCI Instrument", "Sign Res", "Cholesky")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

corr_plot <- here::here(paste0("output/appendix/figures/","sf_all_models_corr_vfci",'.pdf', sep = ''))
pdf(corr_plot, width = 4, height = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, diag = TRUE,
         type = "lower", col = col(200), addCoef.col = "black", cl.pos = "n")
dev.off()

#-------------------------------------------------------------------------------
# 5. Correlation matrix of price level structural shocks from all models and instruments
#-------------------------------------------------------------------------------
shock_type <- c("sf_bvar_Log_Core_PCE", "sf_sign_res_Log_Core_PCE", "sf_chol_vfci_Log_Core_PCE")
#No instrument for price level

vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1970-01-01"))) %>%
  select(c(all_of(shock_type))) 
colnames(vars_for_corr) <- c("Vol-BVAR", "Sign Res", "Cholesky")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

corr_plot <- here::here(paste0("output/appendix/figures/","sf_all_models_corr_p",'.pdf', sep = ''))
pdf(corr_plot, width = 4, height = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, diag = TRUE,
         type = "lower", col = col(200), addCoef.col = "black", cl.pos = "n")
dev.off()

#-------------------------------------------------------------------------------
# 6. Correlation matrix of output structural shocks from all models and instruments
#-------------------------------------------------------------------------------
shock_type <- c("sf_bvar_Log_Real_GDP", "sf_sign_res_Log_Real_GDP", "sf_chol_vfci_Log_Real_GDP")
#No instrument since its for real GDP growth whereas the shocks are for log real GDP

vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1984-01-01"))) %>%
  select(c(all_of(shock_type))) 
colnames(vars_for_corr) <- c("Vol-BVAR", "Sign Res", "Cholesky")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

corr_plot <- here::here(paste0("output/appendix/figures/","sf_all_models_corr_y",'.pdf', sep = ''))
pdf(corr_plot, width = 4, height = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, diag = TRUE,
         type = "lower", col = col(200), addCoef.col = "black", cl.pos = "n")
dev.off()
#-------------------------------------------------------------------------------

