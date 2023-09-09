#-------------------------------------------------------------------------------
# TVP-BVAR 
#-------------------------------------------------------------------------------

install.packages("bvarsv")
library("bvarsv")

vars_in_system_baseline     <- c('lgdp','lpce','vfci','fedfunds') 
vars_vfci_last_baseline     <- c('lgdp','lpce','fedfunds','vfci') 

vars_in_system_stationary   <- c('ygr','infl_pce','vfci','fedfunds') 
vars_vfci_last_stationary   <- c('ygr','infl_pce','fedfunds','vfci')

vars_in_system_vfci_lev     <- c('lgdp','lpce','vfci_lev','fedfunds') 
vars_vfci_last_vfci_lev     <- c('lgdp','lpce','fedfunds','vfci_lev')

vars_in_system_vfci_lev_stationary <- c('ygr','infl_pce','vfci_lev','fedfunds')  
vars_vfci_last_vfci_lev_stationary <- c('ygr','infl_pce','fedfunds','vfci_lev')

#-------------------------------------------------------------------------------
#Estimation

#### The following works only with stationary data???
##Help on this: https://github.com/FK83/bvarsv/issues/2

# a <- vfci_data[vars_vfci_last_baseline]
# x <- a %>% mutate(lgdp = lgdp/100, lpce = lpce/100)
# y <- a %>% mutate(fedfunds = fedfunds*100, vfci=vfci*10)

b <- vfci_data[vars_in_system_stationary]
z <- b %>% mutate(ygr = ygr/100, infl_pce = infl_pce/100)
#tvp_input <- ts(vfci_data[vars_vfci_last_baseline])
tvp_input <- ts(z)
set.seed(123)
bv <- bvar.sv.tvp(tvp_input, nrep=10000)

#-------------------------------------------------------------------------------

#VFCI shock panel
plot.new()
par(mfrow = c(2, 2)) 
impulse.responses(bv, impulse.variable = 3, response.variable = 1)
mtext("VFCI shock to Output Growth", side = 3, line = 1, cex = 1)
impulse.responses(bv, impulse.variable = 3, response.variable = 4)
mtext("VFCI shock to Fed Funds", side = 3, line = 1, cex = 1)
impulse.responses(bv, impulse.variable = 3, response.variable = 2)
mtext("VFCI shock to Inflation", side = 3, line = 1, cex = 1)
impulse.responses(bv, impulse.variable = 3, response.variable = 3)
mtext("VFCI shock to VFCI", side = 3, line = 1, cex = 1)

#VFCI response panel
plot.new()
par(mfrow = c(2, 2)) 
impulse.responses(bv, impulse.variable = 1, response.variable = 3)
mtext("VFCI Response to Output Growth Shock", side = 3, line = 1, cex = 1)
impulse.responses(bv, impulse.variable = 4, response.variable = 3)
mtext("VFCI Response to Fed Funds Shock", side = 3, line = 1, cex = 1)
impulse.responses(bv, impulse.variable = 2, response.variable = 3)
mtext("VFCI Response to Inflation Shock", side = 3, line = 1, cex = 1)
impulse.responses(bv, impulse.variable = 3, response.variable = 3)
mtext("VFCI Response to VFCI Shock", side = 3, line = 1, cex = 1)

#https://cran.r-project.org/web/packages/bvarsv/bvarsv.pdf

#to see the content of the BVAR function (just type the function and press enter)
bvar.sc.tvp
impulse.response

#to see the contents of the IRF package (to get 68th and 95th percentiles)
ls(bv)

