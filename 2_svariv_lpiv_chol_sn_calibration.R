#-------------------------------------------------------------------------------
# 1. Specify input into VARs depending on scenario
#-------------------------------------------------------------------------------
library("dplyr")

vars_in_system_baseline     <- c('lgdp','lpce','vfci','fedfunds') 
vars_vfci_last_baseline     <- c('lgdp','lpce','fedfunds','vfci') 
vars_vfci_first_baseline    <- c('vfci','lgdp','lpce','fedfunds') 

vars_in_system_stationary   <- c('ygr','infl_pce','vfci','fedfunds') 
vars_vfci_last_stationary   <- c('ygr','infl_pce','fedfunds','vfci')
vars_vfci_first_stationary   <- c('vfci','ygr','infl_pce','fedfunds')

vars_in_system_vfci_lev     <- c('lgdp','lpce','vfci_lev','fedfunds') 
vars_vfci_last_vfci_lev     <- c('lgdp','lpce','fedfunds','vfci_lev')
vars_vfci_firstt_vfci_lev     <- c('vfci_lev','lgdp','lpce','fedfunds')

vars_in_system_vfci_lev_stationary <- c('ygr','infl_pce','vfci_lev','fedfunds')  
vars_vfci_last_vfci_lev_stationary <- c('ygr','infl_pce','fedfunds','vfci_lev')
vars_vfci_last_vfci_lev_stationary <- c('vfci_lev', 'ygr','infl_pce','fedfunds')

#-------------------------------------------------------------------------------
# 2. Specify relevant instruments
#-------------------------------------------------------------------------------

y_instrument    <- c("std_y_shock")

mp_instrument   <- c("std_mp_shock_int_rr_ns")

vfci_instrument <- c("vfci")

#-------------------------------------------------------------------------------
# 3. Sample and tidyng-up
#-------------------------------------------------------------------------------

# Inputs tailored to start date for instruments
vfci_data_mp <- subset(vfci_data, date>=as.Date('1969-01-01'))
vfci_data_y  <- subset(vfci_data, date>=as.Date('1983-01-01'))

# Scaling 
vfci_data <- vfci_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(ygr = ygr/100,
                infl_pce = infl_pce/100)

#-------------------------------------------------------------------------------
# 4. Visualize the output of the models before sending to panel graph stage?
#-------------------------------------------------------------------------------

# Note: Should only be configured to 1 if the estimation file is run on a stand-alone basis
plot_within_this_code <- 0 #if 1, then plots are generated

#-------------------------------------------------------------------------------
