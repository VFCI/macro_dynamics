run_type <- function(x) {
  
  rm(list=ls(all = TRUE))
  
  path <- "//data4/users2/TIyer/My Documents/vfci_feb2023"
  setwd(path)
  
  source('load_functions.R')
  load_functions() 

  vfci_data <- openxlsx::readWorkbook("dataset_vfci_feb2023.xlsx")
  vfci_data$date = seq.Date(as.Date('1962-01-01'),as.Date('2022-07-01'),by = 'quarter')
  
  type <- x
  
  source('1_vol_bvar_calibration.R')
  source('1_vol_bvar_estimation.R')
  source('1_vol_bvar_output.R')
}
  
run_type("regimes")
run_type("normal")
