load_functions = function() {
  
  folders = c('functions/bpss_tools','functions/opt_tools','functions/var_tools','functions/irf_tools','functions/vfci_tools','functions/internal_vfci_tools/')
  
  ## enumerating all the functions and sourcing them
  for (ifolder in folders) {
    funlist = list.files(path = ifolder, pattern = '[.]R$', full.names=TRUE)
    sapply(funlist, FUN = source)
  }
  
  ## requiring certain R packages
  source('install_packages.R')
}

