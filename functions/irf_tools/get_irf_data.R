#' Get Impulse Response Function Data
#'
#' This function loads and processes impulse response function (IRF) data from 
#' MCMC output files. It computes median IRFs and confidence intervals, then 
#' returns the data in a tidy tibble format suitable for plotting and analysis.
#'
#' @param type Character string specifying the type of IRF data to load. 
#'             This corresponds to the suffix in the filename pattern 
#'             "mcmc_out_{type}_ir.Rdata"
#'
#' @return A tibble containing IRF data with the following columns:
#'   \item{variable}{Character. Name of the response variable}
#'   \item{shock}{Character. Name of the shock variable}  
#'   \item{period}{Numeric. Time period (1-20)}
#'   \item{irf}{Numeric. Median impulse response}
#'   \item{irf_lb1}{Numeric. Lower bound of 68% confidence interval}
#'   \item{irf_lb2}{Numeric. Lower bound of 90% confidence interval}
#'   \item{irf_ub1}{Numeric. Upper bound of 68% confidence interval}
#'   \item{irf_ub2}{Numeric. Upper bound of 90% confidence interval}
#'   \item{type}{Character. The input type parameter for identification}
#'
#' @details The function expects an RData file containing objects 'ir', 
#'          'var_names', and 'shock_names'. The IRF data is processed to compute
#'          68% and 90% confidence intervals using quantiles from the MCMC draws.
#'
get_irf_data <- function(
  type
) {
  load(paste0("./output/analysis_data/mcmc_out_", type, "_ir.Rdata")) ## loads ir, var_names, shock_names

  conf <- c(0.68,0.90)

  irdraws = ir$ir[,,,1,]
  ir <- apply(ir$ir[,,,1,],c(1:3),median)

  nsteps <- dim(irdraws)[3]

  ## Create a DF
  irq = apply(irdraws[,,1:nsteps,],1:3,quantile,
                  ##probs = c(rev((1 - conf) / 2), .5 + conf/2))
                  probs = c((1-conf)/2,0.5 + conf/2))
  irq = aperm(irq,perm=c(2,3,4,1))

  dimnames(ir)[[1]] <- var_names
  dimnames(ir)[[2]] <- shock_names
  dimnames(ir)[[3]] <- 1:20
  df <- as.data.frame.table(ir) |> dplyr::as_tibble()
  colnames(df) <- c("variable", "shock", "period", "irf")

  dimnames(irq)[[1]] <- var_names
  dimnames(irq)[[2]] <- shock_names
  dimnames(irq)[[3]] <- 1:20
  dimnames(irq)[[4]] <- c("lb1", "lb2", "ub1", "ub2")
  dfq <- as.data.frame.table(irq) |> dplyr::as_tibble()
  colnames(dfq) <- c("variable", "shock", "period", "quantile", "irf")

  dfq_wide <- dfq |> tidyr::pivot_wider(names_from = quantile, values_from = irf, names_prefix = "irf_")

  df <- dplyr::full_join(df, dfq_wide, by = c("variable", "shock", "period"))

  df$period <- as.numeric(df$period)
  df$type <- type

  return(df)
}