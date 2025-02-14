#' Recover data from a vars::VAR object with any number of lags.
#' Note that date columns are not saved in a var and cannot be recovered.
#'
#' @param var vars::VAR
#'
#' @return A data.frame
#' @export
#'
get_data_from_var <- function(
  var
) {

  p <- var$p
  K <- var$K
  datamat <- var$datamat

  all_obs_data <- var$datamat[, seq_len(K)]
  lag_data <- datamat[seq_len(p), (1 + p * K):(K + p * K)]

  lag_data <- stats::setNames(lag_data, colnames(all_obs_data))

  data <- rbind(lag_data, all_obs_data)

  return(data)
}
