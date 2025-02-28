#' Fit a linear regression estimating the heteroskedasticity
#' of the variance of the residuals in a VAR.
#'
#' @param var a VAR, either vars::VAR or svars object
#' @param hetreg_lags integer, number of lags to use in het reg, defaults to 0 (just same period values)
#' @param x2 indepedent variables for predicting heteroskedasticity
#' @param extra_data additional data columns to cbind with the data from the VAR,
#' use to add exogenous variables for x2 that are not in the VAR
#'
#' @return A list of a set of linear regressions
#' @export
#'
#' @import data.table
#'
fit_hetreg_from_var <- function(
  var,
  hetreg_lags = 0,
  x2 = NULL,
  extra_data = NULL
) {
  ## Set visible global binding to make R CMD check happy
  . <- log_var_fitted <- log_var_fitted_resid <- t <- value <- name <- NULL

  ## Get the data
  original_data_wide <-
    get_data_from_var(var) |>
    as.data.table() |>
    _[, t := .I - var$p]

  var_colnames <- colnames(original_data_wide[, -"t"]) |> purrr::set_names()

  original_data <-
    original_data_wide |>
    tidyfast::dt_pivot_longer(-t, names_to = "variable", values_to = "original")

  original_data_wide <- cbind(original_data_wide, extra_data)

  ## Lagged data matrix
  lagged_data <- copy(original_data_wide)

  for (i in c(var_colnames, x2)){
    lagged_data[, paste0(i, "_L", 1:max(var$p, hetreg_lags)) := shift(.SD, n = 1:max(var$p, hetreg_lags)), .SDcols = i]
  }

  var_lag_variables <-
    purrr::map(
      1:var$p,
      ~ paste0(c(var_colnames), "_L", .x)
    ) |>
    unlist()

  if (any(hetreg_lags != 0)) {

    if (!is.null(x2)) {
      x2 <-
        purrr::map(
          1:var$p,
          ~ paste0(x2, "_L", .x)
        ) |>
        unlist() |>
        c(x2)
    }

    hetreg_lag_variables <-
      purrr::map(hetreg_lags, ~ if (.x == 0) {
        var_colnames
      } else {
        paste0(var_colnames, "_L", .x)
      }) |>
      unlist()

    extra_data <- cbind(extra_data, lagged_data[, hetreg_lag_variables, with = FALSE])
  }

  ## Create a formula with the needed lags and current variables names
  if (is.null(x2)) {
    x2 <- purrr::map(hetreg_lags, ~ if (.x == 0) {
      var_colnames
    } else {
      paste0(var_colnames, "_L", .x)
    }) |>
      unlist()
  }


  ## Estimate the het-reg
  hetreg_list <-
    var_colnames |>
    purrr::map(~ hetreg_twostep_var(
      var,
      y = .x,
      x2 = x2,
      extra_data = extra_data
    ))

  ## Get the predicted log variance, fitted, and residual values
  

  fitted <-
    matrix(NA, var$p - 1, var$K) |>
    rbind(stats::fitted(var))  |>
    rbind(purrr::map_dbl(stats::predict(var, n.ahead = 1)$fcst, ~ .x[1])) |>
    as.data.table()
  
  ## Residuals
  residuals <-
    matrix(NA, var$p - 1, var$K) |>
    rbind(stats::residuals(var)) |>
    rbind(rep(NA, var$K)) |>
    as.data.table()

  predicted_log_variance <-
    var_colnames |>
    purrr::map(
      ~ data.table(
        fitted = fitted[, .x, with = FALSE][[1]],
        residuals = residuals[, .x, with = FALSE][[1]],
        log_var_fitted_resid =
          c(
            rep(NA, pmax(0, var$p, 1 + 1)),
            rep(NA, pmax(0, max(hetreg_lags) - var$p + 1)),
            stats::fitted(hetreg_list[[.x]]$lm2_adj)
          ) |>
          data.table::shift(n = 1, type = "lead"),
        log_var_fitted =
          predict(hetreg_list[[.x]]$lm2_adj, newdata = cbind(extra_data, original_data_wide))
      ) |>
        _[, t := .I - var$p]
    ) |>
    purrr::list_rbind(names_to = "variable")


  all_data <-
    original_data |>
    merge(predicted_log_variance, by = c("t", "variable"))

  return(list(
    dt = all_data,
    het_regs = hetreg_list
  ))
}
