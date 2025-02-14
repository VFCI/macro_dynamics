#' Return a single column vector of the Q rotation matrix.
#' Uses the IV method in Mertens Rvan (2013) Appendix A.
#'
#' @param var a VAR
#' @param het_reg het_reg from fit_het_reg_from_var
#' @param target variable in VAR use to identify Q column
#'
#' @return A single column vector of the orthonormal matrix Q
#' @export
#'
#' @import data.table
#'
find_hetreg_q_mriv <- function(
  var,
  iv
) {
  variable <- NULL

  resid_data <-
    matrix(NA, var$p - 0, var$K) |> # - 1
    rbind(stats::residuals(var)) |>
    # rbind(rep(NA, var$K)) |>
    as.data.table() |>
    cbind(data.table(iv = iv)) |>
    stats::na.omit()

  u <- as.matrix(resid_data[, colnames(var$y), with = FALSE]) |> t()
  m <- as.matrix(resid_data[, "iv", with = FALSE]) |> t()
  Sig_uu <- u %*% t(u)
  Sig_mu <- m %*% t(u)

  ## Mertens Rvan (2013) Appendix A method
  Sig_u1u1 <- Sig_uu[1, 1]
  Sig_u2u1 <- Sig_uu[2:var$K, 1]
  Sig_u2u2 <- Sig_uu[2:var$K, 2:var$K]

  Sig_mu1 <- Sig_mu[, 1]
  Sig_mu2 <- Sig_mu[, 2:var$K]

  B21B11I <- t(solve(Sig_mu1) %*% Sig_mu2)
  Z <- B21B11I %*% Sig_u1u1 %*% t(B21B11I) - (Sig_u2u1 %*% t(B21B11I) + B21B11I %*% t(Sig_u2u1)) + Sig_u2u2
  B12B12t <- t(Sig_u2u1 - B21B11I %*% Sig_u1u1) %*% solve(Z) %*% (Sig_u2u1 - B21B11I %*% Sig_u1u1)
  B11B11t <- Sig_u1u1 - B12B12t
  B22B22t <- Sig_u2u2 + B21B11I %*% (B12B12t - Sig_u1u1) %*% t(B21B11I)
  B12B22I <- (B12B12t %*% t(B21B11I) + t(Sig_u2u1 - B21B11I %*% Sig_u1u1)) %*% solve(B22B22t)

  SSt <- (1 - B12B22I %*% B21B11I) %*% B11B11t %*% t(1 - B12B22I %*% B21B11I)
  S <- sqrt(SSt)

  B11 <- solve(1 - B12B22I %*% B21B11I) %*% S
  B21 <- (B21B11I %*% solve(1 - B12B22I %*% B21B11I)) %*% S

  B1 <- c(t(B11), t(B21))
  Q1 <- solve(t(chol(Sig_uu))) %*% B1

  return(Q1)
}
