library("data.table")
library("tidyfast")

#--------------------------------------------------------------------
# Model 0: Estimate reduced form VAR and internal VFCI
#--------------------------------------------------------------------

var_data <- vfci_data[vars_internal_vfci]

rf_var <- vars::VAR(
  y = var_data,
  p = nlags,
  type = "const"
  )

hr <-
  rf_var |>
  hetreg_twostep_var(
    y = "lgdp",
    x2 = pc_vars,
    extra_data = vfci_data[, pc_vars]
  )

vfci_data$int_vfci <- predict(hr$lm2_adj, newdata = vfci_data[, pc_vars])

comp_vfci_int_vfci_data <- vfci_data[, c("date", "vfci", "int_vfci")]

variables <-
  variables |>
  left_join(vfci_data[, c("qtr", "int_vfci")], by = "qtr")

#--------------------------------------------------------------------
#Model 1: Cholesky
#--------------------------------------------------------------------

chol_var_vfci_last <-
  sovereign::VAR(
    data = vfci_data[, c("date", vars_int_vfci_last)],
    p = nlags,
    horizon = 19,
    freq = 'quarter',
    structure = 'short'
  )

chol_irf_vfci_last <-
  sovereign::var_irf(
    chol_var_vfci_last,
    horizon = 19,
    CI = c(0.05, 0.95)
  )

chol_irf_vfci_last.68 <-
  sovereign::var_irf(
    chol_var_vfci_last,
    horizon = 19,
    CI = c(0.16, 0.84)
  )

chol_irf_vfci_last$response.lower.68 <- chol_irf_vfci_last.68$response.lower
chol_irf_vfci_last$response.upper.68 <- chol_irf_vfci_last.68$response.upper

df_irf_chol_int_vfci_last <-
  chol_irf_vfci_last |>
  filter(shock == "int_vfci")


chol_var_vfci_first <-
  sovereign::VAR(
    data = vfci_data[, c("date", c("int_vfci", "lgdp", "lpce", "fedfunds"))],
    p = nlags,
    horizon = 19,
    freq = 'quarter',
    structure = 'short'
  )

chol_irf_vfci_first <-
  sovereign::var_irf(
    chol_var_vfci_first,
    horizon = 19,
    CI = c(0.05, 0.95)
  )

chol_irf_vfci_first.68 <-
  sovereign::var_irf(
    chol_var_vfci_first,
    horizon = 19,
    CI = c(0.16, 0.84)
  )

chol_irf_vfci_first$response.lower.68 <- chol_irf_vfci_first.68$response.lower
chol_irf_vfci_first$response.upper.68 <- chol_irf_vfci_first.68$response.upper

df_irf_chol_int_vfci_first <-
  chol_irf_vfci_first |>
  filter(shock == "int_vfci")

#--------------------------------------------------------------------
#Model 2: IV
#--------------------------------------------------------------------

iv_var_int_vfci <-
  sovereign::VAR(
    data = vfci_data[,c("date", vars_internal_vfci, "int_vfci")] |> mutate(int_vfci =  scale(int_vfci)),
    p = nlags,
    horizon = 1,
    freq = 'quarter',
    structure = 'IV',
    instrument = "int_vfci",
    instrumented = "lgdp"
  )

iv_irf_int_vfci <-
  sovereign::var_irf(
    iv_var_int_vfci,
    horizon = 20,
    CI = c(0.05, 0.95)
  )

iv_irf_int_vfci_68 <-
  sovereign::var_irf(
    iv_var_int_vfci,
    horizon = 20,
    CI = c(0.16, 0.84)
  )

iv_irf_int_vfci$response.lower.68 <- iv_irf_int_vfci_68$response.lower
iv_irf_int_vfci$response.upper.68 <- iv_irf_int_vfci_68$response.upper

df_irf_iv_int_vfci <-
  iv_irf_int_vfci |>
  filter(shock == "lgdp") |>
  mutate(shock = "int_vfci") |>
  mutate(response = -1 * response) |>
  mutate(response.lower = -1 * response.lower) |>
  mutate(response.upper = -1 * response.upper) |>
  mutate(response.lower.68 = -1 * response.lower.68) |>
  mutate(response.upper.68 = -1 * response.upper.68)

#--------------------------------------------------------------------
#Model 3: Hetreg IV
#--------------------------------------------------------------------

## Find rotation vector q
q <- find_hetreg_q_mriv(rf_var, as.numeric(scale(vfci_data[, "int_vfci"][[1]])))

## Make the rest of Q orthogonal
Q <- matrix(0, rf_var$K, rf_var$K)
rownames(Q) <- colnames(rf_var$y)
Q[, 1] <- q
Q[, 2:rf_var$K] <- pracma::nullspace(t(q))

## Rotate the SVAR Cholesky for Linear Het ID
cv <- svars::id.chol(rf_var)
hv <- copy(cv)
hv$B <- cv$B %*% Q
hv$Q <- Q
hv$het_reg <- hr

## Check if Sovereign package IV matches our own implementation
## The first column should be the same
sovereign:::solve_B(iv_var_int_vfci) |> round(digits = 4)
hv$B |> round(digits = 4)

