hetreg <- function(data,y,x,het=x, ..., gls_opt = list()) {
  
  mean_eq <- paste(y,"~",paste(x, collapse='+'))
  vol_eq <- eval(parse(text=glue::glue(
      "nlme::varComb( \n",
      glue::glue_collapse(glue::glue("nlme::varExp(form = ~ {het})"), ",\n"),
      ")"
    )))
  frm <- formula(paste(mean_eq, collapse = " "))
  # do.call(eval(parse(text="nlme::gls")), c(frm,data=data, weights=vol_eq))
  nlme::gls(frm,data=data, weights=vol_eq,method = "ML")
}

# pca <- get_pc(variables,var_names)
# pca %>%
#   broom::tidy(matrix = "rotation")
# pca %>%
#   broom::tidy(matrix = "eigenvalues")
# 
# pca %>%
#   tidy(matrix = "eigenvalues") %>%
#   ggplot(aes(PC, percent)) +
#   geom_col(fill = "#56B4E9", alpha = 0.8) +
#   scale_x_continuous(breaks = 1:9) +
#   scale_y_continuous(
#     labels = scales::percent_format(),
#     expand = expansion(mult = c(0, 0.01))
#   ) +
#   theme_minimal_hgrid(12)
get_pc <- function(
  data,
  var_names,
  date_begin = "1962 Q1",
  date_end = "2022 Q3",
  cor = TRUE,
  scores = TRUE,
  ...
  ) {
  prcomp_tidy <- function(x,...){ 
    princomp(x, cor = TRUE,scores = TRUE,...)
  }
  pcs <- data %>%
    tsibble::as_tsibble() %>% 
    tsibble::filter_index(date_begin ~ date_end) %>% 
    tsibble::as_tibble() %>% 
    dplyr::select(all_of(var_names)) %>%
    prcomp_tidy()
}

get_vfci <- function(data,y,x,het=x,prcomp=TRUE,n_prcomp = 4, date_begin="1962 Q1", date_end="2022 Q3", ..., gls_opt = list()) {
  optarg <- list(...)
  data <- data %>% 
    tsibble::as_tsibble() %>% 
    tsibble::filter_index(date_begin ~ date_end) %>% 
    tibble::as_tibble()
  if (prcomp) {
    pca <- get_pc(data,x,...)
    pcs <- pca$scores # time series of principal components
    colnames(pcs) <- paste0("pc", 1:ncol(pcs))
    data_pcs <- dplyr::bind_cols(data,tibble::as_tibble(pcs),.name_repair = c("minimal"))
    x_pcs <- paste0("pc", 1:n_prcomp)
    h <- hetreg(data_pcs,y,x_pcs, ..., gls_opt = gls_opt)
    h$pc <- data_pcs %>% dplyr::select(dplyr::all_of(colnames(pcs)) )
  }
  else {
    h <- hetreg(data,y,x,het, ..., gls_opt = gls_opt)
  }
  
  # mean equation
  h$mean_eq$coefficients <- h$coefficients # coefficients
  h$mean_eq$se <- sqrt(diag(h$varBeta)) # std. err.
  h$mean_eq$fitted <- h$fitted # fitted values, same as Stata's `predict`

  # vol equation
  h$vol_eq$coefficients <- 2 * attr(h$apVar, "Pars") # coefficients
  h$vol_eq$se <- 2 * sqrt(diag(h$apVar)) # std. err.
  h$vol_eq$exp_fitted <- attr(h$residuals, "std") # fitted values, same as Stata's `predict, sigma`

  # return vfci, mu, time series, and hetreg results
  if (prcomp) {
    out <- list(
      ts = dplyr::bind_cols(h$pc,
        tibble::tibble(qtr = data$qtr,
               vfci=log(h$vol_eq$exp_fitted),
               mu=unname(h$fitted))),
      hetreg = h
    )
  }
  else {
    out <- list(
      ts = tibble(qtr = data$qtr, vfci=log(h$vol_eq$exp_fitted), mu=unname(h$fitted)),
      hetreg=h
    )
  }
  return(out)
  
  
}