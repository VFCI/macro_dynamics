get_pc <- function(
    data,
    var_names,
    date_begin = "1962 Q1",
    date_end = "2022 Q3",
    cor = TRUE,
    scores = TRUE,
    center = TRUE,
    scale. = TRUE,
    match_stata = TRUE,
    ...
) {
  prcomp_tidy <- function(x,...){ 
    #preferred method is use prcomp, using princomp to match stata
    if (match_stata){
      princomp(x, cor = cor ,scores = scores, ...) #covmat = MASS::cov.rob(x)  
    }
    else{
      prcomp(x, center = center, scale. = scale., ...) 
    }
  }
  pcs <- data %>%
    tsibble::as_tsibble() %>% 
    tsibble::filter_index(date_begin ~ date_end) %>% 
    tsibble::as_tibble() %>% 
    dplyr::select(all_of(var_names)) %>%
    prcomp_tidy()
}

get_vfci <- function(data,y,x,het=x,prcomp=TRUE,n_prcomp = 4, date_begin="1962 Q1", date_end="2022 Q3",match_stata=TRUE, ..., gls_opt = list()) {
  Call <- match.call()
  optarg <- list(...)
  data <- data %>% 
    tsibble::as_tsibble() %>% 
    tsibble::filter_index(date_begin ~ date_end) %>% 
    tibble::as_tibble()
  
  if (prcomp) {
    pca <- get_pc(data,x,match_stata=match_stata,...)
    if (match_stata){
      pcs <- pca$scores # time series of principal components in princomp
    }
    else {
      pcs <- pca$x # time series of principal components in princomp  
    }
    
    colnames(pcs) <- paste0("pc", 1:ncol(pcs))
    data_pcs <- dplyr::bind_cols(data,tibble::as_tibble(pcs),.name_repair = c("minimal"))
    x_pcs <- paste0("pc", 1:n_prcomp)
    h <- hetreg(data_pcs,y,x_pcs, ..., gls_opt = gls_opt)
    h$pc <- pca
    h$pc_ts <- data_pcs %>% dplyr::select(dplyr::all_of(colnames(pcs)) )
  }
  else {
    h <- hetreg(data,y,x,het, ..., gls_opt = gls_opt)
  }
  
  # # mean equation
  # h$mean_eq$coefficients <- h$coefficients # coefficients
  # h$mean_eq$se <- sqrt(diag(h$varBeta)) # std. err.
  # h$mean_eq$fitted <- h$fitted # fitted values, same as Stata's `predict`
  # 
  # # vol equation
  # h$vol_eq$coefficients <- 2 * attr(h$apVar, "Pars") # coefficients
  # h$vol_eq$se <- 2 * sqrt(diag(h$apVar)) # std. err.
  # h$vol_eq$exp_fitted <- attr(h$residuals, "std") # fitted values, same as Stata's `predict, sigma`
  
  # return vfci, mu, time series, and hetreg results
  if (prcomp) {
    out <- list(
      ts = dplyr::bind_cols(h$pc_ts,
                            tibble::tibble(qtr = data$qtr,
                                           vfci=log(attr(h$residuals, "std")),
                                           mu=unname(h$fitted))),
      hetreg = h,
      call = Call
    )
  }
  else {
    out <- list(
      ts = tibble(qtr = data$qtr, vfci=log(attr(h$residuals, "std")), mu=unname(h$fitted)),
      hetreg=h,
      call = Call
    )
  }
  return(out)
  
  
}