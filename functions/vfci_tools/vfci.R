#' @importFrom generics tidy
#' @export
generics::tidy
#' @importFrom generics glance
#' @export
generics::glance
#' @importFrom generics augment
#' @export
generics::augment
#' @export
hetreg <- function(data,y,x,het=x, ..., gls_opt = list()) {
  # y_i = x_i \beta + \epsilon_i
  # Stata:  var(\epsilon_i) = sigma_i^2 = \exp(z_i \alpha)
  # R nlme: var(\epsilon_i) = sigma_i^2 = \exp(2 \theta v_i ) 
  Call <- match.call()
  mean_eq <- paste(y,"~",paste(x, collapse='+'))
  weights <- eval(parse(text=glue::glue(
    "nlme::varComb( \n",
    glue::glue_collapse(glue::glue("nlme::varExp(form = ~ {het})"), ",\n"),
    ")"
  )))
  frm <- formula(paste(mean_eq, collapse = " "))
  h <- nlme::gls(frm,data=data, weights=weights,method = "ML")
}
#' @export
tidy.het <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  # Class het is a subclass of gls
  # y_i = x_i \beta + \epsilon_i
  # Stata:  var(\epsilon_i) = sigma_i^2 = \exp(z_i \alpha)
  # R nlme: var(\epsilon_i) = sigma_i^2 = \exp(2 \theta v_i )
  
  get_attr <- \(x) attr(x,"formula")
  idx <- list("modelStruct","varStruct", \(x) map(x,get_attr))
  vareq_names <- pluck(x,!!!idx) %>% unname %>% map(deparse1) %>% str_remove_all("~") %>% str_split(" ")
  vareq_coef <- 2 * attr(x$apVar, "Pars") %>% unname
  
  result <- dplyr::tibble(
    term = c(vareq_names,"(Intercept)") %>% unlist,
    estimate = vareq_coef, # \alpha coefficients of vol eq 
    std.error = 2 * sqrt(diag(x$apVar)) %>% unname, # std.error of \alpha
    statistic = estimate/std.error,
    p.value = 2 * pnorm(-abs(statistic)) 
  ) 
  if (conf.int) {
    ci <- confint(x, level = conf.level)
    result <- dplyr::left_join(result, ci, by = "term")
  }
  result
}
#' @export
glance.het <- function(x, ...) {
  x <- structure(x, class = c("gls"))
  glance(x)
}
#' @export
augment.het <- function(x, ...) {
  data.frame(
    "Model" = "het",
    ".vfci" = log(attr(x$residuals, "std")) # vfci
  )
}

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