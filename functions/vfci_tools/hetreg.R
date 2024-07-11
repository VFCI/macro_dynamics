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
hetreg <- function(data,y,x,het=x, ...) {
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

  frm <- stats::formula(paste(mean_eq, collapse = " "))
  h <- nlme::gls(frm, data = data, weights = weights, ...)
  h$call$hetreg <- call
  return(h)
}

