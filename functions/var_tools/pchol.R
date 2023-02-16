pchol <- function(sig, porder) {
  invporder <- match(1:length(porder), porder)
  return(chol(sig[porder, porder])[invporder, invporder])
}
