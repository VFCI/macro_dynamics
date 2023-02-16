vdecomp <- function(resp) {
  vdc <- apply(resp^2, c(1,2), cumsum)
  for (it in 1:dim(resp)[3]) {
    vdc[it , , ] <- diag(1/apply(vdc[it, , ], 1, sum)) %*% vdc[it , , ]
  }
  vdc <- aperm(vdc, c(2,3,1))
  dimnames(vdc) <- dimnames(resp)
  return(vdc)
}
