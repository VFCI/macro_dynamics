kfVC <- function(y, X, shat, sig, M) {
  ## s is the state, and the plant equation is s(t)=G %*% s(t-1)+t(M) %*% e, where e is
  ## N(0,I).  The observation equation is y(t)=Hs(t).  The prior distribution for
  ## s is N(shat,sig).  To handle the standard Kalman Filter setup where the observation
  ## equation is y(t)=Hs(t)+Nu(t), expand s to become [s;v] (where v=Nu), expand H to [H I], replace
  ## G by [G 0;0 0], and replace M with [M 0;0 N].  The resulting model has no "error" in the
  ## observation equation but is equivalent to the original model with error in the state equation.
  ## The posterior on the new state is N(shatnew,signew) and lh is a two-dimensional vector containing
  ## the increments to the two component terms of the log likelihood function.  They are added 
  ## to form the log likelihood, but are used separately in constructing a concentrated or marginal
  ## likelihood. fcsterr is the error in the forecast of y based on the prior.
  ## ----------------
  ## This version specializes to the case of a constant-coefficient VAR, where H= cbind(kronecker(I, X[it, ], I)
  ## and G is the identity (for the constant coefficients) with zeros (for the shocks in the state vector)
  ## appended in the lower right.  Also, kf2's M is all zeros except for the lower right nvar x nvar.  By
  ## bypassing lots of multiplication by zero, this version is faster than generic kf2by a factor of four for
  ## a 7-variable, 13-lag VAR.  Here M is the
  ## transposed cholesky factor of the VAR shock covariances, not the full state equation M (whichis full of
  ## zeros).  With M constant, plain rfvar3 is more efficient.
  ## ----------------------
  SMALLSV <- 1e-7
  nv <- length(y)
  nx <- length(X)
  nXX <- nv*nx
  nstate <- length(shat)
  omega <- matrix(0, nstate, nstate)
  omega[1:nXX, 1:nXX] <- sig[1:nXX,1:nXX]
  sigObs <- crossprod(M)
  omega[nXX + (1:nv), nXX + (1:nv)] <- sigObs
  nstate <- length(shat)
  ## stopifnot (nstate >= nobs)
  ##------------ Don't need separate treatment of H == 0.  H %*% G %*% t(H) = 0 covers it.
  ##   if (isTRUE(all.equal(H, 0))) { # No observation case.  Just propagate the state.
  ##     lh <- c(0,0)
  ##     shatnew <- G %*% shat
  ##     signew <- omega
  ##     fcsterr <- y                        # y had better be 0
  ##     if (!all.equal(y,0) ) warning("zero H but non-zero y")
  ##   } else {
  ## ho <- H %*% omega
  ho <- matrix(0, nv, nstate)
  for (iv in 1:nv)
    ho[iv, ] <- c(X %*% omega[ ((iv - 1) * nx) + 1:nx, 1:nXX],   sigObs[iv, ])
  hoh <- matrix(0, nv, nv)
  for (jv in 1:iv)
    hoh[, jv] <- ho[ , nx * (jv-1) + (1:nx)] %*% X + sigObs[ , jv]
  svdhoh <- svd( hoh )
  shatmat <- matrix(shat[1:(nv * nx)], nv, nx, byrow=TRUE) 
  if (all(svdhoh$d < SMALLSV)) { # Observation is uninformative. Propagate state.
    lh <- c(0,0)
    shatnew <- shat
    signew <- omega
    fcsterr <- y - shatmat %*% X     # had better be 0
    if (!all(abs(fcsterr) < 1e-7)) warning("Uninformative H but non-zero fcsterr")
  } else {
    first0 <- match(TRUE, svdhoh$d < SMALLSV)
    if (is.na(first0)) first0 <- nv + 1
    u <- svdhoh$u[ , 1:(first0-1), drop=FALSE]
    v <- svdhoh$v[ , 1:(first0-1), drop=FALSE]
    d <- svdhoh$d[1:(first0-1), drop=FALSE]
    fcsterr <- y - shatmat %*% X
    hohifac <- (1/sqrt(d)) * t(u)       #diag(1/sqrt(d)) %*% t(u)
    ferr <- hohifac %*% fcsterr
    lh <- c(0,0)
    lh[1] <- -.5 * crossprod(ferr)
    lh[2] <- -.5 * sum( log(d) )
    hohoifac <-hohifac %*% ho
    shatnew <- crossprod(hohoifac, ferr) + c(shat[1:nXX], rep(0,nv))
    signew <- omega - crossprod(hohoifac)
  }
  return(list(shat=shatnew, sig=signew, lh=lh, fcsterr=fcsterr))
}
