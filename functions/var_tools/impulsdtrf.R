impulsdtrf <- function(vout=NULL, smat=NULL, nstep=40, order=NULL)
### vout:           output structure from rfvar3.
###                 To use this with output from postdraw, create a dummy vout with vout$By=pout$By[ , , ,id] and provide smat=pout$smat[ , ,id]
### smat:           if !is.null(vout) and order and smat are NULL, the impulse responses will be for a cholesky decomp with variables
###                 ordered as in the input to rfvar3.  More generally, can be any set of initial
###                 values for the shocks.  To keep the shocks orthogonal and scaled properly,
###                 smat should be such that smat %*% t(smat) == crossprod(vout$u)/dim(u)[1].
###                 However, the routine works with singular smat or with smat's column dimension
###                 less than its row dimension.
### order:          To get a cholesky decomp with a different ordering, set order to an integer
###                 vector giving the desired ordering.  
### response:       nvar x nshocks x nstep array of impulse responses.
###
###                 with vout from rfvarKF, smat argument is required, since there is no vout$u.
###
### Code written by Christopher Sims, based on 6/03 matlab code.  This version 3/27/04.
### Added dimension labeling, 8/02/04.  Allow non-square smat, integrate with rfvar3 output, 4.7.10.
  {
    ##-----debug--------
    ##browser()
    ##------------------
    B <- vout$By
    neq <- dim(B)[1]
    nvar <- dim(B)[2]
    lags <- dim(B)[3]
    dimnB <- dimnames(B)
    if (is.null(smat)) {
      if (is.null(order) ) {
        order <- 1:neq
      }
      smat <- t(pchol(crossprod(vout$u)/dim(vout$u)[1], order)) # makes first shock affect all variables
    }
    nshock <- dim(smat)[2]
    if(dim(smat)[1] != dim(B)[1]) stop("B and smat conflict on # of equations") #
    response <- array(0,dim=c(neq,nshock,nstep+lags-1));
    response[ , , lags] <- smat
    response <- aperm(response, c(1,3,2))
    irhs <- 1:(lags*nvar)
    ilhs <- lags * nvar + (1:nvar)
    response <- matrix(response, ncol=nshock)
    B <- B[, , seq(from=lags, to=1, by=-1)] #reverse time index to allow matrix mult instead of loop
    B <- matrix(B,nrow=nvar)
    for (it in 1:(nstep-1)) {
      response[ilhs, ] <- B %*% response[irhs, ]
      irhs <- irhs + nvar
      ilhs <- ilhs + nvar
    }
    ## for (it in 2:nstep)
    ##       {
    ##         for (ilag in 1:min(lags,it-1))
    ##           response[,,it] <- response[,,it]+B[,,ilag] %*% response[,,it-ilag]
    ##       }
    dim(response) <- c(nvar, nstep + lags - 1, nshock)
    response <- aperm(response[ , -(1:(lags-1)), ,drop=FALSE], c(1, 3, 2)) #drop the zero initial conditions; array in usual format
    dimnames(response) <- list(dimnB[[1]], dimnames(smat)[[2]], NULL)
    ## dimnames(response)[2] <- dimnames(smat)[1]
    ## dimnames(response)[1] <- dimnames(B)[2]
    return(response)
  }
