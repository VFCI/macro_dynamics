mgnldnsty <- function(ydata,lags,xdata=NULL, const=TRUE, breaks=NULL,lambda=5,mu=1,mnprior=list(tight=3,decay=.5),
                      vprior=list(sig=NULL,w=1),train=0,flat=FALSE,nonorm=FALSE,ic=NULL)
### ydata:        endogenous variable data matrix, including initial condition dates.
### xdata:        exogenous variable data matrix, including initial condition dates.  
### const:        Constant term is added automatically if const=TRUE.
### breaks:       breaks in the data.  The first lags data points after a break are used
###               as new initial conditions, not data points for the fit.
### lambda:       weight on the co-persistence prior dummy observation.  (5 is reasonable)
###               lambda>0 => x variables included; lambda<0 => x variables excluded;
### mu:           weight on variable-by-variable sum of coeffs dummy obs. (1 is reasonable)
### mnprior$tight:weight on the Minnesota prior dummies.  Prior std dev on first lag is
###               1/mnprior$tight
### mnprior$decay:prior std dev on own lag j is 1/j^decay
### vprior$sig:   vector of nv prior std dev''s of equation shocks.  vprior$sig is needed
###               to scale other components of the prior, even if vprior$w=0. Not needed for a pure training
###               sample prior.
### vprior$w:     weight on vcv dummies.  (1 is reasonable; higher values tighten up.)
### train:        If non-zero, this is the point in the sample at which the
###               "training sample" ends.  Prior x likelihood to this point is weighted to
###               integrate to 1, and therefore is treated as if it were itself the prior.
###               To do a pure training sample prior, set lambda=mu=0, mnprior=NULL, vprior$w=0,
###               train>lags.  
### flat:         Even with lambda=mu=vprior$w=0, mnprior=NULL, det(Sigma)^(-(nv+1)/2) is used
###               as a "prior", unless flat=TRUE. flat=TRUE is likely not to work unless train is reasonably large.
### nonorm:       If true, use dummy observations but do not normalize posterior to make them a
###               proper prior.  Useful to duplicate results obtained by others, to use
###               dummy observations that do not imply a proper prior, or to save computing time in case only the
###               posterior on this model's parameters, not the weight on the model, is needed.  
### ic:           Initial conditions matrix for use in forming the sums of coefficients dummy observations.
###               If ic=NULL, the means of the first lags observations in ydata are used.  If !is.null(ic),
###               ic should be a single "observation" on the y's and x's that will be used as the persistent
###               values entering the sums of coefficients dummies.
###
###               Note that to enter a prior directly as dummy observations, one can treat the
###               Dummy observations as a training sample.
###
{
  if (is.null(dim(ydata)))  ydata <- matrix(ydata, ncol=1)
  T <- dim(ydata)[1]
  nv <- dim(ydata)[2]
  if (const) {
    xdata <- cbind(xdata, matrix(1,T,1))
  }
  ## looks likely that const=FALSE, xdata=NULL case crashes.  (2012.9.24)
  if (!is.null(xdata) ) stopifnot( dim(xdata)[1] == T)
  Tx <- dim(xdata)[1]
  nx <- dim(xdata)[2]
  ## 2013.8 fix:  added urprior here, set lambda and mu to NULL in rfvar3 call, so
  ## prior dummies treated correctly in normalizing the prior.
  if (is.null(ic)) {
    ybar <- apply(ydata[1:lags, , drop=FALSE], 2, mean)
  } else {
    ybar <- ic
  }
  vp <- varprior(nv,nx,lags,mnprior,vprior, urprior=list(lambda=lambda, mu=mu), ybar=ybar)
  ## vp$: ydum,xdum,pbreaks
  var = rfvar3(ydata=rbind(ydata, vp$ydum), lags=lags, xdata=rbind(xdata,vp$xdum), breaks=matrix(c(breaks, T, T + vp$pbreaks), ncol=1),
    const=FALSE, lambda=NULL, mu=NULL, ic=ic) # const is FALSE in this call because ones alread put into xdata
  Tu <- dim(var$u)[1]
  if ( var$snglty > 0 ) error( var$snglty, " redundant columns in rhs matrix")
  w <- matrictint(crossprod(var$u),var$xxi,Tu-flat*(nv+1))-flat*.5*nv*(nv+1)*log(2*pi);
  if(train!=0) {
      if(train <= lags) {
          cat("end of training sample <= # of lags\n")  #
              return
      }
      Tp <- train
      tbreaks <- c(breaks[breaks<train],Tp)
  } else {
      Tp <- lags
      ## because need initial conditions to form lambda/mu prior dummy obs
      tbreaks <- Tp
  }
  ytrain <- ydata[1:Tp,,drop=FALSE]
  xtrain <- xdata[1:Tp,,drop=FALSE]
  if (!nonorm) {
      ## fixed 2013.8.14:  Looks as if dummy obs from urprior are missed here.  Should include
      ## non-null lambda, mu in call to varprior, not in rfvar3 call.
      ## varp <- rfvar3(ydata=rbind(ytrain, vp$ydum), lags=lags, xdata=rbind(xtrain, vp$xdum),
      ##               breaks=c(tbreaks, Tp+vp$pbreaks), lambda=lambda, mu=mu, const=FALSE, ic=ic)
      varp <- rfvar3(ydata=rbind(ytrain, vp$ydum), lags=lags, xdata=rbind(xtrain, vp$xdum),
                     breaks=c(tbreaks, Tp+vp$pbreaks), lambda=NULL, mu=NULL, const=FALSE, ic=ic)
      ## const is FALSE here because xdata already has a column of ones.
      if (varp$snglty > 0) {
          warning("Prior improper, short ", varp$snglty, " df.  Results likely nonsense.")
      } else {
          Tup <- dim(varp$u)[1]
          wp <- matrictint(crossprod(varp$u),varp$xxi,Tup-flat*(nv+1)/2)-flat*.5*nv*(nv+1)*log(2*pi)
          w=w-wp
      }
  } else {
      varp <- NULL
  }
  return(list(w=w,var=var,varp=varp,prior=list(lambda=lambda,mu=mu,vprior=vprior,mnprior=mnprior), call=match.call()))
}
