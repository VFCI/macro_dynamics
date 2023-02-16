matrictint <- function(S,XXi,T,cx=NULL,cs=NULL)
###  S:  usually sample cross product matrix of LS residuals
###  cs: instead of S, can provide UT cs s.t. cs'cs==S, as from chol(S) or qr.R(qr(S))
### XXi:  inv(X'X) matrix for rhs variables
### cx:  instead of XXi, can provide UT cx
###  T:  number of observations
###  w:  log of integrated posterior for SUR or RF VAR with det(Sigma)^(-(m+1)/2) Jeffreys-like prior
###  To get the log of the integral of the likelihood for a VAR with T observations, 
###   k rhs variables in each equation, and m equations, set T=T-m-1 and subtract .5*m*(m+1)*log(2*pi).
### We are integrating the exponential of -.5*T*m*log(2*pi)-.5*(T+m+1)*log(det(Sigma))-.5*trace(Sigma\S(beta)).
{ if(is.null(cx))
    {
      ## browser()
      k<-dim(XXi)[1]
      ##cx <- chol(XXi)
      cx <- try(chol(XXi))
      if(inherits(cx,"try-error")) stop("XXI not p.d.")
    }
  else
    {
      k <- dim(cx)[1]
    }
  if(is.null(cs))
    {
      m<-dim(S)[1]
      ##cs <- chol(S)
      cs<-try(chol(S));
      if(inherits(cs,"try-error")) stop("S not p.d.")
    }
  else
    {
      m <- dim(cs)[1]
    }
  ##-------debug---------
  ##browser()
  ##---------------------
    w<-(-T+k+(m-1)/2)*m*.5*log(pi)-(T-k)*sum(log(diag(cs)))+m*sum(log(diag(cx)))+ggammaln(m,(T-k)/2)
    return(w)
  }
  
ggammaln <- function (m,ndf)
###function gg<-ggamma(m,ndf)
### From 8.2.22 on p.427 of Box and Tiao, this is the log of generalized
### gamma divided by gamma(.5)^(.5*m*(m-1))
{
  if( ndf<=(m-1)/2)
    stop('too few df in ggammaln')
  else
    ##lgg=.5*m*(m-1)*gammaln(.5); % normalizing factor not used in Wishart integral
    garg<-ndf+.5*seq(0,1-m,by=-1)
  lgg<-sum(lgamma(garg))
  return(lgg)
}    
