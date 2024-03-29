varprior <-  function(nv=1,nx=0,lags=1,mnprior=list(tight=5,decay=.5),
                      vprior=list(sig=1,w=1),
                      urprior=list(lambda=NULL, mu=NULL), xsig=NULL, ybar=NULL, xbar=1, nstat=rep(TRUE,nv),
                      mnstart = 1,
                      cosprior = NULL
)
### ydum, xdum:   dummy observation data that implement the prior
### breaks:       vector of points in the dummy data after which new dummy obs start
###                   Set breaks=T+matrix(c(0,breaks),ncol=1), ydata=rbind(ydata,ydum), xdum=rbind(xdata,xdum), where 
###                   actual data matrix has T rows, in preparing input for rfvar3
### nv,nx,lags: VAR dimensions
### mnprior$tight:Overall tightness of Minnesota prior. 1/tight ~ own lag std dev
### mnprior$decay:Standard deviations of lags shrink as lag^(-decay)
### vprior$sig:   Vector of prior modes for square roots of diagonal elements of r.f. covariance matrix
###                  Names of this vector name columns of output ydum.
### vprior$w:     Weight on prior on vcv.  1 corresponds to "one dummy observation" weight
###                   vprior$sig is needed
###                   to scale the Minnesota prior, even if the prior on sigma is not used itself.
###                   Set vprior$w=0 to achieve this.
###                   mnprior and vprior.w can each be set to NULL, thereby eliminating the corresponding
###                   dummy observations.
### xsig:          rough scale of x variances.  names of this vector name output xdum
### urprior:       Parameters of the "unit roots" and "co-persistence" priors that are
###                   implemented directly in rfvar3.  lambda and mu should be NULL here if
###                   the dummy observations generated here are used with rfvar3 and lanbda and mu
###                   are not NULL in rfvar3.   lambda < 0 means x'st not included.  Note that constant
###                   is assumed to be last element of x.  If you want lambda < 0 to be the only source
###                   of a prior on the constant, but xsig is not null, set the last element of xsig
###                   to zero.  
### ybar,xbar:        estimates of data means, used in constructing urprior component, but not otherwise.
###                   The default xbar=1 is correct when the constant is the only x.    
### nstat:         Set components corresponding to non-persistent variables to FALSE.
### Note:          The original Minnesota prior treats own lags asymmetrically, and therefore
###                   cannot be implemented entirely with simple dummy observations.  It is also usually
###                   taken to include the sum-of-coefficients and co-persistence components
###                   that are implemented directly in rfvar3.R.  The diagonal prior on v, combined
###                   with sum-of-coefficients and co-persistence components and with the unit own-first-lag
###                   prior mean generates larger prior variances for own than for cross-effects even in 
###                   this formulation, but here there is no way to shrink toward a set of unconstrained 
###                   univariate ARs.
###-----------------------
###
{ require(abind)
    ## nx=0 case messes up, at least at the end (2012.9.23)
    if (!is.null(mnprior)) ## implement an MN prior
    { ## single-coefficient prior dummy obs.
        ## each vbl and each lag has a dummy observation, and each dummy obs has values for current and lagged
        ## y's  and current x's. we separate the y's and the x's into two arrays.  The last two indexes, lag
        ## and rhsy, index the dummy observations.  
        xdum <- if(nx > 0) {
                    array(0, dim=c(lags + 1, nx, lags, nv), dimnames=list(obsno=1:(lags + 1), xvbl=1:nx, lag=1:lags, rhsy=1:nv))
                } else {
                    NULL
                }
        ydum <- array(0,dim=c(lags+1,nv,lags,nv),dimnames=list(obsno=1:(lags+1),rhsy=1:nv,lag=1:lags, rhsy=1:nv))
        for (il in 1:lags) {
            ##-----debug---------
            ## browser()
            ##------------------

                                        #ydum[il + 1,,il,] <- il^mnprior$decay * diag(vprior$sig,nv,nv)

                                        # KS alteration, July 2014: allowing the mn prior to start the decay at the mnstart + 1 lag (default 1, so decay starts with 2)
            decayfactor = (il <= mnstart) * 1 + ((il > mnstart) * (il - mnstart + 1))^(mnprior$decay)
                                        # il <= mnstart gets decayfactor = 1; il > mnstart gets decay normally associated with (il - mnstart + 1)
                                        # e.g., start prior at ilag = 2; no decay on lag 1, 2 variances; lag 3 variance is treated like lag 2 variance in previous set-up
            
            ydum[il+1,,il,] <- decayfactor*diag(vprior$sig,nv,nv)              
        }
        ## If we have non-trivial x's, need dobs's for them, also.
        if(!is.null(xsig)) {
            ydumx <-  array(0, dim=c(lags + 1, nv, nx), dimnames=list(obsno=1:(lags + 1), rhsy=1:nv, dx=1:nx))
            xdumx <-  array(0, dim=c(lags + 1, nx, nx), dimnames=list(obsno=1:(lags + 1), xvbl=nx, dx=1:nx))
            xdumx[1, , ] <- diag(xsig, nx, nx)
            ## note that xvalues for obsno 2:(lags+1) don't matter.  This is one dummy obseervation,
            ## so only the "current" x is used.
        }
        ydum[1,,1,] <- diag(vprior$sig * nstat, nv, nv) # so own lag has mean zero if nstat FALSE
        ydum <- mnprior$tight * ydum
        dim(ydum) <- c(lags+1,nv,lags*nv)
        ydum <- ydum[seq(lags+1,1,by=-1),,]
        xdum <- mnprior$tight*xdum
        dim(xdum) <- c(lags+1,nx,lags*nv)
        xdum <- xdum[seq(lags+1,1,by=-1),,]
    } else {
        ydum <- NULL;
        xdum <- NULL;
        breaks <- NULL;
        lbreak <- 0;
    }

    if (!is.null(cosprior)){ ## Implement the cosine transformation prior

        ## same size arrays as with the MN prior
        xdumc = if(nx > 0) {
                    array(0, dim=c(lags + 1, nx, lags, nv), dimnames=list(obsno=1:(lags + 1), xvbl=1:nx, lag=1:lags, rhsy=1:nv))
                } else {
                    NULL
                }


        ydumc = array(0,dim=c(lags+1,nv,lags,nv),dimnames=list(obsno=1:(lags+1),rhsy=1:nv,lag=1:lags, rhsy=1:nv))

        cosmat = ctmat(lags) ## cosine transformation matrix, common for all coefficients
        dmat = cosprior$smooth^ (0:(lags-1)) * cosmat %*% diag(cosprior$damp ^ (0:(lags-1))) ## matrix of dummies, except for variance scaling for rhs

        for (iv in 1:nv){ ## iterate over lhs variables
            ydumc[1 + 1:lags, iv, 1:lags, iv] = t(dmat) * vprior$sig[iv]
        }
        
        for (iv in 1:nv){
            ydumc[1,iv,,iv] = ydumc[2,iv,,iv]
        }

        ydumc = cosprior$tight * ydumc
        dim(ydumc) = c(lags+1,nv,lags*nv)
        ydumc = ydumc[seq(lags+1,1,by=-1),,]

        ## ydumc = array(0,c(lags + 1, nv, lags*nv))

        ## for (irhs in 1:nv){ ## each rhs variable
        ##     idmat = dmat * vprior$sig[irhs] ## scaled dummies
        ##     xseq = seq(irhs,irhs + (lags - 1)*nv,nv)
        ##     for (ilhs in 1:nv){
        ##         ydumc[1 + 1:lags,ilhs, xseq] = idmat
        ##     }

        ##     ydumc[1,irhs,1:nv] = ydumc[1 + 1:lags, irhs, xseq]
        ## }

        
        dim(xdumc) = c(lags+1,nx,lags*nv)
        xdumc = cosprior$tight*xdumc
        xdumc = xdumc[seq(lags+1,1,by=-1),,,drop = FALSE]

    } else {
        ydumc = NULL
        xdumc = NULL
        
    }
    
    if (!is.null(urprior$lambda) ) {
        ## lambda obs.  just one
        ydumur <- matrix(ybar, nrow=lags+1, ncol=nv, byrow=TRUE) * abs(urprior$lambda)
        if(urprior$lambda > 0) {
            xdumur <- matrix(xbar, lags + 1, nx, byrow=TRUE) * urprior$lambda # (all but first row redundant)
        } else {
            xdumur <- matrix(0, lags + 1, nx)
        }
    } else {
        ydumur <- NULL
        xdumur <- NULL
    }
    ## mu obs. sum(nstat) of them
    if (!is.null(urprior$mu)) {
        ydumuri <-array(0, c(lags+1, nv, nv))
        for (iv in which(nstat)) {
            ydumuri[ , iv, iv] <- ybar[iv]
        }
        ydumur <- abind(ydumur, urprior$mu *ydumuri, along=3)
        xdumur <- abind(xdumur, array(0, c(lags+1, nx, nv)), along=3)
    }
    if (!is.null(vprior) && vprior$w > 0)
    {
        ydum2 <- array(0,dim=c(lags+1,nv,nv))
        xdum2 <- array(0,dim=c(lags+1,nx,nv))
        ydum2[lags+1,,] <- diag(vprior$sig,nv,nv)*vprior$w #The vprior$w factor was missing until 11/29/06
                                        # Original idea, not implemented, was probably that w be an integer
                                        # repetition count for variance dobs.
                                        # Now it's just a scale factor for sig. in variance prior.
    } else {
        ydum2 <- NULL
        xdum2 <- NULL
    }
    ## stack everything up.
    if (!is.null(ydum)){
        dim(ydum) <- c(lags + 1, nv, lags * nv) # merge all the individual mn dobs
        dim(xdum) <- c(lags + 1, nx, lags * nv)
    }

    ydum <- abind(ydum, ydumc, ydumur, ydum2, along=3)
    xdum <- abind(xdum, xdumc, xdumur, xdum2, along=3)
    breaks <- (lags+1) * (1:(dim(ydum)[3] -1)) # end of sample is not a "break".
    ydum <- aperm(ydum, c(1, 3, 2))
    ydum <- matrix(ydum, ncol=dim(ydum)[3])
    xdum <- aperm(xdum, c(1,3,2))
    xdum <- matrix(xdum, ncol=dim(xdum)[3])
    ##   dim(ydum2) <- c((lags+1)*nv,nv)
    ##   dim(ydum) <- c((lags+1)*nv,lags*nv)
    ##   ydum <- cbind(ydum,ydum2)
    ##   dim(xdum2) <- c((lags+1)*nx,nv)
    ##   dim(xdum) <- c((lags +1)*nx,lags*nv)
    ##   xdum <- cbind(xdum,xdum2)
    ##   dim(ydum) <- c(lags+1,nv,dim(ydum)[2])
    ##   ydum <- aperm(ydum,c(1,3,2))
    ##   dim(ydum) <- c(dim(ydum)[1]*dim(ydum)[2],nv)
    ##   dim(xdum) <- c(lags+1,nx,dim(xdum)[2])
    ##   xdum <- aperm(xdum,c(1,3,2))
    ##   dim(xdum) <- c(dim(xdum)[1]*dim(xdum)[2],nx)
    ##   if(nv>1){
    ##     breaks <- c(breaks, (lags+1)*(0:(nv-1))+lbreak)
    ##   }
    ## } else {
    ##   if (!is.null(ydum)) { # case with mnprior non-null, but vprior null
    ##     ydum <- aperm(ydum, c(1, 3, 2))
    ##     dim(ydum) <- c(prod(dim(ydum)[1:2]), dim(ydum)[3])
    ##     xdum <- aperm(xdum, c(1,3,2))
    ##     dim(xdum) <- c(prod(dim(xdum)[1:2]), dim(xdum)[3])
    ##   }
    ## }
    dimnames(ydum) <- list(NULL, names(vprior$sig))
    dimnames(xdum) <- list(NULL, names(xsig))
    return(list(ydum=ydum,xdum=xdum,pbreaks=breaks))
    ## data here in the form of T by nv y, and T x nx x.  Lagged y's not put in to a rhs
    ## regression matrix, so a "breaks" vector is needed.  
    ## rfvar3 adds persistence and sum of coeffs dummy observations at end of  data in lhs and rhs
    ## regression matrix form.  So to combine this with rfvar3, set lambda and mu to NULL in one or the
    ## other program.
}
