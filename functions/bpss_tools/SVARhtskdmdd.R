SVARhtskdmdd <- function(ydata,lags,xdata=NULL, const=TRUE, A0, lmd, Tsigbrk, breaks=NULL,
                         urprior=list(lambda=5,mu=1), mnprior=list(tight=3,decay=.5),
                         vprior=list(sig=NULL,w=1), train=0,flat=FALSE,nonorm=FALSE,ic=NULL,
                         mnstart = 1, cores = 1, cosprior = NULL,
                         lmdmean = FALSE, oweights = NULL,
                         drawbe = FALSE)
### This gives the posterior integrated over A+ (the right-hand side coefficients), conditional
### on A0 and lmd.
###---------------------------------------------
### ydata:        endogenous variable data matrix, including initial condition dates.
### xdata:        exogenous variable data matrix, including initial condition dates.  
### const:        Constant term is added automatically if const=TRUE.
### A0:           Contemporaneous coefficient matrix --- constant.
### lmd:          Column Vectors of log variances of structural shocks.
### Tsigbrk:      Dates at which lmd vectors change.  Last date with old lmd (not first with new).
### breaks:       breaks in the data.  The first lags data points after a break are used
###               as new initial conditions, not data points for the fit.
### lambda:       weight on the co-persistence prior dummy observation.  (5 is reasonable)
###               lambda>0 => x variables included; lambda<0 => x variables excluded;
### mnprior       see vprior() comments
### urprior:      
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
    ybar <- apply(ydata[1:lags, ], 2, mean)
    T <- dim(ydata)[1]
    nv <- dim(ydata)[2]
    if (const) {
        xdata <- cbind(xdata, matrix(1,T,1))
    }
    ## looks likely that const=FALSE, xdata=NULL case crashes.  (2012.9.24)
    if (!is.null(xdata) ) stopifnot( dim(xdata)[1] == T)
    Tx <- dim(xdata)[1]
    nx <- dim(xdata)[2]
    vp <- varprior(nv,nx,lags,mnprior,vprior, urprior=urprior, ybar=ybar, mnstart = mnstart,cosprior = cosprior) # vp$: ydum,xdum,pbreaks

    ## -------- set lmd for prior dummies --------------
    if (is.null(dim(lmd))){
        lmdbar <- lmd ## only 1 variance regimex
    } else if (lmdmean){
        lmdbar <- apply(lmd, 1, mean) ## take mean of logs
    } else { ## zero is the arithmetic mean always
        lmdbar = rep(0,dim(lmd)[1])
    }
    lmd <- cbind(lmd, lmdbar)


    ## -------- set A0 for prior dummies, if required ------
    if (length(dim(A0)) == 3){
        A0 = abind(A0,apply(A0,1:2,mean),along=3)
    }
        
    ## --------------------- Tsigbrk assumed to be indexes into ydata matrix, not
    ## --------------------- dates.  Conversion from dates and adding T done in bvarWrap3().
    ## Tsigbrk <- c(invTime(Tsigbrk, ydata), T)            #dummy obs at end
    
    ##-------------------------------------------
    ## var = rfvar3(ydata=rbind(ydata, vp$ydum), lags=lags, xdata=rbind(xdata,vp$xdum), breaks=matrix(c(breaks, T, T + vp$pbreaks), ncol=1),
    ## const=FALSE, lambda=lambda, mu=mu, ic=ic) # const is FALSE in this call because ones alread put into xdata

    ## var = rfvar3(ydata=rbind(ydata, vp$ydum), lags=lags, xdata=rbind(xdata,vp$xdum),
    ##     breaks=matrix(c(breaks, T, T + vp$pbreaks), ncol=1), const=FALSE, lambda=NULL,
    ##     mu=NULL, ic=ic, sigpar=list(A0=A0,lmd=lmd,Tsigbrk=Tsigbrk), cores = cores)

    ## ISSUE 7/27: lmdbar is not being used for the dummies! because of the way Tsigbrk is coded
    var = rfvar3(ydata=rbind(ydata, vp$ydum), lags=lags, xdata=rbind(xdata,vp$xdum),
                 breaks=matrix(c(breaks, T, T + vp$pbreaks), ncol=1), const=FALSE, lambda=NULL,
                 mu=NULL, ic=ic, sigpar=list(A0=A0,lmd=lmd,Tsigbrk=c(Tsigbrk,T)), cores = cores,
                 oweights = oweights, drawbe = drawbe)

	if (is.null(var)) {
		return(list(w = -Inf))
	}
	
    ##  const is FALSE in this call because ones alread put into xdata
    Tu <- dim(var$u)[1]
    if ( any(var$snglty > 0) ) error( var$snglty, " redundant columns in rhs matrix")
    lmdllh <- .5 * sum(var$lmdseries)

    ## Check if A0 is constant or changing across regimes
    if (length(dim(A0)) == 2){
        detTerm = determinant(A0)$modulus
    } else {

        ## Take determinants for each A0 in the list
        dets = apply(A0,3,function(x){determinant(x)$modulus})

        ## Weighted sum of determinants
        detTerm = sum(dets[var$freqs])/length(var$freqs)
        
    }
    
    
    llh <- -.5 * sum(var$u^2) + Tu * (-nv * log(2 * pi)/2 + detTerm) +
        lmdllh
    ## nb: determinant() returns log of abs value of determinant
    nX <- lags * nv + 1
    w <-  llh + .5 * sum(var$logdetxxi) + nv * nX * log(2 * pi)/2
    if(train!=0) {
        if(train <= lags)
            {
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
        priorTsigbrk <- c(0, Tp)
        ## It is assumed that there are no breaks in lmd in the training sample!
        priornsig <- 2
        priorlmd <- cbind(lmd[ , 1], lmd[ , dim(lmd)[2]])

        if (length(dim(A0)) == 3){  # in case of time varying A0
            priorA0 = abind(A0[,,1],A0[,,dim(A0)[3]],along=3)
        } else {
            priorA0 = A0
        }
        
        

        
        varp <- rfvar3(ydata=rbind(ytrain, vp$ydum), lags=lags, xdata=rbind(xtrain, vp$xdum),
                       breaks=c(tbreaks, Tp+vp$pbreaks), 
                       lambda=NULL, mu=NULL, const=FALSE, ic=ic,
                       sigpar=list(A0=priorA0,lmd=priorlmd, Tsigbrk=priorTsigbrk), 
					   cores = cores)
        ## const is FALSE here because xdata already has a column of ones.
		
		## If regression blew up, return infinite likelihood so optimization knows not to proceed
		if (is.null(varp)){
			return(list(w = -Inf))
		}
        if (any(varp$snglty > 0)) {
            warning("Prior improper, short ", varp$snglty, " df.  Results likely nonsense.")
        } else {
            Tup <- dim(varp$u)[1]
            lmdllhp <- .5 * sum(varp$lmdseries)

            if (length(dim(A0)) == 3){  # use the prior A0
                
                detsp = dets[c(1,length(dets))] # determinants for right regimes

                ## Weighted sum of determinants
                detPriorA0 = sum(detsp[varp$freqs])/length(varp$freqs)
                
            } else {
                detPriorA0 = determinant(A0)$modulus
            }
            
            llhp <- -.5 * sum(varp$u^2) - Tup * (nv * log(2 * pi)/2 - detPriorA0) +
                lmdllhp
                
            normalizer <- .5 * sum(varp$logdetxxi) + nv * nX * log(2 * pi)/2
            wp <- llhp + normalizer
            w <- w-wp
            llh <- llh - normalizer
            ## llh is height of posterior density over A0, lmd, A+ at peak.  w is height of
            ## marginal posterior for A0, lmd, with A+ integrated out.
        }
    } else {
        varp <- NULL
    }
    return(list(w=w,var=var,varp=varp,prior=list(urprior=urprior, vprior=vprior, mnprior=mnprior)))
}
