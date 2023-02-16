rfvar3 = function(ydata=NA,lags=6,xdata=NULL,const=TRUE,breaks=NULL,
                  lambda=5,mu=2,ic=NULL, sigpar=NULL, cores = 1,
                  oweights = NULL, drawbe = FALSE) {
#### This algorithm goes for accuracy without worrying about memory requirements.
#### ---------------------------------------------------------------------------
#### The standard prior it implements is NOT APPROPRIATE for seasonally unadjused data, even
#### if seasonal dummies are included in xdata.  The prior shrinks toward simple persistence, so it
#### will tend to prevent the dummies from picking up all the seasonality.
#### ---------------------------------------------------------------------------
#### ydata:   T x nvar dependent variable data matrix.  
#### xdata:   T x nx exogenous variable data matrix.  
####          Note that if either ydata or xdata has only one column, it must still have a dim vector.  In
####          other words it must be a Tx1 array, not a vector of length T.
####------------------
#### const:   If TRUE, a column of ones is added to (or becomes, if xdata is NULL) the xdata matrix.
#### lags:    number of lags
#### breaks:  rows in ydata and xdata after which there is a break.  This allows for
####          discontinuities in the data (e.g. war years) and for the possibility of
####          adding dummy observations to implement a prior.  This must be a column vector.
####          Note that a single dummy observation becomes lags+1 rows of the data matrix,
####          with a break separating it from the rest of the data.  The function treats the 
####          first lags observations at the top and after each "break" in ydata and xdata as
####          initial conditions. 
#### lambda:  weight on "co-persistence" prior dummy observations.  This expresses
####          belief that when all variables are at a fixed initial level, they tend to
####          stay there.  This is consistent with stationarity and with nonstationarity with or
####          without cointegration.  With lambda < 0 , the 
####          constant term is not included in the dummy observation, so that stationary models
####          with means equal to initial ybar do not fit the prior mean.  With lambda>0, the prior
####          implies that large constants are unlikely if unit roots are present.  To omit this type of
####          dummy observation, use lambda=NULL.
#### mu:      weight on "own persistence" prior dummy observation.  Expresses belief
####          that when y_i has been stable at its initial level, it will tend to persist
####          at that level, regardless of the values of other variables.  There is
####          one of these for each variable.  A reasonable first guess is mu=2.
####          To omit this type of dummy observation, use mu=NULL
#### ic:      for direct input of the initial conditions mean that is used in the persistence dummy observations,
####          as ic$ybar and ic$xbar. 
####          If is.null(ic), the mean of the first lags observations in ydata, xdata are used.
####      The program assumes that the first lags rows of ydata and xdata are real data, not dummies.
####      Dummy observations should go at the end, if any.  If pre-sample x's are not available,
####      repeating the initial xdata(lags+1,:) row or copying xdata(lags+1:2*lags,:) into 
####      xdata(1:lags,:) are reasonable subsititutes.  These values are used in forming the
####      persistence priors.
#### sigpar: list(A0, lmd, Tsigbrk) Allow SVAR with time varying shock variances.  See below.
#### returns:
#### By:      nvar x nvar x lags matrix of coefficients on lagged y's.  1st dimension is "equation number"
#### Bx:      nvar x nx matrix of coefficients on x's
#### u:       (T-6+ (number of dummy obs)) x nvar matrix of residuals.  If ydata is a ts object, u will be also, and will
####          be correctly dated.  u observations dated after end(ydata) are dummy observations.
#### xxi:     X'X inverse, same for all equations.  kronecker(cov(u),xxi) is the full covariance matrix of the regression coefficients.
#### snglty:  Usually 0.  If the rhs variable matrix is not full column rank, this is the gap between the number of columns and the
####          number of non-zero singular values.
#### Code written by Christopher Sims.  This version 8/13/04.
#### 12/18/05:  added ts properties for u, better comments.
####---------------------------------------
#### Modified 2013.8.12 to allow use of A0, lmd, Tsigbrk.  With non-null A0, By is A+ from
#### A0 %*% y(t) = A+(L) %*% y(t) + exp(.5 lmd(t)) * eps(t) .  This works even with
#### lmd constant, but in that case running a single rf estimate (A0=I), then iterating
#### on (A0, lmd) alone makes more sense. With lmd varying, rf estimates change with lmd.
#### --------------------------------------------------------
    if (is.null(dim(ydata))) dim(ydata) = c(length(ydata),1)
    T =dim(ydata)[1]
#### Note that if rfvar3() has been called with dummy obs's already in place, this T
#### includes the dummies.
    nvar=dim(ydata)[2]
####nox=isempty(xdata)
    if (const) {
        xdata = cbind(xdata,matrix(1,T,1))
    }
    nox = identical(xdata,NULL)
    if(!nox){
        T2 = dim(xdata)[1]
        nx = dim(xdata)[2]
    } else {
        T2 = T; nx = 0; xdata= matrix(0,T2,0)
    } 
#### note that x must be same length as y, even though first part of x will not be used.
#### This is so that the lags parameter can be changed without reshaping the xdata matrix.
#### ------------------------
    if (!identical(T2,T)) {
        print('Mismatch of x and y data lengths')
        return()
    }
    if (identical(breaks,NULL))
        nbreaks = 0
    else {
#### if (is.ts(ydata)) {                ## Can use Yr, month-or-quarter pairs, or real number dates.
####   if (is.matrix(breaks) ) {
####     breaks = breaks[ , 1] + (breaks[ ,2] - 1) / frequency(ydata)
####   } else {
####     if (any(abs(breaks - round(breaks))) > 1e-8) {
####       breaks = match(breaks, time(ydata))
####     }
####   }                               ##if not real numbers, not yr-month pairs, it's just obs number
#### }
#### Any use of tsp(ydata) has to be in external processing functions.
        nbreaks=length(breaks)
    }
    breaks = c(0,breaks,T)
    if(any(breaks[2:length(breaks)] < breaks[1:(length(breaks)-1)]))
        stop("list of breaks must be in increasing order\n")
#### initialize smpl as null if initial observations are only there for lambda/mu prior.
#### matlab code uses the fact that in matlab a:b is null if b<a, which is not true for R.
#### if(breaks[2]>lags)
####   smpl = (lags+1):breaks[2]
#### else
####   smpl = NULL
#### if(nbreaks>0){
####   for (nb in 2:(nbreaks+1))
####     smpl = c(smpl,(breaks[nb]+lags+1):breaks[nb+1])
#### }
    smpl = NULL
    for (nb in 2:(nbreaks + 2)) {
        if ( breaks[nb] > breaks[nb-1] + lags )
            smpl = c(smpl, (breaks[nb-1] + lags + 1):breaks[nb])
    }
#### With logic above, one can use an mts-type ydata and omit sections of it by including sequences of breaks separated by
#### less than lags+1.  E.g. with lags=6, monthly data, breaks=rbind(c(1979,8), c(1980,2), c(1980,8), c(1980,12)) omits
#### Sep 1979 through Dec 1981, plus 6 months after that, which are initial conditions for the next sample segment.
    Tsmpl = length(smpl)
    X = array(0,dim=c(Tsmpl,nvar,lags))
    for(ix in seq(along=smpl))
        X[ix,,] = t(ydata[smpl[ix]-(1:lags),,drop=FALSE])
    dim(X) = c(Tsmpl,nvar*lags)
    X = cbind(X, xdata[smpl,,drop=FALSE])
    y = ydata[smpl,,drop=FALSE]
#### Everything now set up with input data for y=Xb+e 
#### ------------------Form persistence dummies-------------------
    if (! (is.null(lambda) & is.null(mu) ) ) {
        if(is.null(ic)) {
            ybar = apply(as.array(ydata[1:lags,,drop=FALSE]),2,mean)
            dim(ybar) = c(1,dim(ydata)[2])
            if (!nox) {
                xbar = apply(array(xdata[1:lags,,drop=FALSE],dim=c(lags,dim(xdata)[2])),2,mean)
                dim(xbar)=c(1,dim(xdata)[2])
            } else {
                xbar = NULL
            }
        } else {
            ybar = ic$ybar
            xbar = ic$xbar
        }
        if (!is.null(lambda)){
            if (lambda<0){
                lambda = -lambda
                xbar = array(0,c(1,dim(xdata)[2]))
            }
            xdum = lambda * cbind(array(rep(ybar,lags),dim=c(1,lags*length(ybar))), xbar)
            ydum = array(0,c(1,nvar))
            ydum[1,] = lambda*ybar
            y = rbind(y,ydum)
            X = rbind(X,xdum)
        }
        if (!is.null(mu)) {
            xdum = cbind(
                array(rep(diag(as.vector(ybar),nrow=length(ybar)),lags),
                      dim=c(dim(ybar)[2],dim(ybar)[2]*lags)),
                array(0,dim=c(nvar,dim(xdata)[2])))*mu
            ydum = mu*diag(as.vector(ybar),nrow=length(ybar))
            X = rbind(X,xdum)
            y = rbind(y,ydum)
        }
    }
    if (!is.null(sigpar)) {
        Tsigbrk = sigpar$Tsigbrk
        lmd = sigpar$lmd
        A0 = sigpar$A0
        if (!is.null(Tsigbrk)) {
#### Tsigbrk = invtime(Tsigbrk, ydata) ##so Tsigbrk given as dates
            nsig = length(Tsigbrk)
        } else {
            nsig = 1
        }
        Tsigbrk = c(Tsigbrk, T)
        lmdndx = rep(1:nsig, times=diff(Tsigbrk))
        lmdseries = lmd[ , lmdndx]
        if ( Tsmpl < dim(y)[1] ) {      ##dummy obs formed in rfvar3
#### Should not be combining this branch with dummy obs's from varprior()
#### already included in ydata.
            lmdp = apply(lmdseries[ ,smpl], 1, mean)
            lmdseries = cbind(lmdseries[ , smpl], matrix(lmdp, nvar, dim(y)[1] - Tsmpl))
        } else {
            lmdseries = lmdseries[ , smpl]
        }

        if (!is.null(oweights)){
            ## add outlier weights
            ## scaled as log standard deviation
            lmdseries[,1:dim(oweights)[1]] =
                lmdseries[,1:dim(oweights)[1]] -
                2*(t(oweights)) 
        }
        
#### i.e., use mean of lmdseries for dummy observation weights.  Note that
#### since lmd is logged, this is geometric mean, maybe not best. 

        nX = dim(X)[2]

        ## Apply A0
        if (length(dim(A0)) == 2){      # constant A0
            ya0 = y %*% t(A0)
        } else {
            ## time-varying A0
            A0list = A0[,,lmdndx]
            A0list[,,smpl]

            ny = dim(y)[1]              # number of observations
            ya0 = array(0, dim(y))
            for (iy in 1:ny){           # apply A0 to each observation
                ya0[iy,] = y[iy,] %*% t(A0list[,,iy])
            }
        }

        ## Save frequency of each regime
        freqs = lmdndx[smpl]
        


        
        ##B = matrix(0,  nX, nvar)
        ##u = matrix(0, Tsmpl, nvar)
        ##uraw = u
        uraw = NULL ##why does it matter?
        ##xxi = array(0, c(nX, nX, nvar))
        ##logdetxxi = vector("numeric", nvar)
        ##snglty = vector("numeric", nvar)
        
        ## Parallel implementation
        ##linreg in a separate file
        ##listOutput = mclapply(1:nvar, linreg, lmdseries, X, ya0, mc.cores = cores)
        listOutput = lapply(1:nvar, linreg, lmdseries, X, ya0, drawbe = drawbe)
        errorflags = matrix(unlist(sapply(listOutput, '[[', 1)))
        if (any(errorflags)) return(NULL) ##tells program to blow up the likelihood
        B = matrix(unlist(sapply(listOutput, '[[', 2)), ncol = nvar)
        u = matrix(unlist(sapply(listOutput, '[[', 3)), ncol = nvar)
        logdetxxi = matrix(unlist(sapply(listOutput, '[[', 4)))
        snglty = matrix(unlist(sapply(listOutput, '[[', 5)))
        ## xx, not xxi!
        xx = array(unlist(sapply(listOutput, '[[', 6)), dim = c(nX, nX, nvar))

        ## draws of coefs and epsilon_it
        Bdraw = matrix(unlist(sapply(listOutput, '[[', 7)), ncol = nvar)
        udraw = matrix(unlist(sapply(listOutput, '[[', 8)), ncol = nvar)

        ## B = matrix(0,nX,nvar)
        ## u = matrix(0,dim(X)[1],nvar)
        ## logdetxxi = matrix(0,nvar,1)
        ## snglty = matrix(0,nvar,1)
        ## xx = array(0,c(nX,nX,nvar))
        ## for (iv in 1:nvar){
        ##     linout = linreg(iv,lmdseries,X,ya0)
        ##     B[,iv] = linout[[2]]
        ##     u[,iv] = linout[[3]]
        ##     logdetxxi[iv] = linout[[4]]
        ##     snglty = linout[[5]]
        ##     xx[,,iv] = linout[[6]]
        ## }
        
    } else {
#### Instead of svd below, could invoke lsfit.  Faster?
        vldvr = svd(X)
        dfx = sum(vldvr$d > 100*.Machine$double.eps)
        di = 1./vldvr$d[1:dfx]
        vldvr$u = vldvr$u[, 1:dfx]
        vldvr$v = vldvr$v[, 1:dfx]
        snglty = dim(X)[2] - dfx
        logdetxxi = 2 * sum(log(abs(di)))
####B = vldvr$v %*% diag(di,nrow=length(di)) %*% t(vldvr$u) %*% y (line below is just more efficient)
        B = vldvr$v %*% (di * (t(vldvr$u) %*% y))
        u =  y-X %*% B
        xxi =  di * t(vldvr$v)
        xxi =  crossprod(xxi)
        uraw = NULL       ## so it won't be missing in the list of outputs
    }
    if (!is.null(tsp(ydata))) u = ts(u, start=start(ydata)+c(0,lags),freq=frequency(ydata))
#### dates at end of sample are for dummy obs, meaningless.  If there are other
#### nontrivial breaks, the dates for u are also meaningless.
#### dim(B) =  c(nvar*lags+nx,nvar) ## rhs variables, equations (this was redundant)
    By =  B[1:(nvar*lags),]
    dim(By) =  c(nvar,lags,nvar)       ## variables, lags, equations
    By =  aperm(By,c(3,1,2)) ##equations, variables, lags to match impulsdt.m
#### label all the output, if the data matrices had labels
    if(!is.null(dimnames(ydata)[2]))
    {
        ynames = dimnames(ydata)[[2]]
    }else
    {
        ynames = rep("",times=nvar)
    }
    if(!nox)
    {
        if(!is.null(dimnames(xdata)[[2]]))
        {
            xnames = dimnames(xdata)[[2]]
        } else {
            xnames = rep(" ",times=nx)
        }
    }
    dimnames(By) = list(ynames,ynames,as.character(1:lags))
    ##xxinames = c(paste(rep(ynames,lags),rep(1:lags, each=length(ynames)),sep=""),xnames)
    ##dimnames(xxi) = list(xxinames,xxinames)
    if (nox)
        Bx =  NULL
    else
    {
        Bx =  matrix(B[nvar*lags+(1:nx),],dim(B)[2],nx)
        dimnames(Bx) = list(ynames,xnames)
    }
###### logintlh =  matrictint(u'*u,xxi,size(X,1)-nvar-1)-.5*nvar*(nvar+1)*log(2*pi);
###### Might want to create a version without the dimnames if using this in a program.
####------------
#### returns some things that are not available with sigpar=NULL.  Either split to
#### separate programs, or create alternate return lists.
    if(!is.null(sigpar)) {
        return(list(By=By, Bx=Bx, u=u, uraw=uraw, xx = xx, snglty=snglty, logdetxxi=logdetxxi,
                    lmdseries=lmdseries,
                    Bdraw = Bdraw, udraw = udraw,
                    freqs = freqs,
                    call=match.call())) ##var.logintlh =  logintlh
    } else {
        return(list(By=By, Bx=Bx, u=u, xxi= xxi, snglty=snglty, logdetxxi=logdetxxi, call=match.call()))
    }
}
