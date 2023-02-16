rfrun = function(ydata,
    xdata = NULL,
    const = TRUE,
    lags = 8,
    nstep = 16,
    mnprior = list(tight = 3, decay = .5),
    urprior = list(lambda = 5, mu = 1),
    vprior = list(sigma = rep(.01,nv), w = 1)){

    ## runs reduced form regressions
    ## this specifies prior "externally" instead of in rfvar3

    ## -------------------- INPUTS --------------------
    ## ydata = main data for VAR
    ## xdata = any extra data for x side
    ## const = if TRUE, add constant to xdata
    ## lags = number of lags in VAR
    ## nstep = number of steps of IR to calculate
    ## mnprior, urprior, vprior: parameters for var prior

    ## ----------------------------------------

    ## so R doesnt complain later
    vnames = colnames(ydata)
    ydata = as.matrix(ydata)
    nv = dim(ydata)[2]
    
    ## for the unit root prior
    ybar = apply(ydata[1:lags, ], 2, mean)
    
    T = dim(ydata)[1]
    nv = dim(ydata)[2]
    if (const) {
        xdata = cbind(xdata, matrix(1,T,1))
    }
    if (!is.null(xdata) ) stopifnot( dim(xdata)[1] == T)
    Tx = dim(xdata)[1]
    nx = dim(xdata)[2]

        
    ## prior
    vp = varprior(nv,nx,lags,mnprior,vprior, urprior=urprior, ybar=ybar) # vp$: ydum,xdum,pbreaks
    varp = rfvar3(ydata = vp$ydum,
        lags = lags,
        xdata = vp$xdum,
        lambda = NULL, mu = NULL, ic = NULL, const = FALSE)

    ## posterior mode of standard model
    var = rfvar3(ydata = rbind(ydata, vp$ydum),
        lags = lags,
        xdata = rbind(xdata, vp$xdum),
        lambda = NULL, mu = NULL, ic = NULL, const = FALSE,
        breaks = matrix(c(dim(ydata)[1], dim(ydata)[1]+ vp$pbreaks),ncol=1))

    ## p. mode of model with "flat prior"
    vnp = rfvar3(ydata = rbind(ydata),
        lags = lags,
        xdata = rbind(xdata),
        lambda = NULL, mu = NULL, ic = NULL, const = FALSE)

    ## p. mode of model with only unit root prior
    vp2 = rfvar3(ydata = rbind(ydata),
                 lags = lags,
                 xdata = rbind(xdata),
                 lambda = 5, mu = 1, ic = NULL, const = FALSE)


    ## impulse responses
    irmn = impulsdtrf(vout = var, nstep = nstep) ## standard model
    irout = impulsdtrf(vout = vp2, nstep = nstep) ## with just unit root prior
    irnp = impulsdtrf(vout = vnp, nstep = nstep) ## no prior IR
    

    ## marginal likelihood (code is copied from a different program)
    Tu = dim(var$u)[1]
    Tup = dim(varp$u)[1]
    flat = FALSE
    w = matrictint(crossprod(var$u),var$xxi,Tu-flat*(nv+1))-flat*.5*nv*(nv+1)*log(2*pi);
    wp = matrictint(crossprod(varp$u),varp$xxi,Tup-flat*(nv+1)/2)-flat*.5*nv*(nv+1)*log(2*pi)
    w=w-wp
    
    return(list(w = w, vp2 = vp2, var = var, varp = varp, vnp = vnp, irmn = irmn,
                irout = irnp, irp = irout, vnames = vnames))
}

