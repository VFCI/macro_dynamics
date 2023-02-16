get_forecast = function(tout,
    nperiods = 8,
    T = dim(ydata)[1]){

    ## gets a forecast nperiods into the future
    ## starts at cap T

    ## input to this function is full output of optimization

########################################
    
    ydata = as.matrix(tout$listData$Y)

    ## get the reduced form coefficients
    A = tout$A


    Ai = solve(A)

    vout = tout$vout

    By = vout$var$By
    nv = dim(By)[1]
    nlags = dim(By)[3]

    Bx = Ai %*% matrix(vout$var$Bx,nv,1)

    for (ilag in 1:nlags){
        By[,,ilag] = Ai %*% By[,,ilag]
    }

    ## get the system matrix
    sys = sysmat(By = By, Bx = Bx)
    ## get the data vector

    ## T = dim(ydata)[1]
    y = matrix(0,nv,nlags)

    for (ilag in 1:nlags){
        y[,ilag] = ydata[T - ilag + 1,]
    }

    y = c(y,1) # constant at the end

    ## calculate the predictions
    pmat = matrix(0,nperiods,nv)

    for (iperiod in 1:nperiods){
        y = sys %*% y
        pmat[iperiod,] = y[1:nv]
    }

    return(pmat)
}
