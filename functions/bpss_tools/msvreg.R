msvreg = function(dmat,
                  hd = 3,
                  hi = 3,
                  rel = TRUE,
                  ratio = FALSE,
                  nlag = 0,
                  verbose = FALSE,
                  ngdp = NULL){

    ## Runs single equation OLS inspired by Mian, Sufi, and Verner [and other
    ## papers in the "projection regression" literature]
    ## y_{t + hd} - y_{t} = lhs variable
    ## (c/y)_{t-1} - (c/y)_{t-1-hi} = main rhs variable
    ## for each ilag, add y_{t - ilag} - y_{t - ilag - hd}

    ## -------------------- INPUTS --------------------
    ## dmat : matrix of dependent variables. It's assumed that
    ##        column 1 is REAL output
    ##        columns 2 to N are the credit variables
    ## ngdp: specify this as a T x 1 vector of nominal gdp, or log real + log price level
    ## hd and hi: size of difference for dependent and independent variables respectively
    ## rel : if TRUE, take independent variable relative to GDP
    ## ratio: if TRUE, take ind. variable as ratio to GDP
    ## nlag: number of lags of real GDP to add to right hand side
    ## verbose : if true, return (fake) data

    ## ----------------------------------------

    T = dim(dmat)[1]
    
    ## preparing the dep. var
    depvar = matrix(NA,T,1)
    depvar[1:(T-hd)] = dmat[(hd+1):T,1] - dmat[1:(T-hd),1]
    

    
    ## preparing the independent variables
    ncredit = dim(dmat)[2] - 1
    indvar = matrix(NA,T,ncredit)
    
    creditvar = dmat[,-1,drop=FALSE] ## this works no matter how many there are

    if (is.null(ngdp)){
        ## scale with real
        scalegdp = dmat[,1]
    } else {
        scalegdp = ngdp
    }
    
    if (rel){
        ## relative to GDP
        ## log ratio is one option
        creditvar = creditvar - scalegdp

        if (ratio){
            ## but can also use the absolute ratio
            creditvar = exp(creditvar)
        }
        
    }

    indvar[(hi+2):T,] =
        as.matrix(creditvar[(hi+1):(T-1),] - ## t-1
        creditvar[1:(T-1-hi),]) ## t-1-hi
    sd = c(sd(indvar[,1],na.rm=TRUE),sd(indvar[,2],na.rm=TRUE))
    
    ## adding lags if desired
    if (nlag > 0){
        lagmat = matrix(NA,T,nlag)
        diffY = c(0,diff(c(dmat[,1])))
        for (ilag in 1:nlag){
            ## lagmat[(hd +ilag):T,ilag]  = depvar[1:(T-hd-ilag)]
            lagmat[(1 +ilag):T,ilag]  = diffY[1:(T-ilag)]
        }
        indvar = cbind(indvar,lagmat)
    }

    ## running the regression
    regout = lm(depvar ~ indvar)
    regout$sd = sd
    

    if (!verbose){
        return(regout)
    } else {
        return(list(regout = regout, depvar = depvar, indvar = indvar))
    }
}

        

        

        
