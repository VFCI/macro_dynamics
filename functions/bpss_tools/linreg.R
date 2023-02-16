linreg = function(iq, lmdseries, X, ya0, drawbe = FALSE){

    errorflag = FALSE
    
    wt <- exp(.5 * lmdseries[iq, ])
    ## weighting by exp(lmd/2), so log error variances are -lmd
    Xq <-  wt * X
    yq <- wt * ya0[ , iq]
    
    ## set up exception for weights that blow up lsfit: so 
    ## minimization can proceed, knowing such models are not very likely...
    if (any(Xq == Inf) || any(yq == Inf) || any(wt < .Machine$double.eps) ||
	any(is.nan(Xq)) || any(is.na(Xq))){
        errorflag = TRUE
        return(errorflag)
    }
    
    
    lso <- lsfit(Xq, yq, intercept=FALSE)
    ## intercept already in X. resids should be unit vce.
    ## B[ , iq] <- lso$coefficients
    Rq <- qr.R(lso$qr)
    xx = crossprod(Rq)

    if (drawbe){ ## draw from the conditional posterior on the coefficients
        
        xxi = solve(xx)

        ## coefs_noise = mvrnorm(1,rep(0,length(lso$coefficients)),xxi)
        exxi = eigen(xxi)
        if (any(exxi$values < 1e-14)){ ## seems like reasonable tolerance
            coefs_draw = NULL
            udraw = NULL
            snglty = TRUE
            logdetxxi = -Inf
        } else {
            coefs_noise = exxi$vectors %*% diag(sqrt(exxi$values)) %*% rnorm(dim(xxi)[1])
            coefs_draw = lso$coefficients + coefs_noise
            udraw = lso$residuals - Xq %*% coefs_noise
            logdetxxi = sum(log(exxi$values))
            snglty = FALSE
        }
    } else { ## no coefs draw
        coefs_draw = rep(NA,length(lso$coefficients))
        udraw = rep(NA,length(lso$residuals))
        logdetxxi = -2 * sum(log(abs(diag(Rq))))
        snglty = (logdetxxi == -Inf)
    }
    
    ## set up exception for singular crossprod(Rq).
    ## not combined with above for debugging
    
    ## cp = crossprod(Rq)
    ## if (kappa(cp) > 1/(1000*.Machine$double.eps)){ #checking condition number
    ## 	errorflag = TRUE
    ## 	return(errorflag)
    ## }
    ## xxi[ , , iq] = solve(cp)
    
    ## u[ , iq] <- lso$residuals
    ## logdetxxi[iq] <- -2 * sum(log(abs(diag(Rq))))
    ## snglty[iq] <- (logdetxxi[iq] == -Inf)
    
    
    return(list(errorflag = errorflag, coefs = lso$coefficients, u = lso$residuals, 
                logdetxxi = logdetxxi, snglty = snglty, xx = xx,
                coefs_draw = coefs_draw, udraw = udraw))
}
