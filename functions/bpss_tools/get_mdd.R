get_mdd = function(x, lh, trunc = .95, delta = NULL,
    alpha =3, beta = 3, efac = NULL,
    covsub = FALSE){


    ## Small wrapper function for calculated mdd with MHM methods
    ## automatically puts gaussian distribution on x truncated for trunc density
    ## can put extra density in efac (e.g., on the delta_it)
    
    ## x is the draws in a matrix
    ## lh is a vector of -log posterior densities (conditional on certain params)
    ## efac is extra piece numerator density for MHM
    ## covsub determines whether to calc covariance matrix with all xout, or 5000 evenly spaced draws
    ## of it (mainly to save time)
    
    ## trunc is the truncation of the Gaussian (what percentage is kept) for A0 and Lambda
    ## delta is the delta_it in an array, if appropriate
    ## alpha and beta are for the distribution of the delta (maybe you should change this)

    ## END PREAMBLE
######################################################################################

    

    
    ## gets estimates of the MDD for the model

    ## first, get a Gaussian approx
    xmean = apply(x,2,mean)
    T = dim(x)[1]
    nv = dim(x)[2]
    ## covmat = crossprod(x) / T

    if (covsub){
        ## use subsample for the covariance matrix
        xsub = seq(from=1,to=T,length.out = 5000)
        covmat = cov(x[xsub,])
    } else {
        covmat = cov(x)
    }

    ## If covariance matrix is near singular, add noise
    if (min(eigen(covmat)$values) < 1e-8){
        covmat = covmat + 1e-3 * diag(dim(covmat)[1])
    }
    

    ## get det
    cdet = sum(log(eigen(covmat)$values))
    
    ## ## evaluate gaussian approx
    ci = (chol(solve(covmat)))
    xdm = t(x) - xmean
    cixdm = ci %*% xdm
    qscore = apply(cixdm^2,2,sum) ## quadratic part
    
    ## qscore = diag(t(xdm) %*% solve(covmat) %*% (xdm))

    ## qscore = rep(0,dim(xdm)[1])
    ## for (i in 1:dim(xdm)[1]){
    ##     qscore[i] = (xdm[i,]) %*% covmat %*% t(xdm[1,])
    ## }


    dout = -.5 * qscore - .5 * (nv * log(2 *pi) + cdet) ## density

    kval = qscore < qchisq(trunc,nv) ## truncate the tails of the gaussian

    if (!is.null(delta)){ ## put some distributio on the delta
        
        ## The conditional posterior is inverse gamma for the t case,
        ## and a known multinomial for the normal mix case
        ## so we should probably just use those distributions independent for each
        ## not what's implemented here

        
        dmat = matrix(delta, dim(delta)[1] * dim(delta)[2], dim(delta)[3])

        ## ## trial 1: ivnerse gamma
        ## alpha = 3
        ## beta = 3 ## inverse gamma parameters
        ## dweight = -(alpha-1) * (2 * dmat) - beta / exp(2 * dmat)
        ## dweight = dweight + alpha * log(beta) - lgamma(alpha)

        ## ## trial 2: regular gamma
        dweight = log(dgamma(exp(2 * dmat), shape = alpha, rate = 1/alpha))
        dweight = apply(dweight,2,sum)
        dout = dout + dweight
    }

    gdout = dout + lh - log(trunc) ## before truncation

    gdmean = mean(gdout)

    gdfix = gdout
    gdfix[!kval] = -1e10 ## zero values

    ##return(-(gdmean + log(mean(exp(gdfix-gdmean)))))
    if (is.null(efac)){
        return(-(gdmean + log(mean(exp(gdfix-gdmean)))))
    } else {
        m1 = -(gdmean + log(mean(exp(gdfix-gdmean))))

        ## with the additonal correction
        gdout = dout + efac + lh - log(trunc)
        

        gdmean = mean(gdout)
        gdfix = gdout
        gdfix[!kval] = -1e10 ## zero values
        m2 = -(gdmean + log(mean(exp(gdfix-gdmean))))

        outlist = list(mout = c(m1,m2), cv = covmat)
        return(outlist)
    }
}

