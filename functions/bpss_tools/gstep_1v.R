gstep_1v = function(gout, olsfun, ydata, xdata,
    tparam, tscale, dscore, lmdparam = NULL,Sigma = NULL){

    ## Gibbs sampling step for normal mixture single equation models
    ## see gdraw_1.R for more details
    
    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################

    if (is.null(lmdparam)){
        ## OLS step
        olsout = olsfun(ydata,xdata,gout$deltaout)
    } else{ ## mcmc step
        lh0 = olsfun(gout$xout,ydata,xdata,gout$deltaout,lmdparam)
        olsout = GsnMove(olsfun,gout$xout,lh0,Sigma,
            gout$deltaout,verbose = TRUE)
    }
    
    uout = olsout$u * exp(gout$deltaout) ## multiply by last time's deltaout

    if (is.null(tparam)){
        deltaout = drawdelta(u = uout,alpha = alpha, k = k)
    } else { ## draw inverse gamma
        deltaout = drawt(u = uout, alpha = tparam, beta = (tscale^2) * tparam)
    }

    if (dscore){ ## reduce delta to a f(delta) for mdd calculation
        dsout = fd(deltaout, tparam, tscale = tscale)
    } else {
        dsout = NULL
    }

    return(list(xout = olsout$x, deltaout = deltaout,
                tout = tout, lhout = olsout$lh, dsout = dsout))

}
