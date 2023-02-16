gstep = function(gout, lhfcn, Sigma, lcA0, lcLmd,
                 alpha, k,
                 tparam = NULL,
                 tscale = 1,
                 dscore = FALSE, ## if TRUE, calculate the IG density of delta_it
                 ...){

    ## The main iterative step of our Gibbs sampler
    ## Note that most model-related parameters are passed in the ellipses! So see
    ## gdraw.R for more info

    ## --------------------INPUTS--------------------
    ## gout : a list which contains 
    ##        xout, draw of A0 and Lambda
    ##        deltaout, draw of delta_it
    ##        trans, 1 iff metropolis step moved
    ##        lhout, negative log posterior
    ##        dsout, ig density evaluated at delta
    ##        eout, structural resids
    ##        aout, A+
    ## tparam, tscale : specify these for t errors model. tparam is df/2 (which is the first param
    ##                  for the inverse gamma distribution). tscale is the scale of the t
    ##                  which is not identified by the model and easiest to leave as unit
    ## dscore : save the inverse gamma density evaluated at the variance shocks, useful for MDD calculations

    ## --------------------OUTPUT--------------------
    ## see gout input
    
    
    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################


    ## markov step
    x = gout$xout
    
    model = lhfcn(x,lcA0,lcLmd, oweights = gout$deltaout,...,verbose = TRUE)
    almdout = GsnMove(lhfcn, x, model$lh, Sigma,
                      modeA = NULL,
                      modeLmd = NULL,
                      lcA0,
                      lcLmd,
                      model = model,
                      oweights = gout$deltaout,
                      verbose = TRUE, ...)


    ## drawing the new variance weightso
    if (almdout$lh > 1e4){ ## bad draw
        ## this option should never come into play --- it would only happen if both the initial
        ## AND proposed draws were bad for some reason. It is probably possible if the variance weight
        ## from the inverse gamma is large enough to give the linear regression numerical problems, in
        ## which case you will get a singularity in the LH calculation. So this step imposes some kind of
        ## truncation on the prior for those (not completely clear what).
        return(gout)
    } else {

        ## next line does not account for the fact that we drew A+
        ## uout = almdout$u[1:dim(gout$deltaout)[1],] * exp(gout$deltaout) ## multiply by last time's deltaout

        aout = almdout$model$vout$var$Bdraw ## A+ draws
        eout = almdout$model$vout$var$udraw[1:dim(gout$deltaout)[1],] * exp(gout$deltaout) ## multiply by last time's deltaout to get "real" structural resids
        
        if (is.null(tparam)){
            deltaout = drawdelta(u = eout,alpha = alpha, k = k)
        } else { ## draw inverse gamma
            deltaout = drawt(u = eout, alpha = tparam, beta = (tscale^2) * tparam)
        }

        if (dscore){ ## reduce delta to a f(delta) for mdd calculation
            dsout = fd(deltaout, tparam)
        } else {
            dsout = NULL
        }
        
        return(list(xout = almdout$x, deltaout = deltaout,
                    trans = almdout$trans, lhout = almdout$lh, dsout = dsout,
                    eout = eout, aout = aout))
    }
}
