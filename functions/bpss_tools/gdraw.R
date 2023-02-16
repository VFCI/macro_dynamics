gdraw =  function(tvvModel, ndraw, lhfcn = bvarwrap5,
    nburn = 1e3, nsep = 1e2,
    filename = 'gibbsout.Rdata',
    hsnfactor = 0.2,
    alpha = .003, k = 4,
    tparam = NULL,
    tscale = 1,
    savespots = NULL,
    dscore = FALSE,
    drawdout = FALSE, ## if TRUE, keep the delta_it (the xi_it)
    drawe = FALSE, ## if TRUE, keep the e_it
    drawa = TRUE,  ## if TRUE, keep the A+ draws
    hparam_nl = rep(0,5),        # indicates if any hyperparams for nonlinear transformation are used
    nlt = rep(0,5)               # constant parameters for nlt
                  ){

    ## Main wrapper function for Gibbs Sampling from posterior of normal mixture models
    ## Note that this can also be used to sample from the Normal errors model
    ## (for instance, by setting alpha = 1 and k = 1)

    ## The real sampling happens in gstep.R

    ## --------------------INPUTS--------------------
    ## tvvModel : Output of optimization for normal errors model
    ##            <<IMPORTANT>> that this structure includes the data and some other model parameters!
    ##            So if you want to customize everything, please see TvvDir.R
    ## ndraw : Number of draws that will be //recorded//
    ## nburn : Number of initial draws discarded as burn-in. As discussed in the Appendix, etc.,
    ##         it is probably smarter to save everything and monitor convergence "manually"
    ## nsep : The separation between draws. The code is written to make nsep draws and then save
    ##        one draw, so the actual "separation" of draws is nsep-1
    ## filename : remember the extension
    ## hsnfactor : by default, the code looks at tvvModel$opt$H for a covariance matrix (inv. Hessian)
    ##             and scales it by hsnfactor (in //variance// not standard deviation units).
    ## alpha, k : specify these for the n-normal mixture model. alpha is a vector of probabilities.
    ##            k is a vector of standard deviations for each normal
    ## tparam, tscale : specify these for t errors model. tparam is df/2 (which is the first param
    ##                  for the inverse gamma distribution). tscale is the scale of the t
    ##                  which is not identified by the model and easiest to leave as unit
    ## savespots : a list of draws (between 1 asnd ndraw) at which to save. Code will generate some
    ##             automatically if left null. Note the code create a connection for output that
    ##             can be flushed at savespots to reduce memory burden
    ## dscore : save the inverse gamma density evaluated at the variance shocks, useful for MDD calculations
    ## drawdout : if TRUE, keep (save) the delta_it (called xi_it in the paper).
    ## drawe : if TRUE, keep the structural residuals
    ## drawa : if TRUE, keep the A+ (necessary for impulse responses, etc!)

    ## --------------------OUTPUT--------------------
    ## a list with elements:
    ## xout : the A0, Lambda draws
    ## lhout : the negative log posterior from metropolis step
    ## tout : 0 if there was no metropolis move, 1 if metropolis move
    ## dout : array with the delta_it (xi_it)
    ## dsout : matrix of log ig density evaluated at dout
    ## aout : A+ draws
    ## eout : structural residual draws

    
    
    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################


    hsn = tvvModel$opt$H 
    x0 = tvvModel$x

    lcA0 = tvvModel$lcA0
    lcLmd = tvvModel$lcLmd
    ## ndraw = nsep* ndraw + nburn
    nvar = dim(lcA0)[1]

    lcA0 = tvvModel$lcA0
    lcLmd = tvvModel$lcLmd

    Tsigbrk = tvvModel$Tsigbrk
    pparams = tvvModel$pparams
    listData = tvvModel$listData
    lmdblock = tvvModel$lmdblock
    nLags = tvvModel$nLags


    Sigma = hsnfactor * hsn

    ## decide where to save
    if (is.null(savespots)){
        nsaves = 5
        inc = ndraw %/% nsaves
        if (inc == 0){
            savespots = ndraw ## no need to save until end
        } else {
            savespots = c(seq(1, ndraw, inc), ndraw)
        }
    }


    ## allocate space for output
    xout = matrix(0,length(x0),ndraw)
    lhout = rep(0,ndraw)
    tout = rep(0,ndraw)

    ## always save delta, a, e in savespots
    if (drawdout){ ## delta_it
        dout = array(0,
                     c(dim(listData$Y)[1] - nLags,
                       dim(listData$Y)[2],
                       ndraw))
    }  else {
        dout = array(0,
                     c(dim(listData$Y)[1] - nLags,
                       dim(listData$Y)[2],
                       length(savespots)))
    }
    
    if (drawa){ ## A+
        aout = array(0,
                     c(nLags * nvar + 1,
                       nvar,
                       ndraw))
    }  else {
        aout = array(0,
                     c(nLags * nvar + 1,
                       nvar,
                       ndraw))
    }
    
    if (drawe){ ## epsilon_it
        eout = array(0,
                     c(dim(listData$Y)[1] - nLags,
                       dim(listData$Y)[2],
                       ndraw))
    }  else {
        eout = array(0,
                     c(dim(listData$Y)[1] - nLags,
                       dim(listData$Y)[2],
                       length(savespots)))
    }

    isave = 1 ## first save

    if (dscore){
        dsout = rep(0,ndraw)
    } else {
        dsout = NULL
    }
    
    output = NULL

    if (!is.null(tvvModel$dout)){
        dout[,,1] = tvvModel$dout
    }
    

    gout = list(xout = x0, deltaout = dout[,,1])
    

    ## Run the Gibbs Sampler
    if (nburn > 0){
        for (iburn in 1:nburn){
            gout = gstep(gout, lhfcn, Sigma, lcA0, lcLmd, alpha, k,
                         lmdblock = lmdblock, Tsigbrk = Tsigbrk,
                         pparams = pparams, listData = listData, nLags = nLags,
                         tparam = tparam, tscale = tscale, drawbe = TRUE,
                         hparam_nl = hparam_nl, nlt = nlt)
        }
    }
    for (idraw in 1:ndraw){
        for (isep in 1:nsep){
            gout = gstep(gout, lhfcn, Sigma, lcA0, lcLmd, alpha, k,
                         lmdblock = lmdblock, Tsigbrk = Tsigbrk,
                         pparams = pparams, listData = listData, nLags = nLags,
                         tparam = tparam, dscore = dscore, tscale = tscale, drawbe =TRUE,
                         hparam_nl = hparam_nl, nlt = nlt)
        }

        ## update
        xout[,idraw] = gout$xout
        lhout[idraw] = gout$lhout
        tout[idraw]  = gout$trans
        if(drawdout) {
            dout[,,idraw] = gout$deltaout
        }

        if(drawa) {
            aout[,,idraw] = gout$aout
        }

        if(drawe) {
            eout[,,idraw] = gout$eout
        }

        if (dscore){
            dsout[idraw] = gout$dsout
        }
        if(idraw %% 100 ==1) print(paste(filename, "draw number", idraw))
        if (idraw %in% savespots){
            dout[,,isave] = gout$deltaout
            isave = isave + 1
            output = list(xout = xout, lhout = lhout,
                          tout = tout, dout = dout,
                          dsout = dsout, aout = aout, eout = eout)
            outcon <- file(filename, open="wb")
            save(output, file = outcon)
            flush(outcon)
            close(outcon)
        }

    }

    if (is.null(output)){
        output = list(xout = xout, lhout = lhout,
                      tout = tout, dout = dout,
                      dsout = dsout, aout = aout, eout = eout)
    }
    outcon <- file(filename, open="wb")
    save(output, file = outcon)
    flush(outcon)
    close(outcon)
    return(output)
}

