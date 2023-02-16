McmcIr = function(xout, 
                  model, 
                  rootcheck = FALSE, 
                  lrange = 1:n_reg, 
                  cores = 8,
                  oweights = NULL,
                  aplus = NULL, ## don't need both oweights and aplus
                  nStep = 20, 
                  hparam = rep(0,7))
{

    ## Main wrapper function for drawing impulse responses

    ## --------------------INPUTS, for all uses--------------------
    ## xout : A0 and Lambda draws in matrix form
    ## rootcheck : if TRUE, will check if roots are stable
    ## lrange : which variance regimes you want IR for. The IR are all the same up to scale
    ## cores: number of cores to use for mclapply. This is easily parallelized, so use many!
    ## oweights: draws of delta_it
    ## aplus : draws of A+. Code will run a lot faster if you specify this
    ## nStep : number of steps of impulse responses



    ## --------------------OUTPUTS--------------------
    ## list with element ir, the impulse responses [nvar x nshock x nperiod x nregime x ndraws]
    ## other stuff which is less useful (mainly related to orderings/permutations)
    
    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## January 2017

    ## END PREAMBLE
######################################################################################


    library(parallel)
    library(MASS)
    library(coda)

    vars = colnames(model$A)

    nTrials = dim(xout)[1]

    nX = dim(xout)[1]
    if (is.null(nX)) {
        nX = 1
        dim(xout) = c(1,length(xout))
    }

    if (!is.null(oweights)){ ## add these to x
        ## xout = cbind(xout, matrix(0,dim(xout)[1],
        ##                           dim(oweights)[1] * dim(oweights)[2]))
        ## xout[,-c(1:dim(xout)[2])] = matrix(oweights,dim(xout)[1],
        ##                                    dim(oweights)[1] * dim(oweights)[2],
        ##                                    byrow = TRUE)

        owmat = matrix(oweights,dim(xout)[1],
                                  dim(oweights)[1] * dim(oweights)[2],
                                  byrow = TRUE)
        xout = cbind(xout, owmat)

        owflag = TRUE
    } else {
        owflag = FALSE
    }

    if (!is.null(aplus)){
        na = dim(aplus)[1] ## need to remove last row
        amat = matrix(aplus[-na,,],
                      dim(aplus)[3],
                      (na-1) * dim(aplus)[2],
                      byrow = TRUE)
        xout = cbind(xout, amat)
        aflag = TRUE
    } else{
        aflag = FALSE
    }
        

    ## if (!is.null(aplus)){ ## add these to x
    ##     xout = cbind(xout, matrix(0,dim(xout)[1],
    ##                               dim(oweights)[1] * dim(oweights)[2]))
    ##     xout[,-c(1:dim(xout)[2])] = matrix(oweights,dim(xout)[1],
    ##                                        dim(oweights)[1] * dim(oweights)[2],
    ##                                        byrow = TRUE)
    ## }

    
    listXout = lapply(1:nX, function(iRow){xout[iRow,]})

    listIrtrials = mclapply(listXout, IrRun2, model$A,
                            model$lmd, model$listData, model$lcA,
                            model$lcLmd, nLags = model$nLags,
                            owflag = owflag, aflag = aflag,
                            lmdblock = model$lmdblock,
                            hparam = hparam,
                            Tsigbrk = model$Tsigbrk, pparams = model$pparams,
                            lmdPrior = model$lmdPrior, rootcheck = rootcheck,
                            lrange = lrange, nStep = nStep, mc.cores = cores)

    ##save(listIrtrials, file = 'rawIR.Rdata')

    ##irdim = dim(listIrtrials[[1]]$ir)

    ##arrayIrtrials = array(sapply(listIrtrials, '[[', 1), dim = c(irdim, nTrials))

    ##percentiles = apply(arrayIrtrials, 1:4, FUN = quantile, probs = c(.05,.5,.95))

    elements = c('ir','ch','orderings','badshift','trimprove','x','lh')
    if (rootcheck) elements = c(elements, 'isgood','maxmod', 'maxRev', 'iscomplex','modmod')
    output = list()
    for (iElement in elements){
        iObject = sapply(listIrtrials,'[[',iElement)
        if (is.null(dim(iObject))) iObject = unlist(iObject)
        output[[iElement]] = iObject
    }

    output$ir = array(output$ir, dim = c(length(vars), length(vars), 20, length(lrange), nTrials))

    if (class(xout)[1] == 'mcmc'){
        output$x = mcmc(t(output$x), thin = thin(xout))
    }
    output$ess = effectiveSize(output$x)
    if (rootcheck) output$iscomplex = t(output$iscomplex)

    return(output)
}
