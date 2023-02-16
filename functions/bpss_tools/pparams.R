pparams <- function(listData, mnstart = 1, mntight = 3, mndecay = 0.5, vprior = 0,
                    urlambda = 5, urmu = 1, adiag = 1, cosprior = NULL) {

    ## Generates a list with all the priors/stuff for bvarWrap4
    ## Intended to keep some of the repetitive stuff outside of iteration/make it easier to tweak

    nVar = dim(listData$Y)[2]
    varnames = dimnames(listData$Y)[[2]]

    if (!is.null(mntight)){ #### if mn prior is specified
        mnprior = list(tight = mntight, decay = mndecay)
    } else {
        mnprior = NULL #### no mnprior
    }

    ## cosprior just passes through
    ## specify one or the other, probably not both
        

    ##these options are grandfathered in, from a time before we thought it was necessary
    ##to tweak vprior. Else vprior is just specified as a vector
    ##Could raise an error if length vprior != nVars
    if (length(vprior) == 1 && vprior == 0){
        vprior = list(sig = rep(.01,nVar), w = 0)
    } else if (length(vprior) == 1 && vprior == 1){
        ##quick fix: vprior needs to recognize that some variables are percentage points
        vprior = list(sig = c(.01,.01,.01,1,.01,1,1,1), w = 0)
        ## three log, one pct pt rate, one log, 3 pct pt rate
    } else { ##if full vprior is provided
        vprior = list(sig = vprior, w = 0)
    }

    asig <- 2
    asd <- outer(vprior$sig, 1/vprior$sig)
    
    urprior <- list(lambda = urlambda, mu = urmu)

    sigfix <- diag(vprior$sig^2)

    names(vprior$sig) = varnames
    dimnames(sigfix) = list(varnames, varnames)

    return(list(urprior = urprior, asig = asig, mnprior = mnprior, vprior = vprior, asd = asd, mnstart = mnstart, adiag = adiag, cosprior = cosprior))

}
