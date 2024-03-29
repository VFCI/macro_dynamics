bvarwrap5 = function(x, verbose=FALSE, listData = NULL, nLags = 2,
                      lcA0 = NULL, lcLmd = NULL, lmdblock = NULL,
                      Tsigbrk = NULL, pparams = NULL, nVarcores = 1, dprior = TRUE,
                      hparam_nl = rep(0,5), nlt = nlt, hparam = rep(0,7),
                      lmdmean = FALSE,lmdPrior = NULL,
                      oweights = NULL, drawbe = FALSE) {

    ## Main function for calculating log posterior used in Metropolis step

    ## Implements a Dirichlet prior on the variances, and assumes A0 diagonal is estimated

    ## -------------------- INPUTS --------------------
    ## x: vector with all elements of A0, Lambda
    ## verbose: flag of whether to report more than the negative LLH. FALSE is useful
    ## for (some) Metropolis Hastings
    ## listData: data, in a specific format generated by other functions (see TvvDir and example scripts)
    ## nLags: number of lags
    ## lcA0: matrix of TRUE and FALSE indicating which elements of A0 to fill.
    ##       "unfilled" parts of matrix are diagonal
    ## lcLmd: the full "Lmd" matrix is nshock x nregime. lcLmd determines what parts
    ##        to fill with estimated params (in a more complicated way,
    ##        based on next arg. lmdblock, program will fill in the unspecified lambdas).
    ##        The last regime is always filled out by normalization
    ##        as nRegimes - sum(variances in previous regimes), so the arithmetic average is always
    ##        1. This is in lieu of any normalization of A0 and consistent with
    ##        a Dirichlet prior on the variances
    ## lmdblock: specifies exactly how to restrict some variances not to be
    ##        time-varying. Not used in the most recent calculations
    ## Tsigbrk: vector of indices (i.e., integers) specifying the start and end
    ##        points of variance regimes. To avoid an error in counting, easiest to let
    ##        other programs populate this (you can input break points as a date string in
    ##        all the higher level programs)
    ## pparams: struct that contains the VAR prior parameters, which are set in higher level functions.
    ## nVarcores: deprecated option for using multicore processing in calculating the
    ##           equation by equation OLS for the VAR
    ## dprior: if TRUE, use a dirichlet prior. Currently the implementation
    ##         is not very elegant, so you have to go into the code to change the parameters

    ## nlt: these are fixed parameters for the non-linear transformation. see associated functions for more info
    ## hparam_nl: these indicate (with 0 or 1) which paramters of the non-linear
    ##            transformation are being looped over (and hence added to the end of x)
    ## hparam: these indicate which prior parameters, numbered 1 to 7,
    ##         are being looped over (and included in x); i.e., being treated as heirarchical
    ##         currently: cannot loop over these and the non-linear transformation parameters

    ## lmdmean and lmdPrior: not used anymore

    ## oweights: weights on each observation to pass to likelihood
    ##          evalulation step. These are for the normal mixture models (i.e,. the "variance shocks" for each
    ##           observation)
    ## drawbe: if yes, draw the reduced form coefficients and residuals (necessary for normal mixture models)

    ## -------------------- OUTPUT --------------------
    ## if not verbose: just lh, which is the negative log posterior
    ## if verbose: lh plus a bunch of other stuff from the VAR calculation. You want this
    ##             for the normal mixture model (because it has the residuals and A_i coefficients)
    
    ## END PREAMBLE

##################################################################################################################################################


#######
     ## Preparing and formatting data
#######

    
    ## Things that don't change, like nVar and T, /should/ be bound outside
    ## the iteration, but performance gains are probably small
    
    nVar = dim(listData$Y)[2]
    T = dim(listData$Y)[1]
    A = matrix(0, nVar, nVar)
    varnames = dimnames(listData$Y)[[2]]	
    
######
    ## Preparing A
######
    
    diag(A) = 1
    nA = sum(lcA0) ##number of non-fixed parameters
    A[lcA0] = x[1:nA]	
    dimnames(A) = list(varnames, varnames)
    nv = dim(A)[1]

######
    ## Prior on A
######
    
    ## Gaussian with mean 100, variance 200
    ## Prior scaling: correct if all nVar^2 parameters are non-zero
    allh = -.5 * sum((A - diag(nVar) * 100)^2 / 4e4) -
        nVar^2 * (log (2 * pi) / 2 + log(200))

######
    ## Preparing lmd
######
    
    nLmd = sum(lcLmd)
    nSig = length(Tsigbrk)
    if (nLmd > 0){
        ## Fill in lambda
        lmd = matrix(0, nVar, nSig)
        lmd[lcLmd] = x[nA + (1 : nLmd)]


        ## This is a streamlined way of setting certain lambda not to change between periods
        if (!is.null(lmdblock)) {
            nBlocks = dim(lmdblock)[3]
            for (iBlock in 1:nBlocks){
                lmd[lmdblock[,,iBlock]] = (lmd[lmdblock[,,iBlock]])[1]
                ##first of block was filled in
            }
        }

        ## last period's variances are fixed, so (arithmetic) average is 1
        ## ## lmd[,nSig] = -log(nSig - apply(exp(-lmd[,1:(nSig-1)]),1,sum))
        lmd[,nSig] = nSig - apply(lmd[,1:(nSig-1),drop=FALSE],1,sum)


        ## Calculate prior for lmd
        ## Dirichlet(2) for each equation
        lpL = apply(log(lmd) - log(nSig),2,sum) - lgamma(2 * nSig)
        lplmd = sum(lpL) - (nSig-1) * log(nSig)

    } else {
        ## lmd = 1; no prior
        lmd = matrix(1, nVar, nSig)
        lplmd = 0
    }

    
    ## possible hyperparameters for the non-linear transformation
    lastvalue = nA + nLmd ## for picking out extra params
    
    if (any(hparam_nl > 0)) { # Enter loop if any hyper parameters provided

        ## input is A, C, beta, index of param
        ## never put a prior on the last one! that would be silly
        ## dont change the 5th either, because its applied outside the function
        
        ## nlt[hparam_nl > 0] = x[-(1:(nA + nLmd))]
        nlt[hparam_nl > 0] = x[lastvalue + 1:(sum(hparam_nl > 0))]
        lastvalue = lastvalue + sum(hparam_nl > 0)

        nlflag = TRUE

        nlout = applynl(listData$Y, nlt) ## apply nl transform

        listData$Y = nlout$data ## new data
        jterm = nlout$jterm

        pterm = sum(hparam_nl * nlt) ## neg exponential
        
    } else {
        nlflag = FALSE ## no nonlinear transformation
        jterm = 0
        pterm = 0
    }

    if (any(hparam > 0)){ ## hyperparameters
        
        ## (MN tight, MN decay, UR lambda, UR mu,
        ## Cos tight, cos smooth, cos damp)


        ## asume there is one of mn or cos prior
        if (!is.null(pparams$cosprior)){
            pv = c(0,
                   0,
                   pparams$urprior$lambda,
                   pparams$urprior$mu,
                   pparams$cosprior$tight,
                   pparams$cosprior$smooth,
                   pparams$cosprior$damp)

            pv[(hparam > 0)] = x[lastvalue + 1:(sum(hparam > 0))]
            ## pparams$mnprior$tight = pv[1]
            ## pparams$mnprior$decay = pv[2]
            pparams$urprior$lambda = pv[3]
            pparams$urprior$mu = pv[4]
            pparams$cosprior$tight = pv[5]
            pparams$cosprior$smooth = pv[6]
            pparams$cosprior$damp = pv[7]

        } else { ## asume mn prior

            pv = c(pparams$mnprior$tight,
                   pparams$mnprior$decay,
                   pparams$urprior$lambda,
                   pparams$urprior$mu,
                   0,
                   0,
                   0)

            pv[(hparam > 0)] = x[lastvalue + 1:(sum(hparam > 0))]
            pparams$mnprior$tight = pv[1]
            pparams$mnprior$decay = pv[2]
            pparams$urprior$lambda = pv[3]
            pparams$urprior$mu = pv[4]
            ## pparams$cosprior$tight = pv[5]
            ## pparams$cosprior$smooth = pv[6]
            ## pparams$cosprior$damp = pv[7]

        }
               

          

        prior_hp = sum(hparam * pv) ## neg exponential density
        badflag = any(pv < 0)
    } else {
        prior_hp = 0
        badflag = FALSE
    }

        
        
######################################
    ## Estimating the model
######################################

    if (is.null(pparams$mnstart)) pparams$mnstart = 1

    if (any(lmd < 0)){
        ## trash this draw, variances negative

        lh = 1e5
        vout = NULL

        if (!verbose){
            return(lh) #### dont need anything else
        } else {
            return(list(lh = lh))
        }
        

    } else if(nlflag && any(nlt < 0)){
        ## none of the nonlinear transformation parameters can be negative
        lh = 1e5
        vout = NULL
        ## return(lh)

        if (!verbose){
            return(lh) #### dont need anything else
        } else {
            return(list(lh = lh))
        }
        

    } else if(badflag){
        ## hyperparameter cannot be negative
        lh = 1e5
        vout = NULL
        ## return(lh)

        if (!verbose){
            return(lh) #### dont need anything else
        } else {
            return(list(lh = lh))
        }
        
    } else { #### proceed normally
        
        vout = SVARhtskdmdd(listData$Y, lags = nLags, xdata = NULL, const = TRUE, A0 = A,
                            lmd = -log(lmd), Tsigbrk = Tsigbrk,
                            urprior = pparams$urprior, mnprior = pparams$mnprior,
                            vprior = pparams$vprior,
                            cosprior = pparams$cosprior,
                            mnstart = pparams$mnstart, train = 0, cores = nVarcores,
                            lmdmean = FALSE, oweights = oweights, drawbe = drawbe)
        
        lh = -sum(vout$w)
        
        ## Prior that selects against large eigenvalues
        ##currently not implemented
        ev = 0
        ##might look something like:
        ## ev <- eigen(sysmat(vout$By))$values
        ## ev <- sum(abs(ev[abs(ev) > 1] - 1))^2*1e3 ##(so one root of 1.03 penalized by .9 (weak)
        ##but it could mess up MCMC logic

        ## print(lplmd)
        ##print(min(lmd))
        
        lh = lh + ev - lplmd - allh ##marginal posterior | lmd, A
        lh = lh - jterm + pterm ## subtract the jacobian, because negative llh
        lh = lh + prior_hp ## prior on prior paraemters

    }
    
    if(verbose) {
        ## grab standardized residuals
        ustd <- vout$var$u
        ulevel <- vout$var$uraw ## this is always null in the rfvar3 output with non-null sigpar

        return(list(lh=lh, vout=vout, A=A, lambda = lmd, llmd = -log(lmd), u=ulevel,
                    ustd=ustd, asig = pparams$asig, x = x, lplmd = lplmd, allh = allh)) ##llmd included because exp(-lmd) might lose precision.
        
    } else {
        return(lh)
    }
    
}


