TvvDir = function(dfData, freq = NULL,
                   vars = NULL,
                   logseries = NULL, nLags = NULL, lcA0 = NULL,
                   lmdblock = NULL, strTsigbrk = NULL,
                   startdate = NULL, enddate = NULL, timedummy = NULL,
                   verbose = FALSE, seedmodel = NULL, nStep = 20, extracheck = TRUE,
                   nGradcores = 1, nVarcores = 1, seedx = NULL, lmdPrior = FALSE, 
                   mnstart = 1, mntight = 3, mndecay = 0.5, 
                   vprior = 0, cosprior = NULL,
                   nlt = NULL,hparam_nl = rep(0,3), hparam = rep(0,7), critval = 1e-10,
                   seedH = NULL, tvA = FALSE, noLmd = FALSE)

{
    #### TI (9.17.2022) - Raised Minnesota decay parameter to 3 to account for 3 months per quarter
    ##### TI comment: Add the following (instead of a break point) within this function call: #browser()

    ## This version uses the Dirichlet prior / different scalings
    ## The posterior density integrated over A+ is calculated in bvarwrap5
    
    ## A front-end program for finding a maximum posterior density model with
    ## a single structural matrix (inverse(A0)) and time-varying variances for 
    ## structural innovations (exp(-lmd[,timePeriod])).

    ## Automatically 'cleans' but does not load the data. Need to run 'datacat' or something
    ## similar to grab data from different sources/spreadsheets and format appropriately

    ## lcA0 is a matrix with TRUE for a fitted value
    ## and FALSE for a non-fitted value. lcA0 defaults to lower triangular if left 
    ## null, forces ones on the diagonal, and zero to other FALSE fields. lmdblock is a
    ## nVar x nPeriods x nBlocks array that identifies, with TRUE, blocks of parameters
    ## that are constant (presumably across periods). The most obvious application is
    ## keeping certain parameters constant over periods, but it could also be used to
    ## have one value in 1/2 the periods, another in the other half, etc. If left null,
    ## all variances change in every period.

    ## Options can tweak other aspects of the model. Defaults are reasonable (but likely no optimal)

    ## Prior parameters / options can be tweaked in the pparams() function, which is called
    ## later in the function tree.

    ## -------------------- INPUTS --------------------
    ## dfData - data frame of "raw" data, with no transformations applied yet! (so not in logs!)
    ## freq - character, 'quarterly' or 'monthly'
    ## vars - character array with variable names
    ## logseries - vector of zeros
    ##            ones indicating which variables are log-transformed, nLags - integer, 
    ## lcLmd: the full "Lmd" matrix is nshock x nregime. lcLmd determines what parts
    ##        to fill with estimated params (in a more complicated way,
    ##        based on next arg. lmdblock, program will fill in the unspecified lambdas).
    ##        The last regime is always filled out by normalization
    ##        as nRegimes - sum(variances in previous regimes), so the arithmetic average is always
    ##        1. This is in lieu of any normalization of A0 and consistent with
    ##        a Dirichlet prior on the variances
    ## lmdblock: specifies exactly how to restrict some variances not to be
    ##        time-varying. Not used in the most recent calculations
    ## strTsigbrk - (nBreak minus 1) length vector of strings, which specify when periods end
    ## startDate and endDate - character/string,
    ## timeDummy - formatted like start/endDate, but currently
    ##           not used, could input a dummy for one/more period,
    ## verbose - if true will more information than just optimal x (generally should be set true)
    ## seedmodel - a list that looks like the output
    ##            of this function from which a first iteration x vector can be extracted
    ##           intended usage: if you tweak the prior a bit, the mode will be close to old mode
    ##           downside: estiamte of the inverse hessian from optimization will be poor

    ## -------------------- OUTPUTS --------------------
    ## If verbose: the raw output of the optimization routine (in $opt), plus information about the
    ## VAR estimated at the posterior mode. The whole structure is an input for the programs that run MCMC
    
    ## Karthik Sastry
    ## R 3.0.2, 64 Bit
    ## June 2014 and July 2016

    ## END PREAMBLE
######################################################################################

######
    ## Libraries
######

    library(parallel)
######
    ## Loading data
######

    ##rawdata = gzimport(freq = freq)

   listData = dataprep(dfData, vars, freq = freq, Tsigbrk = strTsigbrk,
                        logseries = logseries, startdate = startdate, enddate = enddate,
                        timedummy = timedummy)

    Tsigbrk = listData$Tsigbrk ##from string breaks to numeric breaks
    ##also adding zero at the beginning
    
##########################################################
    ## Grabbing prior parameters
    ## sticks them all in a list that will be accessed by the program.
    ## the "efficient" thing would be to generate the actual dummy observations now, but
    ## I haven't found that to be the time consuming part of posterior evaluation
##########################################################

    pparams = pparams(listData, mnstart = mnstart, mntight = mntight, mndecay = mndecay, vprior = vprior, cosprior = cosprior) ##all options are handled in this function
    
##########################################################
    ## Setting initial parameters for optimization
##########################################################

    ##Current method: Estimate a full-sample model and use linear projections
    ##to get individual lmd

    ##Restrictions on A0, lmd by brute force (seed model has no restrictions)

    nVar = dim(listData$Y)[2]
    T = dim(listData$Y)[1] - nLags

    if (is.null(lcA0)) {
	lcA0 = matrix(FALSE,nVar,nVar)
	lcA0[lower.tri(lcA0)] = TRUE ##lower triangular default structure
    }

    if (!is.null(seedmodel)){
	##use a previous output of this routine to seed the model
	
	##program will re-apply zero restrictions as appropriate, but number of variables	
##must be correct
	seedLmd = seedmodel$lambda ## not lmd for this model
	seedA = seedmodel$A
	
	nRegimes = dim(seedLmd)[2]
    } else if (is.null(seedx)){
	##project a reduced form with fixed variances onto the format we want

	if (any(hparam_nl > 0)){
            ## apply the nonlinear transformation
            mydata = applynl(listData$Y,nlt)$data
        } else{
            mydata = listData$Y
        }

        if ((nLags * dim(mydata)[2]) > (dim(mydata)[1] - nLags)){ ## dimensionality problem
            nLagsrf = 4 ## crude
        } else {
            nLagsrf = nLags
        }
        seedrfmodel = rfvar3(ydata = mydata, lags = nLagsrf, lambda = pparams$urprior$lambda,
                             mu = pparams$urprior$mu)


        tempAinv = (t(chol(crossprod(seedrfmodel$u) / dim(seedrfmodel$u)[1]))) ##unscaled
  
        
	u = seedrfmodel$u
	regimes = c(0, Tsigbrk - nLagsrf, T)
	nRegimes = length(regimes) - 1
	seedLmd = matrix(1, nVar, nRegimes)

	seedA = solve(tempAinv)

	seedA[!lcA0] = 0 ##restrictions
	seedA[upper.tri(seedA)] = 0 ##avoiding floating point junk; unclear if important

        seedAinv = solve(seedA)
	seedAinv[upper.tri(seedAinv)] = 0 

        ## if (nRegimes == 1) { ##one regime, no need for below
        ## 	seedLmd = fullLmd
        ## } else{
        ## 	for (iRegime in (1:nRegimes)) {
        ## 		iRange = (regimes[iRegime] + 1) : (regimes[iRegime + 1])
        ## 		##Tsigbrk indicates new regime starts at next time
        ## 		iU = u[iRange,]
        ## 		iOmega = crossprod(iU) / dim(iU)[1] ##reduced form variances
        ## 		iLambda = seedAinv %*% iOmega %*% t(seedAinv) ##structural variances, in a sense
        ## 		##Just taking diagonal elements
        ## 		seedLmd[,iRegime] = -log(diag(iLambda))
        ## 	}
        ## }

        ## fullLmd = kronecker(matrix(fullLmd,nrow = length(fullLmd)),t(rep(1,nRegimes)))
    }

    nRegimes = length(strTsigbrk) + 1
    lcLmd = matrix(TRUE, nVar, nRegimes)
    lcLmd[,nRegimes] = FALSE
    ##like lcA0, tells program what values to fill from x

    if (!is.null(lmdblock)){
	##generate matrix that tells optimizer what to fill in lmd
	nBlocks = dim(lmdblock)[3]
	for (iBlock in 1:nBlocks){
            ##could not get logical indexing to work here at all, so I'm doing an awkward loop
            reps = which(lmdblock[,,iBlock])
            nReps = length(reps)
            
            for (iRep in 2:nReps) {
                lcLmd[reps[iRep]] = FALSE
            }
            ##(lcLmd[which(lmdblock[,,iBlock])[-1]]) = 
            ##rep(FALSE, length(which(lmdblock[,,iBlock])) - 1) ##fill in first of the block
            
            if (is.null(seedmodel)){
                ## seedLmd[lmdblock[,,iBlock]] = fullLmd[lmdblock[,,iBlock]] ##seed with full sample lmd estimates
                seedLmd[lmdblock[,,iBlock]] = 1 ##seed with full sample lmd estimates 	
            }
	}
    }
    
    ## If number of lambda parameters = number of variables, don't include any lambda parameters
    ## ie variances are constant
    if (sum(lcLmd) <= nVar){
        lcLmd[,] = FALSE
    }
    

    ##construct an x vector, if one not already provided
    if (is.null(seedx)){
        if (tvA == FALSE){              #A0 is constant
            seedx = c(c(seedA[lcA0]), c(seedLmd[lcLmd]))
        } else {
            if (noLmd == TRUE){
                ## Estimate without a prior on lmd --- set constant to 1
                seedx = c(rep(c(seedA[lcA0]),times=nRegimes))
            } else {
                ## Estimate with the prior on lmd
                seedx = c(rep(c(seedA[lcA0]),times=nRegimes),c(seedLmd[lcLmd]))
            }
            
        }
    }


    if (any(hparam_nl > 0)){
        ## add params for nl transformation if desired
        if (length(seedx) == (sum(lcA0) + sum(lcLmd))){
            ## add new params
            sval = c(.3,.4,1,
                     3,2)
            ## some guesses for a,c,beta,iv,k
            seedx = c(seedx, sval[hparam_nl > 0])

            ## nAparams = sum(lcA0)
            ## Nlmdparams = sum(lcLmd)
            ## seedH = diag(c(rep(1, nAparams),
            ##                .1 * c(seedLmd[lcLmd]),
            ##                rep(.001,sum(hparam_nl > 0)),
            ##                rep(1e-2, sum(hparam > 0))))
        }
    }

    if (any(hparam > 0)){
        ## add regular hyperparameters if theyre not there
        if (length(seedx) == (sum(lcA0) + sum(lcLmd) + sum(hparam_nl > 0))){

            ## assume we have one of the mn or cosine priors
            ## this is a quick fix
            if (!is.null(cosprior)){
                pv = c(0,
                       0,
                       pparams$urprior$lambda,
                       pparams$urprior$mu,
                       pparams$cosprior$tight,
                       pparams$cosprior$smooth,
                       pparams$cosprior$damp)
            } else { ## asume mn prior

                pv = c(pparams$mnprior$tight,
                       pparams$mnprior$decay,
                       pparams$urprior$lambda,
                       pparams$urprior$mu,
                       0,
                       0,
                       0)
            }

            seedx = c(seedx, pv[hparam > 0])
        }
    }

    
    ## } else {
    ##     ## no nl, or hypers
        
    ##     ##initial inverse hessian; want log stuff to move less in dx = -H0 * g
    ##     seedH = diag(c(rep(1, nAparams),
    ##                    .1 * c(seedLmd[lcLmd])))
    ##     ##seedH = diag(length(seedx))
    ## }

    nAparams = sum(lcA0)
    if (tvA == TRUE){
        ## More A0 parameters
        nAparams = nAparams * nRegimes
    }
    
    nLmdparams = sum(lcLmd)

    ## seedH = diag(c(rep(1, nAparams),
    ##                .1 * c(seedLmd[lcLmd]),
    ##                rep(.001,sum(hparam_nl > 0)),
    ##                rep(1e-2, sum(hparam > 0))))


    if (is.null(seedH)){
        seedH = diag(c(rep(1, nAparams),
                       1e-5 * rep(1, nLmdparams),
                       rep(.001,sum(hparam_nl > 0)),
                       rep(1e-2, sum(hparam > 0))))
    }

        
##########################################################
    ## Running optimization
##########################################################

    ##Adding zero at beginning of Tsigbrk
    Tsigbrk = c(0,Tsigbrk)

    ##optoutput = csminwelNew(bvarwrap5, seedx, seedH, nit = 200,
    ##listData = listData, lcA0 = lcA0, Tsigbrk = Tsigbrk,
    ##pparams = pparams, nLags = nLags, grad = seedG)

    if (tvA == FALSE){
        lhfcn = bvarwrap5
    } else {
        lhfcn = bvarwrap_tvA
    }
    

    optoutput = csminwelNew(lhfcn, seedx, seedH, nit = 500, nCores = nGradcores,
        listData = listData, lcA0 = lcA0, lcLmd = lcLmd, lmdblock = lmdblock,
        Tsigbrk = Tsigbrk, pparams = pparams, nLags = nLags, crit = critval,
        Verbose = FALSE, nVarcores = nVarcores, nlt = nlt, hparam_nl = hparam_nl, hparam = hparam)

    ##cleaner pointer names
    lh = optoutput$fh
    x = optoutput$xh

    ## checking reason for termination above

    term1 = optoutput$retcodeh

######################################################
    ## Checking optimization, if appropriate
######################################################

    ## currently, this is triggered for ANYTHING except critical value
    ## termination, or if specified in original TvvVar() call. Does not
    ## have different strategies for different errors (e.g., change gradient
    ## if problem was bad gradient, etc.)

    ## alternatively could make this recursive (keep trying until we reach max iterations
    ## or get a benign termination)

    if (term1 != 0) extracheck = TRUE

    if (extracheck){
	## checks if we had problems related to bad Hessian by trying with
	## 'agnostic' Hessian we started with
	optoutput2 = csminwelNew(lhfcn, x, seedH, nit = 500, nCores = nGradcores,
                                 listData = listData, lcA0 = lcA0, lcLmd = lcLmd, lmdblock = lmdblock,
                                 Tsigbrk = Tsigbrk, pparams = pparams, nlt = nlt, hparam_nl = hparam_nl, nLags = nLags, hparam = hparam, crit = critval, 
                                 Verbose = FALSE, nVarcores = nVarcores)
	
	term2 = optoutput2$retcodeh
	different = any((x - optoutput2$xh) < (1e4 * .Machine$double.eps))
	
	if (different) {
            x = optoutput2$xh
            lh = optoutput$fh
	}
	
    } else {
	different = NULL
	optoutput2 = NULL
	term2 = NULL
    }


##########################################################
    ## Retrieving model and parameters
##########################################################

    ##sigpar = list(A = A, lmd = lmd, Tsigbrk = strTsigbrk)
    mlmodel = lhfcn(x, verbose = TRUE, listData = listData, nLags = nLags, 
                        lcA0 = lcA0, lmdblock = lmdblock, lcLmd = lcLmd, Tsigbrk = Tsigbrk, 
                        pparams = pparams, nVarcores = nVarcores, nlt = nlt,
                        hparam_nl = hparam_nl, hparam = hparam)

    ##cleaning up pointer names, etc.
    A = mlmodel$A
    lmd = mlmodel$llmd ##ACTUAL lmd
    lambda = mlmodel$lambda ##exponentiated
    relLambda = lambda / lambda[,1] ##variances relative to the first period
    vout = mlmodel$vout
    lh = mlmodel$lh

##########################################################
    ## Calculating an impulse response
##########################################################

    ## This is a somewhat clumsy way of getting the reduced form parameters,
    ## but this step is very quick anyways

    nLambdas = dim(lambda)[2]
    ir = array(0, dim = c(nVar, nVar, nStep, nLambdas))

    for (iLambda in (1:nLambdas)){

        if (tvA == TRUE){
            iA = A[,,iLambda]
        } else{
            iA = A
        }
        
        
	smat = solve(iA) %*% diag(sqrt(lambda[,iLambda]))
	By = vout$var$By
	nLags = dim(By)[3]
	for (iLag in (1:nLags)) {
            By[,,iLag] = solve(iA) %*% By[,,iLag]
	}

	tempvar = vout$var
	tempvar$By = By
	ir[,,,iLambda] = impulsdtrf(tempvar, smat = smat, nstep = 20)
	dimnames(ir[,,,iLambda])[[1]] = vars
    }

    output = list(A = A, lmd = lmd, lambda = lambda, relLambda = relLambda,
        vout = vout, ir = ir, x = x, Tsigbrk = Tsigbrk,
        startdate = startdate, enddate = enddate, lcLmd = lcLmd,
        lcA0 = lcA0, logseries = logseries, pparams = pparams, 
        listData = listData, lmdblock = lmdblock, different = different,
        extracheck = extracheck, term1= term1,
        term2 = term2, nLags = nLags, lh = lh)
    
    if (!verbose){
	return(output)
    } else {
	##return full optimization output
	##return(c(output, list(opt = optoutput, opt2 = optoutput2)))	
	output$opt = optoutput
	output$opt2 = optoutput2
	return(output)
    }
}

