GsnMove = function(lhfcn, x0, lh0, Sigma, modeA, modeLmd, lcA0, lcLmd, 
    verbose = FALSE, model = NULL, ordercheck = FALSE,...){

    ## One iteration of an MCMC run, using a Gaussian transition density
    ## Based off code in Chris Sims SVN directory

    ## Contains additional options to check the permutation of shocks relative
    ## to some baseline. We don't use these features in our final analysis in the paper

    ## END PREAMBLE
######################################################################################



    x1 = x0 + MASS::mvrnorm(n = 1, mu = rep(0, length(x0)), Sigma = Sigma)

    newmodel = lhfcn(x1, lcA0 = lcA0, lcLmd = lcLmd, ..., verbose = TRUE)
    lh1 = newmodel$lh



    ## trans = ((lh0 - lh1 > log(runif(1))) && !attr(lh1, "bad"))
    trans = (lh0 - lh1 > log(runif(1)))

    ## this logic could be better, but trying to avoid checking for reorderings if transition not accepted
    if (trans) {
	if (ordercheck) {
            mxOutput = GetALmd(x1)
            perm = normAlmd(mxOutput$A, mxOutput$lmd, modeA, modeLmd)
            x1 = GetX(perm$A, perm$lmd, lcA, lcLmd)
            
            reordering = perm$noloop
	} else {
            reordering = FALSE
	}
	lh0 = lh1
	x0 = x1
	model = newmodel
    }

    if (!verbose){
	return(list(x = x0, lh = lh0, trans = trans ))
    } else {
	return(list(x = x0, lh = lh0, trans = trans, aplusmode = model$vout$var$By,
                    xxi = model$vout$var$xxi, u = model$vout$var$u,model=model))
    }

}
