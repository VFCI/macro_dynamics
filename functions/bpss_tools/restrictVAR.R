restrictVAR <- function(vout, type=c("3", "KF","SVhtskd"), rmat=NULL, yzrone=NULL, xzrone=NULL,
                        const=NULL, cyzr=NULL, cxzr=NULL) {
    ## restrictions can be specified as rows of rmat, with coefficients applied to elements of By and Bx
    ## stacked as they are in xxi (and then repeated across the equation index), or they can be specified
    ## in yzrone, xzrone.  Each zero element of yzrone or xzrone generates a restriction that sets the corresponding
    ## coefficient in By or Bx to zero (or to a constant, if !is.null(const)).  Both kinds of restrictions
    ## can be non-trivial in the same call.
    ##-------------------------------------------
    ## type:     vout as from rfvar3 ("3") or as from rfvarKF ("KF")
    ## const:    the right hand side of rmat %*% coeff = const, not the constant in the var.
    ## cyzr, cxzr:  If using yzrone, xzrone with non-trivial constants, leave const=NULL and specify
    ##           constants with cyzr and cxzr
    ##------------------------------------------
    ## sc:       The Schwarz criterion rejects the restriction if the chisq value plus the sc value
    ##           is positive.  This version of the sc is scale-sensitive.  Variables with higher
    ##           variance are penalized more strongly, as with a prior that expects higher-variance
    ##           variables to explain more variance.
    ##
    ##------------------------------------------------
    ## Note 2013-3-4:  Try eliminating scale effects by converting X'X to correlation matrix
    if (length(type) > 1) type <- type[1]
    if (type == "SVhtskd") {
        require("tensor")
        bvw <- vout
        vout <- bvw$vout$var
    }
    ncf <- dim(vout$By)[2] * dim(vout$By)[3] + dim(vout$Bx)[2]
    neq <- dim(vout$By)[1]
    ny <- dim(vout$By)[2]
    lags <- dim(vout$By)[3]
    nx <- dim(vout$Bx)[2]
    if (is.null(rmat)) {
        rmat <- matrix(0, 0, ncf *neq)
    }
    if (!is.null(yzrone)) {
        byz <- which(yzrone == 0, arr.ind=TRUE)
        nrstr <- dim(byz)[1]
        if (is.null( cyzr)) cyzr <- array(0, dim(yzrone))
        for (ir in 1:nrstr ) {
            newrow <- rep(0, neq * ncf)
            newrow[(byz[ir,1] - 1) * ncf + (byz[ir, 3] -1) * ny + byz[ir, 2]] <- 1
            rmat <- rbind(rmat,newrow)
        }
        const <- c(const, cyzr[byz])
    }
    if (!is.null(xzrone)) {
        bxz <- which(xzrone == 0, arr.ind=TRUE )
        nrstr <- dim(bxz)[1]
        if (is.null(cxzr)) cxzr <- matrix(0, neq, nx)
        for (ir in 1:nrstr)  {
            newrow <- rep(0,ncf * neq)
            newrow[(bxz[ir,1] - 1) * ncf + ny * lags + bxz[ir, 2]] <- 1
            rmat <- rbind(rmat, newrow)
        }
        const <- c(const, cxzr[bxz])
    }
    svdr <- svd(rmat)
    if (max(abs(svdr$d)) > 1e10 * min(abs(svdr$d))){
        error("restrictions not full rank")
    }
    ## Note that t(rv) spans the same space as rmat, so the restrictiosn are crossprod(v,coeffs)=gamma
    ## rv <- svdr$v    #2013.5.9
    if (length(type) > 1) type <- type[1]
    T <- if (type == "3" || type == "SVhtskd") dim(vout$u)[1] else dim(vout$fcsterr)[1]
    if (type == "3") {
        sig <- cov(vout$u)
        svdsig <- svd(sig)
        singsig <- (max(svdsig$d) > 1e10 * min(svdsig$d))
        if(singsig) warning("Near singular sig matrix in restrictVAR")
        svdxxi <- svd(vout$xxi)
        singxxi <- (max(svdxxi$d) > 1e10 * min(svdxxi$d))
        ## if(singxxi) warning("Near singular xxi matrix in restrictVAR")
        ## schwarz <- rmat %*% kronecker(svdsig$u %*% diag(1/sqrt(svdsig$d)), svdxxi$u %*% diag(1/sqrt(svdxxi$d)))
        ##schwarz <- kronecker((1/sqrt(svdsig$d)) * t(svdsig$u), (1/sqrt(svdxxi$d)) * t(svdxxi$u)) %*% rv  #2013.5.9
        ## sqrtVb <- kronecker(sqrt(svdsig$d) * t(svdsig$u), 1/sqrt(svdxxi$d)) * t(svdxxi$u)
        ## line above seems to be a mistake, since xxi is already x'x-inverse
        sqrtVb <- kronecker(sqrt(svdsig$d) * t(svdsig$u), sqrt(svdxxi$d) * t(svdxxi$u))
        dgVb <- apply(sqrtVb^2, 2, sum)
        rmatC <- rmat %*% diag(sqrt(T * dgVb))
        sqrtVbC <- sqrtVb %*% diag(1/sqrt(T * dgVb))
        lndetVb <- sum(log(svdsig$d)) * dim(vout$xxi)[1] + sum(log(svdxxi$d)) * dim(sig)[1]
        lndetVbC <- lndetVb - sum(log(dgVb * T))
    } else if (type == "KF") {          #type=="KF"
        svdVb <- svd(vout$Vb)
        sqrtVb <- sqrt(diag(svdVb$d)) %*% t(svdVb$u)
        dgVb <- diag(vout$Vb)
        rmatC <- rmat %*% diag(sqrt(T * dgVb))
        sqrtVbC <- sqrtVb %*% diag(1/sqrt(T * dgVb))
        lndetVb <- sum(log(svdVb$d))
        lndetVbC <- lndetVb - sum(log(dgVb * T))
        ## schwarz <- rmat %*% svdVb$u %*% diag(1/sqrt(svdVb$d)) #below is more efficient version for large Vb
        ## schwarz <- (1/sqrt(svdVb$d)) * (t(svdVb$u) %*% rv)
    } else {                            #type="SVhtskd"
        nv <- dim(vout$By)[1]
        nX <- dim(vout$xxi)[1]
        if (is.null(nX)) nX = dim(vout$xx)[1]
        Vb <- matrix(0, nX * nv, nX * nv)


        #check if xxi is missing
        if (is.null(vout$xxi)){
            #instead we have xx, which can be inverted to get xxi
            vout$xxi = vout$xx
            for (iv in 1:nv){
                vout$xxi[,,iv] = solve(vout$xx[,,iv])
            }
        }
        
        for (iq in 1:nv) {
            Vb[nX * (iq-1) + 1:nX, nX * (iq-1) + 1:nX] <- vout$xxi[ , , iq]
        }
        
        A0i <- solve(bvw$A)
        Vb <- kronecker(A0i, diag(nX)) %*% Vb %*% kronecker(t(A0i), diag(nX))
        svdVb <- svd(Vb)

        sqrtVb <- sqrt(diag(svdVb$d)) %*% t(svdVb$u)
        # KS note: is this correct?
        sqrtVb2 = svdVb$u %*% sqrt(diag(svdVb$d)) %*% t(svdVb$u)
        dgVb <- diag(Vb)
        rmatC <- rmat %*% diag(sqrt(T * dgVb))
        sqrtVbC <- sqrtVb %*% diag(1/sqrt(T *dgVb))

        lndetVb <- sum(log(svdVb$d))
        lndetVbC <- lndetVb - sum(log(dgVb * T))
    }

    vr2 = (sqrtVb2 %*% t(rmat))
    cvr2 = crossprod(vr2)
    svdvr2 = svd(vr2)
    
    svdvr <- svd(sqrtVb %*% t(rmat))
    svdvrC <- svd(sqrtVbC %*% t(rmatC)) #result == line above?
    vdim1 <- dim(svdvr$u)[1]
    svdvrp <- svd(diag(vdim1) - svdvr$u %*% t(svdvr$u), nu=vdim1 - dim(rmat)[1])
    svdvrpC <- svd(diag(vdim1) - svdvrC$u %*% t(svdvrC$u), nu=vdim1 - dim(rmat)[1])
    svdvrpuv <- svd(crossprod(svdvrp$u, t(sqrtVb))) 
    svdvrpuvC <- svd(crossprod(svdvrpC$u, t(sqrtVbC))) 
    lndetUR <- sum(log(svdvrpuv$d))
    lndetURC <- sum(log(svdvrpuvC$d))
    df <- dim(rmat)[1]
    ## schwarz <- -2 * sum(log(diag(chol(crossprod(schwarz)))))   +  df * log(2 * pi)
    schwarz <- lndetVb - 2 * lndetUR + df * log(2 * pi)
    schwarzC <- lndetVbC - 2 * lndetURC + df * log(2 * pi)
    if(is.null(const)) const <- rep(0, dim(rmat)[1])

    
    if(type == "SVhtskd") {
        vout$By <- tensor(A0i, vout$By, 2, 1)
        vout$Bx <- A0i %*% vout$Bx
    }
    stackedcf <- c(t(cbind(matrix(vout$By, nrow=neq), vout$Bx)))
    gap <- rmat %*% stackedcf - const
    ##svdv <- svd(rmat %*% vout$Vb %*% t(rmat))
    chstat <- (1/svdvr$d) * t(svdvr$v) %*%  gap
    chstat <- crossprod(chstat)

    #trying with other sqrt matrix -- doesn't seem to make a difference?
    chstat4 = (1/svdvr2$d) * t(svdvr2$v) %*% gap
    chstat4 = crossprod(chstat4)
    
    ## # alternate method?
    ## # seems to work better
    iR = apply(rmat == 1, 2, sum)
    rcoefs = stackedcf
    lcR = (iR == 1)
    rcoefs[!lcR] = 0
    ## # rcoefs is zero for everything unrestricted, equal to coefficient for everything restricted
    chstat2 = t(rcoefs) %*% solve(Vb) %*% rcoefs

    ## ## another alternate method : looking at implied difference in structural form coefficients
    ##    newrfc = stackedcf
    ##    newrfc[lcR] = 0
    ##    stackedmat = t(cbind(matrix(vout$By, nrow = neq), vout$Bx))
    ##    newstmat = array(newrfc, dim = dim(stackedmat))
    ##    By = array(c(t(newstmat)), dim  = dim(vout$By))

    ##    for (i in dim(By)[3]){
    ##        By[,,i] = bvw$A %*% By[,,i]
    ##    }
    
    ##    listXxi = lapply(1:dim(vout$xxi)[3], function(i) vout$xxi[,,i])
    ##    covmat = matrix(bdiag(listXxi), nrow = length(listXxi) * dim(listXxi[[1]])[1])
    
    ##    oldstack = t(cbind(matrix(oldBy, nrow=neq), oldBx))
    ##    newstack = t(cbind(matrix(By, nrow=neq), vout$Bx))

    ##    difference = oldstack - newstack
    
    ##    chstat3 = t(c(difference)) %*% covmat %*% c(difference)
    chstat3 = NULL
    
    return(list(chiSquared=chstat, chstat2 = chstat2, chstat3 = chstat3,
                chstat4= chstat4, df=df, sc=schwarz, pval=pchisq(chstat,df),
                sc2 = schwarz - (ncf*neq-df)*log(1 - df/(neq*ncf)), scC=schwarzC))
}
