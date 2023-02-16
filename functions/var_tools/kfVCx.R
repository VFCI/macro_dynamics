kfVCx <- function(y, X, shat, sig, M) {
    ## This is for the case of the state  constant, with the observation
    ## equation y = X %*% s + e, sqrt(Var(e))=M.
    require(tensor)
    SMALLSV <- 1e-30
    if (!is.null(dim(y))) {
        nv <- dim(y)[2]
        nx <- dim(X)[2]
        T <- dim(y)[1]
    } else {
        nv <- length(y)
        nx <- length(x)
        T <- 1
        y <- matrix(T, nv)
        X <- matrix(T, nx)
    }
    nXX <- nv*nx
    nstate <- length(shat)
    omega <- array(sig, c(nx, nv, nx, nv))
    sigObs <- crossprod(M)
    ## ho: kron(I,X) %*% sighat
    ## ho <- array(0, c(T, nv, nx, nv))
    ## sv <- array(0, c(T, nv, T, nv))
    ##cat("\nstart double X prod loop", proc.time())
    ## for (iv in 1:nv) {
    ##     ho[ , iv, , ] <- tensor(X, omega[ , iv, ,  ], 2, 1)
    ##     for (jv in 1:nv)
    ##         sv[ , iv, , jv] <- tensor(ho[ , iv, , jv ], X, 2, 2) + sigObs[iv, jv] * diag(T)
    ## }
    ho <- tensor(X, omega, 2, 1)  #T, nv, nx, nv
    sv <- tensor(ho, X, 3, 2)     #T, nv, nv, T
    sv <- aperm(sv, c(1,2,4,3))
    for (irow in 1:nv)
        for (icol in 1:nv)
            sv[ , irow, , icol] <- sv[ , irow, , icol] + sigObs[irow, icol] * diag(T)
    ##svdsv <- svd( matrix(sv, nv * T, nv * T ))
    ##cat("\nstart qr", proc.time)
    qrsv <- qr( matrix(sv, nv * T, nv * T ), LAPACK=TRUE)
    rankr <- match(TRUE, abs(diag(qrsv$qr)) < SMALLSV)
    if (is.na(rankr)) {
        rankr <- nv * T
    } else {
        rankr <- rankr - 1
    }
    shatmat <- matrix(shat,nx, nv )
    fcsterr <- y - X %*% shatmat  
    lh <- c(0,0)
    ## we're relying on qr( ,LAPACK=TRUE) always putting zeros in diagonal of R on bottom.
    ## Note that we have to account for possible pivot
    ## in QR
    if (rankr == 0) { # Observation is uninformative. Propagate state.
        shatnew <- shat
        signew <- omega
        if (!all(abs(fcsterr) < 1e-7)) warning("Uninformative H but non-zero fcsterr")
    } else if (rankr < nv *T) {
        R <- qr.R(qrsv)[1:rankr, ]
        d <- diag(R)
        R <- R[ , sort.list(qrsv$pivot)]
        Q <- qr.Q(qrsv)[ , 1:rankr]
        RQ <- R %*% Q
        oie <- Q %*% solve(RQ, crossprod(Q, c(fcsterr)))
        lh[1] <- -.5 * c(fcsterr) %*% oie
        lh[2] <- -.5 * sum(log(abs(d)))
        ho <- matrix(ho, T*nv, nx*nv)
        shatnew <- shat + oie %*% ho   # works because oie is a vector, not a matrix
        qho <- crossprod(Q, ho)
        signew <- sig - crossprod(qho , solve(RQ, qho)) 
    } else {
        ## first0 <- match(TRUE, svdsv$d < SMALLSV)
        ## if (is.na(first0)) first0 <- nv * T + 1
        ## u <- svdsv$u[ , 1:(first0-1), drop=FALSE]
        ## v <- svdsv$v[ , 1:(first0-1), drop=FALSE]
        ## d <- svdsv$d[1:(first0-1), drop=FALSE]
        ## svifac <- (1/sqrt(d)) * t(u)   #diag(1/sqrt(d)) %*% t(u)
        ## ferr <- svifac %*% c(fcsterr)
        ##lh[1] <- -.5 * crossprod(ferr)
        oie <- solve(qrsv, c(fcsterr))
        lh[1] <- -.5 * c(fcsterr) %*% oie
        lh[2] <- -.5 * sum( log(abs(diag(qr.R(qrsv)))) )
        ## svoifac <-svifac %*% matrix(ho, nv * T, nx * nv)
        ## shatnew <- crossprod(svoifac, ferr) + shat
        ## signew <- sig - crossprod(svoifac)
        ho <- matrix(ho, T*nv, nx*nv)
        shatnew <- shat + oie %*% ho
        signew <- sig - crossprod(ho, solve(qrsv, ho))
    }
    return(list(shat=shatnew, sig=signew, lh=lh, fcsterr=fcsterr))
}
