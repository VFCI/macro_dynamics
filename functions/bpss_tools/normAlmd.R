normAlmd <- function(Aml, lmdml, A, lmd) {
    if(is.null(dim(lmd))) lmd <- matrix(lmd, length(lmd), 1)
    if(is.null(dim(lmdml))) lmdml <- matrix(lmdml, length(lmdml), 1)   
    nsig <- dim(lmd)[2]
    nv <- dim(lmd)[1]
    ## normalize diagonal of A, just in case
    sf <- diag(A)
    A <- (1/sf) * A
    lmd <- lmd - 2 * c(log(abs(sf)))        #vector of log sf's gets reused, col by col
    Alml <- array(0, c(nv, nv, nsig))
    Al <- Alml
    for (il in 1:nsig) {
        Alml[ , , il] <- exp(-.5 * lmdml[ , il]) * Aml
        Al[ , , il] <- exp(-.5 * lmd[ , il]) * A
    }
    Alml <- matrix(Alml, nv)
    Al <- matrix(Al, nv)
    xp <- abs(Al %*% t(Alml))
    xp = log(xp) #better approach to avoiding zeros on diagonal, orthogonal rows, etc.
    xpo = xp #to check later if trace actually increased
    ## xp <- abs(cor(t(Al), t(Alml)))
    ## Algorithm tries reordering up to nv times to find an invariant ordering,
    ## then gives up and returns nv'th reordering and noloop=FALSE
    ordrng <- 1:nv
    crit <- vector("numeric", nv)
    noloop <- 0
    for (ntrial in 1:nv) {
        thisOrdrng <- 1:nv
        ## Make any switch with 1 that increases trace(xp), then any with 2, etc.
        for (iv in 1:nv) {
            for (iv2 in iv:nv) {
                crit[iv2] <- xp[iv2,iv] - xp[iv,iv] + xp[iv,iv2] - xp[iv2,iv2]
            }
            idtr <- which.max(crit[iv:nv])
            newiv <- thisOrdrng[iv:nv][idtr]
            thisOrdrng[iv:nv][idtr] <- thisOrdrng[iv]
            thisOrdrng[iv] <- newiv
            Al <- Al[thisOrdrng, ]
            xp <- xp[thisOrdrng, ]
        }
        ordrng <- ordrng[thisOrdrng]
        if (all(thisOrdrng == 1:nv)) {
            noloop <- ntrial
            break
        }
    }

    trimprove = sum(diag(xp)) >= sum(diag(xpo))


    A <- A[ordrng, ]
    sf <- diag(A)

    badshift = any(sf < .Machine$double.eps)
    
    if (badshift){
        A = Aml
        lmd = lmdml
    } else {
        A <- (1/sf) * A
        lmd <- lmd[ordrng, ] - 2 *c(log(abs(sf)))
    }

    changes = any(ordrng != 1:nv)
    return(list(Anormed=A , lmdnormed=lmd, ordrng=ordrng, noloop=noloop, badshift = badshift, changes = changes, trimprove = trimprove))
}
