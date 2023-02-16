fd = function(delta,tparam,tscale = 1){ ## igamma tparam/2, 2/tparam

    ## returns inverse gamma density evaluated at values of matrix delta
    
    alpha = tparam/2
    beta = tparam/2 * tscale^2 ## scale, not rate
    
    cterm = alpha * log(beta) - lgamma(alpha)
    xterm = -(alpha + 1) * (2 * delta) - beta / exp(2 *  delta)

    return(sum(xterm + cterm))
}

