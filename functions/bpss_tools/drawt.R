drawt = function(uout, alpha = 4, beta = alpha){

    ## draws igamma weights, for model with T errors
    ## beta is a rate, not a scale

    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################

    
    u = c(uout) ## treat everything symmetrically

    ## delta = rgamma(rep(alpha + 1/2,length(u)), 1/(1/beta + .5 * u^2)) ## this gives 1/sigma^2w

    
    palpha = rep(alpha + 1/2,length(u))
    pbeta =  beta + .5 * u^2 

    delta = rgamma(n = length(palpha), shape = palpha, rate = pbeta) ##1/variance units
    
    delta = -log(delta)/2 ## log of standard deviation units
    return(matrix(delta,dim(uout)[1],dim(uout)[2]))
}
    
    
    
