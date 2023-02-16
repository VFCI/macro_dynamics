drawdelta = function(uout, alpha = .01, k = 4){

    ## draws from mixture of normals
    ## alpha is the probability OF an outlier

    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################


    
    u = c(uout) ## treat everything symmetrically

    if (length(k) == 1){
        nprob = dnorm(u) ## if standard
        oprob = dnorm(u/k) ## if outlier

        post = (alpha * oprob) / (alpha * oprob + k * (1-alpha) * nprob) ## posterior proability

        delta = log(k) * (runif(length(u)) < post) ## take delta with this probability
    } else { ## n generalization

        scalemat = array(rep(k, times = rep(length(u),length(k))),
                         dim = c(dim(uout),length(k)))
        alphamat = array(rep(alpha, times = rep(length(u),length(k))),
                         dim = c(dim(uout),length(alpha)))

        ubig = array(uout, dim = c(dim(uout),length(k)))

        ratio = alphamat * dnorm(ubig / scalemat) / scalemat

        prob = ratio / c(apply(ratio,c(1,2),sum)) ## normalized to probability
        cprob = aperm(apply(prob,c(1,2),cumsum),c(2,3,1))

        
        ## assume alpha are ordered small to large
        delta = matrix(0,dim(uout)[1],dim(uout)[2])
        dk = c(log(k[1]),diff(log(k)))
        rnum = matrix(runif(length(u)),dim(uout)[1],dim(uout)[2])

        delta  = delta + log(scalemat[,,1]) * (rnum < cprob[,,1])

        ## dkmat = array(rep(dk, times = rep(length(u),length(k))),
        ##                  dim = c(dim(uout),length(k)))

        for (iv in 2:length(k)){
            delta = delta + log(scalemat[,,iv]) * (rnum > cprob[,,iv-1]) *
                (rnum < cprob[,,iv])
        }
            
        
        ## for (iv in 1:length(k)){
        ##     delta = delta + dk[iv] * (rnum < prob[,,iv])
        ## }

        ## dkmat = array(rep(dk, times = rep(length(u),length(k))),
        ##                  dim = c(dim(uout),length(k)))

                    
    }

    return(matrix(delta,dim(uout)[1],dim(uout)[2]))
}
    
    
    
