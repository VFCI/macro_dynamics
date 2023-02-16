logmean = function(x){

    ## Little function to calculate harmonic means of really big or really small log quantities
    
    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################

    lm = rep(0,4)

    ## 1. simple
    lm[1] = log(mean(exp(x)))

    ## 2. demeaned
    mn = mean(x)
    xdm = x - mn
    lm[2] = (mn) + log(mean(exp(xdm)))

    ## 3. trimmed
    lm[3] = log(mean(exp(x), trim = 0.2))

    ## 4. trimmed dm
    lm[4] = (mn) + log(mean(exp(xdm), trim = 0.2))

    return(lm)
}
