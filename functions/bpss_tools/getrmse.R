getrmse = function(fcast,ydata,h = c(1,6,12,24,48)){

    ## get root mean squared error
    ## input is "fcast" array

    nv = dim(fcast)[2] ## number of variables
    nfc = dim(fcast)[3] ## number of forecasts
    nh = length(h) ## number of horizons to consider
    T = dim(ydata)[1]

    rmse = array(0, c(nh,nv,nfc))

    for (it in 1:nfc){
        if (sum(abs(fcast[,,it])) > 1e-10){ ## make sure fc is not blank
            for (ih in 1:nh){
                hzn = h[ih] ## what horizon
                if ((hzn + it) <= T){ ## make sure there is space
                    df = fcast[1:hzn,,it] - ydata[it + 1:hzn,]
                    if (hzn > 1){
                        rmse[ih,,it] = sqrt(apply(df^2,2,sum) / hzn)
                    } else {
                        rmse[ih,,it] = abs(df)
                    }
                    
                }
            }
        }
    }
    return(rmse)
}
