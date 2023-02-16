gdraw_1v =  function(ydata, xdata, olsfun, ndraw,
    x0,
    nburn = 1e3, nsep = 1e2,
    filename = 'gdraw_1v.Rdata',
    sigfactor = 0.2,
    tparam = NULL,
    tscale = 1,
    savespots = NULL,
    dscore = FALSE,
    drawdout = FALSE){

    ## Gibbs sampling for normal mixture single equation models
    ## see gdraw.R for more details
    
    ## Karthik Sastry
    ## R 3.1.2, 64 Bit
    ## August 2016

    ## END PREAMBLE
######################################################################################

    ## decide where to save
    if (is.null(savespots)){
        nsaves = 5
        inc = ndraw %/% nsaves
        if (inc == 0){
            savespots = ndraw ## no need to save until end
        } else {
            savespots = c(seq(1, ndraw, inc), ndraw)
        }
    }


    ## allocate space for output
    xout = matrix(0,length(x0),ndraw)
    lhout = rep(0,ndraw)
    tout = rep(0,ndraw)

    if (drawdout){
        dout = matrix(0,length(ydata),ndraw)
    }  else {
        dout = matrix(0,length(ydata),length(savespots))
    }

    isave = 1 ## first save

    if (dscore){
        dsout = rep(0,ndraw)
    } else {
        dsout = NULL
    }
    
    output = NULL

    gout = list(xout = x0, deltaout = dout[,,1])
    

    ## Run the Gibbs Sampler
    if (nburn > 0){
        for (iburn in 1:nburn){
            gout = gstep_1v(gout, olsfun, ydata, xdata,
                tparam, tscale, dscore)
        }
    }
    for (idraw in 1:ndraw){
        for (isep in 1:nsep){
            gout = gstep_1v(gout, olsfun, ydata, xdata,
                tparam, tscale, dscore)
        }

        ## update
        xout[,idraw] = gout$xout
        lhout[idraw] = gout$lhout
        tout[idraw]  = gout$trans
        if(drawdout) {
            dout[,idraw] = gout$deltaout
        }

        if (dscore){
            dsout[idraw] = gout$dsout
        }
        
        if (idraw %in% savespots){
            dout[,,isave] = gout$deltaout
            isave = isave + 1
            output = list(xout = xout, lhout = lhout,
                          tout = tout, dout = dout,
                          dsout = dsout)
            save(output, file = filename)
        }

    }


    if (is.null(output)){
        output = list(xout = xout, lhout = lhout,
                      tout = tout, dout = dout,
                      dsout = dsout)
    }

    save(output, file = filename)
    return(output)
}

