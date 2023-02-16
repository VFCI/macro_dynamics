scaleplots <- function(ir1, ir2,
                       ird1 = NULL,
                       ird2 = NULL,
                       shockscale = rep(0,10),
                       filename = 'impulse_plot', format = 'pdf',
                       shockvars = NULL, responsevars = NULL,
                       varnames = rep('',dim(ir)[1]),
                       color1 = c(0, 0, 1), gr = 0.7,
                       color2 = c(1,0,0),
                       width = 5, height = 8,
                       conf = .68, nSteps = 60,
                       ymat = NULL,
                       logseries = rep(0,10),
                       shocknames = NULL,
                       addtitle = FALSE)
{

    ## Plots all shocks/responses for an SVAR
    
    ## Code is specific to one (not very general) organization of input -- key things to preserve in a more
    ## general edit would preserve how the plots look and change how the data is processed
    
    ## Karthik Sastry
    ## R 3.0.2, 64 Bit
    ## First edit, June 2014


    ## END Preamble
    ######################################################################################################################################################################

    require(grDevices)
    
    #### filename = paste('/home/karthik/R/summer/macrofin/plotting','/plots/', filename,sep='')
    
    filename = paste('plots/', filename,sep='')
    if (format == 'pdf'){
        filename = paste(filename, '.pdf', sep = '')
        pdf(filename, width = width, height = height)
        color1 = rgb(color1[1], color1[2], color1[3], 1)
        color2 = rgb(color2[1], color2[2], color2[3], 1)
    } else if (format == 'eps'){
        filename = paste(filename, '.eps', sep = '')
        trellis.device(device="postscript", color = TRUE)
        ##setEPS()
        postscript(filename, width = width, height = height)

        ## Cannot do transparent colors, so here is a crude workaround
        alphafy = function(col,alpha=1) {
              rr = 1-alpha*(1-c(col/255))
              return(rgb(rr[1],rr[2],rr[3]))
        }
        color = alphafy(color, alpha)
        
    } ##else raise some kind of error?


    ######
    ## Determining format of output
    ######

    ## Defaults to all structural shocks, all responses
    nVar = dim(ir)[1]

    if (is.null(shockvars)) shockvars = 1:nVar
    if (is.null(responsevars)) responsevars = 1:nVar

    nShockvars = length(shockvars)
    nRespvars = length(responsevars)

    if (is.null(varnames)){
        varnames = rownames(ir)
    }

    gRange = 1:(nSteps) ##graph range
    ##In a model sense, you are only going to nSteps - 1 periods (index 1 is really period 0)

    ## Converting probability "range" into quantiles
    probs = c(rev((1 - conf) / 2), .5 + conf/2)


    ## Scale every shock correctly
    for (ishock in shockvars){
        sval = ir1[shockscale[ishock],
            ishock,1]

        ir2[,ishock,] = ir2[,ishock,] * sval / ir2[shockscale[ishock],ishock,1]
        ird1[,ishock,,] = ird1[,ishock,,] * sval / rep(
                ird1[shockscale[ishock],ishock,1,], each = length(ird1[,ishock,,1]))

        ird2[,ishock,,] = ird2[,ishock,,] * sval / rep(
                ird2[shockscale[ishock],ishock,1,], each = length(ird2[,ishock,,1]))
    }
    


    ######
    ## Plot arrangement
    ######

    arr = c(nRespvars, nShockvars)

    ## par(mfrow = arr,col.lab="black",col.main="black",
    ##     oma=c(1,3,1,2), mar=c(.5,.25,.5,.25), tcl=-0.1, mgp=c(0,0,0))

    par(mfrow = arr,
        col.lab="black",col.main="black",
        oma=c(1,5,1,2), mar=c(.5,.25,.5,.25), tcl=-0.1, mgp=c(3,1,0))

    for (irsvar in responsevars) {
        
        irsseries_1 = ir1[irsvar,,]
        irstrials_1 = ird1[irsvar,,,]

        irsseries_2 = ir2[irsvar,,]
        irstrials_2 = ird2[irsvar,,,]


        ## Getting error bands, if appropriate
        if (!is.null(irstrials_1) & length(irstrials_1> 0)){
            irsbounds_1 = array(NA, c(nSteps, length(probs), 10))
            for (iShock in shockvars){
                iShocktrials = irstrials_1[iShock,,]
                iShockbounds = t(apply(iShocktrials,1,quantile,probs = probs))
                irsbounds_1[,,iShock] = iShockbounds[1:(nSteps),]
            }

            irsbounds_2 = array(NA, c(nSteps, length(probs), 10))
            for (iShock in shockvars){
                iShocktrials = irstrials_2[iShock,,]
                iShockbounds = t(apply(iShocktrials,1,quantile,probs = probs))
                irsbounds_2[,,iShock] = iShockbounds[1:(nSteps),]
            }

        } else {
            irsbounds = NULL
        }

        ##Determining plot scaling/ axis size
        ##A few different reasonable approaches...

        ##yMax = max(irsseries, irsbounds, 0) ##+ .1e-5 * abs(max(irsseries, irsbounds))
        ##yMin =  min(irsseries, irsbounds, 0) ##- 1e-5 * abs(min(irsseries, irsbounds))

        if (is.null(ymat)){
            yMax = max(irsseries_1[shockvars,], irsbounds_1[1:nSteps,,shockvars], irsseries_2[shockvars,], irsbounds_2[1:nSteps,,shockvars], 0) ##+ .1e-5 * abs(max(irsseries, irsbounds))
            yMin = min(irsseries_1[shockvars,], irsbounds_1[1:nSteps,,shockvars], irsseries_2[shockvars,], irsbounds_2[1:nSteps,,shockvars], 0) ##+ .1e-5 * abs(max(irsseries, irsbounds))
        } else {
            yMin = ymat[1,irsvar]
            yMax = ymat[2,irsvar]
        }
        

        for (iShockvar in shockvars){

            ##Plotting each series

            plottitle = paste(varnames[irsvar], ' (log)'[logseries[irsvar]],
                sep = '')

            ## if (length(probs) > 2){ #### 2 sets
            ##     upper = irsbounds[,4,iShockvar]
            ##     lower = irsbounds[,1,iShockvar]

            ##     p84 = irsbounds[,3,iShockvar]
            ##     p16 = irsbounds[,2,iShockvar]

            ## } else {
            upper1 = irsbounds_1[,3,iShockvar]
            lower1 = irsbounds_1[,2,iShockvar]

            upper2 = irsbounds_2[,3,iShockvar]
            lower2 = irsbounds_2[,2,iShockvar]

                ## p16 = NULL
                ## p84 = NULL
            ## }


            plot(irsseries_1[iShockvar, 1:nSteps], ylim = c(yMin, yMax), type = 'l', lwd = 1,
                 xlab = '', ylab = '', yaxt = 'n', xaxt = 'n',
                 ## fg = gray(gr),
                 xlim = c(1,nSteps),
                 xaxs = 'i',
                 col = color1)
            lines(irsseries_2[iShockvar, 1:nSteps], lwd = 1, col = color2)
            
            abline(a = 0, b = 0, lwd = 0.75)

            lines(upper1, lwd = 1, col = color1, lty = 2)
            lines(lower1, lwd = 1, col = color1, lty = 2)

            lines(upper2, lwd = 1, col = color2, lty = 2)
            lines(lower2, lwd = 1, col = color2, lty = 2)


            ##Adding variable name and axis on leftmost plot
            if (which(shockvars == iShockvar, arr.ind = TRUE) == 1) {
                ## mtext(plottitle, side = 2, line = 2, cex = .5)
                mtext(plottitle, side = 2, line = 5, cex = 0.5, las = 1, adj = 0)
                axis(side = 2, cex.axis = .75, las = 1)
            }
            ##Right side axis labels on right most plot
            ## if (which(shockvars == iShockvar, arr.ind = TRUE) == nShockvars) {
            ##     axis(side = 4, cex.axis = .5, fg = gray(gr))
            ## }

            ##Shock name if appropriate
            if (!is.null(shocknames) && (which(responsevars == irsvar, arr.ind = TRUE) == 1)) {
                mtext(shocknames[iShockvar], side = 3, line = 0, cex = .5)
            }
        }
    }

    ######
    ## Titles
    ######

    if (addtitle){
        bigtitle = paste(type, 'over', as.character(nSteps), 'periods', sep = ' ')
        title(bigtitle, outer = TRUE, cex = 1.2)
    }
    
    dev.off()
    ##dev.copy2pdf(file = filename)
}
