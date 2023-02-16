impulseplots = function(ir, ## array of impulse response objects, nvar x nshock x nperiod
                        irdraws = NULL, ## nvar x nshock x nperiod x ndraw
                        conf = c(.68,.90), ## what confidence bands to put on the plot
                        blocks = NULL, ## possible block structure, see below for example
                        filename = 'impulse_plot',
                        format = 'pdf',
                        addtitle = FALSE, ## put a big title on the top?
                        nsteps = NULL, ## number of steps to use
                        shocknames = NULL, ## names of the shocks to print as titles
                        varnames = rep('',dim(ir)[1]), ## names of the response variables
                        ptype = 'Impulse response', ## part of the title in some cases
                        color = c(0, 0, 1), ## base color (default blue)
                        alpha = c(0.5,0.3), ## how much alpha for each oonf band color
                        gr = 0.7, ## what shade of grey
                        width = 5, ## inches
                        height = 8,
                        response_ylim = NULL, ## can specify the size of each response row
                        xtick = NULL ## where to put xticks; default, every 12 months
                        ) {

    ## Plots impulse response draws
    ## more current than other code in the same folder
    ## Handles blocking of the impulse responses into several minigarphs 

###################################################################################
    require(grDevices)

    ## Some formatting issues
    ## filename = paste('/home/karthik/R/svar_kit/plotting/plots/', filename,sep='')
    
    
    if (format == 'pdf'){ ## different methods for the "light color" for pdf and eps
        shades = rgb(color[1],color[2],color[3], alpha)
    } else if (format == 'eps'){
        trellis.device(device="postscript", color = TRUE)
        #setEPS()
        ## postscript(filename, width = width, height = height)

        # Cannot do transparent colors, so here is a crude workaround
        alphafy = function(col,alpha=1) {
              rr = 1-alpha*(1-c(col/255))
              return(rgb(rr[1],rr[2],rr[3]))
        }
        color = alphafy(color, alpha)
        
    } #else raise some kind of error?

    if (is.null(varnames)){ ## get the names of each shock
        varnames = rownames(ir)
    }

    if (is.null(xtick)){
        xtick = seq(1,nsteps,by=4)
    }

    ##
    ## Calculating the IR quantiles and
    ## y limits for plots
    ##

    nv = dim(ir)[1] ## variables
    ns = dim(ir)[2] ## shocks

    ir  = ir[,,1:nsteps] ## trimming unnecessary steps

    if (!is.null(irdraws)){
        ## get the correct quantiles
        irq = apply(irdraws[,,1:nsteps,],1:3,quantile,
                    ##probs = c(rev((1 - conf) / 2), .5 + conf/2))
                    probs = c((1-conf)/2,0.5 + conf/2))
        irq = aperm(irq,perm=c(2,3,4,1))

        nconf = length(conf)
    } else{
        irq = array(0,c(dim(ir),1))
        nconf = NULL
    }

    ## determine the ylimits for each variable, if necessary
    if (is.null(response_ylim)){
        response_ylim = matrix(0,nv,2) ## idea is that this should be identical for all blocks
        for (iv in 1:nv){
            response_ylim[iv,1] = min(ir[iv,,],irq[iv,,,],0)
            response_ylim[iv,2] = max(ir[iv,,],irq[iv,,,],0)
        }
    }
        
    ##
    ## Blocking prep
    ##

    if (is.null(blocks)){
        blocks = list()
        blocks[[1]] = list(x = 1:nv, y = 1:nv)
    }
    
    nb = length(blocks)
    
    for (ib in 1:nb){

        ## open the file
        if (format == 'pdf'){ 
            fname = paste(filename,'_',ib,'.pdf', sep = '')    
            pdf(fname, width = width, height = height)
        } else if (format == 'eps'){
            fname = paste(filename,'_',ib,'.pdf', sep = '')
            trellis.device(device="postscript", color = TRUE)
            postscript(fname,width=width,height=height)        
        }


        par(mfrow = c(length(blocks[[ib]]$x),length(blocks[[ib]]$y)),
            col.lab="black",col.main="black",
            oma=c(1,5,1,2), mar=c(.5,.25,.5,.25), tcl=-0.1, mgp=c(3,1,0))


        for (rv in blocks[[ib]]$x){ ## responses
            
            ptitle = varnames[rv] ## name of data series

            for (sv in blocks[[ib]]$y){ ## shocks
                plot(ir[rv,sv,],
                     ylim = response_ylim[rv,],
                     type = 'l',
                     lwd = 0.5,
                     xlab = '',
                     ylab = '',
                     yaxt  = 'n',
                     xaxt = 'n',
                     ## fg = gray(gr),
                     xlim = c(1,nsteps), xaxs = 'i')

                ytick = pretty(response_ylim[rv,],4)

                abline(h = ytick, lty = 'dotted')## ,col=gray(gr))
                abline(v = xtick, lty = 'dotted') ## ,col=gray(gr))
                
                abline(a=0,b=0,lwd=0.75)

                if (!is.null(nconf)){ ## plot confidence bands
                    for (ic in 1:nconf){
                        polygon(c(1:nsteps, nsteps:1),
                                c(irq[rv,sv,,ic],rev(irq[rv,sv,,ic+nconf])),
                                col = shades[ic],
                                border = NA)
                    }
                }

                ##Adding variable name and axis on leftmost plot
                if (sv == blocks[[ib]]$y[1]){
                    mtext(ptitle, side = 2, line = 5, cex = 0.5, las = 1, adj = 0)
                    axis(side = 2, cex.axis = .75, las = 1,at=ytick)
                }

                ## ##Right side axis labels on right most plot
                ## if (sv == blocks[[ib]]$y[length(blocks[[ib]]$y)]){
                ##     axis(side = 4, cex.axis = .75, las = 1)
                ## }

                ##Shock name if appropriate
                if (!is.null(shocknames) &&
                    rv == blocks[[ib]]$x[1]) {
                    mtext(shocknames[sv], side = 3, line = 0, cex = .5)
                }
            }
        }

        if (addtitle){
            bigtitle = paste(type, 'over', as.character(nSteps), 'periods', sep = ' ')
            title(bigtitle, outer = TRUE, cex = 1.2)
        }

        dev.off()

    }
}

