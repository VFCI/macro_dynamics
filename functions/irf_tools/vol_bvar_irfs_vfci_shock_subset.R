impulseplots_subset <- function(ir, ## array of impulse response objects, nvar x nshock x nperiod
                        irdraws = NULL, ## nvar x nshock x nperiod x ndraw
                        varind = 1, ## Index of the variable for which the plot is specified
                        findvar= NULL, ## Index of other variables, by default all vars are taken
                        conf = c(.68,.90), ## what confidence bands to put on the plot
                        sstruct = "TO", ## set to TO if you want var to be the shocked var; FROM for a response var
                        #filename = 'impulse_plot',
                        filename = '',
                        format = 'pdf', #'pdf',
                        addtitle = FALSE, ## put a big title on the top?
                        nsteps = NULL, ## number of steps to use
                        varnames = rep('',dim(ir)[1]), ## names of the response variables
                        ptype = 'Impulse response', ## part of the title in some cases
                        color = c(0, 0, 1), ## base color (default blue)
                        alpha = c(0.5,0.3), ## how much alpha for each oonf band color
                        gr = 0.7, ## what shade of grey
                        width = 5, ## inches
                        height = 8,
                        response_ylim = NULL, ## can specify the size of each response row
                        y_axis_custom = NA,
                        xtick = NULL, ## where to put xticks; default, every 4 quarters
                        savedata = TRUE, ## TRUE IF YOU WANT TO SAVE DATA FOR IR AND CONFIDENCE INTERVALS
                        newplot  = TRUE, ## TRUE IF YOU WANT TO CREATE A NEW PLOT
                        plotyaxis = TRUE,
                        plottitle = TRUE
) {

  ###################################################################################
  require(grDevices)
  if (format == 'pdf'){ ## different methods for the "light color" for pdf and eps
    if(sum(is.na(color))==0){
        shades = rgb(color[1],color[2],color[3], alpha)
    } else {
        shades = c(NA,NA,NA,NA)
    }
  } else if (format == 'eps'){
    trellis.device(device="postscript", color = TRUE)
    #setEPS()
    ## postscript(filename, width = width, height = height)
    
    alphafy = function(col,alpha=1) {
      rr = 1-alpha*(1-c(col/255))
      return(rgb(rr[1],rr[2],rr[3]))
    }
    color = alphafy(color, alpha)
  } 
  if (is.null(xtick)){
    xtick = seq(0,nsteps,by=5)
  }
  
  ##
  ## Calculating the IR quantiles and
  ## y limits for plots
  ##
  #browser()
  nv = dim(ir)[1] ## variables
  ns = dim(ir)[2] ## shocks
  
  if(is.null(findvar)){
     findvar <- seq(1,nv,1)
  }
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
    if(sstruct == "TO"){
        for (iv in 1:nv){
         # response_ylim[iv,1] = min(ir[iv,findvar,],irq[iv,findvar,,])
         # response_ylim[iv,2] = max(ir[iv,findvar,],irq[iv,findvar,,])
          response_ylim[iv,1] = min(ir[varind,findvar,],irq[varind,findvar,,])
          response_ylim[iv,2] = max(ir[varind,findvar,],irq[varind,findvar,,])
        }
    } else {
        for (iv in 1:nv){
          response_ylim[iv,1] = min(ir[varind,findvar,],irq[varind,findvar,,])
          response_ylim[iv,2] = max(ir[varind,findvar,],irq[varind,findvar,,])
        }
    }
  }
  
   numplots <- length(findvar)
   # DETERMINE DIMENSIONS OF PLOTS
   if((numplots %% 2) > 0){
     sizeplot1 <- floor(numplots/2) + 1
     sizeplot2 <- floor(numplots/2)
   } else {
     sizeplot1 <- numplots/2 -> sizeplot2
   }

  if(newplot == TRUE){
    fname = paste(filename,'_',varnames[varind],'_',ifelse(sstruct=="TO","shock","response"),'.pdf', sep = '') 
    if (format == 'pdf'){ 
      pdf(fname, width = width, height = height)
    } else if (format == 'eps'){
      trellis.device(device="postscript", color = TRUE)
      postscript(fname,width=width,height=height)        
    }
    
    par(mfrow = c(sizeplot1,sizeplot2),
    col.lab="black",col.main="black",
    #oma=c(1,5,1,2), mar=c(.5,.25,.5,.25), tcl=-0.1, mgp=c(3,1,0)) #to change the plot margins
    oma=c(0,5,1,2), mar=c(4,.25,.5,4), tcl=-0.1, mgp=c(3,1,0)) #to change the plot margins
  }
  

  if(sstruct=="TO"){
      irnew <-ir[,varind,]
  } else {
      irnew <- ir[varind,,]
  }

  plotleftax <- 1
  irout <- c()
  namesirout <- c()
  for (j in findvar){ ## responses

      ptitle = ifelse(sstruct=="TO",paste(varnames[varind],"shock to",varnames[j],sep=" "),
                      paste(varnames[j],"shock to",varnames[varind],sep=" "))## name of data series
      if (sum(unlist(lapply(yaxis_vfci_shock,is.na))) > 0){
      plot(irnew[j,],
           ylim = response_ylim[j,],
           type = 'l',
           lwd = 2,
           xlab = '',
           ylab = '',
           yaxt  = 'n',
           xaxt = 'n',
           col  = "blue4",
           ## fg = gray(gr),
           xlim = c(1,nsteps), xaxs = 'i')
        ytick = pretty(response_ylim[j,],4)
      } else {
        plot(irnew[j,],
             ylim = y_axis_custom[[j]], 
             type = 'l',
             lwd = 2,
             xlab = '',
             ylab = '',
             yaxt  = 'n',
             xaxt = 'n',
             col  = "blue4",
             ## fg = gray(gr),
             xlim = c(1,nsteps), xaxs = 'i')
        ytick = pretty(y_axis_custom[[j]],4)
      }
      
      
      
      
      #abline(h = ytick, lty = 'blank')## ,col=gray(gr))
      #abline(v = xtick, lty = 'blank') ## ,col=gray(gr))
      abline(h = ytick, lty = 'dotted', col = "lightgray")## ,col=gray(gr))
      abline(v = xtick, lty = 'dotted', col = "lightgray") ## ,col=gray(gr))
      
      abline(a=0,b=0,lwd=0.75)
      irout <- cbind(irout,irnew[j,])
      namesirout <- c(namesirout,paste0("ir_",gsub(" ","_",ptitle)))
                     
      if (!is.null(nconf)){ ## plot confidence bands
        for (ic in 1:nconf){
          if(sstruct == "TO"){
            polygon(c(1:nsteps, nsteps:1),
                    c(irq[j,varind,,ic],rev(irq[j,varind,,ic+nconf])),
                    col = ifelse(sum(is.na(color)) == 0,shades[ic],NA),
                    border = NA, #"lightgray"
                    lty = 2,
                    lwd = 2)
            irout <- cbind(irout,irq[j,varind,,ic],irq[j,varind,,ic+nconf])
          } else {
            polygon(c(1:nsteps, nsteps:1),
                    c(irq[varind,j,,ic],rev(irq[varind,j,,ic+nconf])),
                    col = ifelse(sum(is.na(color)) == 0,shades[ic],NA),
                    border = NA,
                    lty = 2,
                    lwd = 2)
            irout <- cbind(irout,irq[varind,j,,ic],irq[varind,j,,ic+nconf])
          }
          namesirout <- c(namesirout,paste0("ir_",gsub(" ","_",ptitle),"_",conf[ic],"_lb"),
                          paste0("ir_",gsub(" ","_",ptitle),"_",conf[ic],"_ub"))  
        }
      }
      
      # ##Adding variable name and axis on leftmost plot
      if(sizeplot2 > 0){
        if ((plotleftax %% sizeplot2)>0){
        #     mtext(ptitle, side = 2, line = 5, cex = 0.5, las = 1, adj = 0)
         axis(side = 2, cex.axis = 1.3, las = 1,at=ytick)
        }
      } 
      
      if(plotyaxis == TRUE){
        axis(side = 2, cex.axis = 1.3, las = 1,at=ytick)
      }
      ## ##Right side axis labels on right most plot
      ## if (sv == blocks[[ib]]$y[length(blocks[[ib]]$y)]){
      ##     axis(side = 4, cex.axis = .75, las = 1)
      ## }
      
      if(plottitle == TRUE)
          mtext(ptitle, side = 3, line = 0, cex = 1.3)
      
      axis(side = 1,cex.axis = 1,las = 1,at =xtick) #adds quarters 1-t to the x-axis
      plotleftax <- plotleftax + 1
  }
  if (addtitle){
    bigtitle = paste(type, 'over', as.character(nSteps), 'periods', sep = ' ')
    title(bigtitle, outer = TRUE, cex = 1.3)
  }
 
  if(newplot == TRUE){
    #dev.copy(x11)              # copy the content of the pdf device into the x11 device
    #dev.set(which = 2)         # set the pdf device as actice
    dev.off()
  }
  
  #Save CSV Spreadsheet
  if(savedata == TRUE){
    colnames(irout) <- namesirout
    write.table(irout,file=paste0(filename,'_',varnames[varind],'_',ifelse(sstruct=="TO","shock","response"),'.csv') 
                  ,col.names = TRUE,row.names = FALSE,sep=",")
  }
}