impulseplots_subset <- function(ir, ## array of impulse response objects, nvar x nshock x nperiod
                        irdraws = NULL, ## nvar x nshock x nperiod x ndraw
                        varind = 1, ## Index of the variable for which the plot is specified
                        findvar= NULL, ## Index of other variables, by default all vars are taken
                        conf = c(.68,.90), ## what confidence bands to put on the plot
                        sstruct = "TO", ## set to TO if you want var to be the shocked var; FROM for a response var
                        #filename = 'impulse_plot',
                        filename = '',
                        format = 'svg', #'pdf',
                        addtitle = FALSE, ## put a big title on the top?
                        nsteps = NULL, ## number of steps to use
                        varnames = rep('',dim(ir)[1]), ## names of the response variables
                        ptype = 'Impulse response', ## part of the title in some cases
                        color = c(0, 0, 1), ## base color, rgb (default blue)
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

  fname = paste0(filename,'_',gsub("\\s+", "_", varnames[varind]),'_',ifelse(sstruct=="TO","shock","response"), ".", format) 
  color = rgb(color[1], color[2], color[3])

  ##
  ## Calculating the IR quantiles and
  ## y limits for plots
  ##
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
  

  if(sstruct=="TO"){
      irnew <-ir[,varind,]
  } else {
      irnew <- ir[varind,,]
  }

  plotleftax <- 1
  irout <- c()
  namesirout <- c()

  plot_list <- lapply(findvar, function(j) { ## responses

      ptitle = ifelse(
        sstruct=="TO",
        paste(varnames[varind],"shock to",varnames[j],sep=" "),
        paste(varnames[j],"shock to",varnames[varind],sep=" ") ## name of data series
      )

      if(sstruct == "TO"){
        irf_bands_1 <- cbind(irq[j,varind,,1], irq[j,varind,,3])
        irf_bands_2 <- cbind(irq[j,varind,,2], irq[j,varind,,4])
      } else {
        irf_bands_1 <- cbind(irq[varind,j,,1], irq[varind,j,,3])
        irf_bands_2 <- cbind(irq[varind,j,,2], irq[varind,j,,4])
      }

      p <- ggplot() + 
      geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
      geom_line(aes(x = seq_len(nsteps), y = irnew[j,]), color = "black") +
      geom_ribbon(aes(x = seq_len(nsteps), ymin = irf_bands_2[,1], ymax = irf_bands_2[,2]), alpha = alpha[1], fill = color) +
      geom_ribbon(aes(x = seq_len(nsteps), ymin = irf_bands_1[,1], ymax = irf_bands_1[,2]), alpha = alpha[2], fill = color) +
      scale_x_continuous(limits = c(1,nsteps), expand = c(0, 0)) + 
      scale_y_continuous(limits = c(y_axis_custom[[j]])) +
      labs(
        title = ptitle,
        x = NULL,
        y = NULL
      ) +
      theme_classic(base_size = 11) + ## Base font size, ggplot2 defaults to 11
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.text = element_text(margin = margin(0, 6, 0, 3)),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(5, 0, 0, 0, unit = "pt")),
        axis.text.y = element_text(margin = margin(0, 5, 0, 0, unit = "pt"))
      )
    irout <- cbind(irnew[j,], irf_bands_1, irf_bands_2)
    colnames(irout) <- paste0(paste0("ir_",gsub(" ","_",ptitle)), c("", paste0(c("_lb", "_ub"), conf[1]), paste0(c("_lb", "_ub"), conf[2])))
    
    return(p)
  })

  p <- patchwork::wrap_plots(plot_list, ncol = 2)

  ggsave(fname, p, width = width, height = height, units = "in")
  
  if (savedata == TRUE) {
    write.table(irout,file=paste0(filename,'_',gsub("\\s+", "_", varnames[varind]),'_',ifelse(sstruct=="TO","shock","response"),'.csv'),col.names = TRUE,row.names = FALSE,sep=",")
  }
}

