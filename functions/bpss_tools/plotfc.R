plotfc = function(fcout, ydata, dateseq,
                  vnames, fulldates,
                  ymat = c(4.35,4.70,
                           4.55,4.65,
                           8.2,8.5,
                           7.00,7.40,
                           7.20,7.60,
                           -0.02,0.06,
                           5.6,6.4,
                           -0.01,0.05,
                           0.00,0.12,
                           -0.01,0.05),
                  filename = 'fcout',
                  cushion = 0){

    ## Come up with a nice way of plotting all the forecasts
    ## assume ydata starts at datseq[1]
    
    nfwd = dim(fcout)[1] ## how far fwd does each fc go
    nx = dim(fcout)[2]
    nd = dim(fcout)[3] ## number of dates

    ## fulldates = seq(from = datseq[1], to = datseq[nd], by = 'month') ## dates for yvec
    
    ymat = matrix(ymat, nrow = 2)
    fclist = list()
    
    ## make everything a time series
    for (idate in 1:nd){
        ## sdate = seq(from = datseq[idate], length = 2, by = 'month')
        ## sdate = sdate[2] ## month after

        sdate = dateseq[idate]
        iy = which(fulldates == dateseq[idate])
        yt = ydata[iy,] # y data

        fclist[[idate]] = ts(rbind(yt,fcout[,,idate]),
                             start =
                                 c(as.numeric(format(sdate,format = '%Y')),
                                   as.numeric(format(sdate,format = '%m'))),
                             frequency = 12)
        ## ylist[[idate]] = ts(yt,
        ##                     start =
        ##                         c(as.numeric(format(sdate,format = '%Y')),
        ##                           as.numeric(format(sdate,format = '%m'))),
        ##                     frequency = 12)
    }

    ## ystart = which(fulldates == dateseq[1])
    ## yend = which(fulldates == dateseq[nd]) + 1 + nfwd

    ## ytrim = ts(ydata[ystart:yend,],
    ##            start =
    ##                c(as.numeric(format(dateseq[1],format = '%Y')),
    ##                  as.numeric(format(dateseq[1],format = '%m'))),
    ##            frequency = 12)

    ytrim = ts(ydata,
               start =
                   c(as.numeric(format(fulldates[1],format = '%Y')),
                     as.numeric(format(fulldates[1],format = '%m'))),
               frequency = 12)
        

    



    
    pdf(paste(filename,'.pdf',sep = ''),width = 6.5, height = 8)

    ## plotting, could be adjusted for nv
    par(mfrow = c(5,2),col.lab="black",col.main="black",
        oma=c(4,4,4,4), mar=c(2,2,2,2), tcl=-0.1, mgp=c(0,0,0))

    for (iv in 1:nx){
        plot(
            ## ts(tsdata[fullrange,iv],start = c(2006,9),frequency = 12),
            ytrim[,iv],
            type = 'l', lwd = 1.5,
            ylab = '',xlab = '',
            ylim = ymat[,iv])
        title(main = vnames[iv])

        grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
                  lwd = 1, equilogs = TRUE)
        
        for (idate in 1:nd){
            lines(fclist[[idate]][,iv], lwd = .75, col = 'red')
        }
    }


    dev.off()
}
