plotrmse = function(sdate, rmse1, rmse2, rmse3, filename,
    vname = rep(NULL,nv), ih = 1, height = 8, width = 6.5, diff = FALSE,
    rshade = TRUE){

    nv = dim(rmse1)[2]

    pdf(paste(filename,'.pdf',sep = ''),height = 8, width = 6.5)
    
    par(mfrow = c(nv,1),col.lab="black",col.main="black",
        oma=c(1,3,1,2), mar=c(2,1,1,1), tcl=-0.1, mgp=c(0,0,0))

    nrmse = c(1,6,12,24,48) ## number of months
    nrmse = nrmse[ih]
    ## snip these off the end
    T = dim(rmse1)[3]

    irange = 1:(T-nrmse)

    ## rmse1 = rmse1[,,1:(T-nrmse)]
    ## rmse2 = rmse1[,,1:(T-nrmse)]
    ## rmse3 = rmse1[,,1:(T-nrmse)]

    if (rshade) {
        ## gen list of recessions
        rlist = c(1980, 1980 + 6/12,
                  1981 + 6/12,  1982 + 10/12,
                  1990 + 6/12, 1991 + 2/12,
                  2001 + 2/12, 2001 + 10/12,
                  2007 + 11/12, 2009 + 5/12)
        rlist = matrix(rlist,2,5)
    }


    for (iv in 1:nv){
        ## plot each variable

        if (diff){
            ## difference mode

            drmse = rmse1[ih,iv,] - rmse2[ih,iv,]

            plot(ts(drmse,sdate,frequency = 12), type = 'l', lwd = 1,
                 ylab = NULL, xlab = NULL)
            abline(a = 0, b = 0, lwd = 0.75) ## axis
            grid(lwd = .5)

        } else {
            ##ymin = min(rmse1[ih,iv,],rmse2[ih,iv,])
            ymin = 0
            ymax = max(rmse1[ih,iv,],rmse2[ih,iv,]) * 1.1 ## breathing room

            ## plot(ts(rmse1[ih,iv,],sdate,frequency=12), type = 'l', lwd = .75, col = 'blue',
            ##      ylab = NULL, xlab = NULL, bty = 'l',ylim = c(ymin,ymax))

            plot(ts(rmse1[ih,iv,],sdate,frequency=12), type = 'n', lwd = .75, col = 'blue',
                 ylab = NULL, xlab = NULL, bty = 'l',ylim = c(ymin,ymax), panel.first = {
                     grid(lwd = 1)
                 })

            if (rshade){
                for (ir in 1:5){

                    polygon(c(rlist[,ir],rev(rlist[,ir])),
                            c(-1e10,-1e10,1e10,1e10),
                            col = 'grey',border = NA)
                }
            }

            lines(ts(rmse1[ih,iv,irange],sdate,frequency=12), lwd = .75, col = 'blue')
            lines(ts(rmse2[ih,iv,irange],sdate,frequency=12),lwd = .75, col = 'red')
            lines(ts(rmse3[ih,iv,irange],sdate,frequency=12),lwd = .75, col = 'green4')

            ## abline(a = 0, b = 0, lwd = 0.75) ## axis
            

        }


        title(main = vname[iv])

        
    }

    dev.off()
}
