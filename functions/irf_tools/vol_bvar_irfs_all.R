impulseplots = function(ir, ## array of impulse response objects, nvar x nshock x nperiod
                        irdraws = NULL, ## nvar x nshock x nperiod x ndraw
                        conf = c(.68,.90), ## what confidence bands to put on the plot
                        blocks = NULL,
                        filename = 'impulse_plot',
                        format = 'svg',
                        addtitle = NULL,
                        nsteps = NULL, ## number of steps to use
                        shocknames = NULL, ## names of the shocks to print as titles
                        varnames = rep('',dim(ir)[1]), ## names of the response variables
                        ptype = NULL,
                        color = c(0, 0, 1), ## base color (default blue)
                        alpha = c(0.5,0.3), ## how much alpha for each conf band color
                        gr = 0.7, ## what shade of grey
                        width = 5, ## inches
                        height = 8,
                        response_ylim = NULL,
                        xtick = NULL,
                        ylims = NULL
                        ) {

    fname = paste0(filename, '.', format)   
    color = rgb(color[1], color[2], color[3])

    ir
    irdraws

    irq = apply(irdraws[,,1:nsteps,],1:3,quantile,
                    ##probs = c(rev((1 - conf) / 2), .5 + conf/2))
                    probs = c((1-conf)/2,0.5 + conf/2))
    irq = aperm(irq,perm=c(2,3,4,1))

    dimnames(ir)[[1]] <- var_names
    dimnames(ir)[[2]] <- shock_names
    dimnames(ir)[[3]] <- 1:20
    df <- as.data.frame.table(ir) |> dplyr::as_tibble()
    colnames(df) <- c("variable", "shock", "period", "irf")

    dimnames(irq)[[1]] <- var_names
    dimnames(irq)[[2]] <- shock_names
    dimnames(irq)[[3]] <- 1:20
    dimnames(irq)[[4]] <- c("lb1", "lb2", "ub1", "ub2")
    dfq <- as.data.frame.table(irq) |> dplyr::as_tibble()
    colnames(dfq) <- c("variable", "shock", "period", "quantile", "irf")

    dfq_wide <- dfq |> tidyr::pivot_wider(names_from = quantile, values_from = irf, names_prefix = "irf_")

    df <- dplyr::full_join(df, dfq_wide, by = c("variable", "shock", "period"))

    df$period <- as.numeric(df$period)

    p <-
        df |>
        ggplot(aes(
            x = period,
            y = irf
        )) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
        geom_line(color = color) +
        geom_ribbon(aes(ymin = irf_lb2, ymax = irf_ub2), alpha = 0.25, fill = color) +
        geom_ribbon(aes(ymin = irf_lb1, ymax = irf_ub1), alpha = 0.5, fill = color) +
        scale_x_continuous(expand = c(0,0)) + 
        scale_y_continuous(expand = c(0,0), limits = ylims) +
        facet_grid(rows = vars(variable), cols = vars(shock), switch = "y", scales = "free_y") +
        theme_classic(base_size = 11) + ## Base font size, ggplot2 defaults to 11
        labs(
            x = NULL,
            y = NULL
        ) +
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
        ) +
        theme(
            strip.placement = "outside",
            strip.background = element_blank(),
            strip.text.y.left = element_text(angle = 90)
        )

    ggsave(fname, p, width = width, height = height)

}