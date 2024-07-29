# Figures -----------------------------------------------------------------
library(dplyr)
library(tidyquant)
library(zoo)
library(dint)
library(ggplot2)
library(paletteer)
output_dir <- "output/baseline/figures/"
colors <- c("#4E8542","#2166AC","#8E0152","darkorange","darkgoldenrod3")
vfci_color <- paletteer_d("ggthemes::excel_Ion")[1] #colors[1]
fig_width <- 8 #in inches
fig_height <- 5 #in inches
base::load(file = here::here("variables.RData"))

## Standardized FCI ---------------------------------------
# pick dates, variables
date_begin <- "1970 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr,qtr, nfci,gsfci,vixcls, vfci) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end) 

tibble::add_column(variables_fig, vfci_new = (variables_fig %>%  select(yr,qtr, nfci,gsfci,vixcls, vfci)) )

p <- ggplot(variables_fig, aes(qtr)) + 
  geom_line(aes(y=scale(nfci),colour="NFCI"),na.rm=FALSE,size=1.25) +
  geom_line(aes(y=scale(gsfci),colour="GSFCI"),na.rm=FALSE,size=1.25) +
  geom_line(aes(y=scale(vixcls),colour="VIX"),na.rm=FALSE,size=1.25) +
  geom_line(aes(y=scale(vfci),colour="VFCI"),na.rm=FALSE,alpha=0.2,size=1.75) +
  ylab("Normalized index") +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  ) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.25, 0.93),
    legend.direction="horizontal",
    legend.location = "plot",
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) + 
  scale_color_manual(breaks = c("NFCI","GSFCI","VIX","VFCI"),
                     values=c(colors[1:2],colors[4], "black"),
                     labels=c("NFCI","GSFCI","VIX","VFCI"))+
  ylim(-2,6)
p %>% print

fname <- here::here(paste0(output_dir,"FCI_std.png"))
#ggsave(fname, width = fig_width, height = fig_height)
cowplot::save_plot(fname, p,base_width = fig_width, base_height = fig_height)

##  Figure. VFCI ----------------------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr,qtr, vfci) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE,size=1.25)  +
  scale_fill_hue(l=40) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  ) +
  ylab("Normalized index") +
  ylim(-0.75, 1.25) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    legend.position="none",
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_color_manual(values=vfci_color)
p %>% print

fname <- here::here(paste0(output_dir,"just_vfci.png"))
cowplot::save_plot(fname, p, base_width = fig_width, base_height = fig_height)

## Conditional mean and variance --------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr, qtr, vfci, mu) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(x=vfci,y=mu)) +
  geom_point(aes(color="Fitted values"),shape="diamond",size=3) +
  geom_smooth(aes(color="OLS line"),method=lm, se=FALSE,linewidth=1) +
  ylab("Conditional mean of GDP growth") +
  xlab('Conditional volatility of GDP growth (log)')  +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)
  ) +
  scale_color_manual(values=c("Azure4",vfci_color)) +
  guides(color = guide_legend(
    override.aes = list(
      shape = c(18,NA),
      size=c(3,0)
    )
  )
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.85,0.9),
    legend.direction="vertical",
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  ) 
p %>% print

fname <- here::here(paste0(output_dir,"musigma_gdp.png"))
#ggsave(fname, width = fig_width, height = fig_height)
cowplot::save_plot(fname, p, base_width = fig_width, base_height = fig_height)


## GDP and PCE VFCI ---------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

vfci_cons <- dplyr::select(results$fgr1.pcecc96$ts, c("qtr","vfci")) %>% dplyr::rename(vfci_pce = vfci)

variables_fig <- variables %>%
  dplyr::inner_join(vfci_cons,by="qtr") %>%
  dplyr::select(yr,qtr, vfci, vfci_pce) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end) %>% 
  dplyr::mutate(date=as.Date(qtr))

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE,size=1.25) +
  geom_line(aes(y=vfci_pce,colour="PCE-VFCI"),na.rm=FALSE,linetype="longdash",size=1.25) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  )  +
  ylab("Normalized index") +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.17,0.92),
    legend.direction="vertical",
    axis.title.y = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  )  +
  ylim(-1.1,1.3) +
  scale_color_manual(breaks = c("VFCI","PCE-VFCI"),
                     values=c(vfci_color, colors[1]),
                     labels=c("VFCI","Consumption-VFCI")
  )
p %>% print

fname <- here::here(paste0(output_dir,"vfci_gdp_and_pce.png"))
#ggsave(fname, width = fig_width, height = fig_height)
cowplot::save_plot(fname, p, base_width = fig_width, base_height = fig_height)



## PCA and Indiv VFCI ------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::inner_join(results$vfci_ind, by="qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ind) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE,linewidth=1.25) +
  geom_line(
    aes(y=vfci_ind,colour="Individual-VFCI"),
            na.rm=FALSE,linetype="longdash",linewidth=1.25
  ) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  ) +
  ylab("Normalized index") +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.25,0.9),
    legend.direction="vertical",
    axis.title.y = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  )  +
  ylim(-1.2, 2) +
  scale_color_manual(breaks = c("VFCI","Individual-VFCI"),
                     values=c(vfci_color, colors[5]),
                     labels=c("VFCI","VFCI using all financial variables")
  )
p %>% print

fname <- here::here(paste0(output_dir,"vfci_gdp_and_indiv.png"))
#ggsave(fname, width = fig_width, height = fig_height)
cowplot::save_plot(fname, p, base_width = fig_width, base_height = fig_height)

## US and EA VFCI ----------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig  <- variables %>%
  dplyr::left_join(results$vfci_ea, by="qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ea) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE,size=1.25) +
  geom_line(aes(y=vfci_ea,colour="EA-VFCI"),na.rm=FALSE,linetype="longdash",size=1.25) + 
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  )  +
  ylab("Normalized index") +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.18,0.9),
    legend.direction="vertical",
    axis.title.y = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  )  +
  ylim(-1, 3) +
  scale_color_manual(breaks = c("VFCI","EA-VFCI"),
                     values=c(vfci_color, colors[2]),#c("blue","red"),
                     labels=c("VFCI United States","VFCI Euro Area")
  )
p %>% print

fname <- here::here(paste0(output_dir,"us_ea_vfci.png"))
#ggsave(fname, width = fig_width, height = fig_height)
cowplot::save_plot(fname, p, base_width = fig_width, base_height = fig_height)

