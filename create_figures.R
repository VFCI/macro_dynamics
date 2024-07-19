# Figures -----------------------------------------------------------------
library(dplyr)
library(tidyquant)
library(zoo)
library(dint)
library(ggplot2)
library(paletteer)
output_dir <- "output/baseline/figures/"
colors <- c("#4E8542","#2166AC","#8E0152","darkorange")
vfci_color <- paletteer_d("ggthemes::excel_Ion")[1] #colors[1]
fig_width <- 5 #in inches
fig_height <- 5 #in inches
base::load(file = here::here("variables.RData"))

## Standardized FCI ---------------------------------------
# pick dates, variables
date_begin <- "1970 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr,qtr, nfci,gsfci,vixcls) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end) 

tibble::add_column(variables_fig, vfci_new = (variables_fig %>%  select(yr,qtr, nfci,gsfci,vixcls)) )

p <- ggplot(variables_fig, aes(qtr)) + 
  geom_line(aes(y=scale(nfci),colour="NFCI"),na.rm=FALSE) +
  geom_line(aes(y=scale(gsfci),colour="GSFCI"),na.rm=FALSE) +
  geom_line(aes(y=scale(vixcls),colour="VIX"),na.rm=FALSE) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  ) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.88),
    legend.direction="vertical",
    axis.title.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  ) + 
  scale_color_manual(values=colors[1:3]) +
  ylim(-2, 6)
p %>% print

fname <- here::here(paste0(output_dir,"FCI_std.png"))
ggsave(fname, width = fig_width, height = fig_height)


##  Figure. VFCI ----------------------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr,qtr, vfci) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE)  +
  scale_fill_hue(l=40) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  ) +
  ylim(-1, 1.5) +
  theme_classic() +
  theme(
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  ) +
  scale_color_manual(values=vfci_color)
p %>% print

fname <- here::here(paste0(output_dir,"just_vfci.png"))
ggsave(fname, width = fig_width, height = fig_height)

## Conditional mean and variance --------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr, qtr, vfci, mu) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(x=vfci,y=mu)) +
  geom_point(aes(color="Fitted values"),shape="diamond",size=3) +
  geom_smooth(aes(color="OLS line"),method=lm, se=FALSE,linewidth=0.5) +
  ylab("Conditional Mean of GDP Growth") +
  xlab('Conditional Volatility of GDP Growth (log)')  +
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
    legend.direction="vertical"
  ) +
  ylim(-1.5, 1.5)+
  xlim(-0.5, 1.25)
p %>% print

fname <- here::here(paste0(output_dir,"musigma_gdp.png"))
ggsave(fname, width = fig_width, height = fig_height)

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
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE) +
  geom_line(aes(y=vfci_pce,colour="PCE-VFCI"),na.rm=FALSE,linetype="longdash") +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  )  +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.12,0.92),
    legend.direction="vertical",
    axis.title.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )  +
  ylim(-1, 1.5) +
  scale_color_manual(breaks = c("VFCI","PCE-VFCI"),
                     values=c(vfci_color, colors[1]),
                     labels=c("VFCI","PCE-VFCI")
  )
p %>% print

fname <- here::here(paste0(output_dir,"vfci_gdp_and_pce.png"))
ggsave(fname, width = fig_width, height = fig_height)

## PCA and Indiv VFCI ------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::inner_join(results$vfci_ind, by="qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ind) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE) +
  geom_line(aes(y=vfci_ind,colour="Individual-VFCI"),na.rm=FALSE,linetype="longdash") +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  ) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.15,0.91),
    legend.direction="vertical",
    axis.title.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )  +
  ylim(-1, 2) +
  scale_color_manual(breaks = c("VFCI","Individual-VFCI"),
                     values=c(vfci_color, colors[1]),
                     labels=c("VFCI","Individual-VFCI")
  )
p %>% print

fname <- here::here(paste0(output_dir,"vfci_gdp_and_indiv.png"))
ggsave(fname, width = fig_width, height = fig_height)

## US and EA VFCI ----------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig  <- variables %>%
  dplyr::left_join(results$vfci_ea, by="qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ea) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE) +
  geom_line(aes(y=vfci_ea,colour="EA-VFCI"),na.rm=FALSE,linetype="longdash") + 
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  )  +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.12,0.91),
    legend.direction="vertical",
    axis.title.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )  +
  ylim(-1, 3.5) +
  scale_color_manual(breaks = c("VFCI","EA-VFCI"),
                     values=c(vfci_color, colors[1]),#c("blue","red"),
                     labels=c("VFCI","EA-VFCI")
  )
p %>% print

fname <- here::here(paste0(output_dir,"us_ea_vfci.png"))
ggsave(fname, width = fig_width, height = fig_height)