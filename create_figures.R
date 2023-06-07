# Figures -----------------------------------------------------------------

library(dplyr)
library(tidyquant)
library(zoo)
library(dint)
library(ggplot2)

## Figure 1. Standardized FCI figure ---------------------------------------
# pick dates, variables

base::load(file = here::here("variables.RData"))
date_begin <- "1970 Q1"
date_end <- "2022 Q3"

variables_fig1 <- variables %>%
  dplyr::select(yr,qtr, nfci, gsfci, vixcls) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(variables_fig1, aes(qtr)) +
  geom_line(aes(y=scale(nfci),colour="NFCI"),na.rm=FALSE) +
  geom_line(aes(y=scale(gsfci),colour="GSFCI"),na.rm=FALSE) +
  geom_line(aes(y=scale(vixcls),colour="VIX"),na.rm=FALSE) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q"
  ) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.5,0.95),
    legend.direction="horizontal",
    axis.title.y=element_blank()
  ) + 
  scale_colour_hue(l = 40) +
  ylim(-2, 6)
p %>% print

##  Figure 2. VFCI ----------------------------------------------------------

base::load(file = here::here("variables.RData"))
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig2 <- variables %>%
  dplyr::select(yr,qtr, vfci) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig2), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE)  +
  scale_fill_hue(l=40) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q"
  ) +
  scale_colour_hue(l = 40) +
  theme_classic() +
  theme(
    axis.title.y=element_blank(),
    legend.position="none"
  )
p %>% print


## Figure 3. Conditional mean and variance --------------------------------
base::load(file = here::here("variables.RData"))
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig3 <- variables %>%
  dplyr::select(yr,qtr, vfci,mu) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig3), aes(x=vfci,y=mu)) +
  geom_point(aes(color="Conditional mean in VFCI regression"),shape="diamond",size=3) +
  geom_smooth(aes(color="Fitted values"),method=lm, se=FALSE,linewidth=0.5) +
  xlab("Conditional Mean of GDP Growth") +
  ylab('Conditional Volatility of GDP Growth (log)')  +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)
  ) +
  scale_color_manual(values=c("Azure4","DarkRed")) +
  guides(color = guide_legend(
    override.aes = list(
      shape = c(18,NA),
      size=c(3,0)
    )
  )
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.4,0.1),
    legend.direction="horizontal"
  )
p %>% print


## Figure 9. GDP and PCE VFCI ---------------------------------------------
base::load(file = here::here("variables.RData"))
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

vfci_cons <- dplyr::select(results$fgr1.pcecc96$ts, c("qtr","vfci")) %>% dplyr::rename(vfci_pce = vfci)

variables_fig9 <- variables %>%
  dplyr::inner_join(vfci_cons,by="qtr") %>%
  dplyr::select(yr,qtr, vfci,vfci_pce) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end) %>% 
  dplyr::mutate(date=as.Date(qtr))

p <- ggplot(as.data.frame(variables_fig9), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE) +
  geom_line(aes(y=vfci_pce,colour="PCE-VFCI"),na.rm=FALSE,linetype="longdash") +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q"
  )  +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.5,0.95),
    legend.direction="vertical",
    axis.title.y=element_blank()
  )  +
  ylim(-1, 1.5) +
  scale_color_manual(breaks = c("VFCI","PCE-VFCI"),
                     values=c("blue","maroon"),
                     labels=c("VFCI","PCE-VFCI")
  )
p %>% print

## Figure 10. PCA and Indiv VFCI ------------------------------------------
base::load(file = here::here("variables.RData"))
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig10 <- variables %>%
  dplyr::inner_join(vfci_ind,by="qtr") %>%
  dplyr::select(yr,qtr, vfci,vfci_ind) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig10), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE) +
  geom_line(aes(y=vfci_ind,colour="Individual-VFCI"),na.rm=FALSE,linetype="longdash") +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q"
  ) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.5,0.95),
    legend.direction="vertical",
    axis.title.y=element_blank()
  )  +
  ylim(-1, 2) +
  scale_color_manual(breaks = c("VFCI","Individual-VFCI"),
                     values=c("blue","darkgreen"),
                     labels=c("VFCI","Individual-VFCI")
  )
p %>% print

## Figure 11. US and EA VFCI ----------------------------------------------
base::load(file = here::here("variables.RData"))
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig11 <- variables %>%
  dplyr::select(yr,qtr, vfci,nfci) %>%
  tsibble::as_tsibble() %>% 
  tsibble::filter_index(date_begin ~ date_end) 

p <- ggplot(as.data.frame(variables_fig11), aes(qtr)) +
  geom_line(aes(y=vfci,colour="VFCI"),na.rm=FALSE) +
  geom_line(aes(y=nfci,colour="EA-VFCI"),na.rm=FALSE,linetype="dashed") +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q"
  )  +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.position = c(0.5,0.95),
    legend.direction="vertical",
    axis.title.y=element_blank()
  )  +
  ylim(-2, 6) +
  scale_color_manual(breaks = c("VFCI","EA-VFCI"),
                     values=c("blue","red"),
                     labels=c("VFCI","EA-VFCI")
  )
p %>% print

