# Figures -----------------------------------------------------------------
library(dplyr)
library(tidyquant)
library(zoo)
library(dint)
library(ggplot2)
library(paletteer)
output_dir <- "output/baseline/figures/"
colors <- c("#4E8542", "#2166AC", "#8E0152", "darkorange", "darkgoldenrod3", "cyan3")
vfci_color <- paletteer_d("ggthemes::excel_Ion")[1] # colors[1]
fig_width <- 5.5 # in inches
fig_height <- fig_width / 1.618 # in inches
base::load(file = here::here("variables.RData"))

## Declare Custom GGplot elements (theme, date breaks, etc) for plots
update_geom_defaults("line", list(linewidth = 0.8)) ## Default is 0.5

custom_theme <-
  theme_classic(base_size = 11) + ## Base font size, ggplot2 defaults to 11
  theme(
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

custom_zero_line <-
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25)

custom_date_breaks <-
  as.yearqtr(seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"))

custom_scale_dates <-
  tsibble::scale_x_yearquarter(
    name = NULL,
    date_labels = "%Y Q%q",
    breaks = custom_date_breaks
  )

custom_legend_position <- c(0.025, 0.975)

## Standardized FCI ---------------------------------------
# pick dates, variables
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr, qtr, nfci, gsfci, vixcls, vfci) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

tibble::add_column(variables_fig, vfci_new = (variables_fig %>% select(yr, qtr, nfci, gsfci, vixcls, vfci)))

p <- ggplot(variables_fig, aes(qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(nfci), colour = "NFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(gsfci), colour = "GSFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vixcls), colour = "VIX"), na.rm = FALSE) +
  ylab("Normalized index") +
  custom_scale_dates +
  custom_theme +
  theme(
    legend.position = custom_legend_position,
    legend.direction = "horizontal"
  ) +
  scale_color_manual(
    breaks = c("NFCI", "GSFCI", "VIX", "VFCI"),
    values = c(colors[1:2], colors[4], "gray80"),
    labels = c("NFCI", "GSFCI", "VIX", "VFCI")
  ) +
  ylim(-2, 6)

fname <- here::here(paste0(output_dir, "FCI_std.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)

##  Figure. VFCI ----------------------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr, qtr, vfci) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <-
  ggplot(as.data.frame(variables_fig), aes(qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  custom_scale_dates +
  ylab("Normalized index") +
  ylim(-2, 5) +
  custom_theme +
  theme(
    legend.position = "none"
  ) +
  scale_color_manual(values = vfci_color)


fname <- here::here(paste0(output_dir, "just_vfci.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)

## Conditional mean and variance --------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(yr, qtr, vfci, mu) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(x = vfci, y = mu)) +
  custom_zero_line +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) + 
  geom_point(aes(color = "Fitted values"), shape = "diamond", size = 3) +
  geom_smooth(aes(color = "OLS line"), method = lm, se = FALSE, linewidth = 1) +
  ylab("Conditional mean of GDP growth") +
  xlab("Conditional volatility of GDP growth (log)") +
  scale_color_manual(values = c("Azure4", vfci_color)) +
  guides(color = guide_legend(
    override.aes = list(
      shape = c(18, NA),
      size = c(3, 0)
    )
  )) +
  custom_theme +
  theme(
    legend.position = c(0.7, 0.9)
  )

fname <- here::here(paste0(output_dir, "musigma_gdp.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## GDP and PCE VFCI ---------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

vfci_cons <- dplyr::select(results$fgr1.pcecc96$ts, c("qtr", "vfci")) %>% dplyr::rename(vfci_pce = vfci)

variables_fig <- variables %>%
  dplyr::inner_join(vfci_cons, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_pce) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end) %>%
  dplyr::mutate(date = as.Date(qtr))

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vfci_pce), colour = "PCE-VFCI"), na.rm = FALSE, linetype = "longdash") +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    breaks = c("VFCI", "PCE-VFCI"),
    values = c(vfci_color, colors[1]),
    labels = c("VFCI", "Consumption-VFCI")
  )

fname <- here::here(paste0(output_dir, "vfci_gdp_and_pce.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)



## PCA and Indiv VFCI ------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::inner_join(results$vfci_ind, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ind) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(
    aes(y = scale(vfci_ind), colour = "Individual-VFCI"),
    na.rm = FALSE, linetype = "longdash"
  ) +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 6) +
  scale_color_manual(
    breaks = c("VFCI", "Individual-VFCI"),
    values = c(vfci_color, colors[5]),
    labels = c("VFCI", "VFCI using all financial variables")
  )

fname <- here::here(paste0(output_dir, "vfci_gdp_and_indiv.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)

## US and EA VFCI ----------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_ea, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ea) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vfci_ea), colour = "EA-VFCI"), na.rm = FALSE, linetype = "longdash") +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    breaks = c("VFCI", "EA-VFCI"),
    values = c(vfci_color, colors[2]), # c("blue","red"),
    labels = c("VFCI United States", "VFCI Euro Area")
  )

fname <- here::here(paste0(output_dir, "us_ea_vfci.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI and VFCI-Yields ----------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_yields, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_yields) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vfci_yields), colour = "VFCI-Yields"), na.rm = FALSE, linetype = "longdash") +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_scale_dates +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    breaks = c("VFCI", "VFCI-Yields"),
    values = c(vfci_color, colors[6]),
    labels = c("VFCI", "VFCI with Yields")
  )

fname <- here::here(paste0(output_dir, "vfci_yields.svg"))
ggsave(fname, width = fig_width, height = fig_height)



## VFCI and Lags ----------------------------------------------
## VFCI and Lags in mean --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_lags_in_mean$ts, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_lags_in_mean) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vfci_lags_in_mean), colour = "VFCI-lags-in-mean"), na.rm = FALSE, linetype = "longdash") +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    breaks = c("VFCI", "VFCI-lags-in-mean" ),
    values = c(vfci_color, "gray30"),
    labels = c("VFCI", "VFCI with lags in mean")
  )

fname <- here::here(paste0(output_dir, "vfci_lags_in_mean.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI and Lags in vol --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_lags_in_vol$ts, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_lags_in_vol) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vfci_lags_in_vol), colour = "VFCI-lags-in-vol"), na.rm = FALSE, linetype = "longdash") +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    breaks = c("VFCI", "VFCI-lags-in-vol"),
    values = c(vfci_color, "gray70"),
    labels = c("VFCI", "VFCI with lags in volatility")
  )

fname <- here::here(paste0(output_dir, "vfci_lags_in_vol.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)

## VFCI and Lags in mean and vol --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_lags$ts, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_lags) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(as.data.frame(variables_fig), aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), colour = "VFCI"), na.rm = FALSE) +
  geom_line(aes(y = scale(vfci_lags), colour = "VFCI-lags"), na.rm = FALSE, linetype = "longdash") +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    breaks = c("VFCI", "VFCI-lags"),
    values = c(vfci_color, "gray50"),
    labels = c("VFCI",  "VFCI with lags")
  )

fname <- here::here(paste0(output_dir, "vfci_lags.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI with all PCs --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

pcs_df <- results_pcs |>
  purrr::map(~ .x$ts) |>
  purrr::list_rbind(names_to = "pc") |>
  dplyr::mutate(pc = as.numeric(pc)) |>
  dplyr::group_by(pc) |>
  dplyr::mutate(vfci_scaled = scale(vfci)) |>
  ungroup()

variables_fig <- pcs_df

p <- ggplot(variables_fig, aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = vfci_scaled, color = as.factor(pc))) +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 6) +
  scale_color_manual(
    values = c("#dadaeb", "#bcbddc", "#9e9ac8", vfci_color, "#756bb1", "#4a1486"),
    labels = 1:6,
    name = "PCs"
  )

fname <- here::here(paste0(output_dir, "vfci_pcs.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI with some PCs --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- pcs_df |>
  dplyr::filter(pc %in% 3:5)

p <- ggplot(variables_fig, aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = vfci_scaled, color = as.factor(pc))) +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 6) +
  scale_color_manual(
    values = c("#9e9ac8", vfci_color, "#756bb1" ),
    labels = 3:5,
    name = "PCs"
  )

fname <- here::here(paste0(output_dir, "vfci_some_pcs.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI compared to total and residual log vol --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::select(qtr, vfci, total_log_vol, resid_log_vol) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(variables_fig, aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), color = "VFCI")) +
  geom_line(aes(y = scale(total_log_vol), color = "Total Log Vol")) +
  geom_line(aes(y = scale(resid_log_vol), color = "Residual Log Vol"))+
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-4, 5) +
  scale_color_manual(
    values = c(vfci_color, "#41b6c4", "#c7e9b4"),
    labels = c("VFCI", "Total Log Vol", "Residual Log Vol"),
    breaks = c("VFCI", "Total Log Vol", "Residual Log Vol")
  )

fname <- here::here(paste0(output_dir, "vfci_total_and_resid_vol.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI compared Stocks VFCI --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_stocks, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_stocks) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(variables_fig, aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), color = "VFCI")) +
  geom_line(aes(y = scale(vfci_stocks), color = "VFCI - Equity Only"), alpha = 0.8) +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-3, 5) +
  scale_color_manual(
    values = c(vfci_color, "#41b6c4"),
    labels = c("VFCI", "VFCI - Equity Only"),
    breaks = c("VFCI", "VFCI - Equity Only")
  )

fname <- here::here(paste0(output_dir, "vfci_and_vfci_stocks.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)


## VFCI compared Rets VFCI --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_ret, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_ret) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(variables_fig, aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), color = "VFCI")) +
  geom_line(aes(y = scale(vfci_ret), color = "VFCI - Returns Only"), alpha = 0.8) +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-3, 5) +
  scale_color_manual(
    values = c(vfci_color, "#6977de"),
    labels = c("VFCI", "VFCI - Returns Only"),
    breaks = c("VFCI", "VFCI - Returns Only")
  )

fname <- here::here(paste0(output_dir, "vfci_and_vfci_rets.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)

## VFCI compare without Realized Vol --------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_no_rvol, by = "qtr") %>%
  dplyr::select(yr, qtr, vfci, vfci_no_rvol) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

p <- ggplot(variables_fig, aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = scale(vfci), color = "VFCI")) +
  geom_line(aes(y = scale(vfci_no_rvol), color = "VFCI - no Realized Vol")) +
  custom_scale_dates +
  ylab("Normalized index") +
  custom_theme +
  theme(
    legend.position = custom_legend_position
  ) +
  ylim(-2, 5) +
  scale_color_manual(
    values = c(vfci_color, "#41b6c4"),
    labels = c("VFCI", "VFCI - no Realized Vol"),
    breaks = c("VFCI", "VFCI - no Realized Vol")
  )

fname <- here::here(paste0(output_dir, "vfci_and_vfci_no_rvol.svg"))
ggsave(fname, p, width = fig_width, height = fig_height)



## Figure Comparing all VFCI series

pcs_wide <-
  pcs_df |>
  dplyr::filter(pc %in% c(3, 5)) |>
  dplyr::mutate(name = paste0("vfci_pc", pc)) |>
  dplyr::select(qtr, name, vfci) |>
  tidyr::pivot_wider(names_from = "name", values_from = vfci)

variables_fig <- variables %>%
  dplyr::left_join(results$vfci_lags$ts, by = "qtr") %>%
  dplyr::left_join(results$vfci_lags_in_vol$ts, by = "qtr") %>%
  dplyr::left_join(results$vfci_lags_in_mean$ts, by = "qtr") %>%
  dplyr::left_join(results$vfci_ea, by = "qtr") %>%
  dplyr::left_join(results$vfci_ind, by = "qtr") %>%
  dplyr::left_join(vfci_cons, by = "qtr") %>%
  dplyr::left_join(results$vfci_stocks, by = "qtr") %>%
  dplyr::left_join(results$vfci_ret, by = "qtr") %>%
  dplyr::left_join(pcs_wide, by = "qtr") %>%
  dplyr::left_join(results$vfci_yields, by = "qtr") %>%
  dplyr::left_join(results$vfci_no_rvol, by = "qtr") %>%
  dplyr::select(qtr, vfci, vfci_no_rvol, vfci_pc3, vfci_pc5, vfci_lags, vfci_yields, vfci_lev, vfci_ind, vfci_pce) %>%
  tsibble::as_tsibble() %>%
  tsibble::filter_index(date_begin ~ date_end)

labels <-
  c(
    vfci_pce = "VFCI using consumption growth",
    vfci_lev = "exp(VFCI)",
    vfci_pc3 = "VFCI using 3 PCs of financial variables",
    vfci_pc5 = "VFCI using 5 PCs of fianncial variables",
    vfci_ind = "VFCI using all financial variables",
    vfci_lags = "VFCI with four lags of real GDP growth",
    vfci_yields = "VFCI with contemperaneous yields at 1, 5, and 10 year maturity",
    vfci_no_rvol = "VFCI no realized volatility"
  )

p <- 
  variables_fig |>
  mutate(vfci = scale(vfci)) |>
  tidyr::pivot_longer(-c(qtr, vfci)) |>
  group_by(name) |>
  mutate(value_scaled = scale(value)[, 1]) |>
  mutate(name = factor(name, levels = names(labels), labels = labels, ordered = TRUE)) |>
  ggplot(aes(x = qtr)) +
  custom_zero_line +
  geom_line(aes(y = vfci), color = vfci_color) +
  geom_line(aes(y = value_scaled), color = "steelblue", alpha = 0.7) +
  #facet_grid(rows = vars(name), scales = "free_y") +
  facet_wrap(vars(name), scales = "free_y", ncol = 1) +
  custom_scale_dates +
  custom_theme +
  labs(
    y = "Normalized Index"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x.top = element_text(angle = 0, hjust = 0.5, color = "steelblue"),
    strip.placement = "outside"
  )

p

fname <- here::here(paste0("output/appendix/figures/", "compare_vfci_time_series.svg"))
ggsave(fname, p, width = 5.5, height = 8)
