library(ggplot2)
library(data.table)
library(patchwork)

## Settings
output_dir <- "output/baseline/figures/"
appendix_dir <- "output/appendix/figures/"
colors <- c("#4E8542","#2166AC","#8E0152","darkorange","darkgoldenrod3", "#cca5d6")
vfci_color <- paletteer_d("ggthemes::excel_Ion")[1] #colors[1]
fig_width <- 8  #in inches
fig_height <- 5 #in inches

## Load data
base::load("./output/svar_internal_vfci.Rdata")
base::load("./output/svariv_lpiv_chol_sn_baseline.Rdata")

## VFCI and Internal VFCI ----------------------------------------------
date_begin <- "1960 Q1"
date_end <- "2022 Q3"

variables_fig  <-
  comp_vfci_int_vfci_data |>
  mutate(qtr = tsibble::yearquarter(zoo::as.yearqtr(date))) |>
  tsibble::as_tsibble(index = qtr) |>
  tsibble::filter_index(date_begin ~ date_end)

p <-
  variables_fig |>
  ggplot(aes(
    x = qtr
  )) +
  geom_line(aes(y = scale(vfci), colour = "VFCI" ), na.rm = FALSE, size=1.25) +
  geom_line(aes(y = scale(int_vfci), colour = "Internal VFCI"), na.rm = FALSE, linetype = "longdash", size = 1.25) +
  tsibble::scale_x_yearquarter(
    name = "",
    date_labels="%Y-q%q",
    breaks = variables_fig$qtr[seq(1, length(variables_fig$qtr), 40)]
  )  +
  ylab("Normalized index") +
  ylim(-2, 5) +
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
  scale_color_manual(
    breaks = c("VFCI", "Internal VFCI"),
    values = c(vfci_color, colors[6]),
    labels = c(
      "VFCI",
      "Internal VFCI"
    )
  )

p |> print()

fname <- here::here(paste0(output_dir, "vfci_int_vfci.png"))
cowplot::save_plot(fname, p, base_width = fig_width, base_height = fig_height)


## VAR IRF Comparison ----------------------------------------------

target_order <- c("vfci", "int_vfci", "fedfunds", "lgdp", "lpce")
target_label <- c("VFCI", "Internal VFCI", "Federal Funds Rate", "Log Real GDP", "Log Core PCE")

p_iv_int_vfci <- 
  df_irf_iv_int_vfci |>
  as.data.table() |>
  _[, target := factor(target, levels = target_order, labels = paste0("Response of ", target_label), ordered = TRUE)] |>
  ggplot(aes(
    x = horizon
  )) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + 
  geom_ribbon(aes(ymin = response.lower, ymax = response.upper), fill = colors[6], alpha = 0.25) +
  geom_ribbon(aes(ymin = response.lower.68, ymax = response.upper.68), fill = colors[6], alpha = 0.5) +
  geom_line(aes(y = response), color = colors[6]) +
  facet_wrap(vars(target), ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 19), breaks = seq(4, 19, 5), labels = seq(5, 20, 5), expand = c(0,0)) + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.direction="vertical",
    axis.title.y = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    plot.title = element_text(size = 12)
  ) 

p_chol_ext_vfci <- 
  chol_irf_vfci |>
  as.data.table() |>
  _[shock == "vfci"] |>
  _[, target := factor(target, levels = target_order, labels = paste0("Response of ", target_label), ordered = TRUE)] |>
  setorder(target) |>
  ggplot(aes(
    x = horizon
  )) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + 
  geom_ribbon(aes(ymin = response.lower, ymax = response.upper), fill = colors[6], alpha = 0.25) +
  geom_ribbon(aes(ymin = response.lower.68, ymax = response.upper.68), fill = colors[6], alpha = 0.5) +
  geom_line(aes(y = response), color = colors[6]) +
  facet_wrap(vars(target), ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 19), breaks = seq(4, 19, 5), labels = seq(5, 20, 5), expand = c(0,0)) + 
  labs(x = NULL, y = NULL, title = "External VFCI") +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.direction="vertical",
    axis.title.y = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    plot.title = element_text(size = 12)
  )

p_chol_int_vfci_last <- 
  df_irf_chol_int_vfci_last |>
  as.data.table() |>
  _[, target := factor(target, levels = target_order, labels = paste0("Response of ", target_label), ordered = TRUE)] |>
  ggplot(aes(
    x = horizon
  )) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + 
  geom_ribbon(aes(ymin = response.lower, ymax = response.upper), fill = colors[6], alpha = 0.25) +
  geom_ribbon(aes(ymin = response.lower.68, ymax = response.upper.68), fill = colors[6], alpha = 0.5) +
  geom_line(aes(y = response), color = colors[6]) +
  facet_wrap(vars(target), ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 19), breaks = seq(4, 19, 5), labels = seq(5, 20, 5), expand = c(0,0)) + 
  labs(x = NULL, y = NULL, title = "Internal VFCI") +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    legend.direction="vertical",
    axis.title.y = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    plot.title = element_text(size = 12)
  ) 

filler_p <- ggplot() + geom_blank() + theme_classic() + theme_void() + labs(title = "IV, Internal VFCI") + theme(plot.title = element_text(size = 12))

p <-
  (p_chol_ext_vfci |
  p_chol_int_vfci_last )

p |> print()

fname <- here::here(paste0(appendix_dir, "comp_irf_vfci_int_vfci.pdf"))
cowplot::save_plot(fname, p, base_width = 6, base_height = 8)

