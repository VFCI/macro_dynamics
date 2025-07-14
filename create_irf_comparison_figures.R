library(ggplot2)
library(dplyr)
library(colortools)
library(colorspace)


green_color <- rgb(0,0.6,0)

triadic_colors <- triadic(green_color, plot = FALSE)
tetradic_colors <- tetradic(green_color, plot = FALSE)

## Compare VFCI Series

dt_baseline <- get_irf_data("baseline")

dt_robust <- purrr::list_rbind(list(
  get_irf_data("variation_vfci_pce"),
  get_irf_data("variation_vfci_lev"),
  get_irf_data("variation_vfci_pc3"),
  get_irf_data("variation_vfci_pc5"),
  get_irf_data("variation_vfci_ind"),
  get_irf_data("variation_vfci_lags"),
  get_irf_data("variaton_vfci_yields"),
  get_irf_data("variation_vfci_no_rvol")
))

p <- 
  ggplot() +
  geom_line(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, y = irf, color = type)
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2),
    fill = green_color, alpha = 0.25
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1),
    fill = green_color, alpha = 0.5
  ) +
  geom_line(
    data = dplyr::filter(dt_robust, shock == "VFCI shock"),
    aes(x = period, y = irf, group = type,  color = "robustness"),
    alpha = 0.5
  ) +
  facet_wrap(vars(variable), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(baseline = green_color, robustness = "#990099"),
    labels = c(baseline = "Baseline", robustness = "Robustness")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    color = guide_legend(nrow = 1)
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.5,
    legend.text = element_text(margin = margin(0,40,0,5))
  )

fname <- here::here(paste0("output/appendix/figures/", "compare_vfci_irfs.svg"))
ggsave(fname, p, width = 4.5, height = 4)


## Compare VFCI and VFCI with Returns only

dt <- purrr::list_rbind(list(
  get_irf_data("baseline"),
  get_irf_data("variation_vfci_ret")
))

dt <- dt |>
  dplyr::filter(shock == "VFCI shock") |>
  dplyr::filter(variable %in% c("VFCI", "Fed Funds"))

p <- 
  dt |>
  ggplot() +
  geom_line(aes(x = period, y = irf, color = type)) +
  geom_ribbon(
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2, fill = type),
    alpha = 0.25
  ) +
  geom_ribbon(
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1, fill = type),
    alpha = 0.5
  ) +
  facet_wrap(vars(variable), scales = "free_y", ncol = 1) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(green_color, "#990099", "#990000"),
    labels = c(baseline = "Baseline", variation_vfci_ret = "VFCI with only Returns", variation_vfci_stocks = "VFCI with only Equity")
  ) +
  scale_fill_manual(
    name = "VFCI Version",
    values = c(green_color, "#990099", "#990000"),
    labels = c(baseline = "Baseline", variation_vfci_ret = "VFCI with only Returns", variation_vfci_stocks = "VFCI with only Equity")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "right",
    legend.title = element_text()
  )

fname <- here::here(paste0("output/appendix/figures/", "compare_vfci_ret_irfs.svg"))
ggsave(fname, p, width = 4.5, height = 4)


## Compare Normal Errors

dt <- purrr::list_rbind(list(
  get_irf_data("baseline"),
  get_irf_data("normal")
))

p <- 
  dt |>
  ggplot() +
  geom_line(aes(x = period, y = irf, color = type)) +
  geom_ribbon(
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2, fill = type),
    alpha = 0.25
  ) +
  geom_ribbon(
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1, fill = type),
    alpha = 0.5
  ) +
  facet_grid(rows = vars(variable), cols = vars(shock), scales = "free_y", switch = "y") +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    values = c(green_color, "#990099"),
    labels = c(baseline = "Baseline", normal = "Normal")
  ) +
  scale_fill_manual(
    values = c(green_color, "#990099"),
    labels = c(baseline = "Baseline", normal = "Normal")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_blank()
  )

fname <- here::here(paste0("output/appendix/figures/", "compare_normal_irfs.svg"))
ggsave(fname, p, width = 5.5, height = 5.5 )


## Compare other VAR robustness checks

dt_baseline <- get_irf_data("baseline")

dt_robust <- purrr::list_rbind(list(
  get_irf_data("1M"),
  get_irf_data("pre_crisis"),
  get_irf_data("stationary")
))

## Convert Stationary growth rate IRFs to levels for comparison purposes
dt_robust2 <- dt_robust |>
  group_by(type, shock, variable) |>
  mutate(irf := ifelse(type == "stationary" & variable %in% c("GDP Growth", "PCE Inflation"), cumsum(irf), irf) ) |>
  ungroup() |>
  mutate(variable := ifelse(type == "stationary" & variable == "GDP Growth", "Log Real GDP", as.character(variable) ) ) |>
  mutate(variable := ifelse(type == "stationary" & variable == "PCE Inflation", "Log Core PCE", as.character(variable) ) ) |>
    mutate(variable = factor(as.character(variable), levels = c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds") ))


p <- 
  ggplot() +
  geom_line(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, y = irf, color = type)
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2),
    fill = green_color, alpha = 0.25
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1),
    fill = green_color, alpha = 0.5
  ) +
  geom_line(
    data = dplyr::filter(dt_robust2, shock == "VFCI shock"),
    aes(x = period, y = irf, group = type,  color = type),
    alpha = 1
  ) +
  facet_wrap(vars(variable), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(baseline = green_color, pre_crisis = tetradic_colors[2], `1M` = tetradic_colors[3], stationary = tetradic_colors[4]),
    labels = c(baseline = "Baseline", pre_crisis = "Pre-Crisis", `1M` = "1 Million Draws", stationary = "Stationary"),
    breaks = c("baseline", "1M", "pre_crisis", "stationary")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.5,
    legend.text = element_text(margin = margin(0,40,0,5))
  )


fname <- here::here(paste0("output/appendix/figures/", "compare_vfci_other_robustness_irfs.svg"))
ggsave(fname, p, width = 4.5, height = 4.5)


## Models with GZ, ECY, and TEDR

dt_baseline <- get_irf_data("baseline")

dt_robust <- purrr::list_rbind(list(
  get_irf_data("horserace_gz"),
  get_irf_data("horserace_ecy"),
  get_irf_data("horserace_tedr"),
  # get_irf_data("horserace_gz_tedr"),
  get_irf_data("horserace_nfci")
))

p <- 
  ggplot() +
  geom_line(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, y = irf, color = type)
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2),
    fill = green_color, alpha = 0.25
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1),
    fill = green_color, alpha = 0.5
  ) +
  geom_line(
    data = dplyr::filter(dt_robust, shock == "VFCI shock"),
    aes(x = period, y = irf, group = type,  color = type),
    alpha = 1
  ) +
  facet_wrap(vars(variable), ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(baseline = green_color, horserace_gz = tetradic_colors[3], horserace_tedr = tetradic_colors[4], horserace_ecy = "gray40", horserace_nfci =  tetradic_colors[2]),
    labels = c(baseline = "Baseline", horserace_gz = "GZ",  horserace_tedr = "TEDR", horserace_ecy = "ECY", horserace_nfci = "NFCI"),
    breaks = c("baseline" , "horserace_ecy", "horserace_nfci", "horserace_gz" , "horserace_tedr")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.5,
    legend.text = element_text(margin = margin(0,40,0,5))
  )


fname <- here::here(paste0("output/appendix/figures/", "compare_horserace_irfs.svg"))
ggsave(fname, p, width = 4.5, height = 7)


## Models with GZ, ECY, and TEDR and without VFCI

dt_baseline <- get_irf_data("baseline")

dt_robust <- purrr::list_rbind(list(
  get_irf_data("horserace_no_vfci_yes_gz"),
  get_irf_data("horserace_no_vfci_yes_tedr"),
  get_irf_data("horserace_no_vfci_yes_gz_tedr") |> filter(shock == "GZ shock") |> mutate(type = "horserace_no_vfci_yes_gz_tedr_gz_shock"),
  get_irf_data("horserace_no_vfci_yes_gz_tedr") |> filter(shock == "TEDR shock") |> mutate(type = "horserace_no_vfci_yes_gz_tedr_tedr_shock")
))


p <- 
  ggplot() +
  geom_line(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, y = irf, color = type)
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2),
    fill = green_color, alpha = 0.25
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1),
    fill = green_color, alpha = 0.5
  ) +
  geom_line(
    data = dplyr::filter(dt_robust, shock %in% c("GZ shock", "TEDR shock")),
    aes(x = period, y = irf, group = type,  color = type),
    alpha = 1
  ) +
  facet_wrap(vars(variable), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(baseline = green_color, horserace_no_vfci_yes_gz_tedr_gz_shock = lighten(triadic_colors[2], 0.5), horserace_no_vfci_yes_gz = triadic_colors[2], horserace_no_vfci_yes_tedr = triadic_colors[3], horserace_no_vfci_yes_gz_tedr_tedr_shock = lighten(triadic_colors[3], 0.5)),
    labels = c(baseline = "Baseline - VFCI Shock", horserace_no_vfci_yes_gz = "GZ - GZ Shock", horserace_no_vfci_yes_gz_tedr_gz_shock = "GZ and TEDR - GZ Shock", horserace_no_vfci_yes_tedr = "TEDR - TEDR Shock", horserace_no_vfci_yes_gz_tedr_tedr_shock = "GZ and TEDR - TEDR Shock"),
    breaks = c("baseline", "horserace_no_vfci_yes_gz" , "horserace_no_vfci_yes_gz_tedr_gz_shock",  "horserace_no_vfci_yes_tedr", "horserace_no_vfci_yes_gz_tedr_tedr_shock")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90)
  )


fname <- here::here(paste0("output/appendix/figures/", "compare_horserace_no_vfci_irfs.svg"))
ggsave(fname, p, width = 5.5, height = 5)


## Models with GZ, and TEDR and with VFCI

dt_baseline <- get_irf_data("baseline")

dt_robust <- purrr::list_rbind(list(
  get_irf_data("horserace_gz_tedr") |> filter(shock == "VFCI shock") |> mutate(type = "horserace_gz_tedr_vfci_shock"),
  get_irf_data("horserace_gz_tedr") |> filter(shock == "GZ shock") |> mutate(type = "horserace_gz_tedr_gz_shock"),
  get_irf_data("horserace_gz_tedr") |> filter(shock == "TEDR shock") |> mutate(type = "horserace_gz_tedr_tedr_shock") |> mutate(irf = -1 * irf)
))


p <- 
  ggplot() +
  geom_line(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, y = irf, color = type)
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2),
    fill = green_color, alpha = 0.25 
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1),
    fill = green_color, alpha = 0.5
  ) +
  geom_line(
    data = dt_robust,
    aes(x = period, y = irf, group = type,  color = type),
    alpha = 1
  ) +
  facet_wrap(vars(variable), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(
      baseline = green_color,
      horserace_gz_tedr_vfci_shock = tetradic_colors[4],
      horserace_gz_tedr_gz_shock = tetradic_colors[3],
      horserace_gz_tedr_tedr_shock = tetradic_colors[2]
    ),
    labels = c(baseline = "Baseline - VFCI Shock", horserace_gz_tedr_vfci_shock = "GZ and TEDR - VFCI Shock", horserace_gz_tedr_gz_shock = "GZ and TEDR - GZ Shock", horserace_gz_tedr_tedr_shock = "GZ and TEDR - TEDR Shock"),
    breaks = c("baseline", "horserace_gz_tedr_vfci_shock" , "horserace_gz_tedr_gz_shock",  "horserace_gz_tedr_tedr_shock")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.5,
    legend.text = element_text(margin = margin(0,40,0,5))
  )


fname <- here::here(paste0("output/appendix/figures/", "compare_horserace_gz_tedr_irfs.svg"))
ggsave(fname, p, width = 4.5, height = 5.5)



## Models with Total and Resid Volatility

dt_baseline <- get_irf_data("baseline")

dt_robust <- purrr::list_rbind(list(
  get_irf_data("total_log_vol") |> filter(shock == "Total Log Vol shock"),
  get_irf_data("resid_log_vol") |> filter(shock == "Resid Log Vol shock")
))

dt_robust <- dt_robust |>
  mutate(variable = as.character(variable)) |>
  mutate(variable = ifelse(variable == "Tot LV", "Total Log Volatility", variable)) |>
  mutate(variable = ifelse(variable == "Res LV", "Residual Log Volatility", variable)) |>
  mutate(variable = factor(variable, levels = c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds", "Residual Log Volatility", "Total Log Volatility") ))

p <- 
  ggplot() +
  geom_line(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, y = irf, color = type)
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb2, ymax = irf_ub2),
    fill = green_color, alpha = 0.25 
  ) +
  geom_ribbon(
    data = dplyr::filter(dt_baseline, shock == "VFCI shock"),
    aes(x = period, ymin = irf_lb1, ymax = irf_ub1),
    fill = green_color, alpha = 0.5
  ) +
  geom_line(
    data = dt_robust,
    aes(x = period, y = irf, group = type,  color = type),
    alpha = 1
  ) +
  facet_wrap(vars(variable), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(1, 20), expand = c(0, 0)) +
  custom_zero_line +
  custom_theme +
  scale_color_manual(
    name = "VFCI Version",
    values = c(
      baseline = green_color,
      resid_log_vol = triadic_colors[2],
      total_log_vol = triadic_colors[3]
    ),
    labels = c(baseline = "Baseline - VFCI Shock", resid_log_vol = "Residual Log Vol Shock", total_log_vol = "Total Log Vol Shock"),
    breaks = c("baseline", "total_log_vol",  "resid_log_vol")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.5,
    legend.text = element_text(margin = margin(0,40,0,5))
  )


fname <- "output/appendix/figures/compare_resid_total_log_vol_irfs.svg"
ggsave(fname, p, width = 4.5, height = 5.5)
