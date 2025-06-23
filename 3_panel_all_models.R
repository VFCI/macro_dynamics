
#-------------------------------------------------------------------------------

# Load data
filename_mc = paste0("output/","mcmc_out_",type,"_ir",".Rdata")
filename_iv = paste0("output/","svariv_lpiv_chol_sn_",type,".Rdata")
base::load(filename_mc)
base::load(filename_iv)

#-------------------------------------------------------------------------------
# Note - There are 4 configurable parameters 
#-------------------------------------------------------------------------------

# I. General Configuration

# VFCI with MP or Y?
#vfci_pair = "ff"  #choose 'ff' or 'y' ##### CONFIG 1 #####

# Color of plots (don't change this)
if (vfci_pair == "ff") {
  color <- "#ff78d4" # Pink IRF shade
} else {
  color <- "#6666fe"  # Blue IRF shade
}

#-------------------------------------------------------------------------------
# II. This section generalizes the code to capture either the FF or Y IRFs
#      For baseline as well as for robustness
fig_width <- 5

if (vfci_pair == "ff") {
  fig_height <- 7
  col1 <- "Fed Funds Shock to VFCI"
  col2 <- "VFCI Shock to Fed Funds" 
  vol_bvar_second_var <- "Fed Funds"
  vol_bvar_type_vfci <- var_names[3]
  GDP_FF_FOR_IV <- "fedfunds"
  GDP_FF_FOR_CHOL <- "fedfunds"
  SVAR_GDP_OR_FF <- df_irf_ff
  CHOL_GDP_OR_FF <- chol_irf_vfci
  LP_GDP_OR_FF  <-  results_lin_iv_ff
  vfci_type <- "vfci"
  lp_data_vfci_shock <- c("lgdp", "lpce", "fedfunds", "vfci")
  lp_vfci_shock_on <- which(lp_data_vfci_shock == "fedfunds")
      if (type == "stationary") {
      lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci")
      }
      if (type == "vfci_lev") {
        vfci_type <- "vfci_lev"
        lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci_lev")
      }
  } else {
  fig_height <- 5.5
  col1 <- "Output Shock to VFCI"
  col2 <- "VFCI Shock to Output" 
  vol_bvar_second_var <- var_names[1] #GDP
  vol_bvar_type_vfci <- var_names[3]
  GDP_FF_FOR_IV <- "lgdp"
  GDP_FF_FOR_CHOL <- "lgdp"
  SVAR_GDP_OR_FF <- df_irf_y
  CHOL_GDP_OR_FF <- chol_irf_vfci 
  LP_GDP_OR_FF  <-  results_lin_iv_y
  vfci_type <- "vfci"
  lp_data_vfci_shock <- c("lgdp", "lpce", "fedfunds", "vfci")
  lp_vfci_shock_on <- which(lp_data_vfci_shock == "lgdp")
      if (type == "stationary") {
      GDP_FF_FOR_IV <- "ygr"
      GDP_FF_FOR_CHOL <- "ygr"
      lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci")
      lp_vfci_shock_on <- which(lp_data_vfci_shock == "ygr")
      }
      if (type == "vfci_lev") {
        vfci_type <- "vfci_lev"
        lp_data_vfci_shock <- c("ygr", "infl_pce", "fedfunds", "vfci_lev")
      }
  }

#-------------------------------------------------------------------------------
if (type == "baseline") {
  fname <- here::here(paste0("output/baseline/figures/",vfci_pair,'-vfci-',type,'.svg', sep = ''))
} else {
  fname <- here::here(paste0("output/appendix/figures/",vfci_pair,'-vfci-',type,'.svg', sep = ''))
}

#-------------------------------------------------------------------------------
## Gather Dataset for plotting

irdraws <- ir$ir[,,,1,]
irf <- apply(ir$ir[,,,1,],c(1:3),median)
nsteps <- irf_steps
irf  = irf[,,1:nsteps]
conf <- c(.68, .90)

## 
irq = apply(irdraws[,,1:nsteps,],1:3,quantile,
  ##probs = c(rev((1 - conf) / 2), .5 + conf/2))
  probs = c((1-conf)/2,0.5 + conf/2))
irq = aperm(irq,perm=c(2,3,4,1))

dimnames(irf)[[1]] <- var_names
dimnames(irf)[[2]] <- var_names
dimnames(irf)[[3]] <- 1:20
df <- as.data.frame.table(irf) |> dplyr::as_tibble()
colnames(df) <- c("target", "shock", "horizon", "response")

dimnames(irq)[[1]] <- var_names
dimnames(irq)[[2]] <- var_names
dimnames(irq)[[3]] <- 1:20
dimnames(irq)[[4]] <- c("lower.68", "lower", "upper.68", "upper")
dfq <- as.data.frame.table(irq) |> dplyr::as_tibble()
colnames(dfq) <- c("target", "shock", "horizon", "quantile", "response")

dfq_wide <- dfq |> tidyr::pivot_wider(names_from = quantile, values_from = response, names_prefix = "response.")

df <- dplyr::full_join(df, dfq_wide, by = c("target", "shock", "horizon"))

df$horizon <- as.numeric(df$horizon)
df$model <- "Vol-BVAR"

bvar_df1 <- df |> filter(target == vol_bvar_type_vfci & shock == vol_bvar_second_var)
bvar_df2 <- df |> filter(target == vol_bvar_second_var & shock == vol_bvar_type_vfci)

## SVAR output as df
svar_df1 <- SVAR_GDP_OR_FF[SVAR_GDP_OR_FF$target == vfci_type,]
svar_df1$target <- vol_bvar_type_vfci
svar_df1$shock <- vol_bvar_second_var
svar_df1$model <- "SVAR-IV"
svar_df1$horizon <- svar_df1$horizon + 1


svar_df2 <- chol_irf_vfci_first[chol_irf_vfci$target == GDP_FF_FOR_CHOL & chol_irf_vfci$shock == vfci_type,]
svar_df2$target <- vol_bvar_second_var 
svar_df2$shock <- vol_bvar_type_vfci
svar_df2$model <- "SVAR-IV"
svar_df2$horizon <- svar_df2$horizon + 1

## LP output to DF
lp_df1 <- data.frame(
  horizon = 1:20,
  target = vol_bvar_type_vfci,
  shock = vol_bvar_second_var,
  response = LP_GDP_OR_FF$irf_lin_mean[3,],
  response.lower = LP_GDP_OR_FF$irf_lin_low[3,],
  response.upper = LP_GDP_OR_FF$irf_lin_up[3,],
  response.lower.68 = LP_GDP_OR_FF$irf_lin_low.68[3,],
  response.upper.68 = LP_GDP_OR_FF$irf_lin_up.68[3,],
  model = "LP-IV"
)

lp_vfci <- results_lin_vfci
lp_df2 <- data.frame(
  horizon = 1:20,
  target = vol_bvar_second_var,
  shock = vol_bvar_type_vfci,
  response = lp_vfci$irf_lin_mean[lp_vfci_shock_on,],
  response.lower = lp_vfci$irf_lin_low[lp_vfci_shock_on,],
  response.upper = lp_vfci$irf_lin_up[lp_vfci_shock_on,],
  response.lower.68 = lp_vfci$irf_lin_low.68[lp_vfci_shock_on,],
  response.upper.68 = lp_vfci$irf_lin_up.68[lp_vfci_shock_on,],
  model = "LP-IV"
)

## Cholesky Output as DF
chol_df1 <- CHOL_GDP_OR_FF[CHOL_GDP_OR_FF$target == vfci_type & CHOL_GDP_OR_FF$shock == GDP_FF_FOR_CHOL,]
chol_df1$target <- vol_bvar_type_vfci
chol_df1$shock <- vol_bvar_second_var
chol_df1$model <- "Cholesky"
chol_df1$horizon <- chol_df1$horizon + 1

chol_df2 <- chol_irf_vfci[chol_irf_vfci$target == GDP_FF_FOR_CHOL & chol_irf_vfci$shock == vfci_type, ]
chol_df2$target <- vol_bvar_second_var 
chol_df2$shock <- vol_bvar_type_vfci
chol_df2$model <- "Cholesky"
chol_df2$horizon <- chol_df2$horizon + 1

## Sign Restrictions as DF
sr_df <- data.frame(
  horizon = 1:20,
  target = vol_bvar_type_vfci,
  shock = vol_bvar_second_var,
  response = apply(irfs_mp[, ,3],2,median),
  response.lower = apply(irfs_mp[, ,3],2,function(x) quantile(x,0.05)),
  response.upper = apply(irfs_mp[, ,3],2,function(x) quantile(x,0.95)),
  response.lower.68 = apply(irfs_mp[, ,3],2,function(x) quantile(x,0.16)),
  response.upper.68 = apply(irfs_mp[, ,3],2,function(x) quantile(x,0.84)),
  model = "Sign Restriction"
)
sr_df2 <- data.frame(
  horizon = 1:20,
  target = vol_bvar_second_var,
  shock = vol_bvar_type_vfci,
  response = apply(irfs_vfci[, ,4],2,median),
  response.lower = apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.05)),
  response.upper = apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.95)),
  response.lower.68 = apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.16)),
  response.upper.68 = apply(irfs_vfci[, ,4],2,function(x) quantile(x,0.84)),
  model = "Sign Restriction"
)

## Combine all DFs
plot_df <-
  list(
    bvar_df1,
    bvar_df2,
    svar_df1,
    svar_df2,
    lp_df1,
    lp_df2,
    chol_df1,
    chol_df2
  ) |>
  purrr::list_rbind()

if (vfci_pair == "ff") {
  plot_df <- purrr::list_rbind(list(plot_df, sr_df, sr_df2))
}

model_order <- c("Vol-BVAR", "SVAR-IV", "LP-IV", "Cholesky", "Sign Restriction")
plot_df$model <- factor(plot_df$model, levels = model_order, ordered = TRUE)
plot_df$col_label <- paste0(plot_df$shock, " shock to ", plot_df$target)

p1 <-
  plot_df |>
  filter(col_label == unique(plot_df$col_label)[1]) |>
  ggplot(aes(
    x = horizon,
    y = response
  )) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = response.lower, ymax = response.upper), alpha = 0.25, fill = color) +
  geom_ribbon(aes(ymin = `response.lower.68`, ymax = `response.upper.68`), alpha = 0.5, fill = color) +
  scale_x_continuous(expand = c(0,0)) +
  facet_grid(rows = vars(model), cols = vars(col_label), switch = "y") +
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
    strip.text.y.left = element_text(angle = 90),
    panel.spacing = unit(10, "pt")
  )

p2 <-
  plot_df |>
  filter(col_label == unique(plot_df$col_label)[2]) |>
  ggplot(aes(
    x = horizon,
    y = response
  )) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = response.lower, ymax = response.upper), alpha = 0.25, fill = color) +
  geom_ribbon(aes(ymin = `response.lower.68`, ymax = `response.upper.68`), alpha = 0.5, fill = color) +
  scale_x_continuous(expand = c(0,0)) +
  facet_grid(rows = vars(model), cols = vars(col_label), switch = "y") +
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
    strip.text.y.left = element_blank(),
    panel.spacing = unit(10, "pt")
  )

p <- p1 + p2

ggsave(fname, p, width = fig_width, height = fig_height)
