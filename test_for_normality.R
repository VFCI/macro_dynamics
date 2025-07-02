library(mvnormalTest)
library(ggplot2)
library(patchwork)
library(gt)

load('output/analysis_data/res_ins_data.Rdata') ## data_res

# Non-normality of BVAR structural shocks and Q-Q plots
shock_type <- grep("^sf_bvar", names(data_res), value = TRUE)
vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1963-01-01"))) %>%
  dplyr::select(c(all_of(shock_type)))
vars <-  c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
colnames(vars_for_corr) <- vars
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)


#Mardia Test (Skewness and Kurtosis) for Multivariate Normality
mardia_result <- mardia(vars_for_corr)

# Mardia Test (Skewness and Kurtosis) for univariate normality
mardia_list <- vars |> purrr::set_names() |> purrr::map(~ mardia(vars_for_corr[, .x]))

mardia_list[[vars[[1]]]]$mv.test

mardia_df  <- vars |>
  purrr::set_names() |>
  purrr::map(~
    tibble(
      Test = mardia_list[[.x]]$mv.test$Test,
      Statistic = as.numeric(as.character(mardia_list[[.x]]$mv.test$Statistic)),
      `p-value` = as.numeric(as.character(mardia_list[[.x]]$mv.test$`p-value`))
    ) |>
    dplyr::filter(Test != "MV Normality") |>
    tidyr::pivot_longer(-Test)
  ) |>
  purrr::list_rbind(names_to = "Variable")

## Rotational Robust Shapiro-Wilk Type (SWT) Test for Multivariate Normality
## Note: also runs univariate Shapiro Wilk tests, which is what we report
fa_result <- faTest(vars_for_corr)

## Create table from faTest

## Make a data.frame
rn <- fa_result$uv.shapiro |> rownames()
cn <- fa_result$uv.shapiro |> colnames()
dims <- c(length(rn), length(cn))
vals <- fa_result$uv.shapiro |> as.character()
data <- matrix(vals, nrow = dims[1], ncol = dims[2], dimnames = list(rn, cn))
sw_df <- data |> as.data.frame() |> tibble::rownames_to_column("Variable")
sw_df$`Statistic` <- as.numeric(sw_df$W)
sw_df$W <- NULL
sw_df$`p-value` <- as.numeric(sw_df$`p-value`)
sw_df$UV.Normality <- NULL
sw_df <- sw_df |> tidyr::pivot_longer(-Variable)
sw_df$Test <- "Normality"

df <- list(
  mardia_df |> mutate(model = "Mardia"),
  sw_df |> mutate(model = "Shapiro-Wilks")
) |>
  purrr::list_rbind()

## Make table
tb <-
  df |>
  dplyr::arrange(model, Test, name) |>
  tidyr::pivot_wider(names_from = c(model, Test, name), values_from = value, names_sep = "_") |>
  gt(rowname_col = "Variable") |>
  tab_spanner_delim(
    delim = "_"
  ) |>
  fmt_number(decimals = 0, columns = c(2, 4)) |>
    fmt_number(decimals = 2, columns = c(6)) |>
  fmt_number(decimals = 4, columns = c(3, 5, 7), drop_trailing_zeros = TRUE) |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular")

tb |>
  writeLines("./output/appendix/tables/SWtest.tex")
 
#------

qq_plot <- function(data, var_name) {
  ggplot(data, aes(sample = !!sym(var_name))) +
    geom_hline(yintercept = 0, linewidth = 0.25) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    stat_qq() +
    stat_qq_line(color = "steelblue", linewidth = 1) +
    labs(title = var_name, x = "Theoretical", y = "Sample") +
    custom_theme +
    theme(plot.title = element_text(hjust = 0.5))
}

p1 <- qq_plot(vars_for_corr, "Log Real GDP") 
p2 <- qq_plot(vars_for_corr, "Log Core PCE")
p3 <- qq_plot(vars_for_corr, "VFCI")
p4 <- qq_plot(vars_for_corr, "Fed Funds")

p <- (p1 | p2) / (p3 | p4)

ggsave("./output/appendix/figures/qq-plots.svg", width = 5, height = 5)



