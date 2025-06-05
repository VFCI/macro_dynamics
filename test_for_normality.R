library(mvnormalTest)
library(ggplot2)
library(patchwork)

# Non-normality of BVAR structural shocks and Q-Q plots
shock_type <- grep("^sf_bvar", names(data_res), value = TRUE)
vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1963-01-01"))) %>%
  select(c(all_of(shock_type)))
colnames(vars_for_corr) <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)


#Mardia Test (Skewness and Kurtosis) for Multivariate Normality
mardia_result <- mardia(vars_for_corr)

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
df <- data |> as.data.frame() |> tibble::rownames_to_column("Variable")
df$W <- as.numeric(df$W)
df$`p-value` <- as.numeric(df$`p-value`)

## Make table
tb <-
  df |>
  gt() |>
  fmt_number(decimals = 3, columns = 2) |>
  as_latex() |>
  as.character() |>
  str_replace_all("longtable", "tabular")

tb |>
  writeLines("./output/baseline/tables/SWtest.tex")
 
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



