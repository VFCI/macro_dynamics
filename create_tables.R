base::load(file = here::here("variables.RData"))
date_begin <- "1970 Q1"
date_end <- "2022 Q3"
library(modelsummary)
library(gt)
options("modelsummary_format_numeric_latex" = "plain")

# Tables ------------------------------------------------------------------

## Goodness of Fit values used for modelsummary
gm <-tibble::tribble(
  ~raw,        ~clean, ~fmt,
  "nobs",      "N",     0,
  "r.squared", "R²", 2
)

## Table. Regression of FYQ and FCQ on PCs ---------------------------------
library(stringr)
library(purrr)
m1 <- results$fgr1.gdpc1$hetreg
m2 <- results$fgr1.pcecc96$hetreg
v1 <- structure(m1, class = c("het"))
v2 <- structure(m2, class = c("het"))

signs_to_flip <- -1 * sign(attr(v1$apVar, "Pars")[-5])

m1$coefficients <- coef(m1) * c(1, signs_to_flip)
m2$coefficients <- coef(m2) * c(1, signs_to_flip)

attr(v1$apVar, "Pars") <- attr(v1$apVar, "Pars") * c(signs_to_flip, 1)
attr(v2$apVar, "Pars") <- attr(v2$apVar, "Pars") * c(signs_to_flip, 1)

#### Generate table
tab_a <- modelsummary(
  list(
    "GDP" = m1,
    "C" = m2
  ),
  output = "gt",
  coef_rename = toupper,
  statistic = "({statistic})",
  gof_map = NA,
  fmt= '%.2f',
  coef_omit= '(Intercept)',
  stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01)
) |>
  gt::rm_source_notes() |>
  gt::tab_spanner(label =  "Panel A: Conditional Mean", columns = everything())

tab_a |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  writeLines("./output/baseline/tables/mean_vol_reg_coefs_means.tex")

tab_b <- modelsummary(
  list(
    "GDP" = v1,
    "C" = v2
  ),
  output = "gt",
  coef_rename = toupper,
  statistic = "({statistic})",
  gof_map = NA,
  fmt= '%.2f',
  coef_omit= '(Intercept)',
  stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01)
) |>
  gt::rm_source_notes() |>
  gt::tab_spanner(label =  "Panel B: Log Conditional Volatility", columns = everything())

tab_b |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  writeLines("./output/baseline/tables/mean_vol_reg_coefs_vols.tex")



## Table. Principal Components Loadings -----------------------------------
loadings <- summary(vfci_baseline$hetreg$pc)

cumvar <- 100*cumsum(loadings$sdev^2)/sum(loadings$sdev^2)

weights <- loadings$rotation[c("annual_ret","gspc_vol","t10y3m","tb3smffm","aaa10ym","baa_aaa"),1:4]

### Make all PCs have positive impact on Ret
weights <- t(t(weights) * signs_to_flip)

table_data <- t(rbind(
  weights,
  cumvar[1:4]
))
colnames(table_data)[7]<-"cum_var"

table_data <- table_data %>% 
  tidyr::as_tibble(., rownames = "PC") %>% 
  rename(
    "Cred" = aaa10ym,
    "Ret" = annual_ret,
    "Def" = baa_aaa,
    "Cumulative Variance" = cum_var,
    "Vol" = gspc_vol,
    "Term" = t10y3m,
    "Liq" = tb3smffm
  )
### Generate table
tab <- table_data |>
  gt() |>
  cols_label(PC = "") |>
  fmt_number(decimals = 2) |>
  fmt_percent(columns = 8, decimals = 1, scale_values = FALSE) |>
  cols_move_to_end(columns = "Cumulative Variance") |>
  cols_move(
    columns = c("Vol","Term","Liq","Cred","Def"),
    after = "Ret"
    ) |>
  cols_align('center', columns = 2:8) |>
  tab_spanner(label = "Loadings", columns = 2:7)

tab |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  writeLines("./output/baseline/tables/pc_table.tex")


## Table. Regression of FCIs on PCs -------------------------------------
reg1 <- paste("nfci","~",paste(paste0("pc", 1:4), collapse='+'))
reg2 <- paste("gsfci","~",paste(paste0("pc", 1:4), collapse='+'))
reg3 <- paste("vixcls","~",paste(paste0("pc", 1:4), collapse='+'))

### Regressions
models<-list(
  "NFCI" = lm(reg1,variables),
  "GSFCI" =  lm(reg2,variables),
  "VIX" = lm(reg3,variables)
)

models[[1]]$coefficients <- models[[1]]$coefficients * c(1, signs_to_flip)
models[[2]]$coefficients <- models[[2]]$coefficients * c(1, signs_to_flip)
models[[3]]$coefficients <- models[[3]]$coefficients * c(1, signs_to_flip)

### Generate table
tab <- modelsummary::modelsummary(
  models,
  coef_rename = toupper,
  statistic = "({statistic})",
  stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01),
  gof_map = gm,
  fmt_statistic("estimate" = 2, "statistic" = 2),
  coef_omit= '(Intercept)',
  output = "gt"
) |>
  rm_source_notes()

tab |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  stringr::str_replace_all("N ", "\\\\midrule\\\\addlinespace[2.5pt]
  N ") |>
  writeLines("./output/baseline/tables/fcis_on_pcs.tex")



## Table. Regression of risk premia on VFCI ----------------------------------------
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(tibble)
library(modelsummary)
library(stringr)

y <- c("gz","ecy","ebp",
       "tedr","tedrate","lior3m","es",
       "ACMY01","ACMTP01","ACMTP05","ACMTP10"
       ) #
y_lag <- c("lag(gz)","lag(ecy)","lag(ebp)",
           "lag(tedr)","lag(tedrate)","lag(lior3m)"
           ,"lag(es)",
           "lag(ACMY01)","lag(ACMTP01)","lag(ACMTP05)","lag(ACMTP10)"
          ) #
x <- c("vfci","nfci","gsfci","vixcls")
x <- c(x,paste0(x,collapse="+"))

formulas <- y %>%
  stringr::str_c(y_lag,sep = "~") %>% 
  expand_grid(x) %>% 
  tidyr::unite("formula",sep="+")

models <-
  purrr::map(formulas[[1]],\(x) lm(as.formula(x),data=variables)) %>% 
  setNames(toupper(str_extract(formulas[[1]],".*(?=~)")) ) %>%
  tibble::enframe()

nmod <- (models %>% 
           group_by(name) %>%
           summarize(n = n()) %>% 
           pull(n) + 1) %>% 
  setNames(toupper(y))


rows <- tribble(~"0",~"1",~"2",~"3",~"4",~"5","","GZ","GZ","GZ","GZ","GZ")
attr(rows,"position")<-c(0)

tidy_name = function(x,nn){
  x |>
  str_replace("\\s*\\([^\\)]+\\)", str_c(" ",toupper(nn))) |>
  toupper() |>
  str_replace("LAG","Lag") |>
  str_replace("VIXCLS", "VIX")
}

make_reg_table <- function(yvar, short_label, label) {
  models |>
  filter(name == yvar) |>
  pull(value) |>
  modelsummary(
    statistic = "statistic",
    fmt = fmt_statistic("estimate" = 2, "statistic" = 1),
    vcov = "stata",
    stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01),
    gof_map = gm,
    coef_omit= '(Intercept)',
    output = "gt",
    coef_rename = \(x) tidy_name(x, short_label),
    escape = FALSE
  ) |>
  gt::rm_source_notes() |>
  gt::tab_spanner(label = label, columns = 2:6)
}

make_reg_table("GZ", "GZ", "Credit Spread (GZ)") |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  stringr::str_replace_all("N ", "\\\\midrule\\\\addlinespace[2.5pt]
  N ") |>
  writeLines("./output/baseline/tables/reg_GZ.tex")

make_reg_table("ECY", "ECY", "Excess PE Ratio (ECY)") |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  stringr::str_replace_all("N ", "\\\\midrule\\\\addlinespace[2.5pt]
  N ") |>
  writeLines("./output/baseline/tables/reg_ECY.tex")

make_reg_table("ACMTP01", "TP", "Term Premium (TP)") |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  stringr::str_replace_all("N ", "\\\\midrule\\\\addlinespace[2.5pt]
  N ") |>
  writeLines("./output/baseline/tables/reg_TP.tex")


## Table. VAR variables Summary Statistics -----------------------------------
VAR_date_begin = "1962 Q1"
VAR_date_end = "2022 Q3"

VAR_fred_vars <- variables |>
  filter(between(as.Date(qtr), as.Date(as.yearqtr(VAR_date_begin)), as.Date(as.yearqtr(VAR_date_end)))) |>
  select(fedfunds, lgdp, lpce, vfci) |>
  mutate(vfci = scale(vfci)) |>
  rename(logGDP = "lgdp", logP = "lpce", VFCI = "vfci")

tab <-
  modelsummary::datasummary(
    formula = logGDP + logP + fedfunds + VFCI ~ Mean + SD + Min + Max,
    data = VAR_fred_vars,
    output = "gt"
  )

tab |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular") |>
  writeLines("./output/baseline/tables/summary_stats.tex")

