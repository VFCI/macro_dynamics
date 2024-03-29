# pick dates, variables

base::load(file = here::here("variables.RData"))
date_begin <- "1970 Q1"
date_end <- "2022 Q3"

# Tables ------------------------------------------------------------------

## Table. Factor loadings -------------------------------------
match_stata <- TRUE
if (match_stata){
  
} else {
  loadings <- summary(results$fgr1.gdpc1$hetreg$pc)
  table_data <- t(rbind(
    loadings$importance["Cumulative Proportion",1:4, drop= FALSE],
    loadings$rotation[,1:4]
  ))
  dimnames(table_data)[[2]][[1]]<-"Cumul. Var."
}

## Table. Regression of FCIs on PCs -------------------------------------
library(modelsummary)
library(gt)
options("modelsummary_format_numeric_latex" = "plain")

reg1 <- paste("nfci","~",paste(paste0("pc", 1:4), collapse='+'))
reg2 <- paste("gsfci","~",paste(paste0("pc", 1:4), collapse='+'))
reg3 <- paste("vixcls","~",paste(paste0("pc", 1:4), collapse='+'))

### Regressions
models<-list(
  "NFCI" = lm(reg1,variables),
  "GSFCI" =  lm(reg2,variables),
  "VIX" = lm(reg3,variables) 
)
### Generate table
Map(\(out) modelsummary::modelsummary(models,
                                      coef_rename = toupper,
                                      statistic = "std.error", #statistic = "[{statistic}]",
                                      stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01),
                                      gof_map = c("nobs", "r.squared"),
                                      fmt= '%.3f',
                                      coef_omit= '(Intercept)',
                                      caption = "\\textbf{Association between FCIs and PCs} \\label{tab:FCIregressions}",
                                      output = out
), list("gt",here::here("output","fcis_on_pcs_new.tex")) )


## Table. Regression of FYQ and FCQ on PCs ---------------------------------
library(stringr)
library(purrr)
library(broom.mixed)
m1 <- results$fgr1.gdpc1$hetreg
m2 <- results$fgr1.pcecc96$hetreg
v1 <- structure(m1, class = c("het"))
v2 <- structure(m2, class = c("het"))

#### Generate table
models <- list(
  "Conditional Mean"=list(
    "Real GDP Growth" = m1,
    "Real Consumption Growth" = m2
  ),
  "Conditional Log-Volatility"=list(
    "Real GDP Growth" = v1,
    "Real Consumption Growth" = v2
  )
)
Map(\(out) modelsummary::modelsummary(models,
                                      shape="rbind",
                                      coef_rename = toupper,
                                      statistic = "({std.error})", 
                                      notes = "Standard errors in parentheses",
                                      stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01),
                                      gof_map = c("nobs", "r.squared"),
                                      fmt= '%.3f',
                                      coef_omit= '(Intercept)',
                                      caption = "\\textbf{Heteroskedasticity Linear Regression of GDP and PCE Growth on PCs} \\label{tab:reg2}",
                                      output = out
), list("gt",here::here("output","significance_of_pcs_1962_2022_new.tex")) )

#                        #statistic = "({p.value})", #statistic = "[{statistic}]",
#notes = "p-values in parentheses",
#

# The LR test at the bottom of the output is a test for the parameters of the variance function. The
# χ
# 2
# (1) statistic of 19.59 is significant, indicating that heteroskedasticity is present. If we had preferred
# the Wald test for heteroskedasticity instead of the LR test, we would have specified the waldhet
# option.
# see if parameters and performance support your model type


## Table. Regression of risk premia on VFCI ----------------------------------------
library(recipes)
library(workflowsets)
library(parsnip)
library(rsample)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(tibble)
library(modelsummary)
library(stringr)

y <- c("gz","ecy") #,"ebp"
y_lag <- c("lag(gz)","lag(ecy)") #,"lag(ebp)"
x <- c("vfci","nfci","gsfci","vixcls")
x <- c(x,paste0(x,collapse="+"))

formulas <- y %>% str_c(y_lag,sep = "~") %>% 
  expand_grid(x) %>% 
  unite("formula",sep="+")

models<- purrr::map(formulas[[1]],\(x) lm(as.formula(x),data=variables)) %>% 
  setNames(toupper(str_extract(formulas[[1]],".*(?=~)")) ) %>% enframe

nmod <- (models %>% 
           group_by(name) %>%
           summarize(n = n()) %>% 
           pull(n) + 1) %>% 
  setNames(toupper(y))

library(tribe)
rows <- tribble(~"0",~"1",~"2",~"3",~"4",~"5","","GZ","GZ","GZ","GZ","GZ")
attr(rows,"position")<-c(0)

tidy_name = function(x,nn){
  str_replace(x,"\\s*\\([^\\)]+\\)", str_c(" ",toupper(nn))) %>% toupper %>% str_replace("LAG","Lag")
}
captions = function(nn){
  switch(nn, 
         gz="spread is a corporate bond risk premium measure of \\cite{{GilchristZakrajsek2012}}", 
         ecy="stands for the excess CAPE yield of \\cite{{Shiller2000}} and is a commonly used measure of the equity market CAPE equity risk premium."
  )
}

gm <-tibble::tribble(
  ~raw,        ~clean, ~fmt,
  "nobs",      "N",     0,
  "r.squared", "$R^2$", 2)

Map(\(yvar) 
    Map(\(out) 
        models %>% pivot_wider %>% 
          pull(toupper(yvar)) %>% 
          unlist(.,recursive=FALSE) %>% 
          # dvnames %>%
          modelsummary(.,
                       add_rows = rows %>% replace(2:nmod[yvar],values=toupper(yvar)),
                       coef_rename = \(x) tidy_name(x,yvar),
                       vcov = "stata",
                       statistic = "statistic", #statistic = "[{statistic}]","std.error
                       stars = c('*' = 0.10,'**' = 0.05,'***' = 0.01),
                       notes = "$t$ statistics in parentheses",
                       gof_map = gm,
                       fmt= fmt_statistic("estimate" = 3, "statistic" = 2),
                       coef_omit= '(Intercept)',
                       caption = str_glue("\\textbf{{Association between {yvar} spread and FCIs:}} 
                                  The {yvar} ", captions(tolower(yvar)), "\\label{{tab:{tolower(yvar)}_regs}}."),
                       output = out
          ),
        list("gt",here::here("output","reg_gz_new.tex"))), toupper(y))

## Table. VAR variables -----------------------------------
VAR_date_begin = "1962 Q1"
VAR_date_end = "2022 Q3"

VAR_fred_vars <- variables %>%
  select(fedfunds, log.gdpc1, log.pcepilfe, qtr) %>% 
  filter(between(as.Date(qtr), as.Date(as.yearqtr(VAR_date_begin)), as.Date(as.yearqtr(VAR_date_end)))) 

vtable::st(VAR_fred_vars,
           digits = 2,
           summ=c("mean(x)","sd(x)","min(x)","max(x)","notNA(x)"),
           summ.names = c("Mean","SD","Min","Max","N"),
           labels=c("Federal Funds Rate","log of Real GDP","log of Core PCE Deflator"),
           title = "Title",
           note = "\\textbf{Descriptive Statistics for the Macro-Financial Variables:} The VFCI is constructed in the previous section, the remaining data is from the FRED database of the Federal Reserve Bank of St Louis.",
           anchor = "sumstats_for_var",
           file = here::here("output","tables","table7.tex"),
           align = 'p{.3\\textwidth}ccccccc',
           fit.page = '\\textwidth',
           note.align = 'p{.3\\textwidth}ccccccc',
           out = "latex"
           # out = "viewer"
)
