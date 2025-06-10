library(gt)

## MDD for t-distribution BVAR
load("./output/gibbs_out_baseline.Rdata")
mdd_t <- mdd_output$mout[1]

## MDD for N-distribution BVAR
load("./output/gibbs_out_normal.Rdata")
mdd_n <- mdd_output$mout[1]

## Time Varying A(L) over regimes

lambda = 5                              # co persistence dummies
mu = 1
mnprior = list(tight = 3,decay = .5)          # MN prior
vprior = list(sig = rep(.01,4), w = 1) # variance prior

## Data paremeters
nLags = 4
vars =  c("lgdp", "lpce", "vfci", "fedfunds")

startdate <- as.Date('1962-01-01') 
enddate   <- as.Date('2022-07-01')
strTsigbrk  <- as.Date(c('1979-10-01','1983-01-01','1990-01-01','2008-01-01','2011-01-01', '2020-01-01'))

load("./variables.RData")
data_Y <- variables[, c("date", vars)]
data_Y <- data_Y |> filter(date >= startdate & date <= enddate) |> dplyr::select(-date)
data_Tsigbrk <- which(variables$date %in% strTsigbrk)
Tsigbrk = c(nLags+1, data_Tsigbrk, dim(data_Y)[1]) # add the first and last points as well

## Loop over each set of dates and estimate MDD
nreg = length(Tsigbrk)-1                   # number of regimes
mgnl = rep(0,nreg)                         # placeholder for MDD estimates
for (iregime in 1:nreg){
    date_range = (Tsigbrk[iregime]-nLags):(Tsigbrk[iregime+1]) # allowing for 10 periods of initial conditions
    Yrange = data_Y[date_range,]               # relevant range of data

    mgnl[iregime] = mgnldnsty(as.matrix(Yrange),nLags,lambda=lambda,mu=mu,
        mnprior = mnprior,vprior=vprior)$w
}
mdd_tvA = sum(mgnl)

## Simple no-regime BVAR
mgnl = mgnldnsty(as.matrix(data_Y), nLags, lambda=lambda, mu=mu, mnprior=mnprior, vprior=vprior)$w
mdd_rf <- mgnl    

## Make a table
df <- tibble(
  ` ` = c("Baseline", "", "", ""),
  `Variation in...` = md(c( "Structural shock variances", "None", "Structural shock variances", "All of $A_0$ through $A_p$")),
  `Distribution` = c( "t", "Gaussian", "Gaussian", "Gaussian"),
  MDD = c( mdd_t, mdd_rf, mdd_n, mdd_tvA)
)

tb_latex <-
  gt(df) |>
  fmt_number(decimals = 0) |> 
  fmt_markdown(columns = 1) |>
  cols_align(align = "left", columns = c(1,2)) |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("longtable", "tabular")

tb_latex |>
  writeLines("./output/baseline/tables/mdd.tex")
