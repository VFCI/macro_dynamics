library(dplyr)
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)

# FRED data ---------------------------------------------------------------
fred_raw <- tidyquant::tq_get(
  c(
    "GDPC1",
    "GDPPOT",
    "PCEPILFE",
    "PCECC96",
    "FEDFUNDS",
    "ANFCI",
    "NFCI",
    "GS10",
    "TB3MS",
    "MED3",
    "TB3SMFFM",
    "AAA10YM",
    "WTISPLC",
    "BAA10YM",
    "LIOR3M",
    "TEDRATE",
    "VIXCLS",
    "CLVMNACSCAB1GQEA19",
    "DGS10",
    "DGS5",
    "DGS1"
  ),
  get = "economic.data",
  from = "1960-1-1", # "1928-01-01",
  to = "2022-10-2" # today()
)

# Yahoo! Finance -----------------------------------------------------------
yahoo_raw <- c("^GSPC") %>%
  tidyquant::tq_get(
    get  = "stock.prices",
    from = "1960-1-1",
    to   = "2022-10-2"
  ) %>%
  dplyr::mutate(symbol = purrr::map_chr(symbol, ~ stringr::str_replace(.x, pattern = "\\^", replacement = "")))

# Import other data from .dta files ---------------------------------------
## Instruments ------------------------------------------------------------
instruments <- haven::read_dta(here::here("data","instruments.dta"))
#keep only instruments that do not depend on vfci (we construct the ones that depend on the vfci below)
instruments <- instruments %>% dplyr::select(-dplyr::contains("vfci"))

## GS-FCI -----------------------------------------------------------------
gs_fci <- haven::read_dta(here::here("data", "gs_fci_q.dta"))

## GZ, EBP, ECY -----------------------------------------------------------
gz_ebp_ecy <- haven::read_dta(here::here("data", "gz_ebp_ecy.dta"))

## CISS -------------------------------------------------------------------
ciss <- haven::read_dta(here::here("data", "ciss.dta"))

# Create variables --------------------------------------------------------

#' Helper function: compute growth rates
growth_rate <- function(
    ts,
    ...,
    delta = 1,
    type = c("log", "geometric"),
    units = c("decimal", "percent")) {
  gr <- switch(type,
    log = log(ts) - log(dplyr::lag(ts, delta)),
    geometric = ts / dplyr::lag(ts, delta) - 1
  )
  gr <- switch(units,
    decimal = gr,
    percent = 100 * gr
  )
  return(gr)
}
#' Helper function: compute and name leads and lags
multilag <- function(x, n = 1:3, type = c("both", "lag", "lead"), .prefix = "", .postfix = "") {
  type <- match.arg(type)
  if (type == "lag") {
    names(n)[n==1] <- paste0(.prefix, "l", .postfix) #first lag is just "l" and not "l1"
    names(n)[n!=1] <- paste0(.prefix, "l", as.character(n[n!=1]), .postfix)
    tibble::as_tibble(
      purrr::map(n, \(lags) dplyr::lag(x, lags))
    )
  } else if (type == "lead") {
    names(n)[n==1] <- paste0(.prefix, "f", .postfix) #first lead is just "f" and not "f1"
    names(n)[n!=1] <- paste0(.prefix, "f", as.character(n[n!=1]), .postfix)
    tibble::as_tibble(
      purrr::map(n, \(leads) dplyr::lead(x, leads))
    )
  } else if (type == "both") {
      lags <- multilag(x, n = n, type = "lag", .prefix = .prefix, .postfix = .postfix) 
      leads <- multilag(x, n = n, type = "lead", .prefix = .prefix, .postfix = .postfix)
      dplyr::bind_cols(lags,leads)
  }
}

## Variables derived from FRED data ----------------------------------------

# pick dates, variables
date_begin <- "1960 Q1"
date_end <- "2023 Q1"
fred <- fred_raw %>% 
  tsibble::as_tsibble(key = c("symbol")) %>%
  tsibble::group_by_key() %>%
  tsibble::index_by(qtr = ~ tsibble::yearquarter(.))  %>%
  dplyr::summarize_if(is.numeric,mean,na.rm=TRUE) %>%
  tsibble::filter_index(date_begin ~ date_end) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = price) %>%
  dplyr::mutate(dplyr::across(!qtr , ~ dplyr::na_if(., NaN)))
  
fred <- tibble::as_tibble(fred)

# create variables
units <- "percent"
fred_var_list <- c("GDPC1", "PCEPILFE", "PCECC96", "CLVMNACSCAB1GQEA19")
fred <- fred %>%
  dplyr::mutate(
    # logs
    log = dplyr::across(
      dplyr::all_of(fred_var_list),
      ~ log(.x)
    ),
    # spreads
    gap = 100*(GDPC1 / GDPPOT - 1),
    baa_aaa = BAA10YM - AAA10YM,
    t10y3m = GS10 - TB3MS,
    # tedr starts 1970Q1, not 1962Q1 like rest of variables
    tedr = ifelse(tsibble::time_in(qtr, . ~ "1985 Q4"), LIOR3M - TB3MS, TEDRATE), 
    es = MED3 - TB3MS,
    # year-over-year growth rates
    gr4 = dplyr::across(
      dplyr::all_of(fred_var_list),
      ~ growth_rate(.x, delta = 4, type = "geometric", units = units),
      .unpack = TRUE
    ),
    # quarterly growth rates
    gr1 = dplyr::across(
      dplyr::all_of(fred_var_list),
      ~ growth_rate(.x, delta = 1, type = "geometric", units = units),
      .unpack = TRUE
    ),
  )
# leads and lags of growth rates

gr_list <- c("gr1","gr4")
fred <- purrr::map(
    gr_list,
    \(gr) 
        multilag(
        fred[[gr]],
        .postfix = gr
    )
  ) %>% 
  dplyr::bind_cols() %>% 
  tidyr::unnest(.,cols=colnames(.),names_sep=".") %>% 
  dplyr::bind_cols(fred,.)

# rearrange, rename, tidy NA
fred <- tidyr::unnest(fred, cols = everything(), names_sep = ".")
fred <- dplyr::rename_with(fred, ~ tolower(.x))

## Variables derived from Yahoo! finance data -----------------------------
yahoo_avg_prc <- yahoo_raw %>% 
  tsibble::as_tsibble(key = c("symbol")) %>%
  tsibble::group_by_key() %>%
  tsibble::index_by(qtr = ~ tsibble::yearquarter(.))  %>%
  dplyr::summarize_if(is.numeric,mean,na.rm=TRUE) %>%
  tsibble::filter_index(date_begin ~ date_end) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = adjusted) %>%
  dplyr::mutate(dplyr::across(!qtr , ~ dplyr::na_if(., NaN))) %>% 
  dplyr::select(c("qtr","GSPC"))

yahoo_avg_prc <- dplyr::as_tibble(yahoo_avg_prc)

# compute quarterly frequency returns from average daily prices over each quarter
yahoo_var_list <- c("GSPC")
yahoo_avg_prc <- yahoo_avg_prc %>%
  # year-over-year growth rates
  dplyr::mutate(
    gr4 = dplyr::across(
      dplyr::all_of(yahoo_var_list),
      ~ growth_rate(.x, delta = 4, type = "geometric", units = units)
    ),
    # quarterly growth rates
    gr1 = dplyr::across(
      dplyr::all_of(yahoo_var_list),
      ~ growth_rate(.x, delta = 1, type = "geometric", units = units)
    )
  )

# compute quarterly frequency returns and volatility from daily returns
yahoo_avg_ret <- yahoo_raw %>%
  dplyr::group_by(symbol) %>%
  # daily returns
  tidyquant::tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    type = "arithmetic",
    col_rename = "ret"
  ) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = ret) %>%
  dplyr::mutate(qtr = zoo::as.yearqtr(date)) %>%
  dplyr::group_by(qtr) %>%
  dplyr::filter(dplyr::between(
    zoo::as.Date(qtr),
    zoo::as.Date(zoo::as.yearqtr(date_begin)),
    zoo::as.Date(zoo::as.yearqtr(date_end))
  )) %>%
  dplyr::summarize(
    dplyr::across(
      !date,
      list(
        # average daily returns over the quarter
        ret = ~ mean(.x, na.rm = TRUE),
        # annualized std dev of daily returns over the quarter
        vol = ~ 100 * sqrt(252) * sd(.x, na.rm = TRUE)
      )
    )
  ) %>%
  dplyr::mutate(dplyr::across(!qtr, ~ na_if(., NaN)))

# rearrange, rename, merge
yahoo_avg_ret$qtr <- tsibble::yearquarter(yahoo_avg_ret$qtr)

yahoo <- inner_join(yahoo_avg_prc, yahoo_avg_ret)
yahoo <- tidyr::unnest(yahoo, cols = everything(), names_sep = ".")
yahoo <- dplyr::rename_with(yahoo, ~ tolower(.x))

## S&P 500 returns ---------------------------------------------------------
### Helper functions --------------------------------------------------------
#functions to collapse from daily to quarterly
aggregation_funs <- function(x) {
  dplyr::tibble(
    avg = na_if(mean(x, na.rm = TRUE), NaN),
    end_of_period = last(x, na_rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}
#functions to go from quarterly to annual
mean_4q <- timetk::slidify(mean, .period = 4, .align = "right")
cumret_4q <- timetk::slidify(\(x) purrr::reduce(x, \(cumret, ret) cumret * (1 + ret / 4), .init = 1), .period = 4, .align = "right")
cumsum_4q <- timetk::slidify(\(x) purrr::reduce(x, \(cumsum, ret) cumsum + ret / 4), .period = 4, .align = "right")

### Compute S&P 500 returns ------------------------------------------------------
#keep only adjusted end-of-day price
price <- yahoo_raw %>%
  dplyr::select(symbol, date, adjusted)

returns <-
  purrr::map(
    list("arithmetic", "log"),
    \(type)
    price %>%
      tidyquant::tq_transmute(
        select = adjusted,
        mutate_fun = allReturns,
        type = type
      ) %>%
      dplyr::rename_with(\(name) if (type == "log") paste0(name, "_log") else name)
  ) %>%
  dplyr::bind_cols() %>%
  # remove weekly returns
  dplyr::select(!c(date_log) & !contains("weekly")) %>%
  # rename variables
  dplyr::rename_if(is.numeric, \(x) paste0(x, "_ret")) %>%
  # annualize
  dplyr::mutate(
    dplyr::across(dplyr::contains("daily"), \(x) 252 * x),
    dplyr::across(dplyr::contains("monthly"), \(x) 12 * x),
    dplyr::across(dplyr::contains("quarterly"), \(x) 4 * x)
  )

### Aggregate to quarterly frequency --------------------------------------
#### Returns --------------------------------------------------------------
returns_aggregated_quarterly <- timetk::summarize_by_time(
  returns,
  .date_var = date,
  .by = "quarter",
  dplyr::across(dplyr::where(is.numeric),
         \(x) aggregation_funs(x),
         .unpack = TRUE
  ),
  .type = "ceiling"
) %>%
  #shift to the last day of the period
  dplyr::mutate(date = timetk::subtract_time(date, "1 day")) %>%
  #remove column if all NA
  dplyr::select_if(~ !all(is.na(.)))

#rename identical columns with the same name
duplicated_cols <- duplicated(as.matrix(returns_aggregated_quarterly), MARGIN = 2)
names(returns_aggregated_quarterly)[duplicated_cols] <- stringr::str_remove_all(names(returns_aggregated_quarterly)[duplicated_cols], "_end_of_period|_avg")
#remove identical columns
returns_aggregated_quarterly <- dplyr::as_tibble(unique(as.matrix(returns_aggregated_quarterly), MARGIN = 2, fromLast = TRUE)) %>%
  dplyr::mutate(dplyr::across(!c("date"), as.numeric)) %>%
  dplyr::mutate(dplyr::across(dplyr::any_of("date"), lubridate::as_date))
#order columns
returns_aggregated_quarterly <- returns_aggregated_quarterly %>%
  dplyr::relocate(sort(names(.))) %>%
  dplyr::relocate(dplyr::any_of(c("date")))

#### Prices --------------------------------------------------------------
price_aggregated_quarterly <- timetk::summarize_by_time(
  price,
  .date_var = date,
  .by = "quarter",
  dplyr::across(adjusted, last, .unpack = TRUE),
  .type = "ceiling"
) %>%
  #shift to the last day of the period
  dplyr::mutate(date = timetk::subtract_time(date, "1 day")) %>%
  #add lags
  timetk::tk_augment_lags(adjusted, .lags = c(1, 4))

#merge returns and price
ts_q <- dplyr::full_join(returns_aggregated_quarterly, price_aggregated_quarterly, by = c("date"))

### Compute annual returns at quarterly frequency in four different ways --------------------------------------------------------------------
annual_returns <- ts_q %>%
  dplyr::mutate(
    # 1. Average daily returns over a year,
    annual_ret_from_daily_avg = 100 * mean_4q(daily_ret_avg),
    
    # 2. Quarterly returns computed from average daily returns, cumulated over a year
    annual_cumret_from_quart_daily_avg = 100 * (cumret_4q(daily_ret_avg) - 1),
    
    # 3. Quarterly returns averaged over a year
    annual_avgret_from_quart = 100 * mean_4q(quarterly_ret),
    
    # 4. Quarterly returns computed from average daily returns, cumulated over a year
    annual_ret = 100 * (adjusted / adjusted_lag4 - 1)
  ) %>%
  dplyr::select(
    date,
    annual_ret_from_daily_avg, # 1
    annual_cumret_from_quart_daily_avg, # 2
    annual_avgret_from_quart, # 3
    annual_ret # 4
  )

annual_returns$qtr <- tsibble::yearquarter(annual_returns$date)
annual_returns <- annual_returns %>% 
  dplyr::select(-c(date)) 

## Variables derived from data in files imported from Stata ---------------
# convert Stata dates to R dates
toRTime <- function(x) {
  (
    dplyr::mutate(x,
      date = stataXml::fromStataTime(time, "%tq"),
      qtr = tsibble::yearquarter(date),
      time = NULL
    )
  )
}

instruments <- instruments %>%
  toRTime() %>%
  dplyr::select(!(date))
gs_fci <- gs_fci %>%
  toRTime() %>%
  dplyr::select(!(date))
gz_ebp_ecy <- gz_ebp_ecy %>%
  toRTime() %>%
  dplyr::select(!(date))
ciss <- ciss %>%
  toRTime() %>%
  dplyr::select(!(date))



# ACM term premium --------------------------------------------------------
library(readxl)
ACMTermPremium <- read_excel(here::here("data", "ACMTermPremium.xls"))

ACMTermPremium_quarterly <- ACMTermPremium %>% 
  mutate(DATE = as.Date(DATE,format = "%d-%b-%Y")) %>% 
  timetk::summarize_by_time(
    .date_var = DATE,
    .by = "quarter",
    dplyr::across(dplyr::where(is.numeric),
                  \(x) last(x),
                  .unpack = TRUE
    ),
    .type = "ceiling",
    .week_start = 1
  ) %>%
  rename(date = DATE) %>% 
  #shift to the last day of the period
  dplyr::mutate(date = timetk::subtract_time(date, "1 day")) %>%
  #remove column if all NA
  dplyr::select_if(~ !all(is.na(.))) %>% 
  mutate(
    qtr = yearquarter(date)
  ) %>% 
  arrange(qtr) %>% 
  select(!date)

# Merge all ---------------------------------------------------------------
variables <- purrr::reduce(
  list(fred, yahoo, annual_returns, instruments, gs_fci, gz_ebp_ecy, ciss),
  dplyr::full_join, 
  by = "qtr"
)

variables <- purrr::reduce(
  list(variables, ACMTermPremium_quarterly),
  dplyr::left_join, 
  by = "qtr"
)

# VFCI --------------------------------------------------------------------

# financial_vars <- c("gspc_vol","gr4.gspc","t10y3m","tb3smffm","aaa10ym","baa_aaa") # old from stata, wrong
financial_vars <- c("gspc_vol","annual_ret","t10y3m","tb3smffm","aaa10ym","baa_aaa")  # choose returns from # annual_ret_from_daily_avg, annual_cumret_from_quart_daily_avg, annual_avgret_from_quart, annual_ret
dep_vars <- list("fgr1.gdpc1","fgr1.pcecc96")
lag_vars <- c("gr1.gdpc1","lgr1.gdpc1","l2gr1.gdpc1","l3gr1.gdpc1")
date_begin <- "1962 Q1"
date_end <- "2022 Q3"

results <- dep_vars %>% 
  purrr::set_names() %>% 
  purrr::map(~get_vfci(variables,.,financial_vars,prcomp=TRUE,n_prcomp = 4,date_begin=date_begin, date_end=date_end))

vfci_cons <- dplyr::select(results$fgr1.pcecc96$ts, c("qtr", "vfci")) %>% dplyr::rename(vfci_pce = vfci)

results$vfci_ind <- get_vfci(variables,"fgr1.gdpc1",financial_vars,prcomp=FALSE,n_prcomp = 4,date_begin=date_begin, date_end=date_end)$ts %>% 
  rename(vfci_ind = vfci, mu_ind=mu)

results$vfci_ea <- get_vfci(variables,"fgr1.clvmnacscab1gqea19",c("ciss"),prcomp=FALSE,n_prcomp = 4,date_begin="1995 Q1", date_end="2022 Q2")$ts %>% 
  rename(vfci_ea = vfci, mu_ea=mu)

## Try different number of PCs in the estimation
results_pcs <- 1:6 %>% 
  purrr::set_names() %>% 
  purrr::map(~get_vfci(variables,"fgr1.gdpc1",financial_vars,prcomp=TRUE,n_prcomp = .,date_begin=date_begin, date_end=date_end))


vfci_baseline <- results$fgr1.gdpc1 #results$vfci_lags_in_mean

results$vfci_no_rvol <- get_vfci(variables,"fgr1.gdpc1",c("annual_ret","t10y3m","tb3smffm","aaa10ym","baa_aaa"),prcomp=TRUE,n_prcomp = 4,date_begin=date_begin, date_end=date_end)$ts %>% 
  rename(vfci_no_rvol = vfci, mu_no_rvol=mu)

results$vfci_stocks <-
  get_vfci(variables, "fgr1.gdpc1", c("gspc_vol", "annual_ret"), prcomp = FALSE, date_begin = date_begin, date_end = date_end)$ts |>
  rename(vfci_stocks = vfci, mu_stocks = mu)

results$vfci_ret <-
  get_vfci(variables, "fgr1.gdpc1", c("annual_ret"), prcomp = FALSE, date_begin = date_begin, date_end = date_end)$ts |>
  rename(vfci_ret = vfci, mu_ret = mu)

# merge, tidy NA
variables <- variables |> 
  dplyr::full_join(vfci_baseline$ts, by = "qtr")

variables <-
  variables |>
  dplyr::mutate(total_log_vol = log(epsilon^2) / 2) |>
  dplyr::mutate(resid_log_vol = total_log_vol - vfci)

## Need to use the PC calculated above to estimate the following VFCI robustness
pcs <- paste0("pc", 1:4)
variables <- variables |> mutate(f1.dgs1 = lead(dgs1), f1.dgs5 = lead(dgs5), f1.dgs10 = lead(dgs10))
results$vfci_yields <- get_vfci(variables, y = "fgr1.gdpc1", x = c(pcs, "f1.dgs1", "f1.dgs5", "f1.dgs10"), het = c(pcs), prcomp=FALSE, n_prcomp = 4, date_begin=date_begin, date_end=date_end)$ts %>% 
  rename(vfci_yields = vfci, mu_yields = mu)

results$vfci_lags <- get_vfci(
    variables,
    "fgr1.gdpc1",
    c(pcs, lag_vars),
    het = c(pcs, lag_vars),
    prcomp=FALSE,
    n_prcomp = 4,
    date_begin=date_begin,
    date_end=date_end
  ) 
results$vfci_lags$ts <- results$vfci_lags$ts %>% 
  rename(
      vfci_lags = vfci,
      mu_lags=mu
)
  
results$vfci_lags_in_mean <- get_vfci(
    variables,
    "fgr1.gdpc1",
    c(pcs, lag_vars),
    het = pcs,
    prcomp=FALSE,
    n_prcomp = 4,
    date_begin=date_begin,
    date_end=date_end
  )
results$vfci_lags_in_mean$ts <- results$vfci_lags_in_mean$ts %>% 
  rename(vfci_lags_in_mean = vfci, mu_lags_in_mean=mu)

results$vfci_lags_in_vol <- get_vfci(
    variables,
    "fgr1.gdpc1",
    financial_vars,
    het = c(pcs, lag_vars),
    prcomp=FALSE,
    n_prcomp = 4,
    date_begin=date_begin,
    date_end=date_end
  )
results$vfci_lags_in_vol$ts <- results$vfci_lags_in_vol$ts %>% 
  rename(
        vfci_lags_in_vol = vfci,
        mu_lags_in_vol = mu
  )


## Clean up the VFCI_pc# series
pcs_df <- results_pcs |>
  purrr::map(~ .x$ts) |>
  purrr::list_rbind(names_to = "pc") |>
  dplyr::mutate(pc = as.numeric(pc)) |>
  dplyr::group_by(pc) |>
  dplyr::mutate(vfci_scaled = scale(vfci)) |>
  ungroup()

pcs_wide <-
  pcs_df |>
  dplyr::mutate(name = paste0("vfci_pc", pc)) |>
  dplyr::select(qtr, name, vfci) |>
  tidyr::pivot_wider(names_from = "name", values_from = vfci)


## Merge on all of the VFCI robustness
variables <- variables |>
  dplyr::full_join(results$vfci_lags$ts, by = "qtr") %>%
  dplyr::full_join(results$vfci_lags_in_vol$ts, by = "qtr") %>%
  dplyr::full_join(results$vfci_lags_in_mean$ts, by = "qtr") %>%
  dplyr::full_join(results$vfci_ea, by = "qtr") %>%
  dplyr::full_join(results$vfci_ind, by = "qtr") %>%
  dplyr::full_join(vfci_cons, by = "qtr") %>%
  dplyr::full_join(results$vfci_stocks, by = "qtr") %>%
  dplyr::full_join(results$vfci_ret, by = "qtr") %>%
  dplyr::full_join(pcs_wide, by = "qtr") %>%
  dplyr::full_join(results$vfci_yields, by = "qtr") %>%
  dplyr::full_join(dplyr::select(results$vfci_no_rvol, qtr, vfci_no_rvol), by = "qtr")

# Save ---------------------------------------------------------------------

# save and export
variables$date <- zoo::as.Date(variables$qtr)
variables$yr <- tidyquant::YEAR(variables$date)
variables$quarter <- tidyquant::QUARTER(variables$date)

variables <- variables %>%
  dplyr::mutate(
    vfci_lev = exp(vfci)
  ) %>% 
  dplyr::mutate(
      infl_pce = gr1.pcepilfe,
      lgdp = log.gdpc1,
      lpce = log.pcepilfe,
      ygr = gr1.gdpc1
  ) %>% 
  dplyr::relocate(c(
    "date","yr","quarter",
    "gdpc1","gdppot","pcepilfe","pcecc96","fedfunds","anfci","nfci",
    "gs10","tb3ms","med3","tb3smffm","aaa10ym",
    "wtisplc","baa10ym","lior3m","tedrate","vixcls","clvmnacscab1gqea19",
    "ygr","lgdp","infl_pce",
    "gr4.pcecc96","gr4.clvmnacscab1gqea19",
    "gr1.gdpc1","gr1.pcepilfe","gr1.pcecc96","gr1.clvmnacscab1gqea19",
    "lgdp","lpce","log.pcecc96","log.clvmnacscab1gqea19",
    "fgr4.gdpc1","fgr4.pcepilfe","fgr4.pcecc96","fgr4.clvmnacscab1gqea19",
    "fgr1.gdpc1","fgr1.pcepilfe","fgr1.pcecc96","fgr1.clvmnacscab1gqea19",
    "gap","baa_aaa","t10y3m","tedr","es","gspc",
    "gr4.gspc","gr1.gspc","gspc_ret","gspc_vol",
    "mp_shock_ns","mp_shock_mr","mp_shock_rr","mp_shock_int_rr_mar_ns","mp_shock_int_rr_ns",
    "y_shock","std_y_shock","std_mp_shock_ns","std_mp_shock_int_rr_ns",
    "gsfci","ecy","ebp","gz","ciss",
    "pc1","pc2","pc3","pc4","pc5","pc6",
    "mu","vfci","vfci_lev"
    ))

variables <- variables |>
  tsibble::as_tsibble(index = qtr) |>
  tsibble::filter_index(date_begin ~ date_end)
  
save(variables, file = here::here("variables.RData"))

