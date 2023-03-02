# install.packages("tidyverse")
# install.packages("tidyquant")
# install.packages("timetk")
# install.packages("lubridate")
# install.packages("vtable")

library(tidyverse)
library(tidyquant)
library(timetk)
library(lubridate)
library(vtable)

# Helper functions --------------------------------------------------------
# Functions to collapse from daily to quarterly
aggregation_funs <- function(x) {
  tibble(
    avg = na_if(mean(x, na.rm=TRUE),NaN),
    end_of_period = last(x, na_rm = TRUE),
    sd = sd(x, na.rm=TRUE)
  )
}
# Functions to go from quarterly to annual
mean_4q <- slidify(mean, .period = 4, .align = "right")
cumret_4q <- slidify(\(x) reduce(x, \(cumret,ret) cumret*(1+ret/4), .init=1), .period = 4, .align = "right")
cumsum_4q <- slidify(\(x) reduce(x, \(cumsum,ret) cumsum+ret/4), .period = 4, .align = "right")

# Download data -----------------------------------------------------------
yahoo_raw <- tq_get(
  "^GSPC",
  get = "stock.prices",
  from = "1950-01-01",
  to = "2023-01-01"
)

# Compute S&P 500 returns ------------------------------------------------------
# keep only adjusted end-of-day price
price <- yahoo_raw %>% 
  select(symbol,date,adjusted) 

returns <-
  map(
    list("arithmetic", "log"),
    \(type)
    price %>%
      tq_transmute(
        select = adjusted,
        mutate_fun = allReturns,
        type = type
      ) %>%
      rename_with(\(name) if (type == "log") paste0(name, "_log") else name)
  ) %>%
  bind_cols() %>%
  # remove weekly returns
  select(!c(date_log) & !contains("weekly")) %>%
  # rename variables
  rename_if(is.numeric,\(x) paste0(x,"_ret")) %>%
  # annualize
  mutate(
    across(contains("daily"),\(x) 252*x),
    across(contains("monthly"),\(x) 12*x),
    across(contains("quarterly"),\(x) 4*x)
  ) 

# Aggregate to quarterly frequency ----------------------------------------
## Returns ----------------------------------------
returns_aggregated_quarterly <- summarize_by_time(
  returns,
  .date_var = date,
  .by = "quarter",
  across(where(is.numeric),
         \(x) aggregation_funs(x),
         .unpack = TRUE
  ),
  .type = "ceiling"
) %>%
  # Shift to the last day of the period
  mutate(date = subtract_time(date, "1 day")) %>%
  # Remove column if all NA
  select_if(~!all(is.na(.)))

# Rename identical columns with the same name
duplicated_cols <- duplicated(as.matrix(returns_aggregated_quarterly),MARGIN=2)
names(returns_aggregated_quarterly)[duplicated_cols]<-str_remove_all(names(returns_aggregated_quarterly)[duplicated_cols],"_end_of_period|_avg")
# Remove identical columns
returns_aggregated_quarterly <- as_tibble(unique(as.matrix(returns_aggregated_quarterly),MARGIN=2, fromLast = TRUE )) %>%
  mutate(across(!c("date"),as.numeric)) %>%
  mutate(across(any_of("date"),as_date))
# Order columns
returns_aggregated_quarterly <- returns_aggregated_quarterly %>% relocate(sort(names(.))) %>%  relocate(any_of(c("date")))

## Prices ----------------------------------------
price_aggregated_quarterly <- summarize_by_time(
  price,
  .date_var = date,
  .by = "quarter",
  across(adjusted,last,.unpack = TRUE),
  .type = "ceiling"
) %>%
  # Shift to the last day of the period
  mutate(date = subtract_time(date, "1 day")) %>% 
  # Add lags
  tk_augment_lags(adjusted, .lags=c(1,4))

# merge returns and price
ts_q <- full_join(returns_aggregated_quarterly,price_aggregated_quarterly, by=c("date"))

# Compute annual returns at quarterly frequency in four different ways --------------------------------------------------------------------
annual_returns <- ts_q %>%
  mutate(
    # 1. Average daily returns over a year,
    annual_ret_from_daily_avg =  mean_4q(daily_ret_avg),
    
    # 2. Quarterly returns computed from average daily returns, cumulated over a year
    annual_cumret_from_quart_daily_avg =  cumret_4q(daily_ret_avg)-1,
    
    # 3. Quarterly returns averaged over a year
    annual_avgret_from_quart =  mean_4q(quarterly_ret),
    
    # 4. Quarterly returns computed from average daily returns, cumulated over a year
    annual_ret =  adjusted/adjusted_lag4-1
  ) %>%
  select(
    date,
    annual_ret_from_daily_avg, # 1
    annual_cumret_from_quart_daily_avg, # 2
    annual_avgret_from_quart, # 3
    annual_ret # 4
  )

# Show summary stats in viewer ------------------------------------------------------
annual_returns %>% sumtable
print(cor(data.matrix( annual_returns %>% select(-date) ), use="pairwise.complete.obs"),digits=3)

# Nice manners: keep only variable with final results
rm(list=setdiff(ls(), "annual_returns"))


