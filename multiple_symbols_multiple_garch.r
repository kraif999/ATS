# Load all libraries
libraries <- c("PerformanceAnalytics", "ggplot2", "lmtest", "fBasics", "urca", "forecast", "quantmod", "tseries", "fUnitRoots", "xts",  "fBasics", "tseries",
 "car", "FinTS", "fGarch",  "psych", "rugarch", "parallel", "caTools", "plyr", "expss", "base", "tidyr", "dplyr", "MLmetrics", "tibble", "gridExtra", "writexl",
 "doParallel", "parallel", "lubridate", "reshape2", "R6")
invisible(lapply(libraries, library, character.only = TRUE))

# Load helper functions
source("helper_garch.R")

options(scipen = 999)

# Meta data with symbols description
meta <- jsonlite::fromJSON("instr_config.json")

# rm(list = ls())

# Download data
symbol <- "GC=F" # gold
meta$assets[[symbol]]$class

from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2024-10-03", format = "%Y-%m-%d")

symbols <- c("GC=F","SI=F")

final <- list()

for (symbol in symbols) {

class <- meta$assets[[symbol]]$class # class of symbol

data <- getSymbols(symbol, from = from_date, to = to_date, period = "day", auto.assign = FALSE) %>%
  na.omit

# Convert instr to data.frame, rename columns, compute log returns, convert back to xts which is required format in ugarchroll and ugarchfit
instr_ret <- data %>%
  as.data.frame %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(rets = as.numeric(log(Close/lag(Close))), 
             Date = as.Date(rownames(.))) %>%
        na.omit %>%
          select(Date, rets) %>%
            xts(order.by = .$Date) %>%
              subset(select = -Date)

# Check if all dates are weekdays
all(weekdays(index(data)) != "Saturday" & weekdays(index(data)) != "Sunday")

# Compute equity line additionally, remove NAs
instr <- data %>%
  as.data.frame() %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(TradeDate = as.Date(rownames(.))) %>%
        select(TradeDate, Open, High, Low, Close) %>%
          mutate(rets = as.numeric(log(Close/lag(Close))),
                  EquityLine = cumprod(ifelse(is.na(rets), 1, 1 + rets))) %>%
            na.omit
          

# Estimate historical (realized) volatility
histVolest <- estimate_realized_volatility(data)

# Check quantiles of different realized volatility estimators
histVol <- as.data.frame(apply(histVolest %>% select(-TradeDate), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))

# Generate signals based on GARCH model volatility forecasts (as example for commodities, for other classes  you might need to adjust the entry/exit criteria)
# Create class with signals strategies?

generate_entry_signals <- function(volData) {

  modified_volData <- volData %>%
    mutate(
      signal = case_when(
        Forecast < quantile(Forecast, probs = 0.75) ~ 1,
        Forecast > quantile(Forecast, probs = 0.75) ~ -1,
        TRUE ~ 0    
      )
    ) %>%
    slice(-1)  # Remove the first row since it will have NA for signal
  
  return(modified_volData)
}

# Generate performance results given historical volatility estimator, signal generation criteria and GARCH model specification
listgarch <- generate_combinations(
  RV = "close", 
  entry_signal_function = generate_entry_signals,
  specification = "sGARCH", n_start = 252, refit_every = 21, refit_window = 252, distribution_model = "norm", realized_vol = "close"
  )

final[[symbol]] <- listgarch

}

final_df <- bind_rows(final)
