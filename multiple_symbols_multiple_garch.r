# Load all libraries
libraries <- c("PerformanceAnalytics", "ggplot2", "lmtest", "fBasics", "urca", "forecast", "quantmod", "tseries", "fUnitRoots", "xts",  "fBasics", "tseries",
 "car", "FinTS", "fGarch",  "psych", "rugarch", "parallel", "caTools", "plyr", "expss", "base", "tidyr", "dplyr", "MLmetrics", "tibble", "gridExtra", "writexl",
 "doParallel", "parallel", "lubridate", "reshape2", "R6", "stringr")
invisible(lapply(libraries, library, character.only = TRUE))

# Load helper functions
source("helper_garch.R")

options(scipen = 999)

# Asset meta data (asset, symbol, class, description)
meta <- jsonlite::fromJSON("instr_config.json")

# rm(list = ls())

# Dates to download ts data
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2024-10-03", format = "%Y-%m-%d")

# Select all symbols within a given class (for example, Commodities)
symbols <- names(Filter(function(x) x$class == "Commodities", meta$assets))

# Setting up clusters for parallel processing in 'ugarchroll'
getDoParWorkers()
max_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(max_cores)

final <- list() # list  to store the results

# Looping over symbols within a given single class
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

# Compute uantiles of different realized volatility estimators
histVol <- as.data.frame(apply(histVolest %>% select(-TradeDate), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))

# Generate signals based on GARCH model volatility forecasts (as example for commodities, for other classes  you might need to adjust the entry/exit criteria)
generate_entry_signals <- function(volData) {

  modified_volData <- volData %>%
    mutate(
      q75 = quantile(Forecast[1:n()], probs = 0.75),
      signal = case_when(
        # Forecast < quantile(Forecast, probs = 0.75) ~ 1,
        # Forecast > quantile(Forecast, probs = 0.75) ~ -1,
        Forecast < q75 ~ 1,
        Forecast > q75 ~ -1,
        TRUE ~ 0    
      )
    ) %>%
    slice(-1)  # Remove the first row since it will have NA for signal
  
  return(modified_volData)
}

# Generate performance results given historical volatility estimator, signal generation criteria and GARCH model specification

# Multiple specifications
listgarch <- generate_combinations(
  RV = "close", # historical volatility estimation
  entry_signal_function = generate_entry_signals, # signals generation engine
  # GARCH specifications
  specification = c("eGARCH", "gjrGARCH"), 
  n_start = c(252,126), # also, it is window.size
  refit_every = 21, 
  refit_window = c("expanding", "moving"), 
  distribution_model = c("snorm"), 
  realized_vol = "close",
  plots_path = "EquityLines_garch_experiment_modified_signal/", # folder to save equity lines for GARCH based strategy vs Passive strategy 
  cl
  )

final[[symbol]] <- listgarch

}

final_df <- bind_rows(final) %>%
  arrange(desc(aR)) %>%
    filter(trades != 0)

fwrite(final_df, "GARCH_based_strategies_performance.csv")