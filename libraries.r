# Copyright (c) 2024 Oleh Bilyk

# List of required libraries
required_libraries <-  c("PerformanceAnalytics", "ggplot2", "lmtest", "fBasics", "urca", "forecast", "quantmod", "tseries", "fUnitRoots", "xts",  "fBasics", "tseries",
 "car", "FinTS", "fGarch",  "psych", "rugarch", "parallel", "caTools", "plyr", "expss", "base", "tidyr", "dplyr", "MLmetrics", "tibble", "gridExtra", "writexl",
 "doParallel", "parallel", "lubridate", "reshape2", "R6", "stringr", "aTSA", "TTR", "purrr", "slider", "plotly", "vscDebugger", "lubridate", "patchwork")

invisible(lapply(required_libraries, library, character.only = TRUE))

# Function to load/install libraries
load_libraries <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

# Load the required libraries
load_libraries(required_libraries)

options(scipen = 999)

# Asset meta data (asset, symbol, class, description)
meta <- jsonlite::fromJSON("instr_config.json")

# List of assets
assets <- c(
  "EURUSD=X", "GBPUSD=X", "JPY=X", "AUDUSD=X", # FX most tradable
  "USDPLN=X", "MXN=X", # FX emerging
  "^GSPC", "^IXIC",  # US Equtities
  "^FTSE", "^GDAXI", "^N100", # Europe Equities
  # "WIG20.WA", data is not available
  "EPOL", # ishares MCSI Poland
  "^N225", "^HSI", "000001.SS", # Asia Equities
  "AGG", # Fixed Income
  "GC=F", # Commodities (gold)
  "BZ=F", # Commodities (oil)
  "SI=F", # Commodities (silver)
  "NG=F", # Commodities (natural gas)
  "HG=F", # Commodities (copper)
  "BTC-USD", # Cryptocurrency (BTC)
  "ETH-USD" # Cryptocurrency (Ethereum)
  )