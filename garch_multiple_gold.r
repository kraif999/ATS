# Load all libraries
libraries <- c("PerformanceAnalytics", "ggplot2", "lmtest", "fBasics", "urca", "forecast", "quantmod", "tseries", "fUnitRoots", "xts",  "fBasics", "tseries",
 "car", "FinTS", "fGarch",  "psych", "rugarch", "parallel", "caTools", "plyr", "expss", "base", "tidyr", "dplyr", "MLmetrics", "tibble", "gridExtra", "writexl",
 "doParallel", "parallel", "lubridate", "reshape2")
invisible(lapply(libraries, library, character.only = TRUE))

# Load helper functions
source("helper_garch.R")

options(scipen = 999)

# Meta data with symbols description
meta <- jsonlite::fromJSON("instr_config.json")

# rm(list = ls())

# modify transactional cost value as for different asset classes they are different

# Download data
symbol <- "GC=F" # gold
symbol <- "SI=F" # silver
# symbol <- "^GSPC" # sp500
instr <- getSymbols(symbol, from = "2007-01-01", to = "2024-10-03", period = "day", auto.assign = FALSE) %>%
  na.omit

any(is.na(instr))

# Convert instr to data.frame, rename columns, compute log returns, convert back to xts which is required format in ugarchroll and ugarchfit
instr_ret <- instr %>%
  as.data.frame %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(rets = as.numeric(log(Close/lag(Close))), 
             Date = as.Date(rownames(.))) %>%
        na.omit %>%
          select(Date, rets) %>%
            xts(order.by = .$Date) %>%
              subset(select = -Date)

# Check if all dates are weekdays
all(weekdays(index(instr)) != "Saturday" & weekdays(index(instr)) != "Sunday")

# Select only Open, High, Low, Close
ohlc <- instr %>% 
  data.frame %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      select(-Volume, -Adjusted) %>%
        na.omit  %>%
          as.matrix

# Compute equity line additionally, remove NAs
instr <- instr %>%
  as.data.frame() %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(TradeDate = as.Date(rownames(.))) %>%
        select(TradeDate, Open, High, Low, Close) %>%
          mutate(rets = as.numeric(log(Close/lag(Close))),
                  EquityLine = cumprod(ifelse(is.na(rets), 1, 1 + rets))) %>%
            na.omit

# Plot Close price
ggplot(instr, aes(x = TradeDate, y = Close)) +
  geom_line(color = "black") +
  labs(title = paste0("Daily close price of ", symbol, "(", meta$assets[[symbol]]$description, ")"),
       x = "Trade Date",
       y = "Close price") +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") + 
  theme_minimal()
          
# Calculate summary statistics for instr
calculate_summary_statistics(instr)

# Different realized volatility estimators for returns (TTR)
histVolest <- merge(
  garman <- as.xts(na.omit(volatility(ohlc, calc = "garman"))) / sqrt(252),
  close <- as.xts(na.omit(volatility(ohlc[,4], calc = "close"))) / sqrt(252),
  parkinson <- as.xts(na.omit(volatility(ohlc, calc = "parkinson"))) / sqrt(252),
  rogers.satchell <- as.xts(na.omit(volatility(ohlc, calc = "rogers.satchell"))) / sqrt(252),
  garman_modified <- as.xts(na.omit(volatility(ohlc, calc = "gk.yz"))) / sqrt(252),
  yang.zhang <- as.xts(na.omit(volatility(ohlc, calc = "yang.zhang"))) / sqrt(252)
) %>% 
  as.data.frame %>% 
    rename_with(~ c("garman", "close", "parkinson", "rogers_satchell", "garman_modified", "yang_zhang")) %>%
      mutate(TradeDate = as.Date(rownames(.))) %>%
        select(TradeDate, everything(.)) %>%
          na.omit

# Check quantiles of different realized volatility estimators
histVol <- as.data.frame(apply(histVolest %>% select(-TradeDate), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))
histVol

# Generate all possible combinations including performance metrics
listgarch <- expand.grid(
  specification = as.character(c("sGARCH", "eGARCH", "gjrGARCH", "fGARCH")),
  n.start = as.numeric(c("252", "126", "504")),
  refit.every = as.numeric(c("21", "63", "126")),
  refit.window = as.character(c("moving", "expanding")),
  distribution.model = as.character(c("norm", "snorm", "nig")),
  realized.vol = as.character(c("close", "yang_zhang")),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  trades = 0,
  signal_long = 0,
  rmse = 0,
  elf = 0,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>% mutate(across(c(aR, aSD, MD, IR, trades, signal_long, rmse, elf), as.numeric))
colnames(listgarch)[1:6] <- c("specification","window.size", "refit.frequency", "refit.window.type", "distribution.model", "realized.vol.method")

# Generate all possible combinations including performance metrics
listgarch <- expand.grid(
  specification = as.character(c("sGARCH", "eGARCH")),
  n.start = as.numeric(c("252", "504")),
  refit.every = as.numeric(c("63", "126")),
  refit.window = as.character(c("moving", "expanding")),
  distribution.model = as.character(c("norm", "snorm")),
  realized.vol = as.character(c("close")),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  trades = 0,
  signal_long = 0,
  rmse = 0,
  elf = 0,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>% mutate(across(c(aR, aSD, MD, IR, trades, signal_long, rmse, elf), as.numeric))
colnames(listgarch)[1:6] <- c("specification","window.size", "refit.frequency", "refit.window.type", "distribution.model", "realized.vol.method")

dim(listgarch)[1]

################################################################################
# Roll density forecasts (one day ahead)
################################################################################

getDoParWorkers()
max_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(max_cores)
registerDoParallel(cl)

Historical <- "close" # which realized volatility estimation is chosen

for (i in 1:dim(listgarch)[1]){
  tryCatch({ 
  
  if(listgarch[i,1] == "fGARCH") {
    spec <- ugarchspec(
        variance.model = list(
        model = listgarch[i,1],
        garchOrder = c(1, 1), 
        submodel = "TGARCH", 
        external.regressors = NULL, 
        variance.targeting = FALSE), 
        
        mean.model = list(
        armaOrder = c(1, 1), 
        include.mean = TRUE, 
        archm = FALSE, 
        archpow = 1, 
        arfima = FALSE, 
        external.regressors = NULL, 
        archex = FALSE),

        distribution.model = listgarch[i,5]) 
    # , start.pars = list(), fixed.pars = list(), ...)
  } else {
    spec <- ugarchspec(
        variance.model = list(
        model = listgarch[i,1], 
        garchOrder = c(1, 1), 
        submodel = NULL, 
        external.regressors = NULL, 
        variance.targeting = FALSE), 

        mean.model = list(
        armaOrder = c(1, 1), 
        include.mean = TRUE, 
        archm = FALSE, 
        archpow = 1, 
        arfima = FALSE, 
        external.regressors = NULL, 
        archex = FALSE),
        
        distribution.model = listgarch[i,5]) 
  }

  if(listgarch[i,4] == "moving") {
    roll = ugarchroll(
        spec, 
        instr_ret[,1], 
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch[i,2],  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch[i,3], # determines every how many periods the model is re-estimated.
        refit.window = listgarch[i,4], # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead).
        window.size = listgarch[i,2],
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = cl,
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
} else {
    roll = ugarchroll(
        spec, 
        instr_ret[,1], 
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch[i,2],  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch[i,3], # determines every how many periods the model is re-estimated.
        refit.window = listgarch[i,4], # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead)
        # window.size = listgarch[i,2],
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = cl,
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
  }

# roll <- resume(roll, solver= "gosolnp") # if object contains non-converged windows

# show(roll) # 20.02 secs

forecastVolRoll <- data.frame(
  Date = roll@model$index[(listgarch$window.size[i]+1):length(roll@model$index)],
  Forecast = roll@forecast$density$Sigma
)

# Join realized volatility estimation and instr log returns given TradeDate
volForHistRoll <- forecastVolRoll %>%
  mutate(TradeDate = Date) %>% 
    left_join(histVolest, by = 'TradeDate') %>%
      na.omit %>% select(-Date) %>%
        select(TradeDate, everything()) %>%
          left_join(select(instr, TradeDate, Close, rets), by = "TradeDate")

# Choose historical volatility estimator to compare with one day ahead rolling forecasts
as.data.frame(apply(volForHistRoll %>% select(Forecast, histVol %>% names), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))

################################################################################
# Generate entry signals
################################################################################
volForHistRoll <- volForHistRoll %>%
   mutate(
  #   signal = case_when(
  #     Forecast > lag(volForHistRoll[[Historical]]) ~ 1,
  #     Forecast < lag(volForHistRoll[[Historical]]) ~ -1,
  #     TRUE ~ 0
  # ),
    signal = case_when(
      # Condition
      Forecast < quantile(Forecast, probs = 0.75) ~ 1,
      Forecast > quantile(Forecast, probs = 0.75) ~ -1,
      TRUE ~ 0    
          )) %>%
  slice(-1)

short <- sum(volForHistRoll$signal < 0) # how many times we sell
long <- sum(volForHistRoll$signal > 0) # how many times we buy

################################################################################
# Strategy performance
################################################################################

performance <- calculate_eqlGARCH_no_TC(volForHistRoll) # contains number of positions, equity lines, return of equity line given initial investment 

# Performance metrics of strategy based on GARCH
aR <- round(as.numeric(Return.annualized(as.numeric(performance$r_eqlGARCH), scale = 252, geometric = TRUE) * 100), 3)
aSD <- round(as.numeric(StdDev.annualized(as.numeric(performance$r_eqlGARCH), scale = 252) * 100), 3)
IR <- round(as.numeric(aR / aSD), 3) 
MD <- round(as.numeric(maxDrawdown(as.numeric(performance$r_eqlGARCH), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
trades <- sum(c(1, ifelse(performance$signal[-1] * performance$signal[-length(performance$signal)] < 0, 1, 0)))
signal_long <- round(long/(long+short),3) * 100 # long signal percentage
rmse <- round(RMSE(volForHistRoll$Forecast, volForHistRoll[[Historical]]) * 100, 3)
elf <- NA

listgarch[i, "aR"] <- aR
listgarch[i, "aSD"] <- aSD
listgarch[i, "MD"] <- MD
listgarch[i, "IR"] <- IR
listgarch[i, "trades"] <- trades
listgarch[i, "signal_long"] <- signal_long
listgarch[i, "rmse"] <- rmse
listgarch[i, "ELF"] <- elf

# Passive strategy
passive <- data.frame(
  Buy_and_Hold = as.character(paste0(meta$assets[[symbol]]$description, "_buy_and_hold")),
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  stringsAsFactors = FALSE
) %>%
mutate(
  aR = round(as.numeric(Return.annualized(as.numeric(performance$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3),
  aSD = round(as.numeric(StdDev.annualized(as.numeric(performance$r_eqlPassive), scale = 252) * 100), 3),
  IR = round(aR / aSD, 3),
  MD = round(as.numeric(maxDrawdown(as.numeric(performance$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
)

active_passive <- bind_rows(passive %>% rename(Strategy_Specification = Buy_and_Hold), 
          listgarch[i,] %>% rename(Strategy_Specification = specification) %>% select(Strategy_Specification, aR, aSD, MD, IR))

active_passive

print(active_passive)

rm(roll, passive)

  } ,error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#   }, error = function(e) {
#      cat("ERROR :",conditionMessage(e), "\n")
#   })
# }

#fwrite(listgarch, "listgarchAll.csv")

ggplot(performance, aes(x = TradeDate)) +
  geom_line(aes(y = eqlGARCH, color = "eqlGARCH")) +
  geom_line(aes(y = eqlPassive, color = "eqlPassive")) +
  labs(title = paste("Equity Lines (buy and hold) Vs GARCH based investing", symbol, "(", meta$assets[[symbol]]$description, ")"), y = "Equity", x = "Date") +
  scale_color_manual(name = "Equity Line",
  values = c("eqlGARCH" = "blue", "eqlPassive" = "red"))


