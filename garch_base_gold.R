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

# Download data
symbol <- "GC=F" # gold
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
        na.omit

# Visualize realized volatility estimators given different approaches
histVolest %>% 
  ggplot(aes(x = TradeDate)) + 
  ggtitle("Volatility under different estimators from TTR package") +
  ylab("Daily volatility estimator") +
  geom_line(aes(y = close, color = "Close-to-Close")) +
  geom_line(aes(y = garman, color = "Garman-Klass")) +
  geom_line(aes(y = parkinson, color = "Parkinson")) +
  geom_line(aes(y = rogers_satchell, color = "Rogers_Satchell")) +
  geom_line(aes(y = garman_modified, color = "Garman_modified")) +
  geom_line(aes(y = yang_zhang, color = "Yang_Zhang")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years")

# Check quantiles of different realized volatility estimators
histVol <- as.data.frame(apply(histVolest %>% select(-TradeDate), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))
histVol

# Create listgarch data frame with the following parameters
listgarch <- data.frame(
  specification = "sGARCH",
  window.size = 252,
  refit.frequency = 21,
  refit.window.type = "moving",
  distribution.model = "norm",
  realized.vol.method = "close",
  aR = 0,
  aSD = 0,
  MD = 0,
  IR = 0,
  trades = 0,
  signal_long = 0,
  rmse = 0,
  elf = 0, # economic loss function
  stringsAsFactors = FALSE
)

################################################################################
# Fit GARCH model
################################################################################
# GARCH model specificaiton
# Stylized facts
# Volatility clustering
# Fat tails
# Assymetry: different response to shocks with same magnitude
# Long memory
# Leverage: negative returns -> higher volatility

spec <- ugarchspec(
  variance.model = list(
  model = listgarch$specification, 
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
  distribution.model = listgarch$distribution.model
  )

n.roll = 252
fit <- ugarchfit(spec, data = instr_ret$rets, n.roll = n.roll, out.sample = n.roll)

residuals  <- data.frame(
  Date = fit@model$modeldata$index[(n.roll+1):length(fit@model$modeldata$index)],
  Residuals = fit@fit$residuals
)

acf(residuals$Residuals)
ggplot(data.frame(x = fit@fit$residuals), aes(x)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001, fill = "lightblue", color = "black", boundary = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(fit@fit$residuals), sd = sd(fit@fit$residuals)), color = "red", size = 1) +
  labs(title = "Histogram with Normal Distribution Curve", x = "Numeric Vector", y = "Density") +
  theme_minimal()

################################################################################
# FORECAST
################################################################################
# The forecast is based on the expected value of the innovations and hence the density chosen.
# One step ahead forecasts are based on the value of the previous data, while n-step ahead (n>1) are based on the unconditional expectation of the models.
forecast <- ugarchforecast(fit, n.ahead = 1, n.roll = n.roll, out.sample = n.roll)
# out.sample = n.roll + n.ahead to arrive to the last current value
str(forecast)

forecast_data <- extract_forecast_data(forecast)

volForHist <- forecast_data %>%
  mutate(TradeDate = Date) %>% 
    left_join(histVolest, by = 'TradeDate') %>%
      na.omit %>% select(-c(Date, n.roll)) %>%
        select(TradeDate, everything()) %>%
          as.data.table

quantilesForecast <- as.data.frame(apply(volForHist %>% select(-TradeDate), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))

# Create bar plot
barplot(t(quantilesForecast), beside = TRUE, col = rainbow(ncol(quantilesForecast)), 
        main = "Quantiles of Volatility Measures",
        xlab = "Volatility Measure",
        ylab = "Value")
legend("topleft", legend = colnames(quantilesForecast), fill = rainbow(ncol(quantilesForecast)), 
       title = "Quantiles", cex = 0.8, inset = c(0.05, 0.05))

# Plotting from wide-format data
ggplot(volForHist, aes(x = TradeDate)) +
  geom_point(aes(y = SigmaFor, color = "Forecast"), shape = 4) +
  #geom_line(aes(y = garman, color = "Garman")) +
  #geom_line(aes(y = close, color = "Close")) +
  #geom_line(aes(y = parkinson, color = "Parkinson")) +
  #geom_line(aes(y = rogers_satchell, color = "Rogers-Satchell")) +
  #geom_line(aes(y = garman_modified, color = "Garman Modified")) +
  geom_line(aes(y = yang_zhang, color = "Yang-Zhang")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(x = "TradeDate", y = "Value", color = "Variable") +
  theme_minimal()

################################################################################
# End FORECAST
################################################################################

################################################################################
# Roll density forecasts (one day ahead)
################################################################################

getDoParWorkers()
max_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(max_cores)
registerDoParallel(cl)
# stopImplicitCluster()

roll <- ugarchroll(
  spec, 
  instr_ret[,1], 
  # n.ahead = 1 - window size - the number of periods to forecast, supported n.ahead = 1 only
  n.start = listgarch$window.size,  # starting point in the dataset from which to initialize the rolling forecast
  refit.every = listgarch$refit.frequency, # determines every how many periods the model is re-estimated
  refit.window = listgarch$refit.window.type, # whether the refit is done on an expanding window including all the previous data or a moving window
  # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead)
  window.size = listgarch$window.size, 
  solver = "hybrid", # the solver to use 
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05), 
  cluster = cl,
  # realizedVol = instr_ret[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
  keep.coef = TRUE) 

# roll <- resume(roll, solver= "gosolnp") # if object contains non-converged windows
# plot(roll)
show(roll)

forecastVolRoll <- data.frame(
  Date = roll@model$index[(listgarch$window.size+1):length(roll@model$index)],
  Forecast = roll@forecast$density$Sigma
)

# Join realized volatility estimation and instr log returns given TradeDate
volForHistRoll <- forecastVolRoll %>%
  mutate(TradeDate = Date) %>% 
    left_join(histVolest, by = 'TradeDate') %>%
      na.omit %>% select(-Date) %>%
        select(TradeDate, everything()) %>%
          left_join(select(instr, TradeDate, Close, rets), by = "TradeDate")

# Plot realized volatility Vs forecasted
ggplot(volForHistRoll, aes(x = TradeDate)) +
  geom_point(aes(y = Forecast, color = "Forecast"), shape = 4) +
  #geom_line(aes(y = garman, color = "Garman")) +
  geom_line(aes(y = close, color = "Close")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") + 
  #geom_line(aes(y = parkinson, color = "Parkinson")) +
  #geom_line(aes(y = rogers_satchell, color = "Rogers-Satchell")) +
  #geom_line(aes(y = garman_modified, color = "Garman Modified")) +
  #geom_line(aes(y = yang_zhang, color = "Yang-Zhang")) +
  labs(x = "TradeDate", y = "Value", color = "Variable") +
  theme_minimal()

# Extract refitted GARCH model coefficients given TradeDate
coefRoll <- coef_roll_refit(roll)

# Plot 
volFor <- ggplot(volForHistRoll, aes(x = TradeDate)) + 
  geom_line(aes(y = Forecast)) + 
  labs(x = "TradeDate", y = "Value", color = "Variable") +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  theme_minimal()

close <- ggplot(volForHistRoll, aes(x = TradeDate)) + 
  geom_line(aes(y = Close)) + 
  labs(x = "TradeDate", y = "Value", color = "Variable") +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  theme_minimal()

grid.arrange(volFor, close)

# Plot close price and forecasted volatility
ggplot(volForHistRoll) +
  geom_line(aes(x = TradeDate, y = Forecast, color = "Forecast"), linetype = "solid", size = 1, alpha = 0.7) + # Adjust transparency and size
  geom_line(aes(x = TradeDate, y = Close * volForHistRoll$Forecast[1]/volForHistRoll$Close[1], color = "Close"), linetype = "solid", size = 1) + # Adjust scale for Close
  geom_hline(yintercept = quantile(volForHistRoll$Forecast, probs = c(0.75, 0.95, 0.999)), linetype = c("solid", "solid", "solid"), color = c("green", "orange", "red"), alpha = 0.7) + # Add quantiles
  scale_y_continuous(
    name = "Forecast",
    sec.axis = sec_axis(~., name = "Close") # Adjust scale for Close
  ) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
  labs(title = "Forecast and Close price on Dual Y-Axis") +
  theme_minimal()

cor(volForHistRoll$Forecast, volForHistRoll$Close)
grangertest(close ~ Forecast, order = 1, data = volForHistRoll)

# Choose historical volatility estimator to compare with one day ahead rolling forecasts
as.data.frame(apply(volForHistRoll %>% select(Forecast, histVol %>% names), 2, quantile, probs = c(0.5, 0.75, 0.95, 0.999, 1)))

histVol %>% names
Historical <- "close"

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

################################################################################
# Strategy performance
################################################################################
performance <- calculate_eqlGARCH(volForHistRoll)

# Plot Passive (buy and hold) Vs strategy based on GARCH forecasts
ggplot(performance, aes(x = TradeDate)) +
  geom_line(aes(y = eqlGARCH, color = "eqlGARCH")) +
  geom_line(aes(y = eqlPassive, color = "eqlPassive")) +
  labs(title = paste("Equity Lines (buy and hold) Vs GARCH based investing", symbol, "(", meta$assets[[symbol]]$description, ")"), y = "Equity", x = "Date") +
  scale_color_manual(name = "Equity Line",
  values = c("eqlGARCH" = "blue", "eqlPassive" = "red"))

################################################################################
# Compute Performance metrics of strategy based on GARCH
################################################################################
listgarch$aR <- round(as.numeric(Return.annualized(as.numeric(performance$r_eqlGARCH), scale = 252, geometric = TRUE) * 100), 3) # Annualized Return, in %
listgarch$aSD <- round(as.numeric(StdDev.annualized(as.numeric(performance$r_eqlGARCH), scale = 252) * 100), 3)  # Annualized SD, in %
listgarch$IR <- round(as.numeric(listgarch$aR / listgarch$aSD), 3) # Information Ratio
listgarch$MD <- round(as.numeric(maxDrawdown(as.numeric(performance$r_eqlGARCH), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3) # Maximum Drawdown, in %
listgarch$trades <- cumsum(abs(diff(volForHistRoll$signal)) > 0) %>% tail(1) # Total number of trades
listgarch$signal_long <- round(sum(volForHistRoll$signal > 0) / (sum(volForHistRoll$signal > 0) + (sum(volForHistRoll$signal < 0))) , 3) * 100 # % of long signal to all signals
listgarch$rmse <- round(RMSE(volForHistRoll$Forecast, volForHistRoll$close) * 100, 3)
listgarch$elf <- NA # Correct sign change prediction

listgarch

# For SP500 strategy
passive <- data.frame(
  Buy_and_Hold = as.character(meta$assets[[symbol]]$description),
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
          listgarch[1,] %>% rename(Strategy_Specification = specification) %>% select(Strategy_Specification, aR, aSD, MD, IR))

