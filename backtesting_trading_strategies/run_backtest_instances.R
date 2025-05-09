
# Run backtest 
meta <- jsonlite::fromJSON("instr_config.json")
options(timeout = 9999999)

# Source C++ version of apply_risk_management() for execution speed increase
source("backtesting_trading_strategies/strategies.R")
Rcpp::sourceCpp("backtesting_trading_strategies/speedup/apply_risk_management_cpp.cpp")
Rcpp::sourceCpp("backtesting_trading_strategies/speedup/estimate_trading_profile_cpp.cpp")

# List of assets
assets <- c(
  "BTC-USD", # Cryptocurrency (BTC)
  "EURUSD=X", # FX most tradable
  "USDPLN=X", # FX emerging
  "^GSPC",  # US Equtities
  "^FTSE", "^GDAXI", # Europe Equities
  "^N225", # Asia Equities
  "^TYX", # Fixed Income
  "GC=F", # Commodities (gold)
  "CL=F" # Commodities (crude oil)
)

data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

##############################################################################################
# IN-SAMPLE (NO PERIOD SPLIT)
#############################################################################################

########################################################################################################################
sma1 <- SMA1$new(ts, window_size = 20, ma_type = 'SMA')
res_in_sma1 <- sma1$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA", "HMA", "WMA"), 
  window_sizes = round(10 * (1.15 ^ (0:20))),
  # risk management
  leverages = c(1, 5, 10),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.02, 0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sma1, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma1.csv"))
########################################################################################################################

sma2 <- SMA2$new(ts, window_size1 = 20, window_size2 = 60, ma_type = 'SMA')
res_in_sma2 <- sma2$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes1 = round(10 * (1.2 ^ (0:8))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:8))), # Longer periods (slow MA)
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.02, 0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sma2, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma2.csv"))
########################################################################################################################

sma1m <- SMA1M$new(ts, window_size = 20, ma_type = 'SMA')
res_in_sma1m <- sma1m$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes = round(10 * (1.15 ^ (0:20))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,  
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sma1m, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma1m.csv"))
########################################################################################################################

sma2m <- SMA2M$new(ts, window_size1 = 20, window_size2 = 60, ma_type = 'SMA')
res_in_sma2m <- sma2m$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes1 = round(10 * (1.2 ^ (0:8))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:8))), # Longer periods (slow MA)
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.02, 0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sma2m, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma2m.csv"))
########################################################################################################################

macd <- MACD$new(ts, window_size1 = 20, window_size2 = 60, sline = 12, ma_type = 'SMA')
res_in_macd <- macd$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes1 = round(10 * (1.2 ^ (0:6))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:6))), # Longer periods (slow MA)
  sline = round(7 * (1.5 ^ (0:7))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_macd, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_macd.csv"))
########################################################################################################################

tt <- TurtleTrading$new(ts, window_size1 = 20, window_size2 = 60)
res_in_tt <- tt$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes1 = round(10 * (1.2 ^ (0:6))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:6))), # Longer periods (slow MA)
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_tt, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_tt.csv"))
##############################################################################################

dc <- DonchianChannel$new(ts, window_size = 20)
res_in_dc <- dc$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes = round(10 * (1.2 ^ (0:15))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_dc, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_dc.csv"))
##############################################################################################

rsi <- RSI$new(ts, window_size = 20, threshold_oversold = 30, threshold_overbought = 70)
res_in_rsi <- rsi$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes = round(10 * (1.2 ^ (0:10))),
  thresholds_oversold = c(10, 20, 30, 40),
  thresholds_overbought = c(60, 70, 80),
  # risk management
  leverages = c(1,5,10),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_rsi, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_rsi.csv"))
##############################################################################################

sar <- StopAndReversal$new(ts, accel = 0.05, accel_max = 0.2)
res_in_sar <- sar$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  accels = seq(0.01, 0.1, by = 0.01),
  accels_max = seq(0.15, 0.35, by = 0.05), # max acceleration ranges from 15% to 35%
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sar, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_sar.csv"))

##############################################################################################
adx <- ADX$new(ts, ndx = 15, trend_strength = 40)
res_in_adx <- adx$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ndxs = seq(5, 15, by = 1),
  trends_strength = seq(20, 40, by = 5),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_adx, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_adx.csv"))

##############################################################################################
bb <- BollingerBreakout$new(ts, window_size = 20, sd_mult = 2)
res_in_bb <- bb$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes = seq(10, 40, by = 5), 
  sd_mults = seq(0.5, 2, by = 0.25),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.1, 0.2),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_bb, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_bb.csv"))

##############################################################################################
vmr <- VolatilityMeanReversion$new(ts, window_size = 20, ma_type = 'SMA')
res_in_vmr <- vmr$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes = round(10 * (1.2 ^ (0:12))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_vmr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_vmr.csv"))

######################################################################################################################
# IN-SAMPLE GRANULAR ROBUSTNESS
######################################################################################################################

# SMA1

sma1 <- SMA1$new(ts, window_size = 20, ma_type = 'SMA')
res_in_sma1r <- sma1$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA", "WMA"), 
  window_sizes = round(10 * (1.15 ^ (0:20))),
  # risk management
  leverages = c(1, 5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

#fwrite(res_in_sma1r, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma1r.csv"))
res_in_sma1r <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma1r.csv"))

######################################################################################################################

# SMA2

sma2 <- SMA2$new(ts, window_size1 = 20, window_size2 = 60, ma_type = 'SMA')
res_in_sma2r <- sma2$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes1 = round(10 * (1.2 ^ (0:8))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:8))), # Longer periods (slow MA)
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

#fwrite(res_in_sma2r, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma2r.csv"))
res_in_sma2r <- fread(res_in_sma2r, file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma2r.csv"))

res_sma2 <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma2.csv"))
res_sma2 <- res_sma2 %>% add_key(., "sma2") # add key

# Identify all superior based on total gross profit
sma2_superior <- identify_superior(res_sma2, strategy = "sma2")

# In-sample split 
res_in_sma2r_filtered <- res_in_sma2r %>% add_key(., "sma2") %>% filter(key %in% sma2_superior$key)

# Rank strategies based on the robustness metric
sma2_best <- rank_combos(res_in_sma2r_filtered, strategy = "sma2", selection = TRUE)

# Check how entire family performs and what is the expectation
sma2_best <- add_robust_column(res_sma2, sma2_best, hypothesis_value = 20, TRUE) # H0: AR <= 20%; H1: AR > 20%
# (!) only for unleveraged BTC-USD return is higher than 20% given 99% confidence level

######################################################################################################################

# SMA1M

sma1m <- SMA1M$new(ts, window_size = 20, ma_type = 'SMA')
res_in_sma1mr <- sma1m$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes = round(10 * (1.15 ^ (0:20))),
  # risk management
  leverages = c(1, 5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

#fwrite(res_in_sma1mr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma1mr.csv"))
res_in_sma1mr <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma1mr.csv"))

# SMA2M

sma2m <- SMA2M$new(ts, window_size1 = 20, window_size2 = 60, ma_type = 'SMA')
res_in_sma2mr <- sma2m$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes1 = round(10 * (1.2 ^ (0:8))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:8))), # Longer periods (slow MA)
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.02, 0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sma2mr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma2mr.csv"))
res_in_sma2mr <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma2mr.csv"))

######################################################################################################################

# MACD

macd <- MACD$new(ts, window_size1 = 20, window_size2 = 60, sline = 12, ma_type = 'SMA')
res_in_macdr <- macd$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes1 = round(10 * (1.2 ^ (0:6))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:6))), # Longer periods (slow MA)
  sline = round(7 * (1.5 ^ (0:7))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_macdr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_macdr.csv"))
macdr <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_macdr.csv"))

######################################################################################################################

# Turtle Trading (TT)

tt <- TurtleTrading$new(ts, window_size1 = 20, window_size2 = 60)
res_in_tt <- tt$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes1 = round(10 * (1.2 ^ (0:6))),  # Shorter periods (fast MA)
  window_sizes2 =  round(50 * (1.2 ^ (0:6))), # Longer periods (slow MA)
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_tt, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_ttr.csv"))

######################################################################################################################

# DC

dc <- DonchianChannel$new(ts, window_size = 20)
res_in_dcr <- dc$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes = round(10 * (1.2 ^ (0:15))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_dcr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_dcr.csv"))

##############################################################################################

# RSI

rsi <- RSI$new(ts, window_size = 20, threshold_oversold = 30, threshold_overbought = 70)
res_in_rsir <- rsi$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes = round(10 * (1.2 ^ (0:10))),
  thresholds_oversold = c(10, 20, 30, 40),
  thresholds_overbought = c(60, 70, 80),
  # risk management
  leverages = c(1,5,10),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_rsir, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_rsir.csv"))

# SAR
sar <- StopAndReversal$new(ts, accel = 0.05, accel_max = 0.2)
res_in_sarr <- sar$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  accels = seq(0.01, 0.1, by = 0.01),
  accels_max = seq(0.15, 0.35, by = 0.05), # max acceleration ranges from 15% to 35%
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_sarr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_sarr.csv"))

##############################################################################################
adx <- ADX$new(ts, ndx = 15, trend_strength = 40)
res_in_adxr <- adx$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ndxs = seq(5, 15, by = 1),
  trends_strength = seq(20, 40, by = 5),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.05, 0.1),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_adxr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_adxr.csv"))

##############################################################################################
bb <- BollingerBreakout$new(ts, window_size = 20, sd_mult = 2)
res_in_bbr <- bb$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  window_sizes = seq(10, 40, by = 5), 
  sd_mults = seq(0.5, 2, by = 0.25),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.1, 0.2),
  reward_ratios = seq(3, 7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_bbr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_bbr.csv"))

##############################################################################################
vmr <- VolatilityMeanReversion$new(ts, window_size = 20, ma_type = 'SMA')
res_in_vmrr <- vmr$run_backtest(
  # general
  symbols = assets,
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-06-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  # strategy specific
  ma_types = c("SMA", "EMA"), 
  window_sizes = round(10 * (1.2 ^ (0:12))),
  # risk management
  leverages = c(1,5),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risk = c(0.1, 0.2),
  reward_ratios = seq(5, 10, by = 5),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(res_in_vmrr, file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_vmrr.csv"))
