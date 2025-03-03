# Copyright (c) 2024 Oleh Bilyk

source("backtesting_trading_strategies/libraries.R")
source("backtesting_trading_strategies/strategies.R")
options(scipen = 999)

# Specify the following strategy parameters
from_date <- as.Date("2018-01-01") 
to_date <- Sys.Date()
symbol <- "BTC-USD"
capital <- 1000 # USDC
leverage <- 1
apply_rm <- TRUE
flat_after_event <- TRUE
dynamic_limits <- TRUE

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

# Example of Strategy instance (SMA1)
Rcpp::sourceCpp("backtesting_trading_strategies/speedup/apply_risk_management_cpp.cpp")
Rcpp::sourceCpp("backtesting_trading_strategies/speedup/estimate_trading_profile_cpp.cpp")

meta <- jsonlite::fromJSON("instr_config.json")

# IN-SAMPLE (WITHOUT SPLIT)
sma1 <- SMA1$new(ts, window_size = 124, ma_type = 'SMA')
sma1$estimate_range_potential(n = 14)

# in-sample:
sma1_res_in_sample <- t(
  sma1$estimate_performance(
  symbol = symbol,
  capital = capital,
  leverage = 1,
  data_type = "in_sample", 
  split_data = FALSE, 
  cut_date = as.Date("2024-01-01"), 
  #cut_date = Sys.Date(),
  window = 1, 
  apply_rm = apply_rm, 
  flat_after_event = flat_after_event,
  dynamic_limits = dynamic_limits,
  max_risk = 0.2, 
  reward_ratio = 5,
  run_via_cpp = TRUE
    )
  )

sma1_res_in_sample_dt <- cbind(Metric = rownames(sma1_res_in_sample), as.data.table(as.data.frame(sma1_res_in_sample, stringsAsFactors = FALSE)))

sma1_res_in_sample_dt[, units := ifelse(
    .I <= 5 | Metric %in% c("max_risk", "Strategy", "Calmar Ratio", "Number of Trades Per Year", "reward_ratio"), "",
    ifelse(
      Metric %in% c("Annualized Profit", "Percentage of Positive Profit Days", "Percentage of Winning Trades",  "Max Drawdown", "Max Run Up"), "%",
      ifelse(
        Metric %in% c("Length of Largest Win", "Length of Largest Loss", "Length of Average Win", "Length of Average Loss", 
                      "Length of Max Drawdown", "Length of Max Run-Up", "Length of Time in Largest Winning Run", 
                      "Length of Time in Largest Losing Run", "Length of Time in Average Winning Run", 
                      "Length of Time in Average Losing Run", "Largest Winning Run", "Largest Losing Run", 
                      "Average Winning Run", "Average Losing Run"), 
        "days",
        ifelse(grepl("Date", Metric), "Date", 
              ifelse(Metric %in% c("Max Losing Streak", "Max Winning Streak"), "trades", 
                      "USD"  # Default case for other rows
              )
        )
      )
    )
)]

View(sma1_res_in_sample_dt)

dataset <- sma1$data
dataset <- sma1$data %>% select(
    Date, Close, stopLoss, profitTake, signal, position, pnlActiveType,
    eventSL, eventPT, eventSLShift,
    nopActive, pnlActive, eqlActive, r_eqlActive, pnlActiveCumulative, Liquidation, trade_id_m2)

trades <- sma1$get_trades(apply_rm = apply_rm)$trades
View(trades)

# Visualizations
sma1$plot_equity_lines("SMA1", signal_flag = FALSE, capital, symbol)
plots <- sma1$plot_close_vs_vol(30)
grid.arrange(plots$close, plots$n, ncol = 1)
sma1$get_trades(apply_rm = apply_rm)$pnl_hist
sma1$get_trades(apply_rm = apply_rm)$pnl_contr_by_trade
sma1$get_trades(apply_rm = apply_rm)$pnl_cum_by_trade
sma1$get_trades(apply_rm = apply_rm)$pnl_hist_by_trade
sma1$get_trades(apply_rm = apply_rm)$exits
sma1$plot_rm_levels(30, apply_rm = apply_rm)
sma1$plot_nop_evo()
sma1$plot_annualized_vol()

summary(dataset$value)

cor(dataset$annual_vol, dataset$eqlActive, use = "complete.obs")

#Count the number of unique year-month combinations
num_months <- length(unique(format(dataset$Date, "%Y-%m")))

# Print average stop-loss and profit-take events per month
print(paste0("Stop Losses occur every: ", round(1 / ((sum(dataset$eventSL, na.rm = TRUE) / num_months)),0), " month(s)"))
print(paste0("Average Profit Takes per Month: ", round(1 / ((sum(dataset$eventPT, na.rm = TRUE) / num_months)),0), " month(s)"))

# IN-SAMPLE (WITHOUT SPLIT)

# Overall trading profile (NO SPLIT with stop-loss)
sma1 <- SMA1$new(ts, window_size = 116, ma_type = 'EMA')
btc_sma1_in_sample_no_split <- sma1$run_backtest(
  symbols = c("BTC-USD"),
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2025-03-01"),
  slicing_years = 1,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = c("SMA", "EMA"), 
  #window_sizes = round(10 * (1.15 ^ (0:20))),
  window_sizes = round(10 * (1.25 ^ (0:12))),
  leverages = seq(1, 2, by = 1),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risks = c(0.1, 0.2),
  reward_ratios = seq(3,7, by = 4),
  output_df = TRUE,
  run_via_cpp = TRUE
)

fwrite(btc_sma1_in_sample_no_split, "/Users/olegb/Documents/ATS/ATS/bin/res_sma1_btc.csv")
btc_sma1_in_sample_no_split <- fread("bin/res_sma1_btc.csv")

# Backtest visualization
ggplot(btc_sma1_in_sample_no_split %>% filter(Strategy == "Active"), aes(x = Window_Size, y = `Annualized Profit`)) +
  geom_point(aes(color = MA_Type, shape = MA_Type), size = 3, alpha = 0.6) +  # Points with different shapes and colors for each MA_Type
  geom_smooth(method = "loess", se = FALSE, color = "red") +  # Single smooth line
  scale_color_manual(values = c("SMA" = "blue", "EMA" = "green", "WMA" = "purple")) +  # Custom colors for each MA_Type
  scale_shape_manual(values = c("SMA" = 16, "EMA" = 15, "WMA" = 17)) +  # Circle for SMA, Square for EMA, Triangle for WMA
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Horizontal dashed line at y = 0

  labs(
    title = paste("Annualized Profit vs Window Size for", unique(btc_sma1_in_sample_no_split$Symbol)),
    x = "Window Size (Days)",
    y = "Annualized Profit (%)",
    color = "MA Type",
    shape = "MA Type"
  ) +
  scale_x_continuous(breaks = seq(min(btc_sma1_in_sample_no_split$Window_Size), 
                                  max(btc_sma1_in_sample_no_split$Window_Size), by = 10)) +  # Ticks every 10 days
  scale_y_continuous(breaks = seq(floor(min(btc_sma1_in_sample_no_split$`Annualized Profit`, na.rm = TRUE) / 5) * 5, 
                                  ceiling(max(btc_sma1_in_sample_no_split$`Annualized Profit`, na.rm = TRUE) / 5) * 5, by = 5)) +  # Ticks every 5%
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

btc_sma1_in_sample_no_split <- btc_sma1_in_sample_no_split %>%
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs based on rows
  group_by(PairID) %>%
  mutate(
    Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(
        Strategy == "Active" & AnnualizedProfit > AnnualizedProfit[Strategy == "Passive"][1], 
        "Yes", 
        "No"
      )
    } else {
      NA  # Assign NA if the group is incomplete
    }
  ) %>%
  ungroup()

# Filter for Active strategies and group by strategy parameters
btc_sma1_in_sample_no_split %>%
  filter(Strategy == "Active") %>%
  group_by(Methodology, Window_Size, MA_Type, Max_Risk, Reward_Ratio) %>%
  summarise(
    Avg_AnnualizedProfit = mean(AnnualizedProfit, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_AnnualizedProfit)) %>%
  slice(1)

# Best in-sample combinations:
# BTC-USD, SMA 116-day 0.3 6
# ETH-USD
# BNB-USD 
# CANDIDATE: 

# IN-SAMPLE: HOW STRATEGY BEHAVES UNDER DIFFERENT PERIODS
# More granular (split) - only for potential good candidates to check robustness
res_sma1_granular <- sma1$run_backtest(
  symbols = c("BTC-USD"),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 2,
  data_type = "in_sample",
  split_data = TRUE,
  cut_date = as.Date("2024-01-01"),
  ma_types = c("SMA"),
  window_sizes = 116, 
  leverages = seq(2, 3, by = 1),
  apply_rm = TRUE,
  flats_after_event = FALSE,
  max_risks = seq(0.1, 0.3, by = 0.1),
  reward_ratios = seq(2, 3, by = 1),
  output_df = TRUE
)

# Check if a Methodology is superior based on the criteria
superior_methodologies <- res_sma1_granular %>% 
#filter(Symbol == "BNB-USD") %>%
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
  group_by(PairID) %>%
  mutate(
    Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(
        Strategy == "Active" & 
          AnnualizedProfit > AnnualizedProfit[Strategy == "Passive"], 
        "Yes", 
        "No"
      )
    } else {
      NA  # Set NA if either Active or Passive is missing
    }
  ) %>%
  group_by(Methodology) %>% # Now group by Methodology to evaluate all periods
  mutate(
    Superior = if (all(Period_Superior == "Yes", na.rm = TRUE)) {
      "Yes"
    } else {
      "No"
    }
  ) %>%
  ungroup()

paste0("The robustness of in-sample superiority (%) is ", superior_methodologies %>% filter(Period_Superior == "Yes") %>% nrow / (nrow(superior_methodologies) / 2) * 100)

# Evaluate the robustness of each strategy across multiple periods
ranked_strategies <- res_sma1_granular %>% 
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
  group_by(PairID) %>%
  mutate(
    Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(
        Strategy == "Active" & 
          AnnualizedProfit > AnnualizedProfit[Strategy == "Passive"], 
        "Yes", 
        "No"
      )
    } else {
      NA  # Set NA if either Active or Passive is missing
    }
  ) %>%
  ungroup() %>%
  filter(Symbol == "BTC-USD") %>%
  group_by(Strategy, Methodology, Max_Risk, Reward_Ratio) %>%
  summarise(
    Total_Periods = n(),  # Total number of periods
    Superior_Periods = sum(Period_Superior == "Yes", na.rm = TRUE),  # Count superior periods
    Robustness = (Superior_Periods / Total_Periods) * 100  # Calculate robustness as a percentage
  ) %>%
  arrange(desc(Robustness))  # Rank strategies by robustness in descending order

# Output the ranking
ranked_strategies

# ACHIEVE ~60-70% 

# OUT-OF-SAMPLE PERFORMANCE
sma1_os <- SMA1$new(ts, window_size = 100, ma_type = 'HMA')
sma1_res_out_sample <- t(sma1_os$estimate_performance(data_type = "out_of_sample", split = FALSE, cut_date = as.Date("2024-01-01"), window = 1,
 apply_stop_loss = TRUE, stop_loss_threshold = 0.015, reward_ratio = 25, capital, leverage, symbol))

sma1_res_out_sample_dt <- cbind(Metric = rownames(sma1_res_out_sample), as.data.table(as.data.frame(sma1_res_out_sample, stringsAsFactors = FALSE)))

# Apply the same logic for the units column
sma1_res_out_sample_dt[, units := ifelse(
  .I <= 5 | Metric == "NumberOfTradesPerYear", "",  # First five rows and 'NumberOfTradesPerYear' are empty
  ifelse(
    Metric %in% c("AnnualizedProfit", "PercentageOfWinningTrades", "MaxDrawdown", "MaxRunUp"), "%",
    ifelse(
      Metric %in% c("LengthOfLargestWin", "LengthOfLargestLoss", "LengthOfAverageWin", "LengthOfAverageLoss", 
                    "LengthOfMaxDrawdown", "LengthOfMaxRunUp", "LengthOfTimeInLargestWinningRun", "LengthOfTimeInLargestLosingRun", 
                    "LengthOfTimeInAverageWinningRun", "LengthOfTimeInAverageLosingRun", "LargestWinningRun", "LargestLosingRun"), "days",
      ifelse(
        grepl("Date", Metric), "Date", 
        "USD"  # Default case for other rows
      )
    )
  )
)]

sma1_os$plot_equity_lines("SMA1", signal_flag = FALSE)

# Overall trading profile
res_sma1_overall_os <- sma1_os$run_backtest(
  symbols = c("BTC-USD", "BNB-USD", "ETH-USD"),
  window_sizes = 20,
  ma_type = "EMA",  # Add more MA types here
  data_type = "out_of_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  from_date = as.Date("2020-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  apply_stop_loss = FALSE,
  stop_loss_threshold = 0.025,
  reward_ratio = 15,
  output_df = TRUE
  )

# All results in one df
res_all <- fread("Run_backtest_results/res_all.csv")

res_all <- res_all %>% 
  mutate(
    Meth = str_extract(Methodology, "^[^:]+")
) %>%
  select(Symbol, Class, Meth, Methodology, everything(.)) %>%
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
  group_by(PairID) %>%
  mutate(
    Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(Strategy == "Active" & aR > aR[Strategy == "Passive"], "Yes", "No")
    } else {
      NA  # Assign NA if the group is incomplete
    }
  ) %>%
  ungroup()

# View the updated data
res_all %>% filter(Symbol == "BTC-USD" & Strategy == "Active" & Superior == "Yes") %>% arrange(desc(aR)) %>% select(Methodology) %>% unique

best_strategies <- res_all %>% group_by(Symbol) %>%
filter(Strategy == "Active") %>%
  filter(aR == max(aR)) %>%
  ungroup() %>% arrange(Class)

res_all %>%
filter(Symbol == "BTC-USD" & Strategy == "Active") %>%
group_by(Methodology) %>%
 #filter(Methodology == "SMA1: 40 SMA") %>%
  select(aR) %>% 
    summary

# Compute percentage of superior rows for each Methodology for a Symbol
res_all %>%
filter(Symbol == "ETH-USD") %>%
  group_by(Meth) %>%
  summarise(
    TotalRows = n(),
    SuperiorRows = sum(Superior == "Yes"),
    PercentageSuperior = (SuperiorRows / TotalRows) * 100
  ) %>% arrange(desc(PercentageSuperior))

btc_sma1_in_sample_no_split %>%
filter(Symbol == "BTC-USD") %>%
 filter(Methodology == "SMA1: 100 HMA" & Strategy == "Active") %>%
  select(c(AnnualizedProfit, MaxDrawdown, NumberOfTradesPerYear)) %>% 
    summary
