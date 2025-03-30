#############################
# Backtesting results analyis
#############################

source("backtesting_trading_strategies/libraries.R")
source("backtesting_trading_strategies/strategies.R")

Rcpp::sourceCpp("backtesting_trading_strategies/speedup/apply_risk_management_cpp.cpp")
Rcpp::sourceCpp("backtesting_trading_strategies/speedup/estimate_trading_profile_cpp.cpp")

# Helper functions:

# add keys (unique strategy id)
add_key <- function(res, strategy) {

  res <- res %>%

  # Add key (most granular)
    mutate(key = switch(
      strategy,
      sma1 = paste(Symbol, Strategy, Methodology, Window_Size, MA_Type, 
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      sma2 = paste(Symbol, Strategy, Methodology, Window_Size1, Window_Size2, MA_Type, 
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),
    
      sma1m = paste(Symbol, Strategy, Methodology, Window_Size, MA_Type, 
                        Flat, Dynamic_limits, leverage, max_risk, 
                        reward_ratio, sep = "_"),

      sma2m = paste(Symbol, Strategy, Methodology, Window_Size1, Window_Size2, MA_Type, 
                Flat, Dynamic_limits, leverage, max_risk, 
                reward_ratio, sep = "_"),

      macd = paste(Symbol, Strategy, Methodology, Window_Size1, Window_Size2, Sline, MA_Type, 
                Flat, Dynamic_limits, leverage, max_risk, 
                reward_ratio, sep = "_"),

      tt = paste(Symbol, Strategy, Methodology, Window_Size1, Window_Size2, 
                Flat, Dynamic_limits, leverage, max_risk, 
                reward_ratio, sep = "_"),

      dc = paste(Symbol, Strategy, Methodology, Window_Size, 
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      rsi = paste(Symbol, Strategy, Methodology, Window_Size, Threshold_Oversold, Threshold_Overbought,
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      sar = paste(Symbol, Strategy, Methodology, Accel, Accel_Max,
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      adx = paste(Symbol, Strategy, Methodology, Ndx, Trend_Strength,
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      bb = paste(Symbol, Strategy, Methodology, Window_Size, Sd_Mult,
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      vmr = paste(Symbol, Strategy, Methodology, Window_Size, MA_Type,
                   Flat, Dynamic_limits, leverage, max_risk, 
                   reward_ratio, sep = "_"),

      NULL  # Default case if strategy does not match

    )) %>%

    # Add key2 (less granular: strategy family)
    mutate(key2 = switch(
      strategy,
      sma1 = paste(Symbol, Window_Size, sep = "_"),
      sma2 = paste(Symbol, Window_Size1, Window_Size2, sep = "_"),
      sma1m = paste(Symbol, Window_Size, sep = "_"),
      sma2m = paste(Symbol, Window_Size1, Window_Size2, sep = "_"),
      macd = paste(Symbol, Window_Size1, Window_Size2, Sline, sep = "_"),
      tt = paste(Symbol, Window_Size1, Window_Size2, sep = "_"),
      dc = paste(Symbol, Window_Size, sep = "_"),
      rsi = paste(Symbol, Window_Size, Threshold_Oversold, Threshold_Overbought, sep = "_"),
      sar = paste(Symbol, Accel, sep = "_"),
      adx = paste(Symbol, Ndx, sep = "_"),
      bb = paste(Symbol, Window_Size, sep = "_"),
      vmr = paste(Symbol, Window_Size, sep = "_"),
      NULL  # Default case if strategy is not recognized
    ))

  return(res)

}

# identify superior Active strategy
identify_superior <- function(res, strategy) {
  res %>%
    mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs based on rows
    group_by(PairID) %>%
    mutate(
      Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
        # Compare Gross Profit between Active and Passive strategies within the group
        Active_GP <- `Total Gross Profit`[Strategy == "Active"]
        Passive_GP <- first(`Total Gross Profit`[Strategy == "Passive"])  # Get the first "Passive" Gross Profit
        ifelse(Active_GP > Passive_GP, "Yes", "No")
      } else {
        NA  # Assign NA if the group is incomplete
      }
    ) %>%
    ungroup() %>%
    filter(Period_Superior == "Yes")
}

# rank strategies
rank_combos <- function(df_superior, strategy, selection = FALSE) {
  
  # Determine the columns to group by based on strategy using switch
 grouping_columns <- switch(
    strategy,

    "sma1" = c("Symbol", "Strategy", "Methodology", "Window_Size", 
               "MA_Type", "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "sma2" = c("Symbol", "Strategy", "Methodology", "Window_Size1", "Window_Size2", 
               "MA_Type", "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "sma1m" = c("Symbol", "Strategy", "Methodology", "Window_Size", 
                "MA_Type", "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "sma2m" = c("Symbol", "Strategy", "Methodology", "Window_Size1", "Window_Size2", 
                "MA_Type", "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "macd" = c("Symbol", "Strategy", "Methodology", "Window_Size1", "Window_Size2", "Sline", 
               "MA_Type", "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "tt" = c("Symbol", "Strategy", "Methodology", "Window_Size1", "Window_Size2", 
            "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "dc" = c("Symbol", "Strategy", "Methodology", "Window_Size", 
             "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),
             
    "rsi" = c("Symbol", "Strategy", "Methodology", "Window_Size", "Threshold_Oversold", "Threshold_Overbought", 
              "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "sar" = c("Symbol", "Strategy", "Methodology", "Accel", "Accel_Max", 
              "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "adx" = c("Symbol", "Strategy", "Methodology", "Ndx", "Trend_Strength", 
              "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "bb" = c("Symbol", "Strategy", "Methodology", "Window_Size", "Sd_Mult", 
             "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio"),

    "vmr" = c("Symbol", "Strategy", "Methodology", "Window_Size", "MA_Type", 
              "Flat", "Dynamic_limits", "leverage", "max_risk", "reward_ratio")
    )
  
  ranked_strategies <- df_superior %>%
    mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
    group_by(PairID) %>%
    mutate(
      Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
        ifelse(
          Strategy == "Active" & 
            `Annualized Profit` > `Annualized Profit`[Strategy == "Passive"], 
          "Yes", 
          "No"
        )
      } else {
        NA  # Set NA if either Active or Passive is missing
      }
    ) %>%
    ungroup() %>%
    # Group by the dynamic columns based on strategy
    group_by(across(all_of(grouping_columns))) %>%
    summarise(
      Total_Periods = n(),  # Total number of periods
      Superior_Periods = sum(Period_Superior == "Yes", na.rm = TRUE),  # Count superior periods
      Robustness = round((Superior_Periods / Total_Periods), 4) * 100  # Calculate robustness as a percentage
    ) %>% 
    filter(Strategy == "Active") %>%
    arrange(Symbol, desc(Robustness))  # Rank strategies by robustness

    # Apply selection filter if TRUE
    if (selection) {
        ranked_strategies <- ranked_strategies %>% filter(Robustness >= 70)
    }
    
    ranked_strategies <- ranked_strategies %>% 
        add_key(., strategy) %>%
            data.table

    return(ranked_strategies)
}

# critical profit
find_critical_profit <- function(subset_res, alpha = 0.01, tol = 0.0001) {
  if (nrow(subset_res) <= 1) return(NA)  # Not enough data for t-test

  lower <- min(subset_res$`Annualized Profit`)
  upper <- max(subset_res$`Annualized Profit`)
  best_mu <- NA  # Initialize best_mu
  
  while (TRUE) {
    mid <- (upper + lower) / 2
    t_test_result <- t.test(subset_res$`Annualized Profit`, mu = mid, alternative = "greater")
    p_value <- t_test_result$p.value
    
    if (abs(p_value - alpha) < tol) {
      return(mid)  # If p-value is close enough, return mid
    }
    
    if (p_value < alpha) {
      lower <- mid  # Increase mu
      best_mu <- mid
    } else {
      upper <- mid  # Decrease mu
    }
    
    if (abs(upper - lower) < tol) break  # Stop if bounds are too close
  }
  
  return(best_mu)  # Return the closest found value
}

# add mean Annualized return at 0.01 p-value for each strategy
add_robust_column <- function(res, best, unleveraged, alpha = 0.01) {
  # Create an empty dataframe to store the critical profits for each symbol
  critical_values <- data.frame(Symbol = character(), critical_profit = numeric())
  
  # Loop through each unique symbol and calculate the critical profit
  for (symbol in unique(best$Symbol)) {
    # Filter the data for the current symbol and keys
    subset_res <- res %>% filter(Symbol == symbol, key2 %in% best$key2)
    
    # Filter further if unleveraged is TRUE
    if (unleveraged) subset_res <- subset_res %>% filter(leverage == 1)
    
    # Calculate the critical profit for the current symbol
    critical_profit <- find_critical_profit(subset_res, alpha)
    
    # Append the critical profit value to the critical_values dataframe
    critical_values <- rbind(critical_values, data.frame(Symbol = symbol, critical_profit = critical_profit))
  }
  
  # Perform a left join to add the critical_profit to the best dataframe
  best <- best %>%
    left_join(critical_values, by = "Symbol")
  
  return(best)
}

# Plot Trades PnL means distribution for a family of strategies with its Total Gross Profit (more granular view)
plot_robust_strategies <- function(strategy, from_date, to_date, dir, save_plot, output_dir) {
    
  # Load datasets based on the strategy type
  data <- switch(strategy,

    "sma2m" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/sma2m.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_sma2mr.csv")) %>% 
      add_key(., "sma2m")
      )
    },

    "macd" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/macd.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_macdr.csv")) %>% 
      add_key(., "macd")
      )
    },

    "sma1" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/sma1.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_sma1r.csv")) %>% 
      add_key(., "sma1")
      )
    },

    "sma2" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/sma2.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_sma2r.csv")) %>% 
      add_key(., "sma2")
      )
    },

    "rsi" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/rsi.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_in_rsir.csv")) %>% 
      add_key(., "rsi")
      )
    },

    "adx" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/adx.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_in_adxr.csv")) %>% 
      add_key(., "adx")
      )
    },

    "sar" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/sar.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_in_sarr.csv")) %>% 
      add_key(., "sar")
      )
    },

    "bb" = {
    list(
      robust_strategies = fread(file.path(dir, "backtesting_trading_strategies/summary/bb.csv")),
      strategy_all = fread(file.path(dir, "backtesting_trading_strategies/results/in_sample_split/res_in_bbr.csv")) %>% 
      add_key(., "bb")
      )
    }
  )

  # Extract datasets
  robust_strategies <- data$robust_strategies
  strategy_all <- data$strategy_all
  
  symbols <- unique(robust_strategies$Symbol) 
  all_results <- list()  
  summary_list <- list()  
  
  for (sym in symbols) {
    strategy_keys <- switch(strategy,
      "sma2m" = robust_strategies %>% 
        filter(Symbol == sym) %>% 
        mutate(key2 = paste(Symbol, Window_Size1, Window_Size2, sep = "_")) %>% 
        pull(key2),
      "macd" = robust_strategies %>% 
        filter(Symbol == sym) %>% 
        mutate(key2 = paste(Symbol, Window_Size1, Window_Size2, Sline, sep = "_")) %>% 
        pull(key2),
      "sma1" = robust_strategies %>%
        filter(Symbol == sym) %>%
        mutate(key2 = paste(Symbol, Window_Size, sep = "_")) %>%
        pull(key2),
      "sma2" = robust_strategies %>%
        filter(Symbol == sym) %>%
        mutate(key2 = paste(Symbol, Window_Size1, Window_Size2, sep = "_")) %>%
        pull(key2),
      "rsi" = robust_strategies %>%
        filter(Symbol == sym) %>%
        mutate(key2 = paste(Symbol, Window_Size, Threshold_Oversold, Threshold_Overbought, sep = "_")) %>%
        pull(key2),
      "adx" = robust_strategies %>%
        filter(Symbol == sym) %>%
        mutate(key2 = paste(Symbol, Ndx, sep = "_")) %>%
        pull(key2),
      "sar" = robust_strategies %>%
        filter(Symbol == sym) %>%
        mutate(key2 = paste(Symbol, Accel, sep = "_")) %>%
        pull(key2),
      "bb" = robust_strategies %>%
        filter(Symbol == sym) %>%
        mutate(key2 = paste(Symbol, Window_Size, sep = "_")) %>%
        pull(key2)

    ) %>% unique

    for (key_id in strategy_keys) {
      family <- strategy_all %>% filter(key2 == key_id, Strategy == "Active")
      if (nrow(family) == 0) next  

      # Create instance
      strategy_obj <- switch(strategy,
        "sma2m" = SMA2M$new(ts, window_size1 = 20, window_size2 = 60, ma_type = 'SMA'),
        "macd" = MACD$new(ts, window_size1 = 20, window_size2 = 60, sline = 12, ma_type = 'SMA'),
        "sma1" = SMA1$new(ts, window_size = 20, ma_type = "SMA"),
        "sma2" = SMA2$new(ts, window_size1 = 20, window_size2 = 60, ma_type = "SMA"),
        "rsi" = RSI$new(ts, window_size = 20, threshold_oversold = 30, threshold_overbought = 70),
        "adx" = ADX$new(ts, ndx = 15, trend_strength = 40),
        "sar" = StopAndReversal$new(ts, accel = 0.05, accel_max = 0.2),
        "bb" = BollingerBreakout$new(ts, window_size = 20, sd_mult = 2)
      )

      # Get trades
      trades_data <- switch(strategy,
      "sma2m" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        ma_types = c("SMA", "EMA"), 
        window_sizes1 = unique(family$Window_Size1),
        window_sizes2 = unique(family$Window_Size2),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "macd" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        ma_types = c("SMA", "EMA"), 
        window_sizes1 = unique(family$Window_Size1),
        window_sizes2 = unique(family$Window_Size2),
        sline = unique(family$Sline),  # Specific to MACD
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "sma1" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        ma_types = c("SMA", "EMA"), 
        window_sizes = unique(family$Window_Size),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "sma2" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        ma_types = c("SMA", "EMA"), 
        window_sizes1 = unique(family$Window_Size1),
        window_sizes2 = unique(family$Window_Size2),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "rsi" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        ma_types = c("SMA", "EMA"), 
        window_sizes = unique(family$Window_Size),
        thresholds_oversold = unique(family$Threshold_Oversold),
        thresholds_overbought = unique(family$Threshold_Overbought),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "adx" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        ndxs = unique(family$Ndx),
        trends_strength = unique(family$Trend_Strength),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "sar" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        accels = unique(family$Accel),
        accels_max = unique(family$Accel_Max),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      ),

      "bb" = strategy_obj$run_backtest_trades(
        symbols = sym,
        #from_date = as.Date("2018-01-01"),
        from_date = from_date,
        #to_date = as.Date("2024-06-01"),
        to_date = to_date,
        slicing_years = 1,
        data_type = "in_sample",
        split = TRUE,
        cut_date = as.Date("2024-01-01"),
        window_sizes = unique(family$Window_Size),
        sd_mults = unique(family$Sd_Mult),
        leverages = c(1, 5),
        apply_rm = TRUE,
        flats_after_event = c(TRUE, FALSE),
        dynamics_limits = c(TRUE, FALSE),
        max_risk = c(0.02, 0.1, 0.2),
        reward_ratios = seq(5, 10, by = 5),
        output_df = TRUE,
        run_via_cpp = TRUE
      )

    )

      trades_data$key2 <- switch(strategy,
      "sma2m" = paste(trades_data$Symbol, trades_data$Window_Size1, trades_data$Window_Size2, sep = "_"),
      "macd" = paste(trades_data$Symbol, trades_data$Window_Size1, trades_data$Window_Size2, trades_data$Sline, sep = "_"),
      "sma1" = paste(trades_data$Symbol, trades_data$Window_Size, sep = "_"),
      "sma2" = paste(trades_data$Symbol, trades_data$Window_Size1, trades_data$Window_Size2, sep = "_"),
      "rsi" = paste(trades_data$Symbol, trades_data$Window_Size, trades_data$Threshold_Oversold, trades_data$Threshold_Overbought, sep = "_"),
      "adx" = paste(trades_data$Symbol, trades_data$Ndx, sep = "_"),
      "sar" = paste(trades_data$Symbol, trades_data$Accel, sep = "_"),
      "bb" = paste(trades_data$Symbol, trades_data$Window_Size, sep = "_")
      )

      # Filter the trades data to match the strategy keys
      trades_data_filtered <- trades_data %>% filter(key2 %in% family$key2)

      # Number of Trades and Combinations in strategy's family
      ntrades <- sum(trades_data_filtered$Trades, na.rm = TRUE)
      ncombos <- nrow(family)

      # Compute trade PnL expectation
      mean_trade_pnl <- mean(trades_data_filtered$Trade_Mean, na.rm = TRUE)
      sd_trade_pnl <- sd(trades_data_filtered$Trade_Mean, na.rm = TRUE)
      quantile_trade_pnl <- quantile(trades_data_filtered$Trade_Mean, probs = c(0.05, 0.95), na.rm = TRUE)

      mean_tgp <- mean(family$`Total Gross Profit`, na.rm = TRUE)
      sd_tgp <- sd(family$`Total Gross Profit`, na.rm = TRUE)
      quantile_tgp <- quantile(family$`Total Gross Profit`, probs = c(0.05, 0.95), na.rm = TRUE)

      # Store results in summary list
      summary_list[[key_id]] <- data.frame(
        Symbol = sym,
        Key = key_id,
        Strategy = toupper(strategy),
        TradePnL = mean_trade_pnl,
        TradePnLSD = sd_trade_pnl,
        TradePnL_5 = quantile_trade_pnl[1],
        TradePnL_95 = quantile_trade_pnl[2],
        TGP = mean_tgp,
        TGPSD = sd_tgp,
        TGP_5 = quantile_tgp[1],
        TGP_95 = quantile_tgp[2],
        Trades = ntrades,
        Nfamily = ncombos
      )

    plot1 <- ggplot(family %>% filter(`Total Gross Profit` != 0), 
                    aes(x = `Total Gross Profit`, fill = as.factor(leverage))) +
      geom_histogram(bins = 50, alpha = 0.6, position = "stack") +  
      geom_vline(xintercept = mean_tgp, linetype = "solid", color = "black", size = 0.7) +
      scale_fill_brewer(palette = "Set1") +  
      scale_x_continuous(breaks = pretty(family$`Total Gross Profit`, n = 10)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      labs(
        title = paste0("Total Gross Profit\n",
                      "Strategy: ", strategy, " : ", key_id, "\n",
                      "Mean: ", round(mean_tgp, 2), " based on ", ncombos, " family combinations"),
        x = "Total Gross Profit",
        y = "Count",
        fill = "Leverage"
      ) +
      theme_minimal()

    plot2 <- ggplot(trades_data_filtered, aes(x = Trade_Mean, fill = factor(Leverage))) +
      geom_histogram(bins = 50, alpha = 0.6, position = "stack") +  
      geom_vline(xintercept = mean_trade_pnl, linetype = "solid", color = "black", size = 0.7) +  
      scale_fill_brewer(palette = "Set1", name = "Leverage") +  
      scale_x_continuous(breaks = pretty(trades_data_filtered$Trade_Mean, n = 10)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      labs(
        title = paste0("Trade PnL Expectation\n",
                      "Strategy: ", strategy, " : ", key_id, "\n",
                      "Mean of Trade Expectations: ", round(mean_trade_pnl, 2), " based on ", ntrades, " trades"),
        x = "Trade Expectation",
        y = "Count"
      ) +
      theme_minimal()

    # Combine the two plots side by side
    combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)
    all_results[[key_id]] <- combined_plot

      # Save plots locally
      file_name <- paste0(strategy, "_", key_id, ".png")
      
      if(save_plot) {
      ggsave(filename = file.path(output_dir, file_name), plot = combined_plot, width = 12, height = 6)
      }

    }
  }
  
  # Final table:
  summary_table <- bind_rows(summary_list)
  rownames(summary_table) <- seq_len(nrow(summary_table))
  
  return(list(plots = all_results, summary = summary_table))

}

# Robustness conclusion is based on the following:
# Stage 1: filter superior strategies based on gross profit
# Stage 2: check their performance on split in-sample data based on annualized return
# Stage 3: check their performance on out-of-sample data
# Stage 4: check performance for the entire family given robust strategy (trades PnL distribution for key parameters)

##############################################################################################

# SMA1

# IN-SAMPLE GRANULAR

# Read all results for SMA1 strategy
res_sma1 <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma1.csv")) %>% rename('Total Gross Profit' = 'Gross Profit')
res_sma1 <- res_sma1 %>% add_key(., "sma1") # add key

# Identify all superior based on total gross profit
res_sma1_superior <- identify_superior(res_sma1, strategy = "sma1")

# Read in-sample split
res_sma1_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma1r.csv"))
res_sma1_split_filtered <- res_sma1_split %>% add_key(., "sma1") %>% filter(key %in% res_sma1_superior$key)

# Rank strategies based on the robustness metric
sma1_best <- rank_combos(res_sma1_split_filtered, strategy = "sma1", selection = TRUE)

# Check how entire family performs and what is the expectation
sma1_best <- add_robust_column(res_sma1, sma1_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
sma1_best %>% select(Symbol) %>% unique
View(sma1_best) # 0.2%

# OUT-OF-SAMPLE
sma1 <- SMA1$new(ts, window_size = 40, ma_type = 'SMA')
res_sma1_out <- sma1$run_backtest(
  symbols = unique(sma1_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = unique(sma1_best$MA_Type), 
  window_sizes = unique(sma1_best$Window_Size),
  leverages = unique(sma1_best$leverage),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risks = unique(sma1_best$max_risk),
  reward_ratios = unique(sma1_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", sma1_best$key)
# Select superior strategies only (based on in-sample split data)
res_sma1_out_filtered <- res_sma1_out %>% add_key(., "sma1") %>% filter(key %in% sma1_best$key | key %in% key_passive) %>% rename(`Gross Profit` = `Total Gross Profit`)
# Check superior out-of-sample
res_sma1_out_superior <- identify_superior(res_sma1_out_filtered, strategy = "sma1")
# Rank best strategies
sma1_out_best <- rank_combos(res_sma1_out_filtered, strategy = "sma1", selection = TRUE)
# Check in out-of-sample results once again
sma1_final <- res_sma1_out_filtered[res_sma1_out_filtered$key %in% c(sma1_out_best$key, gsub("Active", "Passive", sma1_out_best$key)),] %>% data.table
sma1_final %>% select(key)

sma1_stats <- res_sma1 %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(sma1_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

print(final_table)

fwrite(sma1_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/sma1.csv"))

############################################################################################################

# SMA2

# Read all results for SMA2 strategy
res_sma2 <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma2.csv")) %>% rename('Total Gross Profit' = 'Gross Profit')
res_sma2 <- res_sma2 %>% add_key(., "sma2") # add key

# Identify all superior based on total gross profit
res_sma2_superior <- identify_superior(res_sma2, strategy = "sma2")

# Read in-sample split
res_sma2_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma2r.csv"))
res_sma2_split_filtered <- res_sma2_split %>% add_key(., "sma2") %>% filter(key %in% res_sma2_superior$key)

# Rank strategies based on the robustness metric
sma2_best <- rank_combos(res_sma2_split_filtered, strategy = "sma2", selection = TRUE)

# Check how entire family performs and what is the expectation
sma2_best <- add_robust_column(res_sma2, sma2_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
sma2_best %>% select(Symbol) %>% unique
View(sma2_best)

# OUT-OF-SAMPLE
sma2 <- SMA2$new(ts, window_size1 = 40, window_size2 = 60, ma_type = 'SMA') 
res_sma2_out <- sma2$run_backtest(
  symbols = unique(sma2_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = unique(sma2_best$MA_Type), 
  window_sizes1 = sort(unique(sma2_best$Window_Size1)),  # Shorter periods (fast MA)
  window_sizes2 = sort(unique(sma2_best$Window_Size2)), # Longer periods (slow MA)
  leverages = unique(sma2_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(sma2_best$Flat),
  dynamics_limits = unique(sma2_best$Dynamic_limits),
  max_risks = unique(sma2_best$max_risk),
  reward_ratios = unique(sma2_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", sma2_best$key)
# Select superior strategies only (based on in-sample split data)
res_sma2_out_filtered <- res_sma2_out %>% add_key(., "sma2") %>% filter(key %in% sma2_best$key | key %in% key_passive) %>% rename(`Gross Profit` = `Total Gross Profit`)
# Check superior out-of-sample
res_sma2_out_superior <- identify_superior(res_sma2_out_filtered, strategy = "sma2")
# Rank best strategies
sma2_out_best <- rank_combos(res_sma2_out_filtered, strategy = "sma2", selection = TRUE)
# Check in out-of-sample results once again
sma2_final <- res_sma2_out_filtered[res_sma2_out_filtered$key %in% c(sma2_out_best$key, gsub("Active", "Passive", sma2_out_best$key)),] %>% data.table
sma2_final %>% select(key3)
sma2_final %>% select(key)

fwrite(sma2_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/sma2.csv"))

sma2_stats <- res_sma2 %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(sma2_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

print(final_table)

##############################################################################################

# SMA1M

# IN-SAMPLE GRANULAR

# Read all results for SMA1M strategy
res_sma1m <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma1m.csv"))
res_sma1m <- res_sma1m %>% add_key(., "sma1m") # add key

# Identify all superior based on total gross profit
res_sma1m_superior <- identify_superior(res_sma1m, strategy = "sma1m")

# Read in-sample split
res_sma1m_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma1mr.csv"))
res_sma1m_split_filtered <- res_sma1m_split %>% add_key(., "sma1m") %>% filter(key %in% res_sma1m_superior$key)

res_sma1m_split %>% add_key(., "sma1m") %>% filter(key %in% res_sma1m$key)

# Rank strategies based on the robustness metric
sma1m_best <- rank_combos(res_sma1m_split_filtered, strategy = "sma1m", selection = TRUE)

# Check how entire family performs and what is the expectation
sma1m_best <- add_robust_column(res_sma1m, sma1m_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
View(sma1m_best) # 0.2%

# OUT-OF-SAMPLE
sma1m <- SMA1M$new(ts, window_size = 40, ma_type = 'SMA')
res_sma1m_out <- sma1m$run_backtest(
  symbols = unique(sma1m_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = unique(sma1m_best$MA_Type), 
  window_sizes = unique(sma1m_best$Window_Size),
  leverages = unique(sma1m_best$leverage),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  dynamics_limits = c(TRUE, FALSE),
  max_risks = unique(sma1m_best$max_risk),
  reward_ratios = unique(sma1m_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", sma1m_best$key)
# Select superior strategies only (based on in-sample split data)
#res_sma1m_out_filtered <- res_sma1m_out %>% add_key(., "sma1m") %>% filter(key %in% sma1m_best$key | key %in% key_passive) %>% rename(`Gross Profit` = `Total Gross Profit`)
res_sma1m_out_filtered <- res_sma1m_out %>% add_key(., "sma1m") %>% filter(key %in% sma1m_best$key | key %in% key_passive)
# Check superior out-of-sample
res_sma1m_out_superior <- identify_superior(res_sma1m_out_filtered, strategy = "sma1m")
# Rank best strategies
sma1m_out_best <- rank_combos(res_sma1m_out_filtered, strategy = "sma1m", selection = TRUE)
# Check in out-of-sample results once again
sma1m_final <- res_sma1m_out_filtered[res_sma1m_out_filtered$key %in% c(sma1m_out_best$key, gsub("Active", "Passive", sma1m_out_best$key)),] %>% data.table

sma1m_stats <- res_sma1m %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(sma1m_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

print(final_table)

fwrite(sma1m_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/sma1m.csv"))

##############################################################################################

# SMA2M

# Read all results for SMA2M strategy
res_sma2m <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_sma2m.csv"))
res_sma2m <- res_sma2m %>% add_key(., "sma2m") # add key

# Identify all superior based on total gross profit
res_sma2m_superior <- identify_superior(res_sma2m, strategy = "sma2m")

# Read in-sample split
res_sma2m_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_sma2mr.csv"))
res_sma2m_split_filtered <- res_sma2m_split %>% add_key(., "sma2m") %>% filter(key %in% res_sma2m_superior$key)

# Rank strategies based on the robustness metric
sma2m_best <- rank_combos(res_sma2m_split_filtered, strategy = "sma2m", selection = TRUE)

# Check how entire family performs and what is the expectation
sma2m_best <- add_robust_column(res_sma2m, sma2m_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
View(sma2m_best)
sma2m_best %>% select(Symbol) %>% unique

# OUT-OF-SAMPLE
sma2m <- SMA2M$new(ts, window_size1 = 40, window_size2 = 60, ma_type = 'SMA') 
res_sma2m_out <- sma2m$run_backtest(
  symbols = unique(sma2m_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = unique(sma2m_best$MA_Type), 
  window_sizes1 = sort(unique(sma2m_best$Window_Size1)),  # Shorter periods (fast MA)
  window_sizes2 = sort(unique(sma2m_best$Window_Size2)), # Longer periods (slow MA)
  leverages = unique(sma2m_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(sma2m_best$Flat),
  dynamics_limits = unique(sma2m_best$Dynamic_limits),
  max_risks = unique(sma2m_best$max_risk),
  reward_ratios = unique(sma2m_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", sma2m_best$key)
# Select superior strategies only (based on in-sample split data)
#res_sma2m_out_filtered <- res_sma2m_out %>% add_key(., "sma2m") %>% filter(key %in% sma2m_best$key | key %in% key_passive) %>% rename(`Gross Profit` = `Total Gross Profit`)
res_sma2m_out_filtered <- res_sma2m_out %>% add_key(., "sma2m") %>% filter(key %in% sma2m_best$key | key %in% key_passive)
# Check superior out-of-sample
res_sma2m_out_superior <- identify_superior(res_sma2m_out_filtered, strategy = "sma2m")
# Rank best strategies
sma2m_out_best <- rank_combos(res_sma2m_out_filtered, strategy = "sma2m", selection = TRUE)
# Check in out-of-sample results once again
sma2m_final <- res_sma2m_out_filtered[res_sma2m_out_filtered$key %in% c(sma2m_out_best$key, gsub("Active", "Passive", sma2m_out_best$key)),] %>% data.table
sma2m_final %>% select(key)
sma2m_final %>% select(Symbol) %>% unique

fwrite(sma2m_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/sma2m.csv"))

sma2m_stats <- res_sma2m %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(sma2m_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(sma2m_final)

##############################################################################################

# MACD 

# Read all results for MACD strategy
res_macd <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_macd.csv"))
res_macd <- res_macd %>% add_key(., "macd") %>% rename(`Total Gross Profit` = `Gross Profit`)

# Identify all superior based on total gross profit
res_macd_superior <- identify_superior(res_macd, strategy = "macd")

# Read in-sample split
res_macd_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_macdr.csv"))
res_macd_split_filtered <- res_macd_split %>% add_key(., "macd") %>% filter(key %in% res_macd_superior$key)

# Rank strategies based on the robustness metric
macd_best <- rank_combos(res_macd_split_filtered, strategy = "macd", selection = TRUE)

# Check how entire family performs and what is the expectation
macd_best <- add_robust_column(res_macd, macd_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
View(macd_best) 

# OUT-OF-SAMPLE
macd <- MACD$new(ts, window_size1 = 40, window_size2 = 60, sline = 7, ma_type = 'SMA')
res_macd_out <- macd$run_backtest(
  symbols = unique(macd_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = unique(macd_best$MA_Type), 
  window_sizes1 = sort(unique(macd_best$Window_Size1)),  # Shorter periods (fast MA)
  window_sizes2 = sort(unique(macd_best$Window_Size2)), # Longer periods (slow MA)
  sline = sort(unique(macd_best$Sline)),
  leverages = unique(macd_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(macd_best$Flat),
  dynamics_limits = unique(macd_best$Dynamic_limits),
  max_risks = unique(macd_best$max_risk),
  reward_ratios = unique(macd_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", macd_best$key)
# Select superior strategies only (based on in-sample split data)
res_macd_out_filtered <- res_macd_out %>% add_key(., "macd") %>% filter(key %in% macd_best$key | key %in% key_passive)
# Check superior out-of-sample
res_macd_out_superior <- identify_superior(res_macd_out_filtered, strategy = "macd")
# Rank best strategies
macd_out_best <- rank_combos(res_macd_out_filtered, strategy = "macd", selection = TRUE)
# Check in out-of-sample results once again
macd_final <- res_macd_out_filtered[res_macd_out_filtered$key %in% c(macd_out_best$key, gsub("Active", "Passive", macd_out_best$key)),] %>% data.table
macd_final %>% select(key)
macd_final %>% select(Symbol) %>% unique

fwrite(macd_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/macd.csv"))

macd_stats <- res_macd %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(macd_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(macd_final)

##############################################################################################

# TT

# Read all results for TT strategy
res_tt <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_tt.csv")) %>% rename(`Total Gross Profit` = `Gross Profit`)
res_tt <- res_tt %>% add_key(., "tt")

# Identify all superior based on total gross profit
res_tt_superior <- identify_superior(res_tt, strategy = "tt")

# Read in-sample split
res_tt_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_ttr.csv"))
res_tt_split_filtered <- res_tt_split %>% add_key(., "tt") %>% filter(key %in% res_tt_superior$key)

# Rank strategies based on the robustness metric
tt_best <- rank_combos(res_tt_split_filtered, strategy = "tt", selection = TRUE)

# Check how entire family performs and what is the expectation
tt_best <- add_robust_column(res_tt, tt_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
View(tt_best) 

# OUT-OF-SAMPLE
tt <- TurtleTrading$new(ts, window_size1 = 40, window_size2 = 60)
res_tt_out <- tt$run_backtest(
  symbols = unique(tt_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  window_sizes1 = sort(unique(tt_best$Window_Size1)),  # Shorter periods (fast MA)
  window_sizes2 = sort(unique(tt_best$Window_Size2)), # Longer periods (slow MA)
  leverages = unique(tt_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(tt_best$Flat),
  dynamics_limits = unique(tt_best$Dynamic_limits),
  max_risks = unique(tt_best$max_risk),
  reward_ratios = unique(tt_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", tt_best$key)
# Select superior strategies only (based on in-sample split data)
res_tt_out_filtered <- res_tt_out %>% add_key(., "tt") %>% filter(key %in% tt_best$key | key %in% key_passive)
# Check superior out-of-sample
res_tt_out_superior <- identify_superior(res_tt_out_filtered, strategy = "tt")
# Rank best strategies
tt_out_best <- rank_combos(res_tt_out_filtered, strategy = "tt", selection = TRUE)
# Check in out-of-sample results once again
tt_final <- res_tt_out_filtered[res_tt_out_filtered$key %in% c(tt_out_best$key, gsub("Active", "Passive", tt_out_best$key)),] %>% data.table
tt_final %>% select(key)
tt_final %>% select(Symbol) %>% unique

fwrite(tt_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/tt.csv"))

tt_stats <- res_tt %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(tt_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(tt_final)

##############################################################################################

# DC

# Read all results for DC strategy
res_dc <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_dc.csv")) %>% rename(`Total Gross Profit` = `Gross Profit`)
res_dc <- res_dc %>% add_key(., "dc")

# Identify all superior based on total gross profit
res_dc_superior <- identify_superior(res_dc, strategy = "dc")

# Read in-sample split
res_dc_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_dcr.csv"))
res_dc_split_filtered <- res_dc_split %>% add_key(., "dc") %>% filter(key %in% res_dc_superior$key)

# Rank strategies based on the robustness metric
dc_best <- rank_combos(res_dc_split_filtered, strategy = "dc", selection = TRUE)

# Check how entire family performs and what is the expectation
dc_best <- add_robust_column(res_dc, dc_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
View(dc_best) 

# OUT-OF-SAMPLE
dc <- DonchianChannel$new(ts, window_size = 40)
res_dc_out <- dc$run_backtest(
  symbols = unique(dc_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  window_sizes = sort(unique(dc_best$Window_Size)),  # Shorter periods (fast MA)
  leverages = unique(dc_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(dc_best$Flat),
  dynamics_limits = unique(dc_best$Dynamic_limits),
  max_risks = unique(dc_best$max_risk),
  reward_ratios = unique(dc_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", dc_best$key)
# Select superior strategies only (based on in-sample split data)
res_dc_out_filtered <- res_dc_out %>% add_key(., "dc") %>% filter(key %in% dc_best$key | key %in% key_passive)
# Check superior out-of-sample
res_dc_out_superior <- identify_superior(res_dc_out_filtered, strategy = "dc")
# Rank best strategies
dc_out_best <- rank_combos(res_dc_out_filtered, strategy = "dc", selection = TRUE)
# Check in out-of-sample results once again
dc_final <- res_dc_out_filtered[res_dc_out_filtered$key %in% c(dc_out_best$key, gsub("Active", "Passive", dc_out_best$key)),] %>% data.table
dc_final %>% select(key)
dc_final %>% select(Symbol) %>% unique

fwrite(dc_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/dc.csv"))

dc_stats <- res_dc %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(dc_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(dc_final)

##############################################################################################

# RSI

# Read all results for RSI strategy
res_rsi <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_rsi.csv")) %>% rename(`Total Gross Profit` = `Gross Profit`)
res_rsi <- res_rsi %>% add_key(., "rsi")

# Identify all superior based on total gross profit
res_rsi_superior <- identify_superior(res_rsi, strategy = "rsi")

# Read in-sample split
res_rsi_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_rsir.csv"))
res_rsi_split_filtered <- res_rsi_split %>% add_key(., "rsi") %>% filter(key %in% res_rsi_superior$key)

# Rank strategies based on the robustness metric
rsi_best <- rank_combos(res_rsi_split_filtered, strategy = "rsi", selection = TRUE)

# Check how entire family performs and what is the expectation
rsi_best <- add_robust_column(res_rsi, rsi_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
rsi_best %>% select(Symbol) %>% unique
View(rsi_best) 

# OUT-OF-SAMPLE
rsi <- RSI$new(ts, window_size = 20, threshold_oversold = 30, threshold_overbought = 70)
res_rsi_out <- rsi$run_backtest(
  symbols = unique(rsi_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  window_sizes = sort(unique(rsi_best$Window_Size)),
  thresholds_oversold = sort(unique(rsi_best$Threshold_Oversold)),
  thresholds_overbought = sort(unique(rsi_best$Threshold_Overbought)),
  leverages = unique(rsi_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(rsi_best$Flat),
  dynamics_limits = unique(rsi_best$Dynamic_limits),
  max_risks = unique(rsi_best$max_risk),
  reward_ratios = unique(rsi_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", rsi_best$key)
# Select superior strategies only (based on in-sample split data)
res_rsi_out_filtered <- res_rsi_out %>% add_key(., "rsi") %>% filter(key %in% rsi_best$key | key %in% key_passive)
# Check superior out-of-sample
res_rsi_out_superior <- identify_superior(res_rsi_out_filtered, strategy = "rsi")
# Rank best strategies
rsi_out_best <- rank_combos(res_rsi_out_filtered, strategy = "rsi", selection = TRUE)
# Check in out-of-sample results once again
rsi_final <- res_rsi_out_filtered[res_rsi_out_filtered$key %in% c(rsi_out_best$key, gsub("Active", "Passive", rsi_out_best$key)),] %>% data.table
rsi_final %>% select(key)
rsi_final %>% select(Symbol) %>% unique

fwrite(rsi_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/rsi.csv"))

rsi_stats <- res_rsi %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(rsi_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(rsi_final)

##############################################################################################

# SAR

# Read all results for SAR strategy
res_sar <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_sar.csv"))
res_sar <- res_sar %>% add_key(., "sar")

# Identify all superior based on total gross profit
res_sar_superior <- identify_superior(res_sar, strategy = "sar")

# Read in-sample split
res_sar_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_sarr.csv"))
res_sar_split_filtered <- res_sar_split %>% add_key(., "sar") %>% filter(key %in% res_sar_superior$key)

# Rank strategies based on the robustness metric
sar_best <- rank_combos(res_sar_split_filtered, strategy = "sar", selection = TRUE)

# Check how entire family performs and what is the expectation
sar_best <- add_robust_column(res_sar, sar_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
sar_best %>% select(Symbol) %>% unique
View(sar_best) 

# OUT-OF-SAMPLE
sar <- StopAndReversal$new(ts, accel = 0.05, accel_max = 0.2)
res_sar_out <- sar$run_backtest(
  symbols = unique(sar_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  accels = sort(unique(sar_best$Accel)),
  accels_max = sort(unique(sar_best$Accel_Max)),
  leverages = unique(sar_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(sar_best$Flat),
  dynamics_limits = unique(sar_best$Dynamic_limits),
  max_risks = unique(sar_best$max_risk),
  reward_ratios = unique(sar_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", sar_best$key)
# Select superior strategies only (based on in-sample split data)
res_sar_out_filtered <- res_sar_out %>% add_key(., "sar") %>% filter(key %in% sar_best$key | key %in% key_passive)
# Check superior out-of-sample
res_sar_out_superior <- identify_superior(res_sar_out_filtered, strategy = "sar")
# Rank best strategies
sar_out_best <- rank_combos(res_sar_out_filtered, strategy = "sar", selection = TRUE)
# Check in out-of-sample results once again
sar_final <- res_sar_out_filtered[res_sar_out_filtered$key %in% c(sar_out_best$key, gsub("Active", "Passive", sar_out_best$key)),] %>% data.table
sar_final %>% select(key)
sar_final %>% select(Symbol) %>% unique

fwrite(sar_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/sar.csv"))

sar_stats <- res_sar %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(sar_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(sar_final)

##############################################################################################

# ADX

# Read all results for ADX  strategy
res_adx <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_adx.csv"))
res_adx <- res_adx %>% add_key(., "adx")

# Identify all superior based on total gross profit
res_adx_superior <- identify_superior(res_adx, strategy = "adx")

# Read in-sample split
res_adx_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_adxr.csv"))
res_adx_split_filtered <- res_adx_split %>% add_key(., "adx") %>% filter(key %in% res_adx_superior$key)

# Rank strategies based on the robustness metric
adx_best <- rank_combos(res_adx_split_filtered, strategy = "adx", selection = TRUE)

# Check how entire family performs and what is the expectation
adx_best <- add_robust_column(res_adx, adx_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
adx_best %>% select(Symbol) %>% unique
View(adx_best) 

# OUT-OF-SAMPLE
adx <- ADX$new(ts, ndx = 15, trend_strength = 40)
res_adx_out <- adx$run_backtest(
  symbols = unique(adx_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ndxs = sort(unique(adx_best$Ndx)),
  trends_strength = sort(unique(adx_best$Trend_Strength)),
  leverages = unique(adx_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(adx_best$Flat),
  dynamics_limits = unique(adx_best$Dynamic_limits),
  max_risks = unique(adx_best$max_risk),
  reward_ratios = unique(adx_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", adx_best$key)
# Select superior strategies only (based on in-sample split data)
res_adx_out_filtered <- res_adx_out %>% add_key(., "adx") %>% filter(key %in% adx_best$key | key %in% key_passive)
# Check superior out-of-sample
res_adx_out_superior <- identify_superior(res_adx_out_filtered, strategy = "adx")
# Rank best strategies
adx_out_best <- rank_combos(res_adx_out_filtered, strategy = "adx", selection = TRUE)
# Check in out-of-sample results once again
adx_final <- res_adx_out_filtered[res_adx_out_filtered$key %in% c(adx_out_best$key, gsub("Active", "Passive", adx_out_best$key)),] %>% data.table
adx_final %>% select(key)
adx_final %>% select(Symbol) %>% unique

fwrite(adx_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/adx.csv"))

adx_stats <- res_adx %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(adx_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(adx_final)

##############################################################################################

# BB

# Read all results for BB  strategy
res_bb <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_bb.csv"))
res_bb <- res_bb %>% add_key(., "bb")

# Identify all superior based on total gross profit
res_bb_superior <- identify_superior(res_bb, strategy = "bb")

# Read in-sample split
res_bb_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_bbr.csv"))
res_bb_split_filtered <- res_bb_split %>% add_key(., "bb") %>% filter(key %in% res_bb_superior$key)

# Rank strategies based on the robustness metric
bb_best <- rank_combos(res_bb_split_filtered, strategy = "bb", selection = TRUE)

# Check how entire family performs and what is the expectation
bb_best <- add_robust_column(res_bb, bb_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
bb_best %>% select(Symbol) %>% unique
View(bb_best) 

# OUT-OF-SAMPLE
bb <- BollingerBreakout$new(ts, window_size = 20, sd_mult = 2)
res_bb_out <- bb$run_backtest(
  symbols = unique(bb_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  window_sizes = sort(unique(bb_best$Window_Size)),
  sd_mults = sort(unique(bb_best$Sd_Mult)),
  leverages = unique(bb_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(bb_best$Flat),
  dynamics_limits = unique(bb_best$Dynamic_limits),
  max_risks = unique(bb_best$max_risk),
  reward_ratios = unique(bb_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", bb_best$key)
# Select superior strategies only (based on in-sample split data)
res_bb_out_filtered <- res_bb_out %>% add_key(., "bb") %>% filter(key %in% bb_best$key | key %in% key_passive)
# Check superior out-of-sample
res_bb_out_superior <- identify_superior(res_bb_out_filtered, strategy = "bb")
# Rank best strategies
bb_out_best <- rank_combos(res_bb_out_filtered, strategy = "bb", selection = TRUE)
# Check in out-of-sample results once again
bb_final <- res_bb_out_filtered[res_bb_out_filtered$key %in% c(bb_out_best$key, gsub("Active", "Passive", bb_out_best$key)),] %>% data.table
bb_final %>% select(key)
bb_final %>% select(Symbol) %>% unique

fwrite(bb_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/bb.csv"))

bb_stats <- res_bb %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(bb_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(bb_final)

##############################################################################################

# VMR

# Read all results for VMR  strategy
res_vmr <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample/res_in_vmr.csv"))
res_vmr <- res_vmr %>% add_key(., "vmr")

# Identify all superior based on total gross profit
res_vmr_superior <- identify_superior(res_vmr, strategy = "vmr")

# Read in-sample split
res_vmr_split <- fread(file.path(getwd(), "backtesting_trading_strategies/results/in_sample_split/res_in_vmrr.csv"))
res_vmr_split_filtered <- res_vmr_split %>% add_key(., "vmr") %>% filter(key %in% res_vmr_superior$key)

# Rank strategies based on the robustness metric
vmr_best <- rank_combos(res_vmr_split_filtered, strategy = "vmr", selection = TRUE)

# Check how entire family performs and what is the expectation
vmr_best <- add_robust_column(res_vmr, vmr_best, TRUE) # H0: AR <= 20%; H1: AR > 20%
vmr_best %>% select(Symbol) %>% unique
View(vmr_best) 

# OUT-OF-SAMPLE
vmr <- VolatilityMeanReversion$new(ts, window_size = 20, ma_type = 'SMA')
res_vmr_out <- vmr$run_backtest(
  symbols = unique(vmr_best$Symbol),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  data_type = "out_of_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  window_sizes = sort(unique(vmr_best$Window_Size)),
  ma_types = sort(unique(vmr_best$MA_Type)),
  leverages = unique(vmr_best$leverage),
  apply_rm = TRUE,
  flats_after_event = unique(vmr_best$Flat),
  dynamics_limits = unique(vmr_best$Dynamic_limits),
  max_risks = unique(vmr_best$max_risk),
  reward_ratios = unique(vmr_best$reward_ratio),
  output_df = TRUE,
  run_via_cpp = TRUE
)

# Keys for Passive strategies
key_passive <- gsub("Active", "Passive", vmr_best$key)
# Select superior strategies only (based on in-sample split data)
res_vmr_out_filtered <- res_vmr_out %>% add_key(., "vmr") %>% filter(key %in% vmr_best$key | key %in% key_passive)
# Check superior out-of-sample
res_vmr_out_superior <- identify_superior(res_vmr_out_filtered, strategy = "vmr")
# Rank best strategies
vmr_out_best <- rank_combos(res_vmr_out_filtered, strategy = "vmr", selection = TRUE)
# Check in out-of-sample results once again
vmr_final <- res_vmr_out_filtered[res_vmr_out_filtered$key %in% c(vmr_out_best$key, gsub("Active", "Passive", vmr_out_best$key)),] %>% data.table
vmr_final %>% select(key)
vmr_final %>% select(Symbol) %>% unique

fwrite(vmr_final, file.path(getwd(), "backtesting_trading_strategies/results/robust_strategies/vmr.csv"))

vmr_stats <- res_vmr %>% filter(Strategy == "Active") %>%
  count(Symbol, name = "Total_Count") %>%
  left_join(vmr_best %>% count(Symbol, name = "Best_Count"), by = "Symbol") %>%
  mutate(Best_Count = as.numeric(replace_na(Best_Count, 0)), 
         Best_Percentage = (Best_Count / Total_Count) * 100)

View(vmr_final)

######################################################################################################
# Additional check on family distributions for SMA2M and MACD (they are robust on 7 out of 10 markets)
######################################################################################################

# SMA2M
res_sma2m <- plot_robust_strategies("sma2m", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = "/Users/olegb/Documents/ATS/ATS", save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plots")
sma2m_robust <- res_sma2m$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(sma2m_robust, file.path(getwd(), "backtesting_trading_strategies/summary/sma2m_robust.csv"))

# MACD
res_macd <- plot_robust_strategies("macd", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = "/Users/olegb/Documents/ATS/ATS", save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plots")
macd_robust <- res_macd$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(macd_robust, file.path(getwd(), "backtesting_trading_strategies/summary/macd_robust.csv"))

######################################################################################################
# check FXs (adding run_backtest_trades in other strategies)
######################################################################################################

# SMA1
res_sma1 <- plot_robust_strategies("sma1", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = getwd(), save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plotsFX")
sma1_robust <- res_sma1$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(sma1_robust, file.path(getwd(), "backtesting_trading_strategies/summary/sma1_robust_rest.csv"))

# SMA2
res_sma2 <- plot_robust_strategies("sma2", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = getwd(), save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plotsFX")
sma2_robust <- res_sma2$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(sma2_robust, file.path(getwd(), "backtesting_trading_strategies/summary/sma2_robust_rest.csv"))

# RSI
res_rsi <- plot_robust_strategies("rsi", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = getwd(), save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plotsFX")
rsi_robust <- res_rsi$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(rsi_robust, file.path(getwd(), "backtesting_trading_strategies/summary/rsi_robust_rest.csv"))

# ADX
res_adx <- plot_robust_strategies("adx", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = getwd(), save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plotsFX")
adx_robust <- res_adx$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(adx_robust, file.path(getwd(), "backtesting_trading_strategies/summary/adx_robust_rest.csv"))

# SAR
res_sar <- plot_robust_strategies("sar", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = getwd(), save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plotsFX")
sar_robust <- res_sar$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(sar_robust, file.path(getwd(), "backtesting_trading_strategies/summary/sar_robust_rest.csv"))

# Bollinger Breakout
res_bb <- plot_robust_strategies("bb", from_date = as.Date("2018-01-01"), to_date = as.Date("2024-06-01"), dir = getwd(), save_plot = TRUE, output_dir = "backtesting_trading_strategies/summary/plotsFX")
bb_robust <- res_bb$summary %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
#fwrite(bb_robust, file.path(getwd(), "backtesting_trading_strategies/summary/bb_robust_rest.csv"))

all <- rbind(macd_robust, sma2m_robust, sma1_robust, sma2_robust, rsi_robust, adx_robust, sar_robust, bb_robust) %>% arrange(Symbol, desc(TradePnL)) %>% mutate(RR = TradePnL / TradePnLSD)
fwrite(all, file.path(getwd(), "backtesting_trading_strategies/summary/all.csv"))
