# Copyright (c) June 2024 Oleh Bilyk
# Price-time based strategies

# Define Data parent class (DataFetcher)
DataFetcher <- R6Class(
  "DataFetcher",
  public = list(
    symbol = NULL,
    from_date = NULL,
    to_date = NULL,
    type = NULL,

initialize = function(symbol, from_date, to_date, type = "rets") {
      self$symbol <- symbol
      self$from_date <- from_date
      self$to_date <- to_date
      self$type <- type
},
  
# Download data for multiple symbols and save tibble data frame in wide format    
convert_xts_to_wide_df = function() {
    dfs <- list()
    # Iterate through symbols to retrieve data and create data frames
    for (symbol in self$symbol) {
    # Fetch data for symbol
    tsRaw <- getSymbols(symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    data <- coredata(ts)
    dates <- index(ts)
    close_price <- as.numeric(data[, 4])
    # Create data frame for the symbol
    switch(
        self$type,
        "rets" = {
        # Combine Date and log returns into a data.frame
        df <- data.frame(Date = as.Date(dates), 
                            rets = as.numeric(log(close_price / lag(close_price)))) %>%
            na.omit %>%
            unnest(rets)
        },
        "Close" = {
        df <- data.frame(Date = as.Date(dates), Close = close_price) %>% 
            na.omit %>%
            unnest
        },
        stop("Invalid value for 'type' argument. Choose 'rets' or 'Close'.")
    )
    # Store the data frame in the list
    dfs[[symbol]] <- df
    }
    # Combine all data frames into a single wide data frame
    wide_df <- bind_rows(dfs, .id = "symbol") %>%
    pivot_wider(names_from = "symbol", values_from = ifelse(self$type == "rets", "rets", "Close"), 
                names_prefix = ifelse(self$type == "rets", "rets_", "Close_")) %>%
                    na.omit()
    return(wide_df)
},

# Download xts data and compute log returns
download_xts_data = function() {
    tsRaw <- quantmod::getSymbols(self$symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    ts$value <- log(ts[, grep("\\.Close$", colnames(ts))]) - log(lag(ts[, grep("\\.Close$", colnames(ts))]))
    ts <- na.omit(ts)
    attr(ts, "na.action") <- NULL
    return(ts)
},

# Visualize Close price or returns
plot_close_or_rets = function(type = "close") {
  colnames(ts) <- sub(".*\\.", "", colnames(ts))
  switch(type,
        "close" = {
          # Plot Close
          ggplot(ts, aes(x = as.Date(index(ts)))) +
            geom_line(aes(y = Close, color = "Active Strategy"), color = "black") +
            geom_hline(yintercept = mean(ts$Close), linetype = "dashed", color = "blue") +
            labs(title = "Close price",
                  x = "Date",
                  y = "Close price") +
            scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
            theme_minimal()
        },

        "rets" = {
         ggplot(ts, aes(x = as.Date(index(ts)))) +
            geom_line(aes(y = value, color = "Active Strategy"), color = "black") +
            geom_hline(yintercept = mean(ts$value), linetype = "dashed", color = "blue") +
            labs(title = "Log returns",
                  x = "Date",
                  y = "log return") +
            scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
            theme_minimal()         
        },

        "rets_hist" = {

          ggplot(ts, aes(value)) +
            geom_histogram(aes(y = after_stat(density)), binwidth = 0.001, fill = "lightblue", color = "black", boundary = 0.5) +
            stat_function(fun = dnorm, args = list(mean = mean(ts$value), sd = sd(ts$value)), color = "red", size = 1) +
            labs(title = "Histogram with Normal Distribution Curve", 
                x = "Numeric Vector", 
                y = "Density") +
            theme_minimal()
        },
        stop("Invalid type. Choose either 'close' or 'rets'.")
  )
},

# Compute missing ratio of values that are not available for Close price (US public holidays are not considered)
compute_NA_close_price_ratio = function() {

  dates <- data.frame(Date = as.Date(seq(from = from_date, to = to_date, by = "day") %>%
    `[`(., !weekdays(.) %in% c("Saturday", "Sunday"))))

  ts_df <- data.frame(ts)
  ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything())

  df <- merge(ts_df %>% select(Date, Close), dates, by = "Date", all.y = TRUE)
  print(paste("Missing Close values ratio considering all dates is:", sum(is.na(df$Close))/nrow(df)))

  # Compute missing ratio across business weekdays
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") 
  NA_weekdays <- lapply(days, function(day) {
  missing_ratio <- sum(is.na(df[df$Date %in% df$Date[weekdays(df$Date) %in% day],]$Close)) / nrow(df[df$Date %in% df$Date[weekdays(df$Date) %in% day],])
  return(missing_ratio)
  })

  names(NA_weekdays) <- days
  # Convert the list of missing ratios to a data frame
  NA_weekdays <- data.frame(Weekday = names(NA_weekdays), Missing_Ratio = unlist(NA_weekdays))

  return(NA_weekdays)
}
  )
)


# Define parent Strategy class (modified slightly based on the ideas in Robert Parto)
Strategy <- R6Class(
  "Strategy",
  public = list(
    data = NULL,

initialize = function(data) {
      self$data <- data
},

# Signal generation, specific to each sub-class (generic method)
generate_signals = function() {
},

convert_to_tibble = function(ts) {
    ts_df <- data.frame(ts)
    ts_df <- ts_df %>%
        rename_with(~ sub(".*\\.", "", .), everything()) %>%
          mutate(Date = as.Date(as.character(rownames(.)))) %>%
            select(Date, everything()) %>%
                na.omit() %>% 
                    as_tibble()
    return(ts_df)

    # ts_df <- data.frame(Date = index(ts), coredata(ts)) %>%
    #   rename_with(~ sub(".*\\.", "", .), everything()) %>%
    #     na.omit() %>%
    #       as_tibble()
    # return(ts_df)
},

# Macrospopic level (overall performance) - understand the trading profile of a Strategy (inlcuding 0.1% transaction fee)
estimate_performance = function(data_type, split_data, cut_date, window, apply_stop_loss, stop_loss_threshold, reward_ratio, capital, leverage, symbol) {
  
  # Slice self$data using the private slicer method
  self$data <- private$slicer(self$data, cut_date, data_type)
  
  # Modify the format of Volume column
  self$data$Volume <- format(self$data$Volume, scientific = TRUE)

  # Call generate_signals dynamically
  self$generate_signals()

  if (apply_stop_loss) {
    self$data <- private$apply_bracket(self$data, stop_loss_threshold, reward_ratio)
  }

  self$data <- self$data %>% 
    mutate(
      pnlActive = c(0, diff(Close) * position[-length(position)]),
      pnlPassive = c(0, diff(Close)),
      nopActive = 0,
      nopPassive = 0,
      eqlActive = 0,
      eqlPassive = 0,
      TC = 0,  # Initialize the TC column to store transaction costs
      From = as.Date(NA),
      To = as.Date(NA)
    )

  # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
  self$data$eqlActive[1] <- capital
  self$data$nopActive[1] <- capital / (self$data$Close[1] / leverage)
  self$data$eqlPassive[1] <- capital
  self$data$nopPassive[1] <- capital / (self$data$Close[1])  

  min_stop_loss <- 0.002 # 0.2%
  tc_rate <- 0.001 # Transaction cost rate (0.1%)

  for (i in 2:nrow(self$data)) {
      # Active
      pnlActive <- self$data$pnlActive[i]
      prev_nop_Active <- self$data$nopActive[i - 1]
      
      # Check if a position change occurred (e.g., based on signals or any relevant criteria)
      if (self$data$position[i] != self$data$position[i - 1]) {
        effective_stop_loss <- max(stop_loss_threshold, min_stop_loss)
        
        # Recalculate nopActive only when a position change occurs
        current_nop_Active <- min(self$data$eqlActive[i - 1] * 0.01 / ((self$data$Close[i] * effective_stop_loss) / leverage),
                                  self$data$eqlActive[i - 1] / ((self$data$Close[i] * effective_stop_loss) / leverage))
        
        self$data$nopActive[i] <- current_nop_Active
        
        # Apply transactional cost (deduct from equity line)
        trade_value <- self$data$Close[i] * abs(current_nop_Active - prev_nop_Active) # pay for what we buy or liquidate
        #trade_value <- self$data$Close[i] * prev_nop_Active
        transactional_cost <- trade_value * tc_rate
        
        # Update the TC column with the transactional cost for the current position change
        self$data$TC[i] <- transactional_cost
        
        # Deduct the transactional cost from the equity line
        self$data$eqlActive[i - 1] <- self$data$eqlActive[i - 1] - transactional_cost
      } else {
        # If no position change, carry forward the previous nopActive
        self$data$nopActive[i] <- prev_nop_Active
      }
      
      # Update active equity
      self$data$eqlActive[i] <- self$data$eqlActive[i - 1] + prev_nop_Active * pnlActive
      
      # Passive
      pnlPassive <- self$data$pnlPassive[i]
      prev_nop_Passive <- self$data$nopPassive[i - 1]
      current_nop_Passive <- self$data$eqlPassive[i - 1] / (self$data$Close[i] / leverage)
      
      self$data$eqlPassive[i] <- self$data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
      self$data$nopPassive[i] <- current_nop_Passive
    }

  self$data <- self$data %>%
    mutate(
      cumulative_pnlActive = cumsum(pnlActive * nopActive),
      cumulative_pnlPassive = cumsum(pnlPassive * nopPassive),
      r_eqlActive = (eqlActive - lag(eqlActive)) / lag(eqlActive),
      r_eqlPassive = (eqlPassive - lag(eqlPassive)) / lag(eqlPassive)
    )

  # Ensure Date is in Date format and sorted
  self$data <- self$data %>%
    mutate(Date = as.Date(Date)) %>%
    arrange(Date)

  if (split_data) {
    # Define the start and end dates
    start_date <- min(self$data$Date)
    end_date <- max(self$data$Date)

    # Create a sequence of periods based on the window argument (e.g., 2 years, or any other value)
    period_start <- start_date
    period_end <- period_start %m+% months(window * 12) - days(1)  # Flexible period based on window

    performance_list <- list()

    while (period_start <= end_date) {
      current_end <- min(period_end, end_date)

      # Filter data for the current period
      data_period <- self$data %>%
        filter(Date >= period_start & Date <= current_end)

      # Update self$data to include 'From' and 'To' columns for the current period
      self$data <- self$data %>%
        mutate(From = as.Date(ifelse(Date >= period_start & Date <= current_end, period_start, From)),
              To = as.Date(ifelse(Date >= period_start & Date <= current_end, current_end, To)))

      if (nrow(data_period) > 0) {
        metrics <- private$compute_metrics(data_period, symbol)
        metrics$from <- period_start
        metrics$to <- current_end
        metrics$data_type <- data_type  # Add data_type column
        performance_list[[length(performance_list) + 1]] <- metrics
      }

      # Move to the next period based on the window
      period_start <- period_start %m+% months(window * 12)
      period_end <- period_start %m+% months(window * 12) - days(1)
    }

    # Combine all period performances into one data frame
    performance_df <- bind_rows(performance_list) %>%
      select(ticker, from, to, data_type, Strategy, everything())
    return(performance_df)
  } else {
    # If split_data is FALSE, compute metrics for the entire dataset
    metrics <- private$compute_metrics(self$data, symbol)
    metrics$from <- min(self$data$Date)
    metrics$to <- max(self$data$Date)
    metrics$data_type <- data_type  # Add data_type column
    
    performance_df <- as.data.frame(metrics) %>%
      select(ticker, from, to, data_type, Strategy, everything())
    
    return(performance_df)
  }
},

# Method to plot Close price and running PnL
plot_performance = function() {

    # Filter unique From and To dates
    unique_dates <- self$data %>%
    filter(!is.na(From) | !is.na(To)) %>%
    select(From, To) %>%
    distinct() %>%
    pivot_longer(cols = c(From, To), names_to = "LineType", values_to = "Date") %>%
    filter(!is.na(Date))

    # Generate date breaks for every 2 years
    date_breaks <- seq(from = floor_date(min(self$data$Date), "year"),
                        to = ceiling_date(max(self$data$Date), "year"),
                        by = "2 years")

    # Plot Close price dynamics with vertical lines
    p1 <- ggplot(self$data, aes(x = Date, y = Close)) +
    geom_line(color = "black") +
    geom_vline(data = unique_dates,
                aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "blue") +
    scale_x_date(breaks = date_breaks, date_labels = "%Y") +
    labs(title = paste0("Historical Close Price of ", symbol),
            x = "Date",
            y = "Close Price") +
    theme_minimal()

    # Plot eqlActive and eqlPassive with vertical lines
    p2 <- ggplot(self$data, aes(x = Date)) +
    geom_line(aes(y = eqlActive), color = "red") +
    geom_line(aes(y = eqlPassive), color = "green") +
    geom_vline(data = unique_dates,
                aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "blue") +
    scale_x_date(breaks = date_breaks, date_labels = "%Y") +
    labs(title = paste0("Running PnL for ", symbol, " for Active and Passive Strategies"),
            x = "Date",
            y = "Portfolio Value") +
    theme_minimal()

    # Combine plots using patchwork
    p1 / p2
},

# Microscopic level (tabular list of all trades)
get_trades = function() {

  # Prepare trade summary with tradePnL
  trades <- self$data %>%
    mutate(
      Date = as.Date(Date),
      trade_direction = ifelse(position == -1, "Sell", "Buy"),
      entry = as.Date(ifelse(position != lag(position), Date, NA)), # Entry point
      entry_price = ifelse(position != lag(position), Close, NA), # Entry price
      entry_size = ifelse(position != lag(position), nopActive, NA), # Entry size
      exit = as.Date(ifelse(position != lead(position), Date, NA)), # Exit point
      exit_price = ifelse(position != lead(position), Close, NA), # Exit price
      exit_size = ifelse(position != lead(position), nopActive, NA) # Exit size
    ) %>%
    tidyr::fill(entry, entry_price, entry_size, .direction = "down") %>%
    tidyr::fill(exit, exit_price, exit_size, .direction = "up") %>%
    filter(!is.na(entry) & !is.na(exit)) %>%
    group_by(entry, exit) %>%
    summarise(
      Buy_Sell = first(trade_direction), # Trade type (Buy/Sell)
      EntryDate = as.Date(first(entry)), # Entry date
      EntrySize = first(entry_size), # Correct entry size
      EntryPrice = first(entry_price), # Price at entry
      ExitDate = as.Date(first(exit)), # Exit date
      ExitSize = first(exit_size), # Correct exit size
      ExitPrice = first(exit_price), # Price at exit
      Trade_Cum_PnL = ifelse(Buy_Sell == "Buy", ExitPrice - EntryPrice, EntryPrice - ExitPrice) * EntrySize # Trade profit/loss with size
    ) %>%
    ungroup() %>% # Remove grouping for calculating Running_PnL
    mutate(
      Running_PnL = cumsum(Trade_Cum_PnL), # Running cumulative PnL across all trades
      Efficiency = (Trade_Cum_PnL / abs(Running_PnL)) * 100 # Efficiency as % of Running_PnL
    )

  return(trades)
},

# Visualize equity lines for active strategy and passive (buy and hold)
plot_equity_lines = function(strategy_name, signal_flag = FALSE, symbol, capital) {
  # Line size
  active_line_size <- ifelse(signal_flag, 1, 0.8)
  passive_line_size <- ifelse(signal_flag, 1, 0.8)
  
  p <- ggplot(self$data, aes(x = Date)) +
  labs(
    title = paste0(
      "Asset: ", symbol, ", capital trajectory for Active (", 
      as.character(strategy_name), ") and Passive (buy-and-hold)\n",
      "strategies with original investment of ", capital, " USDC ",
      "over the period from ", self$data$Date %>% head(1), " to ", self$data$Date %>% tail(1)
    ),
    x = "Date",
    y = "Equity line"
  ) +
  theme_minimal()
  
  # Add vertical dashed lines for periods using From and To columns
  # Extract unique From and To values and convert to data.frame
  period_lines <- data.frame(From = unique(self$data$From), To = unique(self$data$To))

  # Add signals if signal_flag is TRUE
  if (signal_flag) {
    p <- p +
      geom_vline(data = self$data[self$data$signal == -1, ], 
                 aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$signal == 1, ], 
                 aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "green", alpha = 0.5)
  }
  
  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Add vertical lines for the From and To columns
  p <- p + 
    geom_vline(data = period_lines, aes(xintercept = as.numeric(From)), 
               linetype = "solid", color = "black", alpha = 1, size = 1) +
    geom_vline(data = period_lines, aes(xintercept = as.numeric(To)), 
               linetype = "solid", color = "black", alpha = 1, size = 1)

  # Print or return the plot
  if (signal_flag) {
    #print(p)
    return(p)
  } else {
    return(p)
  }
},

# Plot Japanese candles for the period of latest ndays
plot_candles = function(ndays) {
  self$data <- tail(self$data, ndays)
  fig <- self$data %>% plot_ly(x = ~Date, type= "candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) 
  fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
  fig
},

# Estimate Average True Range (ATR)
estimate_average_true_range = function(n = 14) {
  # Calculate ATR using self$data
  atr <- ATR(HLC(self$data), n)
  
  # Convert ATR and self$data to data.tables
  atr_data <- as.data.table(data.frame(Date = index(atr), coredata(atr)))
  ts_data <- as.data.table(data.frame(Date = index(self$data), coredata(self$data)))
  
  # Merge ATR data with self$data by Date
  merged_data <- merge(ts_data, atr_data, by = "Date", all.x = TRUE)
  
  # Calculate tr_reserve
  merged_data[, tr_reserve := tr / atr * 100] # Calculate tr_reserve as a percentage
  
  # Update self$data safely
  for (col in c("tr", "atr", "trueHigh", "trueLow", "tr_reserve")) {
    self$data[, (col) := merged_data[[col]]]
  }
  
  # Return self$data
  return(self$data)
}

  ),
    
private = list(

# Function to compute metrics for the trading profile of a Strategy
compute_metrics = function(data_subset, symbol) {
    
  estimate_trading_profile <- function(data_subset, strategy_type) {

      data_subset$Date <- as.Date(data_subset$Date)

      # Select appropriate columns based on strategy type
      pnl_col <- ifelse(strategy_type == "Active", "pnlActive", "pnlPassive")
      eql_col <- ifelse(strategy_type == "Active", "eqlActive", "eqlPassive")
      r_col <- ifelse(strategy_type == "Active", "r_eqlActive", "r_eqlPassive")
      
      # Generate a trade_id based on changes in position
      data_subset <- data_subset %>% mutate(trade_id = cumsum(position != lag(position, default = 1)))

      GrossProfit <- round(GrossProfit <- sum(na.omit(tail(data_subset[[eql_col]], 1)) - na.omit(data_subset[[eql_col]][1])), 0)

      # 1. Annualized Profit
      AnnualizedProfit <- round(as.numeric(Return.annualized(as.numeric(na.omit(data_subset[[r_col]])), scale = 252, geometric = TRUE) * 100), 2)
      #AnnualizedProfit2 <- round((prod(1 + na.omit(data_subset[[r_col]])) / 1)^(1 / (length(na.omit(data_subset[[r_col]])) / 252)) - 1, 2) * 100 

      # 2. Number of Trades per Year
      NumberOfTradesPerYear <- round((if (strategy_type == "Active") sum(diff(data_subset$position) != 0) + 1 else 1) / 
                                    length(unique(format(data_subset$Date, "%Y"))), 0)

      # 3. Percentage of Winning Trades
      PercentageOfWinningTrades <- round(
        sum(aggregate(data_subset[[pnl_col]], by = list(cumsum(c(1, diff(data_subset$position) != 0))), sum, na.rm = TRUE)$x > 0) / 
        nrow(aggregate(data_subset[[pnl_col]], by = list(cumsum(c(1, diff(data_subset$position) != 0))), sum, na.rm = TRUE)) * 100, 2)

      # 4. Largest Win
      LargestWin <- round(max(data_subset[[pnl_col]], na.rm = TRUE), 0)

      # 5. Length of Largest Win
      LengthOfLargestWin <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.max(data_subset[[pnl_col]])], ], 
                                  as.numeric(max(Date) - min(Date) + 1))

      # 6. Average Win
      AverageWin <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] > 0], na.rm = TRUE), 0)

      # 7. Length of Average Win
      AverageWinLength <- data_subset %>%
        transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
        aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
        subset(cum_pnl > 0) %>%
        merge(data_subset, by = "trade_id") %>%
        aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
        with(round(mean(Date, na.rm = TRUE)))
      
      # 8. Largest Loss
      LargestLoss <- round(min(data_subset[[pnl_col]], na.rm = TRUE),0)

      # 9. Length of Largest Loss
      LengthOfLargestLoss <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.min(data_subset[[pnl_col]])], ], 
                                  as.numeric(max(Date) - min(Date) + 1))

      # 10. Average Loss
      AverageLoss <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] < 0], na.rm = TRUE),0)

      # 11. Length of Average Loss
      AverageLossLength <- data_subset %>%
        transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
        aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
        subset(cum_pnl < 0) %>%
        merge(data_subset, by = "trade_id") %>%
        aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
        with(round(mean(Date, na.rm = TRUE)))

      # 12-15: Winning Runs
      is_winning <- data_subset[[pnl_col]] > 0
      winning_runs <- rle(is_winning)$lengths[rle(is_winning)$values]

      # 12. Average Winning Run
      AverageWinningRun <- round(mean(winning_runs), 2)

      # 13. Largest Winning Run
      LargestWinningRun <- max(winning_runs)

      # 14. Length of Time in Largest Winning Run
      largest_run_start <- sum(head(rle(is_winning)$lengths, which.max(winning_runs) - 1)) + 1
      largest_run_end <- largest_run_start + LargestWinningRun - 1
      LengthOfTimeInLargestWinningRun <- as.numeric(diff(range(data_subset$Date[largest_run_start:largest_run_end]))) + 1

      # 15. Length of Time in Average Winning Run
      average_run_lengths <- sapply(winning_runs, function(len) {
        start <- sum(head(rle(is_winning)$lengths, which(winning_runs == len)[1] - 1)) + 1
        end <- start + len - 1
        as.numeric(diff(range(data_subset$Date[start:end]))) + 1
      })
      LengthOfTimeInAverageWinningRun <- round(mean(average_run_lengths), 0)

      # 16-19: Losing Runs
      is_losing <- data_subset[[pnl_col]] < 0
      losing_runs <- rle(is_losing)$lengths[!rle(is_losing)$values]  # Identify losing runs

      # 16. Average Losing Run
      AverageLosingRun <- round(mean(losing_runs, 2))

      # 17. Length of Time in Average Losing Run
      average_run_lengths_losing <- sapply(losing_runs, function(len) {
        start <- sum(head(rle(is_losing)$lengths, which(losing_runs == len)[1] - 1)) + 1
        end <- start + len - 1
        as.numeric(diff(range(data_subset$Date[start:end]))) + 1
      })
      LengthOfTimeInAverageLosingRun <- round(mean(average_run_lengths_losing), 0)

      # 18. Largest Losing Run
      LargestLosingRun <- max(losing_runs, na.rm = TRUE)

      # 19. Length of Time in Largest Losing Run
      largest_run_start <- sum(head(rle(is_losing)$lengths, which(losing_runs == LargestLosingRun)[1] - 1)) + 1
      largest_run_end <- largest_run_start + LargestLosingRun - 1
      LengthOfLargestLosingRun <- as.numeric(diff(range(data_subset$Date[largest_run_start:largest_run_end]))) + 1

      # 20. Maximum equity drawdown (as a percentage)
      MaxDrawdown <- round(min(data_subset %>%
                          mutate(cum_max_eql = cummax(get(eql_col)),
                                  drawdown = (get(eql_col) - cum_max_eql) / cum_max_eql) %>%
                          pull(drawdown), na.rm = TRUE) * 100, 2)

      # 21. Start and end dates of maximum drawdown
      drawdown_data <- data_subset %>%
        mutate(cum_max_eql = cummax(get(eql_col)), 
              drawdown = (get(eql_col) - cum_max_eql) / cum_max_eql)

      peak_idx <- which.max(drawdown_data[[eql_col]][1:which.min(drawdown_data$drawdown)])  # Peak before max drawdown
      trough_idx <- which.min(drawdown_data$drawdown)  # Trough for max drawdown

      StartDateMaxDrawdown <- as.Date(drawdown_data$Date[peak_idx])
      EndDateMaxDrawdown <- as.Date(drawdown_data$Date[trough_idx])

      # 22. Length of maximum drawdown period (in days)
      LengthOfMaxDrawdown <- as.numeric(EndDateMaxDrawdown - StartDateMaxDrawdown)

      # 23. Maximum equity run-up (as a percentage)
      MaxRunUp <- round(max(data_subset %>%
                        mutate(cum_min_eql = cummin(get(eql_col)),
                              run_up = (get(eql_col) - cum_min_eql) / cum_min_eql) %>%
                        pull(run_up), na.rm = TRUE) * 100,2)

      # 24. Start and end dates of maximum run-up
      run_up_data <- data_subset %>%
        mutate(cum_min_eql = cummin(get(eql_col)),
              run_up = (get(eql_col) - cum_min_eql) / cum_min_eql)

      # Identify the peak (maximum run-up) and trough (start of run-up)
      trough_idx_run_up <- which.min(run_up_data[[eql_col]])  # Trough before the run-up
      peak_idx_run_up <- which.max(run_up_data$run_up)  # Peak during the run-up

      # Ensure that the peak happens after the trough
      if (peak_idx_run_up < trough_idx_run_up) {
        peak_idx_run_up <- which.max(run_up_data$run_up[trough_idx_run_up:length(run_up_data$run_up)]) + trough_idx_run_up - 1
      }

      StartDateMaxRunUp <- as.Date(run_up_data$Date[trough_idx_run_up])
      EndDateMaxRunUp <- as.Date(run_up_data$Date[peak_idx_run_up])

      # 25. Length of maximum run-up period (in days)
      LengthOfMaxRunUp <- as.numeric(EndDateMaxRunUp - StartDateMaxRunUp)

      # Return the metrics as a list
      return(
        list(
          GrossProfit = GrossProfit,
          AnnualizedProfit = AnnualizedProfit,
          NumberOfTradesPerYear = NumberOfTradesPerYear,
          PercentageOfWinningTrades = PercentageOfWinningTrades,
          AverageWin = AverageWin,
          LengthOfAverageWin = AverageWinLength,
          AverageLoss = AverageLoss,
          LengthOfAverageLoss = AverageLossLength,
          LargestWin = LargestWin,
          LengthOfLargestWin = LengthOfLargestWin,
          LargestLoss = LargestLoss,
          LengthOfLargestLoss = LengthOfLargestLoss,
          AverageWinningRun = AverageWinningRun,
          LengthOfTimeInAverageWinningRun = LengthOfTimeInAverageWinningRun,
          AverageLosingRun = AverageLosingRun,
          LengthOfTimeInAverageLosingRun = LengthOfTimeInAverageLosingRun,
          LargestWinningRun = LargestWinningRun,
          LengthOfTimeInLargestWinningRun = LengthOfTimeInLargestWinningRun,
          LargestLosingRun = LargestLosingRun,
          LengthOfTimeInLargestLosingRun = LengthOfLargestLosingRun,
          MaxDrawdown = MaxDrawdown,
          StartDateMaxDrawdown = as.Date(StartDateMaxDrawdown),
          EndDateMaxDrawdown = as.Date(EndDateMaxDrawdown),
          LengthOfMaxDrawdown = LengthOfMaxDrawdown,
          MaxRunUp = MaxRunUp,
          StartDateMaxRunUp = as.Date(StartDateMaxRunUp),
          EndDateMaxRunUp = as.Date(EndDateMaxRunUp),
          LengthOfMaxRunUp = LengthOfMaxRunUp
        )
      )
    }

    # Metrics for Active strategy
    active <- estimate_trading_profile(data_subset, "Active")

    # Metrics for Passive strategy
    passive <- estimate_trading_profile(data_subset, "Passive")

    # Combine metrics into a dataframe

    metrics_df <- data.frame(
      Strategy = c("Active", "Passive"),
      ticker = symbol,
      GrossProfit = c(active$GrossProfit, passive$GrossProfit),
      AnnualizedProfit = c(active$AnnualizedProfit, passive$AnnualizedProfit),
      NumberOfTradesPerYear = c(active$NumberOfTradesPerYear, passive$NumberOfTradesPerYear),
      PercentageOfWinningTrades = c(active$PercentageOfWinningTrades, "NotApplicable"),
      AverageWin = c(active$AverageWin, passive$AverageWin),
      LengthOfAverageWin = c(active$LengthOfAverageWin, passive$LengthOfAverageWin),
      LargestWin = c(active$LargestWin, passive$LargestWin),
      LengthOfLargestWin = c(active$LengthOfLargestWin, passive$LengthOfLargestWin),
      AverageLoss = c(active$AverageLoss, passive$AverageLoss),
      LengthOfAverageLoss = c(active$LengthOfAverageLoss, passive$LengthOfAverageLoss),
      LargestLoss = c(active$LargestLoss, passive$LargestLoss),
      LengthOfLargestLoss = c(active$LengthOfLargestLoss, passive$LengthOfLargestLoss),
      AverageWinningRun = c(active$AverageWinningRun, passive$AverageWinningRun),
      LengthOfTimeInAverageWinningRun = c(active$LengthOfTimeInAverageWinningRun, passive$LengthOfTimeInAverageWinningRun),
      LargestWinningRun = c(active$LargestWinningRun, passive$LargestWinningRun),
      LengthOfTimeInLargestWinningRun = c(active$LengthOfTimeInLargestWinningRun, passive$LengthOfTimeInLargestWinningRun),
      AverageLosingRun = c(active$AverageLosingRun, passive$AverageLosingRun),
      LengthOfTimeInAverageLosingRun = c(active$LengthOfTimeInAverageLosingRun, passive$LengthOfTimeInAverageLosingRun),
      LargestLosingRun = c(active$LargestLosingRun, passive$LargestLosingRun),
      LengthOfTimeInLargestLosingRun = c(active$LengthOfTimeInLargestLosingRun, passive$LengthOfTimeInLargestLosingRun),
      MaxDrawdown = c(active$MaxDrawdown, passive$MaxDrawdown),
      LengthOfMaxDrawdown = c(active$LengthOfMaxDrawdown, passive$LengthOfMaxDrawdown),
      StartDateMaxDrawdown = c(as.Date(active$StartDateMaxDrawdown), as.Date(passive$StartDateMaxDrawdown)),
      EndDateMaxDrawdown = c(as.Date(active$EndDateMaxDrawdown), as.Date(passive$EndDateMaxDrawdown)),
      MaxRunUp = c(active$MaxRunUp, passive$MaxRunUp),
      LengthOfMaxRunUp = c(active$LengthOfMaxRunUp, passive$LengthOfMaxRunUp),
      StartDateMaxRunUp = c(as.Date(active$StartDateMaxRunUp), as.Date(passive$StartDateMaxRunUp)),
      EndDateMaxRunUp = c(as.Date(active$EndDateMaxRunUp), as.Date(passive$EndDateMaxRunUp))
  )
  
  return(metrics_df)

},

# Private method that applies a stop loss and reward take based on the thresholds
apply_bracket = function(data, threshold, reward_ratio) {
  
  # Convert data to data.table
  data <- as.data.table(data)
  
  # Remove duplicate `position_original` if it already exists
  if ("position_original" %in% colnames(data)) {
    data[, position_original := NULL]
  }
  
  # Add position_group for tracking groups of positions
  data[, position_group := cumsum(position != shift(position, type = "lag", fill = 0))]
  
  # Initialize columns
  data[, `:=`(stop_loss_event = FALSE, 
              profit_take_event = FALSE, 
              position_modified = position, 
              stop_loss_trigger = NA_real_, 
              profit_take_trigger = NA_real_, 
              calculated_stop_loss = NA_real_, 
              calculated_profit_take = NA_real_)]
  
  # Iterate through each group
  data[, c("stop_loss_event", "profit_take_event", "position_modified", 
           "stop_loss_trigger", "profit_take_trigger", 
           "calculated_stop_loss", "calculated_profit_take") := {
    
    stop_loss_flag <- FALSE
    profit_take_flag <- FALSE
    pos_mod <- position
    stop_loss_evt <- logical(.N)  # Initialize for current group
    profit_take_evt <- logical(.N)  # Initialize for current group
    stop_loss_trig <- rep(NA_real_, .N)  # Initialize stop-loss trigger price
    profit_take_trig <- rep(NA_real_, .N)  # Initialize profit-taking trigger price
    calc_stop_loss <- rep(NA_real_, .N)  # Initialize calculated stop-loss price
    calc_profit_take <- rep(NA_real_, .N)  # Initialize calculated profit-taking price
    
    # Calculate stop-loss and profit-taking prices for each position
    if (position[1] == 1) {
      calc_stop_loss <- Close[1] * (1 - threshold)
      calc_profit_take <- Close[1] * (1 + reward_ratio * threshold)
    } else if (position[1] == -1) {
      calc_stop_loss <- Close[1] * (1 + threshold)
      calc_profit_take <- Close[1] * (1 - reward_ratio * threshold)
    } 

    for (i in seq_len(.N)) {
      if (!stop_loss_flag && !profit_take_flag) {
        # Trigger stop-loss
        if (position[i] == 1 && Close[i] <= calc_stop_loss[1]) {
          stop_loss_evt[i] <- TRUE
          stop_loss_trig[i] <- Close[i]
          stop_loss_flag <- TRUE
        } else if (position[i] == -1 && Close[i] >= calc_stop_loss[1]) {
          stop_loss_evt[i] <- TRUE
          stop_loss_trig[i] <- Close[i]
          stop_loss_flag <- TRUE
        }
        
        # Trigger profit-taking
        if (position[i] == 1 && Close[i] >= calc_profit_take[1]) {
          profit_take_evt[i] <- TRUE
          profit_take_trig[i] <- Close[i]
          profit_take_flag <- TRUE
        } else if (position[i] == -1 && Close[i] <= calc_profit_take[1]) {
          profit_take_evt[i] <- TRUE
          profit_take_trig[i] <- Close[i]
          profit_take_flag <- TRUE
        }
      } else {
        # After stop-loss or profit-taking, reverse position on the next day
        if (i > 1) {
          if (stop_loss_evt[i - 1] || profit_take_evt[i - 1]) {
            pos_mod[i] <- -pos_mod[i - 1]
          } else {
            pos_mod[i] <- 0
          }
        }
      }
    }
    
    list(stop_loss_evt, profit_take_evt, pos_mod, stop_loss_trig, 
         profit_take_trig, calc_stop_loss, calc_profit_take)
  }, by = position_group]
  
  # Rename columns at the end
  setnames(data, "position", "position_original")
  setnames(data, "position_modified", "position")
  
  return(data)
},

# Cut the Strategy time horizon, used for the data split
slicer = function(data, cut_date, data_type) {
  switch(data_type,
         "in_sample" = data %>% filter(Date <= as.Date(cut_date)),
         "out_of_sample" = data %>% filter(Date > as.Date(cut_date)),
         stop("Invalid data_type. Use 'in_sample' or 'out_of_sample'.")
  )
}

  )
)

# Define SMA1 (trend following strategy) class
SMA1 <- R6Class(
  "SMA1",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = NULL,

initialize = function(data, window_size, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$ma_type <- ma_type
},

generate_signals = function() {
      ma_func <- get(self$ma_type)
      self$data <- mutate(self$data, 
                          ma = ma_func(Close, self$window_size, align = "right", fill = NA),
                          signal = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
}

  )
)

# Define SMA2 (trend following strategy) class
SMA2 <- R6Class(
  "SMA2",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = NULL,

initialize = function(data, window_size1, window_size2, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$ma_type <- ma_type
},

generate_signals = function() {
      ma_func <- get(self$ma_type)
      # Calculate first and second moving averages
      self$data <- mutate(self$data, 
                          ma1 = ma_func(Close, self$window_size1, align = "right", fill = NA),
                          ma2 = ma_func(Close, self$window_size2, align = "right", fill = NA),
                          signal = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit

}

  )
)

# Define SMA1 class with modified signals
SMA1M <- R6Class(
  "SMA1M",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = NULL,

initialize = function(data, window_size, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$ma_type <- ma_type
},

# Generate modified signals
generate_signals = function() {
    ma_func <- get(self$ma_type)
    self$data <- mutate(
    self$data, 
    ma = ma_func(Close, self$window_size, align = "right", fill = NA),
    #signal1 = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0))) %>%
    signal1 = ifelse(Close > lag(ma), 1, ifelse(Close < lag(ma), -1, 0))) %>%
    #position = lag(signal1, default = 0)) %>% # position will be defined at the end given additional condition
      na.omit
    
    # Initialize last_long_value and last_short_value (dynamic trailing threshold)
    last_long_value <- NA
    last_short_value <- NA

    # Create empty columns in the data frame to store last long and short values
    self$data$last_long_value <- NA
    self$data$last_short_value <- NA

    # Loop through each row of the data frame
    for (i in 1:nrow(self$data)) {
        # Check if current row has signal == 1
        if (self$data$signal1[i] == 1) {
        # Find the index of the first previous occurrence of 1
        first_previous_index <- NA
        if (any(self$data$signal1[1:(i-1)] == 1)) {
            first_previous_index <- max(which(self$data$signal1[1:(i-1)] == 1))
        }
        # If a previous occurrence of 1 is found, update last_long_value
        if (!is.na(first_previous_index)) {
            #last_long_value <- self$data$value[first_previous_index]
            last_long_value <- self$data$Close[first_previous_index]
        }
        # Assign last long value to the corresponding row in the data frame
        self$data$last_long_value[i] <- last_long_value
        }
        # Check if current row has signal1 == -1
        if (self$data$signal1[i] == -1) {
        # Find the index of the first previous occurrence of -1
        first_previous_index <- NA
        if (any(self$data$signal1[1:(i-1)] == -1)) {
            first_previous_index <- max(which(self$data$signal1[1:(i-1)] == -1))
        }
        # If a previous occurrence of -1 is found, update last_short_value
        if (!is.na(first_previous_index)) {
            #last_short_value <- self$data$value[first_previous_index]
            last_short_value <- self$data$Close[first_previous_index]
        }
        # Assign last short value to the corresponding row in the data frame
        self$data$last_short_value[i] <- last_short_value
        }

        # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
        self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
        self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)
    }

    # Compare data$value[i] with the first previous value and update data$s2
    self$data$signal <- NA

    # self$data$signal <- ifelse((self$data$Close > self$data$last_long_value) & (self$data$Close > self$data$ma), 1,
    #                     ifelse((self$data$Close < self$data$last_short_value) & (self$data$Close < self$data$ma), -1, 0))

    self$data$signal <- ifelse((self$data$Close > self$data$last_long_value) & (self$data$Close > lag(self$data$ma)), 1,
                    ifelse((self$data$Close < self$data$last_short_value) & (self$data$Close < lag(self$data$ma)), -1, 0))

    # Replacing 0s by previous signal value:
    self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
    self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)

    self$data$position <- lag(self$data$signal, default = 0)

    #self$data <- self$data 
    # %>% select(-c(signal1, last_long_value, last_short_value))
}

    )
)

# SMA2 (modified by dynamic trailing stop)
SMA2M <- R6Class(
  "SMA2M",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = NULL,

initialize = function(data, window_size1, window_size2, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$ma_type <- ma_type
},

generate_signals = function() {
      ma_func <- get(self$ma_type)
      self$data <- mutate(self$data, 
                          ma1 = ma_func(Close, self$window_size1, align = "right", fill = NA),
                          ma2 = ma_func(Close, self$window_size2, align = "right", fill = NA),
                          signal1 = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0))) %>%
                          # position = lag(signal1, default = 0)) %>% # position is defined at the end after additional condition implementation 
                            na.omit
      
      # Initialize last_long_value and last_short_value (dynamic trailing threshold)
      last_long_value <- NA
      last_short_value <- NA

      # Create empty columns in the data frame to store last long and short values
      self$data$last_long_value <- NA
      self$data$last_short_value <- NA

      # Loop through each row of the data frame
      for (i in 1:nrow(self$data)) {
          # Check if current row has signal1 == 1
          if (self$data$signal1[i] == 1) {
            # Find the index of the first previous occurrence of 1
            first_previous_index <- NA
            if (any(self$data$signal1[1:(i-1)] == 1)) {
              first_previous_index <- max(which(self$data$signal1[1:(i-1)] == 1))
            }
            # If a previous occurrence of 1 is found, update last_long_value
            if (!is.na(first_previous_index)) {
              last_long_value <- self$data$ma1[first_previous_index]
            }
            # Assign last long value to the corresponding row in the data frame
            self$data$last_long_value[i] <- last_long_value
          }
          # Check if current row has signal1 == -1
          if (self$data$signal1[i] == -1) {
            # Find the index of the first previous occurrence of -1
            first_previous_index <- NA
            if (any(self$data$signal1[1:(i-1)] == -1)) {
              first_previous_index <- max(which(self$data$signal1[1:(i-1)] == -1))
            }
            # If a previous occurrence of -1 is found, update last_short_value
            if (!is.na(first_previous_index)) {
              #last_short_value <- self$data$ma2[first_previous_index]
              last_short_value <- self$data$ma1[first_previous_index]
            }
            # Assign last short value to the corresponding row in the data frame
            self$data$last_short_value[i] <- last_short_value
          }

          # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
          self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
          self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)
      }

      # Signals:
      
      # self$data$signal <- NA
      # self$data$signal <- ifelse((self$data$ma1 > self$data$last_long_value) & (self$data$ma1 > self$data$ma2), 1,
      #                            ifelse((self$data$ma1 < self$data$last_short_value) & (self$data$ma1 < self$data$ma2), -1, 0))

      self$data$signal <- NA
      self$data$signal <- ifelse((lag(self$data$ma1) > self$data$last_long_value) & (lag(self$data$ma1) > lag(self$data$ma2)), 1,
                                 ifelse((lag(self$data$ma1) < self$data$last_short_value) & (lag(self$data$ma1) < lag(self$data$ma2)), -1, 0))
      #self$data <- self$data 
      # %>% select(-c(last_long_value, last_short_value))
    # Replacing 0s by previous signal value:
      self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
      self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)
      self$data$position <- lag(self$data$signal, default = 0)
}

  )
)

# Define MACD class
MACD <- R6Class(
"MACD",
inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    sline = NULL,

initialize = function(data, window_size1 = 12, window_size2 = 26, sline = 9) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$sline <- sline
},

generate_signals = function() {
      self$data <- mutate(self$data,
                          ma1 = EMA(Close, self$window_size1, align = "right", fill = NA),
                          ma2 = EMA(Close, self$window_size2, align = "right", fill = NA),
                          macd_line = ma1 - ma2,
                          signal_line = EMA(macd_line, self$sline),
                          signal = ifelse(macd_line > signal_line, 1, ifelse(macd_line < signal_line, -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit()
}

  )
)

# Define Donchian Channel (DC) class (also, it could be used with RSI and MACD)
DonchianChannel <- R6Class(
  "DonchianChannel",
  inherit = Strategy,
  public = list(
    window_size = NULL,

initialize = function(data, window_size) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$window_size = window_size
},

generate_signals = function() {
  self$data <- mutate(self$data,
                      upper_channel = rollapply(self$data$High, self$window_size, max, align = "right", fill = NA),
                      lower_channel = rollapply(self$data$Low, self$window_size, min, align = "right", fill = NA),
                      mid = (upper_channel + lower_channel) / 2, # optional
                      signal1 = ifelse(Close > lag(upper_channel), 1, ifelse(Close < lag(lower_channel), -1, 0)),
                      signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
                          position = lag(signal, default = 0)) %>% 
                        na.omit()
},

plot_channels = function(name) {
  ggplot(self$data, aes(x = Date)) +
    geom_line(aes(y = Close, color = "black"), size = 1) +
    geom_line(aes(y = upper_channel, color = "green"), linetype = "longdash", size = 1) +
    geom_line(aes(y = lower_channel, color = "red"), linetype = "longdash", size = 1) +
    geom_line(aes(y = mid, color = "yellow"), linetype = "longdash", size = 1) +
    labs(
      title = paste0("Donchian Channel for ", name),
      x = "Date",
      y = "Price"
    ) +
    scale_y_continuous(breaks = seq(min(self$data$lower_channel, na.rm = TRUE), 
                                  max(self$data$upper_channel, na.rm = TRUE), 
                                  by = 20)) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    
    theme_minimal()
}

  )
)

# Define Relative Strength Index class
RSI <- R6Class(
  "RSI",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    threshold_oversold = NULL,
    threshold_overbought = NULL,

initialize = function(data, window_size, threshold_oversold = 30, threshold_overbought = 70) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$threshold_oversold <- threshold_oversold
      self$threshold_overbought <- threshold_overbought
},

generate_signals = function() {
  self$data <- mutate(self$data,
                      avg_gain = rollmean(ifelse(Close > 0, Close, 0), k = self$window_size, align = "right", fill = NA),
                      avg_loss = rollmean(ifelse(Close < 0, abs(Close), 0), k = self$window_size, align = "right", fill = NA),
                      rs = avg_gain / avg_loss,
                      rsi = 100 - (100 / (1 + rs)),
                      signal = ifelse(rsi < self$threshold_oversold, 1, ifelse(rsi > self$threshold_overbought, -1, 0)),
                      position = lag(signal, default = 0)) %>%
              na.omit
},

plot_avg_gain_loss_with_equity_lines = function() {
        ggplot(self$data, aes(x = Date)) + # include the equity lines plot from the parent Strategy class
            geom_line(aes(y = avg_gain, color = "Average Gain")) +
            geom_line(aes(y = avg_loss, color = "Average Loss")) +
            #geom_line(aes(y = equity_line, color = "Equity Line")) +  # Include equity line
            geom_line(aes(y = equity_line, color = "Equity Line")) +  # Include equity line
            labs(title = "Average Gain, Average Loss, and Equity Line",
                x = "Date",
                y = "Value") +
            scale_color_manual(values = c("Average Gain" = "blue", "Average Loss" = "red", "Equity Line" = "green")) +
            theme_minimal()
}

  )
)

# Define TurtleTrading class (Richard)
TurtleTrading <- R6Class(
  "TurtleTrading",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,

initialize = function(data, window_size1, window_size2) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$window_size1 <- window_size1
  self$window_size2 <- window_size2
},

generate_signals = function() {
  self$data <- mutate(self$data,
                      upper_channel1 = rollapply(self$data$High, self$window_size1, max, align = "right", fill = NA),
                      upper_channel2 = rollapply(self$data$High, self$window_size2, max, align = "right", fill = NA),
                      lower_channel1 = rollapply(self$data$Low, self$window_size1, min, align = "right", fill = NA),
                      lower_channel2 = rollapply(self$data$Low, self$window_size2, min, align = "right", fill = NA),
                      mid1 = (upper_channel1 / lower_channel1) / 2,
                      mid2 = (upper_channel2 / lower_channel2) / 2,
                      signal1 = ifelse(Close > lag(upper_channel1) & Close > lag(upper_channel2), 1,
                        ifelse(Close < lag(lower_channel1) & Close < lag(lower_channel2), -1, 0)),
                      signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
                      position = lag(signal, default = 0)) %>% 
                        na.omit()
}

  )
)

# Define Stop and Reversal (SAR) class
StopAndReversal <- R6Class(
  "StopAndReversal",
  inherit = Strategy,
  public = list(
    accel = NULL,
    accel_max = NULL,
    accel_vector = NULL,

initialize = function(data, accel, accel_max) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$accel <- accel
      self$accel_max <- accel_max
      self$accel_vector <- c(accel, accel_max)
    },

generate_signals = function() {
      self$data <- self$data %>% 
        mutate(
          SAR = TTR::SAR(select(., High, Low), accel = self$accel_vector),
          signal = case_when(
            Close > lag(SAR) ~ 1,
            Close < lag(SAR) ~ -1,
            TRUE ~ 0
          ),
          position = lag(signal, default = 0)
        ) %>%
        na.omit()
},

plot_sar = function(name){
  ggplot(self$data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Price"), size = 0.5) +
  geom_point(aes(y = SAR, color = "SAR"), size = 1) +
  labs(
    title = paste0("Parabolic SAR for ", name),
    x = "Date",
    y = "Price"
  ) +
  scale_color_manual(values = c("Price" = "red", "SAR" = "blue")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal()
}

  )
)

# Define Average Directional Index (ADX) class
ADX <- R6Class(
  "AverageDirectionalIndex",
  inherit = Strategy,
  public = list(
    ndx = NULL,
    trend_strength = NULL,

initialize = function(data, ndx, trend_strength) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$ndx = ndx
  self$trend_strength = trend_strength
},

generate_signals = function() {
  self$data <- self$data %>%
                  mutate(
                  self$data,
                  as.data.frame(TTR::ADX(select(., High, Low, Close), n = self$ndx)),
                  signal1 = case_when(
                    #DIp > DIn & ADX > self$trend_strength ~ 1, # lag ?
                    #DIp > lag(DIn) & ADX > self$trend_strength ~ 1, # lag
                    DIp > lag(DIn) & lag(ADX) > self$trend_strength ~ 1, # lag
                    lag(DIp) > lag(DIn) & lag(ADX) > self$trend_strength ~ 1, # lag
                    #DIp < DIn & ADX > self$trend_strength ~ -1,
                    #DIp < lag(DIn) & ADX > self$trend_strength ~ -1,
                    DIp < lag(DIn) & lag(ADX) > self$trend_strength ~ -1,
                    lag(DIp) < lag(DIn) & lag(ADX) > self$trend_strength ~ -1,
                    TRUE ~ 0
                  ),
                  signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
                  position = lag(signal, default = 0)
                  ) %>%
                  na.omit()
                    
}

  )
)

# Define Bollinger Bands Breakout class
BollingerBreakout <- R6Class(
  "BollingerBreakout",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    sd_mult = NULL,  # Multiplier for standard deviation

initialize = function(data, window_size, sd_mult = 2) {
    super$initialize(data)
    self$data <- super$convert_to_tibble(self$data)
    self$window_size <- window_size
    self$sd_mult <- sd_mult
},

generate_signals = function() {
    self$data <- mutate(self$data, 
                        ma = rollmean(Close, k = self$window_size, align = "right", fill = NA),
                        sd = rollapply(Close, width = self$window_size, sd, align = "right", fill = NA),
                        upper_band = ma + self$sd_mult * sd,
                        lower_band = ma - self$sd_mult * sd,
                        signal = case_when(
                          Close > lag(upper_band) ~ 1,
                          Close < lag(lower_band) ~ -1,
                          TRUE ~ 0
                        ),
                        position = lag(signal, default = 0)) %>% 
                          na.omit
}

  )
)

# Define Volatility Mean Reversion class
VolatilityMeanReversion <- R6Class(
  "VolatilityMeanReversion",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = NULL,

initialize = function(data, window_size, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$ma_type <- ma_type
    },

generate_signals = function() {
      # Estimate historical volatility
      ma_func <- get(self$ma_type)

      # Generate signals
      self$data <- mutate(self$data,
                          hist_vol = rollapply(self$data$Close, width = self$window_size, sd, align = "right", fill = NA),
                          mean_vol = ma_func(hist_vol, self$window_size, align = "right", fill = NA),
                          signal = ifelse(hist_vol > lag(mean_vol), -1, 1),
                          position = lag(signal, default = 0)) %>%
                            na.omit
}

  )
)

# Define ARIMA class
ARIMA <- R6Class(
  "ARIMAbased",
  inherit = Strategy,
  public = list(
    window_size = NULL, # for "moving" window_type it is window_size, for "expanding" window_type it is starting window_size only (as then it expands given iterations)
    window_type = NULL,
    best_arima = NULL,
    p1 = NULL,
    d1 = NULL,
    q1 = NULL,
    
initialize = function(data, window_size, window_type, best_arima = TRUE, p1 = NULL, d1 = NULL, q1 = NULL) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$window_type <- window_type
      self$best_arima <- best_arima
      self$p1 <- p1
      self$d1 <- d1
      self$q1 <- q1
},
    
generate_signals = function() {
  # Register parallel backend
  # num_cores <- parallel::detectCores() - 1 
  # registerDoParallel(cores = num_cores)

  n <- nrow(self$data)
  # Preallocate vectors
  forecast_values <- numeric(n - self$window_size + 1)
  actual_values <- numeric(n - self$window_size + 1)
  dates <- as.Date(character(n - self$window_size + 1))
  
  # Rolling window type
  switch(
    self$window_type,
    "moving" = {
      # Perform rolling forecasts
      for (i in 1:(n - self$window_size + 1)) {
      # foreach(i = 1:(n - self$window_size + 1)) %dopar% {

        # Extract current window of data
        window_data <- self$data$Close[i:(i + self$window_size - 1)]
        
        tryCatch({
          if (self$best_arima) {    
            # Fit ARIMA model
            best_arima <- auto.arima(window_data, stepwise = TRUE, approximation = TRUE)
            p <- best_arima$arma[1]
            d <- best_arima$arma[2]
            q <- best_arima$arma[3]
            P <- best_arima$arma[4]
            D <- best_arima$arma[5]
            Q <- best_arima$arma[6]
            
            fit <- arima(window_data, order = c(p, d, q), seasonal = list(order = c(P, D, Q)), method = "CSS-ML")
          } else {
            # Use provided p1, d1, q1 parameters
            fit <- arima(window_data, order = c(self$p1, self$d1, self$q1), method = "CSS-ML")
          }
          
          # Forecast (1 day ahead)
          forecast_result <- forecast(fit, lead = 1, output = FALSE) %>%
            data.frame %>% 
            select(Forecast) %>% as.numeric
          
          # Store dates, forecast, and actual value
          forecast_values[i] <- forecast_result
          actual_values[i] <- self$data$Close[i + self$window_size - 1]
          dates[i] <- self$data$Date[i + self$window_size - 1]
        }, error = function(e) {
          # Handle errors
          print(paste("Error in iteration", i, ":", e$message))
        })
      }
    },

    "expanding" = {
      # Perform expanding window forecasts
      for (i in self$window_size:n) {
      # foreach(i = self$window_size:n) %dopar% {

        # Extract current window of data
        window_data <- self$data$Close[1:i]
        
        tryCatch({
          if (self$best_arima) {    
            # Fit ARIMA model
            best_arima <- auto.arima(window_data, stepwise = TRUE, approximation = TRUE)
            p <- best_arima$arma[1]
            d <- best_arima$arma[2]
            q <- best_arima$arma[3]
            P <- best_arima$arma[4]
            D <- best_arima$arma[5]
            Q <- best_arima$arma[6]
            
            fit <- arima(window_data, order = c(p, d, q), seasonal = list(order = c(P, D, Q)), method = "CSS-ML")
          } else {
            # Use provided p1, d1, q1 parameters
            fit <- arima(window_data, order = c(self$p1, self$d1, self$q1), method = "CSS-ML")
          }
          
          # Forecast (1 day ahead)
          forecast_result <- forecast(fit, lead = 1, output = FALSE) %>%
            data.frame %>% 
            select(Forecast) %>% as.numeric
          
          # Store dates, forecast, and actual value
          forecast_values[i] <- forecast_result
          actual_values[i] <- self$data$Close[i]
          dates[i] <- self$data$Date[i]
        }, error = function(e) {
          # Handle errors
          print(paste("Error in iteration", i, ":", e$message))
        })
      }
    }
  )
  
  res <- data.frame(
    Date = dates,
    Forecast = forecast_values, 
    Actual = actual_values
  )
  
  # Compare Forecast with Actual value
  self$data <- self$data %>%
    left_join(res %>% select(Date, Forecast), by = "Date") %>%
    mutate(
      signal1 = case_when(
        Forecast > Close ~ 1,
        Forecast < Close ~ -1,
        TRUE ~ 0 
      )
      # position = lag(signal, default = 0)
    ) %>%
      na.omit()

  # Initialize last_long_value and last_short_value (dynamic trailing threshold)
  last_long_value <- rep(NA, nrow(self$data))
  last_short_value <- rep(NA, nrow(self$data))

  # Loop through each row of the data frame
  for (i in 1:nrow(self$data)) {
      # Check if current row has signal == 1
      if (self$data$signal1[i] == 1) {
          # Find the index of the first previous occurrence of 1
          first_previous_index <- NA
          if (any(self$data$signal1[1:(i-1)] == 1)) {
              first_previous_index <- max(which(self$data$signal1[1:(i-1)] == 1))
          }
          # If a previous occurrence of 1 is found, update last_long_value
          if (!is.na(first_previous_index)) {
              last_long_value[i] <- self$data$Forecast[first_previous_index]
          }
      }
      # Check if current row has signal1 == -1
      if (self$data$signal1[i] == -1) {
          # Find the index of the first previous occurrence of -1
          first_previous_index <- NA
          if (any(self$data$signal1[1:(i-1)] == -1)) {
              first_previous_index <- max(which(self$data$signal1[1:(i-1)] == -1))
          }
          # If a previous occurrence of -1 is found, update last_short_value
          if (!is.na(first_previous_index)) {
              last_short_value[i] <- self$data$Forecast[first_previous_index]
          }
      }
  }

  # Assign last_long_value and last_short_value to the data frame
  self$data$last_long_value <- last_long_value
  self$data$last_short_value <- last_short_value

  # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
  self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
  self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)

  # Compare data$Forecast[i] with the first previous value and update data$s2
  self$data$signal <- ifelse((self$data$Forecast > self$data$last_long_value) & (self$data$Forecast > self$data$Close), 1,
                      ifelse((self$data$Forecast < self$data$last_short_value) & (self$data$Forecast < self$data$Close), -1, 0))

  # Replacing 0s by previous signal value:
  self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
  self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)

  self$data$position <- lag(self$data$signal, default = 0)
}

  )
)

# Define class for Strategy based on GARCH model
GARCH <- R6Class(
  "GARCHbasedStrategy",
    inherit = Strategy,
    public = list(
    specification = NULL, 
    n_start = NULL, 
    refit_every = NULL, 
    refit_window = NULL, 
    distribution_model = NULL, 
    realized_vol = NULL, 
    cluster = NULL,

initialize = function(
    data,
    specification, 
    n_start, 
    refit_every, 
    refit_window, 
    distribution_model, 
    realized_vol, 
    cluster) {
      super$initialize(data)
      self$specification <- specification
      self$n_start <- n_start
      self$refit_every <- refit_every
      self$refit_window <- refit_window
      self$distribution_model <- distribution_model
      self$realized_vol <- realized_vol
      self$cluster <- cluster
},

# Method to estimate realized volatility by different approaches
estimate_realized_volatility = function(data) {
    ohlc <- data %>% 
        data.frame %>%
            rename_with(~ sub(".*\\.", "", .), everything()) %>%
                select(-Volume, -Adjusted) %>%
                    na.omit  %>%
                        as.matrix

    # Different realized volatility estimators for returns (TTR)
    histVolest <- merge(
    garman <- as.xts(na.omit(TTR::volatility(ohlc, calc = "garman"))) / sqrt(252),
    close <- as.xts(na.omit(TTR::volatility(ohlc[,4], calc = "close"))) / sqrt(252),
    parkinson <- as.xts(na.omit(TTR::volatility(ohlc, calc = "parkinson"))) / sqrt(252),
    rogers.satchell <- as.xts(na.omit(TTR::volatility(ohlc, calc = "rogers.satchell"))) / sqrt(252),
    garman_modified <- as.xts(na.omit(TTR::volatility(ohlc, calc = "gk.yz"))) / sqrt(252),
    yang.zhang <- as.xts(na.omit(TTR::volatility(ohlc, calc = "yang.zhang"))) / sqrt(252)
    ) %>% 
    as.data.frame %>% 
        rename_with(~ c("garman", "close", "parkinson", "rogers_satchell", "garman_modified", "yang_zhang")) %>%
        mutate(TradeDate = as.Date(rownames(.))) %>%
            select(TradeDate, everything(.)) %>%
            na.omit

    return(histVolest)
},

# Method to specify signal criteria (based on GARCH model volatility forecasts)
signal_criteria = function(volData) {
    modified_volData <- volData %>%
    mutate(
        q75 = rollapply(Forecast, width = self$n_start, FUN = function(x) quantile(x, probs = 0.75), align = "right", fill = NA),
        signal = case_when(
        Forecast < q75 ~ 1,
        Forecast > q75 ~ -1,
        TRUE ~ 0    
        ),
        position = lag(signal)
    ) %>% na.omit  # Remove the first row since it will have NA for signal
    return(modified_volData)
},

# Method to generate column of signals and positions
generate_signals = function() {
    histVolest <- self$estimate_realized_volatility(self$data)
    instr <- self$data %>% 
        as.data.frame() %>%
            rename_with(~ sub(".*\\.", "", .), everything()) %>%
                mutate(TradeDate = as.Date(rownames(.))) %>%
                    select(TradeDate, Open, High, Low, Close) %>%
                        mutate(value = as.numeric(log(Close/lag(Close)))) %>%
                            # EquityLine = cumprod(ifelse(is.na(rets), 1, 1 + rets))) %>%
                                na.omit

    listgarch <- expand.grid(
    specification = self$specification,
    window.size = self$n_start,
    refit.frequency = self$refit_every,
    refit.window.type = self$refit_window,
    distribution.model = self$distribution_model,
    realized.vol.method = self$realized_vol,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
    )
    colnames(listgarch)[1:6] <- c("specification", "window.size", "refit.frequency", "refit.window.type", "distribution.model", "realized.vol.method")
      
    if(listgarch$specification == "fGARCH") {
        spec <- ugarchspec(
        variance.model = list(
        model = listgarch$specification,
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
        distribution.model = listgarch$distribution.model) 
    # , start.pars = list(), fixed.pars = list(), ...)
  } else {
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
        distribution.model = listgarch$distribution.model) 
  }

    if(listgarch$refit.window.type == "moving") {
        roll = ugarchroll(
        spec, 
        #instr_ret[,1], 
        self$data[,"value"],
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch$window.size,  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch$refit.frequency, # determines every how many periods the model is re-estimated.
        refit.window = listgarch$refit.window.type, # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead).
        window.size = listgarch$window.size,
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = self$cluster,
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
    } else {
    roll = ugarchroll(
        spec, 
        #instr_ret[,1], 
        self$data[,"value"],
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch$window.size,  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch$refit.frequency, # determines every how many periods the model is re-estimated.
        refit.window = listgarch$refit.window.type, # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead)
        # window.size = listgarch[i,2],
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = self$cluster,
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
  }
    # roll <- resume(roll, solver= "gosolnp") # if object contains non-converged windows
    # show(roll) # 20.02 secs

    forecastVolRoll <- data.frame(
        #Date = roll@model$index[(listgarch$window.size[i]+1):length(roll@model$index)],
        Date = roll@model$index[(listgarch$window.size+1):length(roll@model$index)],
        Forecast = roll@forecast$density$Sigma
            )

    # Join realized volatility estimation and instr log returns given TradeDate
    volForHistRoll <- forecastVolRoll %>%
        mutate(TradeDate = Date) %>% 
            left_join(histVolest, by = 'TradeDate') %>%
            na.omit %>% select(-Date) %>%
                select(TradeDate, everything()) %>%
                left_join(select(instr, TradeDate, Close, value), by = "TradeDate")
    
    volForHistRoll <- self$signal_criteria(volForHistRoll) %>%
        mutate(Date = TradeDate) %>%
            as.tibble()
    
    #self$data <- copy(volForHistRoll)

    self$data <- as.data.frame(self$data)
    self$data <- self$data %>% rename_with(~ sub(".*\\.", "", .), everything()) %>%
                mutate(Date = as.Date(rownames(.))) %>%
                    select(Date, value) %>%
                        left_join(select(volForHistRoll, Date, Close, signal, position)) %>%
                            na.omit %>%
                                as.tibble
}

  )
)