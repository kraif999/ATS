# Copyright (c) 2024 Oleh Bilyk

source("libraries.R")

options(scipen = 999)

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
                          #signal = ifelse(Close > lag(ma), 1, ifelse(Close < lag(ma), -1, 0)),
                          signal = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
}

  )
)

ui <- fluidPage(

    fluidRow(
    column(
      12, 
      tags$div(
        style = "margin-bottom: 10px; font-size: 16px; font-weight: bold;",
        tags$a(
          href = "https://github.com/kraif999/ATS",
          "GitHub repository: https://github.com/kraif999/ATS",
          target = "_blank" # Opens the link in a new tab
        )
      ),
    )
  ),
  
  titlePanel("Backtesting Trading Strategies"),
  sidebarLayout(
    sidebarPanel(
      # User input controls
      textInput("symbol", "Symbol", value = "BTC-USD"),
      dateRangeInput("date_range", "Date Range", start = as.Date("2018-01-01"), end = Sys.Date()),
      numericInput("capital", "Capital", value = 1000),
      numericInput("leverage", "Leverage", value = 1),
      selectInput("data_type", "Data Type", choices = c("in_sample", "out_of_sample")),
      dateInput("cut_date", "Cut-off Date", value = as.Date("2024-01-01")),
      numericInput("window_size", "Window Size", value = 20),
      selectInput("ma_type", "MA Type", choices = c("EMA", "SMA", "HMA", "WMA")),
      checkboxInput("apply_stop_loss", "Apply Stop Loss?", value = FALSE),
      numericInput("stop_loss_threshold", "Stop Loss Threshold", value = 0.015),
      numericInput("reward_ratio", "Reward Ratio", value = 25),
      checkboxInput("signal_flag", "Show Signal Lines?", value = FALSE),
      checkboxInput("split_data", "Split Data for Backtest?", value = FALSE),
      numericInput("window", "Slice Data Into Windows (in years)", value = 1),
      actionButton("backtest_run", "Run Backtest") # Backtest button
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trading Profile", DTOutput("trading_profile")),
        tabPanel("Performance Plot", plotOutput("performance_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to fetch price data
  price_data <- reactive({
    req(input$symbol, input$date_range)  # Ensure inputs are available
    
    symbol <- input$symbol  # Use symbol from input
    print(paste("Fetching data for symbol:", symbol))  # Debug print
    
    # Fetch price data
    fetcher <- DataFetcher$new(symbol, input$date_range[1], input$date_range[2])
    ts <- fetcher$download_xts_data()
    return(ts)
  })
  
  # Reactive expression for strategy instance and plot
  strategy_reactive <- eventReactive(input$backtest_run, {
    req(price_data(), input$window_size, input$ma_type, input$cut_date)
    
    symbol <- input$symbol
    print(paste("Using symbol in strategy:", symbol))  # Debug print
    
    # Create SMA1 strategy instance
    sma_strategy <- SMA1$new(
      data = price_data(),
      window_size = input$window_size,
      ma_type = input$ma_type
    )
    
    # Estimate performance
    performance_result <- sma_strategy$estimate_performance(
      data_type = input$data_type,
      split = input$split_data,
      cut_date = input$cut_date,
      window = input$window,
      apply_stop_loss = input$apply_stop_loss,
      stop_loss_threshold = input$stop_loss_threshold,
      reward_ratio = input$reward_ratio,
      capital = input$capital,
      leverage = input$leverage,
      symbol = input$symbol
    )
    
    # Debug print to check the structure of the result
    print(performance_result)  # Ensure it's a matrix or data frame
    
    # If performance_result is a data frame, transpose it
    trading_profile <- if (is.data.frame(performance_result) || is.matrix(performance_result)) {
      t(performance_result)
    } else {
      performance_result  # Handle if it's already a data frame or list
    }
    
    # Convert to data.table for further processing
    trading_profile <- cbind(Metric = rownames(trading_profile), as.data.table(as.data.frame(trading_profile, stringsAsFactors = FALSE)))

    trading_profile[, units := ifelse(
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

    # Generate and return the plot
    p <- sma_strategy$plot_equity_lines(
      strategy_name = "SMA1",
      signal_flag = input$signal_flag,
      symbol = input$symbol,
      capital = input$capital
    )
    
    return(list(strategy = sma_strategy, plot = p, profile = trading_profile))
  })
  
  # Reactive expression to generate performance metrics
  performance_metrics <- reactive({
    req(strategy_reactive())
    performance_data <- strategy_reactive()$profile  # Correctly accessing the profile from strategy_reactive
    return(performance_data)
  })
  
  # Render trading profile data table
  output$trading_profile <- renderDT({
    req(performance_metrics())
    performance <- performance_metrics()
    
    datatable(
      as.data.frame(performance), 
      options = list(pageLength = 100)  # Set the number of rows per page to 100
    )
  })

  # Render performance plot
  output$performance_plot <- renderPlot({
    req(strategy_reactive())
    strategy_plot <- strategy_reactive()$plot  # Correctly accessing the plot from strategy_reactive
    print(strategy_plot)  # Render the plot
  })
}

# Run the app
shinyApp(ui = ui, server = server)
