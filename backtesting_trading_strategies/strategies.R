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

# Define parent class
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
},

# Macrospopic level (overall performance) - understand the trading profile of a Strategy (inlcuding 0.1% transaction fee)
estimate_performance = function(data_type, split_data, cut_date, window, apply_rm, max_risk, reward_ratio, capital, leverage, symbol) {
  
  # Slice self$data using the private slicer method
  self$data <- private$slicer(self$data, cut_date, data_type)

  # Generate signals
  self$generate_signals()

  if(apply_rm) {
    self$data <- private$apply_risk_management(self$data, max_risk, reward_ratio, leverage, capital)
  } else {

    # Initialize columns
    self$data <- self$data %>% 
    mutate(
        nopActive = 0,
        nopPassive = 0,  # Initial number of passive positions (constant)
        #nopPassive <- capital * leverage / self$data$Close[1],
        pnlActive = 0,
        pnlPassive = c(0, diff(Close)),  # Difference in close price to calculate passive pnl
        pnlActiveCumulative = 0,
        pnlPassiveCumulative = 0,
        eqlActive = capital,
        eqlPassive = capital,
        From = as.Date(NA),
        To = as.Date(NA),
        pnlActiveType = NA
    )

    self$data$position[1] <- 0

    eqlActive <- eqlActive2 <- capital
    eqlPassive <- eqlPassive2 <- capital

    # Iterate over each row in self$data
    for (i in 2:nrow(self$data)) {
    
    prev_nop_Active <- self$data$nopActive[i - 1]
    
    # Handle position change (for active portfolio)
    if (self$data$position[i] != self$data$position[i - 1]) {
        self$data$nopActive[i] <- eqlActive * leverage / self$data$Close[i]
    } else {
        # Keep previous nopActive if position hasn't changed
        self$data$nopActive[i] <- self$data$nopActive[i - 1]
    }
    
    # Compute pnlActive (profit/loss for active positions)
    #self$data$pnlActive[i] <- (self$data$Close[i] - self$data$Close[i - 1]) * self$data$position[i-1] * self$data$nopActive[i]
    self$data$pnlActive[i] <- if (self$data$position[i] == 0) 0 else (self$data$Close[i] - self$data$Close[i - 1]) * self$data$position[i - 1] * self$data$nopActive[i - 1]


    # Update active equity
    eqlActive <- eqlActive + self$data$pnlActive[i]
    eqlActive2 <- if (eqlActive < 0) 0 else eqlActive
    self$data$eqlActive[i] <- eqlActive2

    if (self$data$position[i] == 0) {
      self$data$pnlActiveType[i] <- NA  # No position, so pnlActiveType is NA
    } else if ((self$data$position[i - 1] == 1 && self$data$position[i] == -1) || (self$data$position[i - 1] == -1 && self$data$position[i] == 1)) {
      self$data$pnlActiveType[i] <- "R"  # Realized PnL
    } else {
      self$data$pnlActiveType[i] <- "U"  # Unrealized PnL
    }
    
    # Passive strategy
    self$data$nopPassive[i] <- eqlPassive * leverage / self$data$Close[i]
    self$data$pnlPassive[i] <- (self$data$Close[i] - self$data$Close[i - 1]) * self$data$nopPassive[i - 1]
    eqlPassive <- eqlPassive + self$data$pnlPassive[i]
    eqlPassive2 <- if (eqlPassive < 0) 0 else eqlPassive
    self$data$eqlPassive[i] <- eqlPassive2

    }

  }

  # Add additional metrics
  self$data <- self$data %>%
    mutate(
      annual_vol = rollapply(value, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(365),
      pnlActiveCumulative = cumsum(replace_na(pnlActive, 0)),
      pnlPassiveCumulative = cumsum(replace_na(pnlPassive, 0)),
      r_eqlActive = (eqlActive - lag(eqlActive)) / lag(eqlActive),
      r_eqlPassive = (eqlPassive - lag(eqlPassive)) / lag(eqlPassive)
    )

  if (split_data) {
    start_date <- min(self$data$Date)
    end_date <- max(self$data$Date)
    period_start <- start_date
    period_end <- period_start %m+% months(window * 12) - days(1)

    performance_list <- list()

    while (period_start <= end_date) {
      current_end <- min(period_end, end_date)
      data_period <- self$data %>% filter(Date >= period_start & Date <= current_end)
      self$data <- self$data %>%
        mutate(
          From = as.Date(ifelse(Date >= period_start & Date <= current_end, period_start, From)),
          To = as.Date(ifelse(Date >= period_start & Date <= current_end, current_end, To))
        )
      if (nrow(data_period) > 0) {
        metrics <- private$compute_metrics(data_period, symbol)
        metrics$from <- period_start
        metrics$to <- current_end
        metrics$data_type <- data_type
        metrics$leverage <- leverage
        metrics$max_risk <- max_risk
        metrics$reward_ratio <- reward_ratio
        metrics$capital <- capital
        performance_list[[length(performance_list) + 1]] <- metrics
      }
      period_start <- period_start %m+% months(window * 12)
      period_end <- period_start %m+% months(window * 12) - days(1)
    }

    performance_df <- bind_rows(performance_list) %>%
      select(ticker, from, to, data_type, leverage, max_risk, reward_ratio, capital, Strategy, everything())
    return(performance_df)

  } else {
    metrics <- private$compute_metrics(self$data, symbol)
    metrics$from <- min(self$data$Date)
    metrics$to <- max(self$data$Date)
    metrics$data_type <- data_type
    metrics$leverage <- leverage
    metrics$max_risk <- max_risk
    metrics$reward_ratio <- reward_ratio
    metrics$capital <- capital
    performance_df <- as.data.frame(metrics) %>%
      select(ticker, from, to, data_type, leverage, max_risk, reward_ratio, capital, Strategy, everything())
    return(performance_df)
  }
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
      Trade = first(trade_direction), # Trade type (Buy/Sell)
      EntryDate = as.Date(first(entry)), # Entry date
      Size = round(first(entry_size), 5), # Correct entry size
      EntryPrice = round(first(entry_price), 2), # Price at entry
      ExitDate = as.Date(first(exit)), # Exit date
      ExitPrice = round(first(exit_price), 2), # Price at exit
      Trade_PnL = round(ifelse(Trade == "Buy", ExitPrice - EntryPrice, EntryPrice - ExitPrice) * Size, 0) # Trade profit/loss with size
    ) %>%
    ungroup() %>% # Remove grouping for calculating Running_PnL
    mutate(
      Running_PnL = round(cumsum(Trade_PnL), 2), # Running cumulative PnL across all trades
      Efficiency = round((Trade_PnL / abs(Running_PnL)) * 100, 2) # Efficiency as % of Running_PnL
    ) %>% select(
      Trade, EntryDate, ExitDate, Size, EntryPrice, ExitPrice, Trade_PnL, Running_PnL, Efficiency
    )
  
  # Generate the plot
  pnl_hist <- ggplot(data = data.frame(Trade_PnL = trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0]), 
                    aes(x = Trade_PnL, fill = Trade_PnL < 0)) +
    geom_histogram(binwidth = diff(range(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])) / 100, 
                  color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("green", "red")) +  # Green for positive, red for negative
    labs(title = "Trade Profit and Loss (PnL) Distribution", 
        x = "Trade PnL", 
        y = "Frequency") +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(min(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0]), 
                max(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])),
      breaks = unique(c(seq(floor(min(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])), 
                            ceiling(max(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])), 
                            by = 200), 0))
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal()
  
  # Return both the trades and the plot as a list
  return(list(
    trades = trades,
    plot = pnl_hist
  ))
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
      y = "Equity line",
      color = "Strategy",  # Change label to 'Strategy' for the equity lines
      linetype = "Position"  # Change label to 'Position' for dashed lines
    ) +
    theme_minimal()
  
  # Add vertical dashed lines for positions (short and long)
  if (signal_flag) {
    p <- p +
      geom_vline(data = self$data[self$data$position == -1, ], 
                 aes(xintercept = as.numeric(Date), linetype = "Short Position"), 
                 color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$position == 1, ], 
                 aes(xintercept = as.numeric(Date), linetype = "Long Position"), 
                 color = "green", alpha = 0.5)
  }
  
  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_linetype_manual(values = c("Short Position" = "dashed", "Long Position" = "dashed"))  # Define line types
  
  # Add vertical lines for the From and To columns
  period_lines <- data.frame(From = unique(self$data$From), To = unique(self$data$To))
  p <- p + 
    geom_vline(data = period_lines, aes(xintercept = as.numeric(From)), 
               linetype = "solid", color = "black", alpha = 1, size = 1) +
    geom_vline(data = period_lines, aes(xintercept = as.numeric(To)), 
               linetype = "solid", color = "black", alpha = 1, size = 1)

  # Return the plot
  return(p)
},

# Estimate Average True Range (ATR)
estimate_range_potential = function(n) {
  self$data <- self$data %>% data.table
  
  # Calculate TR1, TR2, TR3
  self$data[, `:=`(
    TR1 = High - Low,
    TR2 = abs(High - shift(Close, type = "lag")),
    TR3 = abs(Low - shift(Close, type = "lag"))
  )]
  
  # Calculate TR as the maximum of TR1, TR2, TR3
  self$data[, TR := pmax(TR1, TR2, TR3, na.rm = TRUE)]
  
  # Calculate ATR as a rolling average of TR over n periods
  self$data[, ATR := frollmean(TR, n, fill = NA)]
  
  # Calculate N as TR / ATR
  self$data[, N := TR / ATR]
  
  # Return updated self$data
  return(self$data)
},

# Plot Close price and volatility (range potential)
plot_close_vs_vol = function(ndays) {

  # Filter self$data for the last ndays
  filtered_data <- tail(self$data, ndays)
  
  # First plot: Line plot of Close price
  close <- ggplot(filtered_data, aes(x = Date, y = Close)) +
    geom_line(color = "black") +
    labs(title = paste0("Close Price (Last ", ndays, " days) for ", symbol), x = "Date", y = "Close") +
    theme_minimal()
  
  # Second plot: Bar plot of N with horizontal dashed lines at 0.5 and 1
  n <- ggplot(filtered_data, aes(x = Date, y = N)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = "N = TR / ATR with thresholds", x = "Date", y = "N") +
    theme_minimal()
  
  # Return the individual plots
  return(list(
    close = close,
    n = n
  ))

}

  ),
    
private = list(

# Apply stop loss and profit take
apply_risk_management = function(data, max_risk, reward_ratio, leverage, capital) {
  
  data$position[1] <- 0
  eqlActive <- eqlActive2 <- capital
  eqlPassive <- eqlPassive2 <- capital
  previous_position <- 0
  stopLoss <- profitTake <- NA
  flat <- FALSE
  reversed_position <- NA
  
  # Initialize columns
  data <- data %>%
    mutate(
      position1 = position,
      group = cumsum(signal != shift(signal, type = "lag", fill = 0)),
      stopLoss = NA,
      profitTake = NA,
      eventSL = NA,
      eventPT = NA,
      nopActive = 0,
      nopPassive = capital / Close[1] * leverage,  # Initial passive number of positions
      pnlActive = 0,
      pnlPassive = 0,
      eqlActive = capital,
      eqlPassive = capital,
      From = as.Date(NA),
      To = as.Date(NA),
      pnlActiveType = NA
    )
  
  # Iterate over each row in the data
  for (i in 2:nrow(data)) {
    
    if (flat) data$position[i] <- 0  # Stay flat after reversal
    
    if (!is.na(reversed_position)) {
      data$position[i] <- reversed_position
      reversed_position <- NA
    }
    
    if (data$position[i] != previous_position) {
      data$nopActive[i] <- eqlActive * leverage / data$Close[i]
      
      if (data$position[i] == 1) {
        stopLoss <- data$Close[i] - (max_risk * eqlActive / data$nopActive[i])
        profitTake <- max(0, data$Close[i] + (reward_ratio * max_risk * eqlActive / data$nopActive[i]))
      } else if (data$position[i] == -1) {
        stopLoss <- data$Close[i] + (max_risk * eqlActive / data$nopActive[i])
        profitTake <- max(0, data$Close[i] - (reward_ratio * max_risk * eqlActive / data$nopActive[i]))
      } else {
        stopLoss <- profitTake <- NA
      }
      previous_position <- data$position[i]
    } else {
      data$nopActive[i] <- data$nopActive[i - 1]
    }
    
    data$stopLoss[i] <- stopLoss
    data$profitTake[i] <- profitTake
    
    if (data$position[i] == 1) {
      data$eventSL[i] <- if (!is.na(stopLoss) && data$Close[i] <= stopLoss) TRUE else NA
      data$eventPT[i] <- if (!is.na(profitTake) && data$Close[i] >= profitTake) TRUE else NA
    } else if (data$position[i] == -1) {
      data$eventSL[i] <- if (!is.na(stopLoss) && data$Close[i] >= stopLoss) TRUE else NA
      data$eventPT[i] <- if (!is.na(profitTake) && data$Close[i] <= profitTake) TRUE else NA
    } else {
      data$eventSL[i] <- data$eventPT[i] <- NA
    }
    
    if (!flat) {
      if (!is.na(data$eventSL[i]) || !is.na(data$eventPT[i])) {
        flat <- TRUE
        reversed_position <- -data$position[i]
      } else {
        data$position[i] <- data$signal[i - 1]
      }
    }
    
    if (i > 2 && data$group[i] != data$group[i - 1]) {
      flat <- FALSE
    }
    
    #data$pnlActive[i] <- (data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1]
    data$pnlActive[i] <- if (data$position[i] == 0) 0 else (data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1]
    eqlActive <- eqlActive + data$pnlActive[i]
    eqlActive2 <- if (eqlActive < 0) 0 else eqlActive
    data$eqlActive[i] <- eqlActive2
    
    if (data$position[i] == 0) {
      data$pnlActiveType[i] <- NA  # No position, so pnlActiveType is NA
    } else if ((data$position[i - 1] == 1 && data$position[i] == -1) || (data$position[i - 1] == -1 && data$position[i] == 1)) {
      data$pnlActiveType[i] <- "R"  # Realized PnL
    } else {
      data$pnlActiveType[i] <- "U"  # Unrealized PnL
    }
    
    # Passive strategy
    data$nopPassive[i] <- eqlPassive * leverage / data$Close[i]
    data$pnlPassive[i] <- (data$Close[i] - data$Close[i - 1]) * data$nopPassive[i - 1]
    eqlPassive <- eqlPassive + data$pnlPassive[i]
    eqlPassive2 <- if (eqlPassive < 0) 0 else eqlPassive
    data$eqlPassive[i] <- eqlPassive2
    
  }
  
  data <- data %>%
    mutate(
      pnlActiveCumulative = cumsum(replace_na(pnlActive, 0)),
      pnlPassiveCumulative = cumsum(replace_na(pnlPassive, 0))
    )
  
  return(data)
},

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
      {if (nrow(.) == 0) return(NA) else .} %>%
      merge(data_subset, by = "trade_id") %>%
      aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
      with(round(mean(Date, na.rm = TRUE)))

      # 12-15: Winning Runs
      is_winning <- data_subset[[pnl_col]] > 0
      winning_runs <- rle(is_winning)$lengths[rle(is_winning)$values]
      winning_runs <- winning_runs[!is.na(winning_runs)]

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
      losing_runs <- losing_runs[!is.na(losing_runs)]

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

# Cut the Strategy time horizon, used for the data split
slicer = function(data, cut_date, data_type) {
  if (inherits(data, c("xts", "zoo"))) {
    # For xts/zoo, filter by the date index
    data <- switch(data_type,
                   "in_sample" = data[index(data) <= as.Date(cut_date), ],
                   "out_of_sample" = data[index(data) > as.Date(cut_date), ],
                   stop("Invalid data_type. Use 'in_sample' or 'out_of_sample'.")
    )
  } else {
    # For non-xts data (data.frame/tibble), filter by 'Date' column
    data <- switch(data_type,
                   "in_sample" = data %>% filter(Date <= as.Date(cut_date)),
                   "out_of_sample" = data %>% filter(Date > as.Date(cut_date)),
                   stop("Invalid data_type. Use 'in_sample' or 'out_of_sample'.")
    )
  }
  
  return(data)
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
},

run_backtest = function(symbols, window_sizes, ma_types, data_type, split, cut_date, from_date, to_date, slicing_years, apply_rm, max_risks, reward_ratios, leverages, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (ma_type in ma_types) {
        for(max_risk in max_risks) {
          for(reward_ratio in reward_ratios) {
            for (leverage in leverages) {

        # Fetch data using DataFetcher for the current symbol and date range
        data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
        data <- data_fetcher$download_xts_data()
        
        # Ensure data is not empty
        if (nrow(data) == 0) {
          warning(paste("No data available for symbol:", symbol))
          next
        }

        # Create an instance of SMA1 strategy
        sma_instance <- SMA1$new(data, window_size = window_size, ma_type = ma_type)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sma_instance$estimate_performance(
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      } else {
        performance <- sma_instance$estimate_performance(
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size:", window_size, 
                        "ma_type:", ma_type))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, ma_type, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("SMA1:", window_size, ma_type),
          Window_Size = window_size,
          MA_Type = ma_type,
          Max_Risk = max_risk,
          Reward_Ratio = reward_ratio,
          #Leverage = leverage,
          Performance = performance
        )

        print(paste0(
          "SMA1 strategy (symbol: ", symbol, 
          ", class: ", meta$assets[[symbol]]$class, 
          ", window_size: ", window_size, 
          ", ma_type: ", ma_type, 
          ", split: ", split, 
          ", max_risk: ", max_risk, 
          ", reward_ratio: ", reward_ratio, 
          ", leverage: ", leverage,
          ")"
          )
        )
            }
          }
        }
      }
    }
  }

  # Check if results list is empty
  if (length(results) == 0) {
    stop("No valid results were generated. Check the input parameters or data availability.")
  }

  # Create the final data frame if output_df is TRUE
  if (output_df) {
    res_df <- do.call(rbind, lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      if ("from" %in% names(performance_data) && "to" %in% names(performance_data)) {
        performance_data$Period <- paste(performance_data$from, "to", performance_data$to)
      } else {
        performance_data$Period <- "Full Period"
      }

      # Remove 'from', 'to', and 'ticker' columns
      performance_data <- performance_data[, !names(performance_data) %in% c("from", "to", "ticker")]

      # Add metadata columns
      cbind(
        Symbol = x$Symbol,
        Class = x$Class,
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio,
        #Leverage = x$Leverage,
        performance_data
      )
    }))

    # Reset row names
    rownames(res_df) <- 1:nrow(res_df)

    return(res_df)
  } else {
    return(results)
  }
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

},

run_backtest = function(symbols, window_sizes1, window_sizes2, ma_types, data_type, split, cut_date, from_date, to_date, slicing_years, apply_rm, max_risks, reward_ratios, leverages, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
        for (ma_type in ma_types) {
          for(max_risk in max_risks) {
            for(reward_ratio in reward_ratios) {
              for (leverage in leverages) {

        # Fetch data using DataFetcher for the current symbol and date range
        data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
        data <- data_fetcher$download_xts_data()
        
        # Ensure data is not empty
        if (nrow(data) == 0) {
          warning(paste("No data available for symbol:", symbol))
          next
        }

        # Create an instance of SMA1 strategy
        sma2_instance <- SMA2$new(data, window_size1 = window_size1, window_size2 = window_size2, ma_type = ma_type)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sma2_instance$estimate_performance(
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      } else {
        performance <- sma2_instance$estimate_performance(
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size1:", window_size1,
                        "window_size2:", window_size2,
                        "ma_type:", ma_type))
          next
        }

        # Store the results
        results[[paste(symbol, window_size1, window_size2, ma_type, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("SMA2:", window_size1, window_size2, ma_type),
          Window_Size1 = window_size1,
          Window_Size2 = window_size2,
          MA_Type = ma_type,
          Max_Risk = max_risk,
          Reward_Ratio = reward_ratio,
          Performance = performance
        )

        print(paste0(
          "SMA2 strategy (symbol: ", symbol, 
          ", class: ", meta$assets[[symbol]]$class, 
          ", window_size1: ", window_size1,
          ", window_size2: ", window_size2, 
          ", ma_type: ", ma_type, 
          ", split: ", split, 
          ", max_risk: ", max_risk, 
          ", reward_ratio: ", reward_ratio, 
          ", leverage: ", leverage,
          ")"
          )
        )
              }
            }
          }
        }
      }
    }
  }

  # Check if results list is empty
  if (length(results) == 0) {
    stop("No valid results were generated. Check the input parameters or data availability.")
  }

  # Create the final data frame if output_df is TRUE
  if (output_df) {
    res_df <- do.call(rbind, lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      if ("from" %in% names(performance_data) && "to" %in% names(performance_data)) {
        performance_data$Period <- paste(performance_data$from, "to", performance_data$to)
      } else {
        performance_data$Period <- "Full Period"
      }

      # Remove 'from', 'to', and 'ticker' columns
      performance_data <- performance_data[, !names(performance_data) %in% c("from", "to", "ticker")]

      # Add metadata columns
      cbind(
        Symbol = x$Symbol,
        Class = x$Class,
        Methodology = x$Methodology,
        Window_Size1 = x$Window_Size1,
        Window_Size2 = x$Window_Size2,
        MA_Type = x$MA_Type,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio,
        performance_data
      )
    }))

    # Reset row names
    rownames(res_df) <- 1:nrow(res_df)

    return(res_df)
  } else {
    return(results)
  }
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
    signal1 = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0))) %>%
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

    self$data$signal <- ifelse((self$data$Close > self$data$last_long_value) & (self$data$Close > lag(self$data$ma)), 1,
                    ifelse((self$data$Close < self$data$last_short_value) & (self$data$Close < lag(self$data$ma)), -1, 0))

    # Replacing 0s by previous signal value:
    self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
    self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)

    self$data$position <- lag(self$data$signal, default = 0)

},

run_backtest = function(symbols, window_sizes, ma_types, data_type, split, cut_date, from_date, to_date, slicing_years, apply_rm, max_risks, reward_ratios, leverages, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (ma_type in ma_types) {
        for(max_risk in max_risks) {
          for(reward_ratio in reward_ratios) {
            for (leverage in leverages) {

        # Fetch data using DataFetcher for the current symbol and date range
        data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
        data <- data_fetcher$download_xts_data()
        
        # Ensure data is not empty
        if (nrow(data) == 0) {
          warning(paste("No data available for symbol:", symbol))
          next
        }

        # Create an instance of SMA1 strategy
        sma_instance <- SMA1$new(data, window_size = window_size, ma_type = ma_type)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sma_instance$estimate_performance(
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      } else {
        performance <- sma_instance$estimate_performance(
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size:", window_size, 
                        "ma_type:", ma_type))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, ma_type, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("SMA1:", window_size, ma_type),
          Window_Size = window_size,
          MA_Type = ma_type,
          Max_Risk = max_risk,
          Reward_Ratio = reward_ratio,
          Performance = performance
        )

        print(paste0(
          "SMA1M strategy (symbol: ", symbol, 
          ", class: ", meta$assets[[symbol]]$class, 
          ", window_size: ", window_size, 
          ", ma_type: ", ma_type, 
          ", split: ", split, 
          ", max_risk: ", max_risk, 
          ", reward_ratio: ", reward_ratio, 
          ", leverage: ", leverage,
          ")"
          )
        )
            }
          }
        }
      }
    }
  }

  # Check if results list is empty
  if (length(results) == 0) {
    stop("No valid results were generated. Check the input parameters or data availability.")
  }

  # Create the final data frame if output_df is TRUE
  if (output_df) {
    res_df <- do.call(rbind, lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      if ("from" %in% names(performance_data) && "to" %in% names(performance_data)) {
        performance_data$Period <- paste(performance_data$from, "to", performance_data$to)
      } else {
        performance_data$Period <- "Full Period"
      }

      # Remove 'from', 'to', and 'ticker' columns
      performance_data <- performance_data[, !names(performance_data) %in% c("from", "to", "ticker")]

      # Add metadata columns
      cbind(
        Symbol = x$Symbol,
        Class = x$Class,
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio,
        performance_data
      )
    }))

    # Reset row names
    rownames(res_df) <- 1:nrow(res_df)

    return(res_df)
  } else {
    return(results)
  }
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
    signal1 = ifelse(ma1 > ma2, 1, ifelse(ma1 < ma2, -1, 0))) %>%
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
      
      self$data$signal <- NA
      self$data$signal <- ifelse((lag(self$data$ma1) > self$data$last_long_value) & (lag(self$data$ma1) > lag(self$data$ma2)), 1,
                                 ifelse((lag(self$data$ma1) < self$data$last_short_value) & (lag(self$data$ma1) < lag(self$data$ma2)), -1, 0))

    # Replacing 0s by previous signal value:
      self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
      self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)
      self$data$position <- lag(self$data$signal, default = 0)
},

run_backtest = function(symbols, window_sizes1, window_sizes2, ma_types, data_type, split, cut_date, from_date, to_date, slicing_years, apply_rm, max_risks, reward_ratios, leverages, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
        for (ma_type in ma_types) {
          for(max_risk in max_risks) {
            for(reward_ratio in reward_ratios) {
              for (leverage in leverages) {

        # Fetch data using DataFetcher for the current symbol and date range
        data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
        data <- data_fetcher$download_xts_data()
        
        # Ensure data is not empty
        if (nrow(data) == 0) {
          warning(paste("No data available for symbol:", symbol))
          next
        }

        # Create an instance of SMA1 strategy
        sma2_instance <- SMA2$new(data, window_size1 = window_size1, window_size2 = window_size2, ma_type = ma_type)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sma2_instance$estimate_performance(
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      } else {
        performance <- sma2_instance$estimate_performance(
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size1:", window_size1,
                        "window_size2:", window_size2,
                        "ma_type:", ma_type))
          next
        }

        # Store the results
        results[[paste(symbol, window_size1, window_size2, ma_type, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("SMA2:", window_size1, window_size2, ma_type),
          Window_Size1 = window_size1,
          Window_Size2 = window_size2,
          MA_Type = ma_type,
          Max_Risk = max_risk,
          Reward_Ratio = reward_ratio,
          Performance = performance
        )

        print(paste0(
          "SMA2M strategy (symbol: ", symbol, 
          ", class: ", meta$assets[[symbol]]$class, 
          ", window_size1: ", window_size1,
          ", window_size2: ", window_size2, 
          ", ma_type: ", ma_type, 
          ", split: ", split, 
          ", max_risk: ", max_risk, 
          ", reward_ratio: ", reward_ratio, 
          ", leverage: ", leverage,
          ")"
          )
        )
              }
            }
          }
        }
      }
    }
  }

  # Check if results list is empty
  if (length(results) == 0) {
    stop("No valid results were generated. Check the input parameters or data availability.")
  }

  # Create the final data frame if output_df is TRUE
  if (output_df) {
    res_df <- do.call(rbind, lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      if ("from" %in% names(performance_data) && "to" %in% names(performance_data)) {
        performance_data$Period <- paste(performance_data$from, "to", performance_data$to)
      } else {
        performance_data$Period <- "Full Period"
      }

      # Remove 'from', 'to', and 'ticker' columns
      performance_data <- performance_data[, !names(performance_data) %in% c("from", "to", "ticker")]

      # Add metadata columns
      cbind(
        Symbol = x$Symbol,
        Class = x$Class,
        Methodology = x$Methodology,
        Window_Size1 = x$Window_Size1,
        Window_Size2 = x$Window_Size2,
        MA_Type = x$MA_Type,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio,
        performance_data
      )
    }))

    # Reset row names
    rownames(res_df) <- 1:nrow(res_df)

    return(res_df)
  } else {
    return(results)
  }
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
    ma_type = NULL,

initialize = function(data, window_size1, window_size2, sline, ma_type) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$window_size1 <- window_size1
  self$window_size2 <- window_size2
  self$sline <- sline
  self$ma_type <- ma_type
},

generate_signals = function() {
  ma_func <- get(self$ma_type)
  self$data <- mutate(self$data,
    ma1 = ma_func(Close, self$window_size1, align = "right", fill = NA),
    ma2 = ma_func(Close, self$window_size2, align = "right", fill = NA),
    macd_line = ma1 - ma2,
    signal_line = EMA(macd_line, self$sline),
    signal = ifelse(macd_line > signal_line, 1, ifelse(macd_line < signal_line, -1, 0)),
    position = lag(signal, default = 0)) %>%
      na.omit()
},

run_backtest = function(symbols, window_sizes1, window_sizes2, slines, ma_types, data_type, split, cut_date, from_date, to_date, slicing_years, apply_rm, max_risks, reward_ratios, leverages, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
        for (sline in slines) {
          for (ma_type in ma_types) {
            for(max_risk in max_risks) {
              for(reward_ratio in reward_ratios) {
                for (leverage in leverages) {

                  # Fetch data using DataFetcher for the current symbol and date range
                  data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
                  data <- data_fetcher$download_xts_data()

                  # Ensure data is not empty
                  if (nrow(data) == 0) {
                    warning(paste("No data available for symbol:", symbol))
                    next
                  }

                  # Create an instance of SMA1 strategy
                  macd_instance <- MACD$new(data, window_size1 = window_size1, window_size2 = window_size2, sline = sline, ma_type = ma_type)

                  # Estimate performance based on the split argument
                  if (split) {
                    performance <- macd_instance$estimate_performance(
                      data_type = data_type,
                      split_data = TRUE,
                      cut_date = cut_date,
                      window = slicing_years,
                      apply_rm = apply_rm,
                      max_risk = max_risk,
                      reward_ratio = reward_ratio,
                      capital = capital,
                      leverage = leverage,
                      symbol = symbol
                    )
                  } else {
                    performance <- macd_instance$estimate_performance(
                      data_type = data_type,
                      split_data = FALSE,
                      cut_date = cut_date,
                      window = slicing_years,
                      apply_rm = apply_rm,
                      max_risk = max_risk,
                      reward_ratio = reward_ratio,
                      capital = capital,
                      leverage = leverage,
                      symbol = symbol
                    )
                  }

                  # Skip if performance is NULL
                  if (is.null(performance) || nrow(performance) == 0) {
                    warning(paste("No performance data for symbol:", symbol, 
                                  "window_size1:", window_size1,
                                  "window_size2:", window_size2,
                                  "sline:", sline,
                                  "ma_type:", ma_type))
                    next
                  }

                  # Store the results
                  results[[paste(symbol, window_size1, window_size2, sline, ma_type, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
                    Symbol = symbol,
                    Class = meta$assets[[symbol]]$class,
                    Methodology = paste("SMA2:", window_size1, window_size2, ma_type),
                    Window_Size1 = window_size1,
                    Window_Size2 = window_size2,
                    Sline = sline,
                    MA_Type = ma_type,
                    Max_Risk = max_risk,
                    Reward_Ratio = reward_ratio,
                    Performance = performance
                  )

                  print(paste0(
                    "MACD strategy (symbol: ", symbol, 
                    ", class: ", meta$assets[[symbol]]$class, 
                    ", window_size1: ", window_size1,
                    ", window_size2: ", window_size2, 
                    ", sline: ", sline, 
                    ", ma_type: ", ma_type, 
                    ", split: ", split, 
                    ", max_risk: ", max_risk, 
                    ", reward_ratio: ", reward_ratio, 
                    ", leverage: ", leverage,
                    ")"
                  ))
                }
              }
            }
          }
        }
      }
    }
  }

  # Check if results list is empty
  if (length(results) == 0) {
    stop("No valid results were generated. Check the input parameters or data availability.")
  }

  # Create the final data frame if output_df is TRUE
  if (output_df) {
    res_df <- do.call(rbind, lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      if ("from" %in% names(performance_data) && "to" %in% names(performance_data)) {
        performance_data$Period <- paste(performance_data$from, "to", performance_data$to)
      } else {
        performance_data$Period <- "Full Period"
      }

      # Remove 'from', 'to', and 'ticker' columns
      performance_data <- performance_data[, !names(performance_data) %in% c("from", "to", "ticker")]

      # Add metadata columns
      cbind(
        Symbol = x$Symbol,
        Class = x$Class,
        Methodology = x$Methodology,
        Window_Size1 = x$Window_Size1,
        Window_Size2 = x$Window_Size2,
        Sline = x$Sline,
        MA_Type = x$MA_Type,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio,
        performance_data
      )
    }))

    # Reset row names
    rownames(res_df) <- 1:nrow(res_df)

    return(res_df)
  } else {
    return(results)
  }
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
      super$initialize(data) # Call the parent class's initialize method
      self$data <- super$convert_to_tibble(self$data) # Convert the data to tibble format
      self$window_size <- window_size
      self$threshold_oversold <- threshold_oversold
      self$threshold_overbought <- threshold_overbought
},

generate_signals = function() {
      
  # Calculate RSI and generate signals
  self$data <- self$data %>%
    mutate(
      # Calculate daily price changes
      change = Close - lag(Close, default = NA),
      gain = ifelse(change > 0, change, 0),
      loss = ifelse(change < 0, abs(change), 0),
      
      # Calculate rolling average gain and loss
      avg_gain = zoo::rollmean(gain, k = self$window_size, align = "right", fill = NA),
      avg_loss = zoo::rollmean(loss, k = self$window_size, align = "right", fill = NA),
      
      # Calculate RS and RSI
      rs = avg_gain / avg_loss,
      rsi = 100 - (100 / (1 + rs)),
      
      # Generate signals: 1 = Buy, -1 = Sell, 0 = Hold
      signal = ifelse(rsi < self$threshold_oversold, 1,
            ifelse(rsi > self$threshold_overbought, -1, 0)),
      
      # Calculate position: lagged signal to avoid lookahead bias
      position = lag(signal, default = 0)
    ) %>%
    na.omit() # Remove rows with NA values
}

  )
)

# Buy high, sell low
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
    upper_channel = rollapply(self$data$Close, self$window_size1, max, align = "right", fill = NA),
    lower_channel = rollapply(self$data$Close, self$window_size2, min, align = "right", fill = NA),
    signal = ifelse(Close > upper_channel, 1, ifelse(Close < lower_channel, -1, 0)),
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

# Define class for Strategy based on GARCH model
GARCH <- R6Class(
  "GARCH",
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
                    na.omit %>%
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
    #print(head(self$data))
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

# Define ARIMA class
ARIMA <- R6Class(
  "ARIMA",
  inherit = Strategy,
  public = list(
    window_size = NULL, # for "moving" window_type it is window_size, for "expanding" window_type it is starting window_size only (as then it expands given iterations)
    window_type = NULL,
    best_arima = NULL,
    p1 = NULL,
    d1 = NULL,
    q1 = NULL,
    
initialize = function(data, window_size, window_type, best_arima = FALSE, p1 = NULL, d1 = NULL, q1 = NULL) {
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
  n <- nrow(self$data)
  
  # Preallocate vectors
  forecast_values <- numeric(n)
  actual_values <- numeric(n)
  dates <- as.Date(character(n))
  
  error_log <- list()
  index <- 1
  
  # Rolling window type
  switch(
    self$window_type,
    "moving" = {
      for (i in 1:(n - self$window_size + 1)) {
        window_data <- na.omit(self$data$Close[i:(i + self$window_size - 1)])
        tryCatch({
          fit <- if (self$best_arima) {
            auto.arima(window_data, stepwise = TRUE, approximation = TRUE)
          } else {
            arima(window_data, order = c(self$p1, self$d1, self$q1), method = "CSS-ML")
          }
          forecast_values[index] <- as.numeric(forecast(fit, h = 1)$mean)
          actual_values[index] <- self$data$Close[i + self$window_size - 1]
          dates[index] <- self$data$Date[i + self$window_size - 1]
          index <- index + 1
        }, error = function(e) {
          error_log[[length(error_log) + 1]] <<- list(index = i, message = e$message)
          print(paste("Error in iteration", i, ":", e$message))
        })
      }
    },
    "expanding" = {
      for (i in self$window_size:n) {
        window_data <- na.omit(self$data$Close[1:i])
        tryCatch({
          fit <- if (self$best_arima) {
            auto.arima(window_data, stepwise = TRUE, approximation = TRUE)
          } else {
            arima(window_data, order = c(self$p1, self$d1, self$q1), method = "CSS-ML")
          }
          forecast_values[index] <- as.numeric(forecast(fit, h = 1)$mean)
          actual_values[index] <- self$data$Close[i]
          dates[index] <- self$data$Date[i]
          index <- index + 1
        }, error = function(e) {
          error_log[[length(error_log) + 1]] <<- list(index = i, message = e$message)
          print(paste("Error in iteration", i, ":", e$message))
        })
      }
    }
  )
  
  res <- data.frame(
    Date = dates[1:(index - 1)],
    Forecast = forecast_values[1:(index - 1)],
    Actual = actual_values[1:(index - 1)]
  )
  
  self$data <- self$data %>%
    left_join(
      res %>% filter(!is.na(Forecast)) %>% select(Date, Forecast),
      by = "Date"
    ) %>%
    mutate(
      signal = case_when(
        !is.na(Forecast) & Forecast > Close ~ 1,
        !is.na(Forecast) & Forecast < Close ~ -1,
        TRUE ~ 0
      ),
      position = lag(signal, default = 0)
    )
}

  )
)

