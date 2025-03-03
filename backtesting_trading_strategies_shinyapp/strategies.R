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
estimate_performance = function(symbol, capital, leverage, data_type, split_data, cut_date, window, 
apply_rm, flat_after_event, dynamic_limits, max_risk, reward_ratio, run_via_cpp) {

  # Slice self$data using the private slicer method
  self$data <- private$slicer(self$data, cut_date, data_type)

  # Generate signals
  self$generate_signals()

  ##########################################################################################################################
  # Update position type (long/short) given risk management option; compute: number of positions, daily PnL, portfolio value
  ##########################################################################################################################

  if(apply_rm) {

    if(run_via_cpp) {
  
  # Inputs for Rcpp function
    Date <- self$data$Date
    Close <- self$data$Close
    High <- self$data$High
    Low <- self$data$Low
    value <- self$data$value
    signal <- self$data$signal
    position <- self$data$position
    position[1] <- 0

    # Call C++ function version
    self$data <- apply_risk_management_cpp(Date, Close, High, Low, value, signal, position, max_risk, reward_ratio, leverage, capital, flat_after_event, dynamic_limits)
    } else 
      self$data <- private$apply_risk_management(self$data, max_risk, reward_ratio, leverage, capital, flat_after_event, dynamic_limits)

  } else {
    # Initialize columns
    self$data <- self$data %>% 
    mutate(
        position1 = position,
        nopActive = 0,
        nopPassive = 0,  # Initial number of passive positions (constant)
        pnlActive = 0,
        pnlPassive = c(0, diff(Close)),  # Difference in close price to calculate passive pnl
        pnlActiveCumulative = 0,
        pnlPassiveCumulative = 0,
        eqlActive = capital,
        eqlPassive = capital,
        From = as.Date(NA),
        To = as.Date(NA),
        pnlActiveType = "U", # by default 'Unrealized'
        group = cumsum(signal != shift(signal, type = "lag", fill = 0)),
        Liquidation = FALSE # liquidation when account balance is equal to 0
    )

    self$data$position[1] <- 0

    eqlActive <- capital
    eqlPassive <- capital
    pnlActiveR <- FALSE
    next_day_zero_pnl <- FALSE

    # Iterate over each row in self$data
    for (i in 2:nrow(self$data)) {

    # No positions after liquidation
    if (eqlActive <= 0) {
      self$data$position[i] <- 0
      self$data$Liquidation[i] <- TRUE
    } else {
    self$data$Liquidation[i] <- self$data$Liquidation[i - 1]  # Carry forward liquidation status
  }
    
    prev_nop_Active <- self$data$nopActive[i - 1]
    
    # Handle position change (for active portfolio)
    if (self$data$position[i] != self$data$position[i - 1]) {
        # self$data$nopActive[i] <- max(0, eqlActive * leverage / self$data$Close[i], 0)
        self$data$nopActive[i] <- ifelse(self$data$position[i] == 0,  
                                 0,  
                                 max(0, eqlActive * leverage / self$data$Close[i]))
    } else {
        # Keep previous nopActive if position hasn't changed
        self$data$nopActive[i] <- max(0, self$data$nopActive[i - 1])
    }

    # Update PnL type (Realzied or Unrealized)
    if (self$data$position[i] == 0) {
      # If position is flat, reset PnL type to "U"
      self$data$pnlActiveType[i] <- "U"
      pnlActiveR <- FALSE  # Reset reversal flag on flat
    } else {
      # Check if a position reversal happened (1 to -1 or -1 to 1)
      if (!pnlActiveR && 
          ((self$data$position[i - 1] == 1 && self$data$position[i] == -1) || 
           (self$data$position[i - 1] == -1 && self$data$position[i] == 1))) {
        self$data$pnlActiveType[i] <- "R"  # Set PnL type to "R" on reversal
        pnlActiveR <- TRUE  # Mark that a reversal happened
      } else if (pnlActiveR) {
        # After reversal, reset to "U" for the next period
        self$data$pnlActiveType[i] <- "U"
        pnlActiveR <- FALSE  # Reset reversal flag
      } else {
        # Default to "U" when no reversal has happened
        self$data$pnlActiveType[i] <- "U"
      }
    }

    # Active strategy portfolio value

    # Handle post reversal PnL calculation
      if (next_day_zero_pnl) {
      self$data$pnlActive[i] <- 0
      next_day_zero_pnl <- FALSE  # Reset flag
      } else {
        if (self$data$pnlActiveType[i] == "R") {
          next_day_zero_pnl <- TRUE  # Set flag for the next period
          self$data$nopActive[i] <- self$data$nopActive[i - 1]
          self$data$pnlActive[i] <- (self$data$Close[i - 1] - self$data$Close[i]) * self$data$position[i] * self$data$nopActive[i - 1]
        } else {
          self$data$pnlActive[i] <- if (self$data$position[i] == 0) 0 else round((self$data$Close[i] - self$data$Close[i - 1]) * self$data$position[i - 1] * self$data$nopActive[i - 1], 2)
        }
      }

    # Update active equity
    eqlActive <- max(0, round(eqlActive + self$data$pnlActive[i], 2))
    self$data$eqlActive[i] <- eqlActive

    # Passive strategy
    self$data$nopPassive[i] <- max(0, eqlPassive * leverage / self$data$Close[i])
    self$data$pnlPassive[i] <- round((self$data$Close[i] - self$data$Close[i - 1]) * self$data$nopPassive[i - 1], 2)
    eqlPassive <- max(0, round(eqlPassive + self$data$pnlPassive[i], 2))
    self$data$eqlPassive[i] <- eqlPassive

    }

  }

  # Estimate volatility over the period of 30 days (after signal generation):
  self$estimate_range_potential(30)
  
  ########################################################################################################################
  # Adding additional metrics (annualized volatility, cumulative daily PnL, and portfolio daily return)
  ########################################################################################################################
  setDT(self$data)

  # Define unique trade id including position close (pnlActiveType == "R")
  self$data$trade_id <- cumsum(c(0, diff(self$data$position) != 0))
  
  # Copy trade_id to trade_id_m
  self$data$trade_id_m <- self$data$trade_id
  
  # Update trade_id_m where pnlActiveType == "R"
  self$data$trade_id_m <- ifelse(self$data$pnlActiveType == "R", dplyr::lag(self$data$trade_id_m, default = first(self$data$trade_id_m)), self$data$trade_id_m)

  # Final trade identifier, to match with trade_id in get_trades() method
  self$data$trade_id_m2 <- cumsum(c(0, diff(self$data$trade_id_m) != 0))

  # Calculate annualized volatility, cumulative PnLs, and return of portfolio
  self$data <- self$data %>%
    mutate(
      annual_vol = rollapply(value, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(252),
      pnlActiveCumulative = round(cumsum(replace_na(pnlActive, 0)), 2),
      pnlPassiveCumulative = round(cumsum(replace_na(pnlPassive, 0)), 2),
      r_eqlActive = (eqlActive - lag(eqlActive)) / lag(eqlActive),
      r_eqlPassive = (eqlPassive - lag(eqlPassive)) / lag(eqlPassive)
      #cryptoClass = ifelse(meta$assets[[symbol]]$class %in% "Cryptocurrency", TRUE, FALSE)
    )

  ########################################################################################################################
  # Estimate trading profile
  ########################################################################################################################
  
  # Case when the data is split
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
        metrics <- private$compute_metrics(data_period, symbol, run_via_cpp)
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

  # Case when data is not split
  } else {
    metrics <- private$compute_metrics(self$data, symbol, run_via_cpp)
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

# Microscopic level (trades analysis)
get_trades = function(apply_rm) {
  
  # Initialize event as FALSE if not already set
  if (apply_rm) {
    self$data$event <- ifelse(self$data$eventSL | self$data$eventPT, TRUE, FALSE)
    self$data$event[is.na(self$data$event)] <- FALSE
    self$data$eventSL[is.na(self$data$eventSL)] <- FALSE
    self$data$eventPT[is.na(self$data$eventPT)] <- FALSE
  } else {
    self$data$event <- FALSE
    self$data$eventSL <- FALSE
    self$data$eventPT <- FALSE
  }
  
  self$data$flat <- ifelse(self$data$position == 0, TRUE, FALSE)
  self$data$L <- lead(self$data$Liquidation)

  trades <- self$data %>%
  mutate(
    trade_direction = if_else(position == -1, "Sell", if_else(position == 1, "Buy", "Flat")),
    # Entries
    entry = if_else(trade_id_m != lag(trade_id_m), Date, as.Date(NA)),
    entry_price = if_else(trade_id_m != lag(trade_id_m), Close, NA_real_),
    entry_size = if_else(trade_id_m != lag(trade_id_m), nopActive, NA_real_),
    entry_account_size = if_else(trade_id_m != lag(trade_id_m), eqlActive, NA_real_),
    # Exits
    exit = if_else(trade_id_m != lead(trade_id_m), Date, as.Date(NA)),
    exit_price = if_else(trade_id_m != lead(trade_id_m), Close, NA_real_),
    exit_size = if_else(trade_id_m != lead(trade_id_m), nopActive, NA_real_),
    exit_account_size = if_else(trade_id_m != lead(trade_id_m), eqlActive, NA_real_)
  ) %>%
  
  # Fill missing values for pnl and entry/exit details
  tidyr::fill(entry, entry_price, entry_size, entry_account_size, .direction = "down") %>%
  tidyr::fill(exit, exit_price, exit_size, exit_account_size, .direction = "up") %>%
  
  filter(!is.na(entry) & !is.na(exit)) %>%
  
  group_by(entry, exit) %>%
  reframe(
    Trade = first(trade_direction),
    ExitBy = if_else(any(L), "Liquidation", if_else(any(eventSL), "Stop-loss", if_else(any(eventPT), "Take-profit", if_else(any(flat), "Flat", "Signal")))),
    Start = as.Date(first(entry)),
    Size = if_else(any(flat), 0, round(first(entry_size), 5)),
    EntryPrice = round(first(entry_price), 4),
    End = as.Date(first(exit)),
    ExitPrice = round(first(exit_price), 4),
    # Compute trade PnL
    TradePnL = round(if_else(any(flat), 0, if_else(first(Trade) == "Buy", ExitPrice - EntryPrice, EntryPrice - ExitPrice) * Size), 0),
    # Account
    BalanceStart = round(first(entry_account_size), 0),
    BalanceEnd = round(BalanceStart + TradePnL, 0)
  ) %>%
  
  ungroup()

  # Find first liquidation index
  first_liquidation_idx <- which(trades$ExitBy == "Liquidation")[1]

  if (!is.na(first_liquidation_idx)) {
    trades <- trades[1:first_liquidation_idx, ]  # Keep only trades up to and including liquidation
  }

  trades <- trades %>% mutate(
    RunningPnL = round(cumsum(TradePnL), 0),
    Efficiency = round((TradePnL / abs(RunningPnL)) * 100, 0)
  ) %>%
  
  rename(`TradePnL/RunningPnL,%` = Efficiency) %>%
  
  select(
    Trade, ExitBy, Start, End, Size, EntryPrice, ExitPrice, BalanceStart, TradePnL, BalanceEnd, RunningPnL, `TradePnL/RunningPnL,%`
  )

  setDT(trades)

  # Visualization

  # Calculate cumulative PnL separately for Buy and Sell trades
  trades[, Cumulative_PnL_Buy := cumsum(ifelse(Trade == "Buy", TradePnL, 0))]
  trades[, Cumulative_PnL_Sell := cumsum(ifelse(Trade == "Sell", TradePnL, 0))]

  # Convert data from wide to long format for ggplot
  trades_long <- melt(trades, id.vars = "Start", 
                      measure.vars = c("Cumulative_PnL_Buy", "Cumulative_PnL_Sell"),
                      variable.name = "TradeType", value.name = "CumulativePnL")

 # 1. Trades PnL distribution
  range_pnl <- range(trades$TradePnL[is.finite(trades$TradePnL) & trades$TradePnL != 0], na.rm = TRUE)

  pnl_hist <- ggplot(data = data.frame(TradePnL = trades$TradePnL[is.finite(trades$TradePnL) & trades$TradePnL != 0]),
                   aes(x = TradePnL, fill = TradePnL > 0)) +
  geom_histogram(binwidth = diff(range_pnl) / 100, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "Trade Profit and Loss (PnL) Distribution", x = "Trade PnL", y = "Frequency") +
  scale_x_continuous(expand = c(0, 0), limits = range_pnl, breaks = pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()

  # 2. Trade type distribution
  pnl_hist_by_trade <- ggplot(data = data.frame(trades %>% filter(Trade != "Flat")), aes(x = Trade, y = TradePnL, fill = Trade)) +
  geom_boxplot(alpha = 0.6) +  # Boxplot with transparency
  labs(title = "Distribution of Trade PnL by Trade Type",
       x = "Trade Type",
       y = "Trade PnL") +
  theme_minimal() +
  scale_fill_manual(values = c("Buy" = "blue", "Sell" = "red")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


  # 3. PnL contribution by trade type
  trades[, YearMonth := format(as.Date(Start), "%Y-%m")]

  # Aggregate PnL by month and trade type
  monthly_pnl <- trades[, .(TotalPnL = sum(TradePnL)), by = .(YearMonth, Trade)]
  monthly_pnl[, YearMonth := as.Date(paste0(YearMonth, "-01"))]

  pnl_contr_by_trade <- ggplot(monthly_pnl, aes(x = YearMonth, y = TotalPnL, fill = Trade)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Trade PnL Contribution by Trade Type",
        x = "Year",
        y = "Total Trade PnL") +
    scale_fill_manual(values = c("Buy" = "blue", "Sell" = "red")) +  # Buy = blue, Sell = red
    scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

  # 4. Cumulative PnL by trade type
  pnl_cum_by_trade  <- ggplot(trades_long, aes(x = Start, y = CumulativePnL, color = TradeType)) +
  geom_line(size = 1.2) +
  labs(title = "Cumulative PnL Over Time by Trade Type",
       x = "Date", y = "Cumulative PnL") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  scale_color_manual(values = c("Cumulative_PnL_Buy" = "blue", "Cumulative_PnL_Sell" = "red")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal()

  # 5. Exit types
  exit_counts <- trades %>% filter(Trade != "Flat") %>%
  group_by(ExitBy) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  # Create the pie chart with custom colors
  exits <- ggplot(exit_counts, aes(x = "", y = Percentage, fill = ExitBy)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +  # Convert to pie chart
    labs(title = "Distribution of Exit Types in Total Trades", 
        fill = "Exit Type") +
    theme_void() +  # Remove unnecessary chart elements
    theme(axis.text.x = element_blank()) +  # Remove x-axis labels
    scale_fill_manual(values = c("Take-profit" = "green", 
                                "Stop-loss" = "red", 
                                "Signal" = "blue",
                                "Liquidation" = "grey")) +
     geom_text(aes(label = paste0(round(Percentage, 1), "%")),  # Add percentages as text
            position = position_stack(vjust = 0.5),  # Center text within each slice
            color = "black")  # Set text color to white for visibilit

  trades <- trades %>% select(Trade, Start, End, ExitBy, Size, EntryPrice, ExitPrice, BalanceStart, TradePnL, BalanceEnd, Cumulative_PnL_Buy, Cumulative_PnL_Sell, RunningPnL, `TradePnL/RunningPnL,%`) %>%
    rename(Running_PnL_Buy = Cumulative_PnL_Buy, Running_PnL_Sell = Cumulative_PnL_Sell)

  trades$trade_id <- seq_len(nrow(trades))

  return(
    list(
      trades = trades, 
      pnl_hist = pnl_hist, 
      pnl_contr_by_trade = pnl_contr_by_trade, 
      pnl_cum_by_trade = pnl_cum_by_trade, 
      pnl_hist_by_trade = pnl_hist_by_trade, 
      exits = exits
      )
      )  
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
      geom_vline(data = self$data[self$data$position == -1 & self$data$pnlActiveType != "R", ], 
                aes(xintercept = as.numeric(Date), linetype = "Short Position"), 
                color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$position == 1 & self$data$pnlActiveType != "R", ], 
                aes(xintercept = as.numeric(Date), linetype = "Long Position"), 
                color = "green", alpha = 0.5)
  }

  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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

  self$data <- self$data %>%
    mutate(
      TR1 = High - Low,
      TR2 = abs(High - lag(Close)),
      TR3 = abs(Low - lag(Close)),
      TR = pmax(TR1, TR2, TR3, na.rm = TRUE),
      ATR = zoo::rollmean(TR, n, fill = NA, align = "right"),
      N = TR / ATR
    )

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
    geom_bar(stat = "identity", fill = "blue") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "yellow") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = "N = TR / ATR", x = "Date", y = "N") +
    theme_minimal()
  
  # Return the individual plots
  return(list(
    close = close,
    n = n
  ))

},

# Plot Close price and stop loss and profit take calculated prices
plot_rm_levels = function(ndays, apply_rm) {
  if (!apply_rm) {
    message("apply_rm is FALSE. Skipping plot.")
    return(NULL)
  }
  
  # Filter the last ndays of data
  data_subset <- tail(self$data, ndays)
  
  # Ensure necessary columns exist
  if (!all(c("eventSL", "eventPT", "position", "profitTake", "stopLoss", "Close", "Date") %in% colnames(data_subset))) {
    stop("Missing required columns in self$data.")
  }
  
  # Handle NA values properly
  data_subset$ForcedExit <- ifelse(lag(data_subset$eventSL, default = FALSE) | 
                                   lag(data_subset$eventPT, default = FALSE), TRUE, FALSE)
  
  # Ensure ForcedExit has no NA values
  data_subset$ForcedExit[is.na(data_subset$ForcedExit)] <- FALSE
  
  # Convert position to character if needed
  data_subset$position <- as.character(data_subset$position)
  
  # Assign position_type
  data_subset$position_type <- ifelse((data_subset$position == "1") & !data_subset$ForcedExit, "Buy", 
                              ifelse((data_subset$position == "-1") & !data_subset$ForcedExit, "Sell", 
                                     ifelse(data_subset$ForcedExit, "ForcedExit", "Flat")))

  # Explicitly set factor levels in the correct order
  data_subset$position_type <- factor(data_subset$position_type, levels = c("Sell", "Buy", "ForcedExit", "Flat"))

  # Create the plot
  p <- ggplot(data_subset, aes(x = Date)) +
    geom_line(aes(y = round(Close, 4)), color = "black", linetype = "dashed", size = 0.4) +  
    geom_point(aes(y = round(Close, 4), color = position_type), size = 2) +  
    geom_point(aes(y = profitTake), shape = 17, color = "green", size = 3) +  
    geom_point(aes(y = stopLoss), shape = 4, color = "red", size = 3) +  
    scale_color_manual(values = c("Sell" = "red", "Buy" = "green", "ForcedExit" = "blue", "Flat" = "grey")) +
    labs(title = "Close Price with Stop Loss and Profit Take",
         x = "Date", y = "Price", color = "Position") +
    theme_minimal()

  print(p)
},

# Plot number of position evolution with account size line
plot_nop_evo = function() {
  
  # Compute scaling factor
  scale_factor <- max(self$data$eqlActive, na.rm = TRUE) / max(self$data$nopActive, na.rm = TRUE)

  p <- ggplot(self$data, aes(x = Date)) +
    geom_col(aes(y = nopActive), fill = "blue", alpha = 0.3) +  # Bars for nopActive
    geom_line(aes(y = eqlActive / scale_factor), color = "red", size = 1.2) +  # Thicker red line
    scale_y_continuous(
      name = "nop",
      breaks = pretty_breaks(n = 10),
      sec.axis = sec_axis(~ . * scale_factor, name = "eqlActive", breaks = pretty_breaks(n = 20)) # More right-side ticks
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Dashed horizontal line at 1
    labs(title = "Active Position Size & Account Balance Over Time", x = "Date") +
    theme_minimal() +
    theme(axis.title.y.right = element_text(color = "red"))
    
    print(p)
},

# Plot the annualized volatility with account size
plot_annualized_vol = function() {
  
  # Compute scaling factor
  scale_factor <- max(self$data$eqlActive, na.rm = TRUE) / max(self$data$annual_vol, na.rm = TRUE)

  p <- ggplot(self$data, aes(x = Date)) +
    geom_line(aes(y = annual_vol), color = "grey", size = 1.2) +  # Red line for annual_vol
    geom_line(aes(y = eqlActive / scale_factor), color = "black", size = 1.2) +  # Black line for eqlActive
    scale_y_continuous(
      name = "annualized_volatility",
      breaks = pretty_breaks(n = 10),
      sec.axis = sec_axis(~ . * scale_factor, name = "eqlActive", breaks = pretty_breaks(n = 20)) # Right axis
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +  # Dashed horizontal line at 1
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "black") +  # Dashed horizontal line at 1
    labs(title = "Annualized Volatility & Account Balance Over Time", x = "Date") +
    theme_minimal() +
    theme(axis.title.y.right = element_text(color = "red"))
    
    print(p)
}

  ),

private = list(

# Apply stop loss and profit take (also, there is an option to jump out of trend and shift stop loss or profit take limits when prices goes in a favourable direction)
apply_risk_management = function(data, max_risk, reward_ratio, leverage, capital, flat_after_event, dynamic_limits) {
  
  data$position[1] <- 0
  eqlActive <- capital
  eqlActive2 <- capital
  eqlPassive <- capital
  previous_position <- 0
  stopLoss <- profitTake <- NA
  flat <- FALSE
  reversed_position <- NA
  pnlActiveR <- FALSE
  next_day_zero_pnl <- FALSE # after position liquidation
  
  # Initialize columns
  data <- data %>%
    mutate(
      position1 = position,
      group = cumsum(signal != shift(signal, type = "lag", fill = 0)),
      stopLoss = NA,
      profitTake = NA,
      oldStopLoss = NA,
      eventSL = NA,
      eventPT = NA,
      eventSLShift = NA,
      nopActive = 0,
      nopPassive = capital / Close[1] * leverage,  # Initial passive number of positions
      pnlActive = 0,
      pnlPassive = 0,
      eqlActive = capital,
      eqlPassive = capital,
      equity_growth_factor = 0,
      From = as.Date(NA),
      To = as.Date(NA),
      pnlActiveType = "U",
      Liquidation = FALSE # liquidation when account balance is equal to 0
    )
  
  # Iterate over each row in the data
  for (i in 2:nrow(data)) {
    
    # No positions after liquidation
    if (eqlActive <= 0) {
      data$position[i] <- 0  # Force position to 0 if equity is depleted
      data$Liquidation[i] <- TRUE  # Mark liquidation
    } else {
      data$Liquidation[i] <- data$Liquidation[i - 1]  # Carry forward liquidation status
    }
    
  # Stay flat after reversal in case flat_after_event 
    if (flat_after_event && flat) {
      data$position[i] <- 0
    }
    
    if (!is.na(reversed_position)) {
      data$position[i] <- reversed_position
      reversed_position <- NA
    } 
    
  # Position change (stop loss and profit take levels calculation)
    if (data$position[i] != previous_position || !is.na(data$pnlActiveType[i - 1]) && data$pnlActiveType[i - 1] == "R") {
      #data$nopActive[i] <- max(0, eqlActive * leverage / data$Close[i])
      data$nopActive[i] <- ifelse(data$position[i] == 0,  
                                 0,  
                                 max(0, eqlActive * leverage / data$Close[i]))

      eqlActive2 = data$eqlActive[i - 1] + ((data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1])

      if (data$position[i] == 1) {
        stopLoss <- data$Close[i] - (max_risk * eqlActive2 / data$nopActive[i])
        profitTake <- max(0, data$Close[i] + (reward_ratio * max_risk * eqlActive2 / data$nopActive[i]))
      } else if (data$position[i] == -1) {
        stopLoss <- data$Close[i] + (max_risk * eqlActive2 / data$nopActive[i])
        profitTake <- max(0, data$Close[i] - (reward_ratio * max_risk * eqlActive2 / data$nopActive[i]))
      } else {
        stopLoss <- profitTake <- NA
      }
      previous_position <- data$position[i]
    } else {
      # Carry forward previous values
      data$nopActive[i] <- data$nopActive[i - 1]
      
      # Shift stop loss
      if (dynamic_limits) {
        
        if(data$position[i] == 0) {
          eqlActive2 = 0
        } else {
          eqlActive2 = data$eqlActive[i - 1] + ((data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1])
        }

        # Compute rm_update dynamically
        data$equity_growth_factor[i] <- ifelse(is.na(eqlActive2 / data$eqlActive[i - 1]) | eqlActive2 <= 0, 
                                               1, 
                                               eqlActive2 / data$eqlActive[i - 1])
        
        # Initialize shift indicators as FALSE
        data$eventSLShift[i] <- FALSE
        
        # Store old stop-loss and profit-take before updating
        data$oldStopLoss[i] <- stopLoss
        
        # Only adjust SL/PT if equity increased (favorable move)
        if (data$position[i] == 1) {  # Long
          # new_stopLoss <- max(stopLoss, data$Close[i] - (data$equity_growth_factor[i] * max_risk * data$eqlActive[i] / data$nopActive[i]))
          new_stopLoss <- max(stopLoss, data$Close[i] - (data$equity_growth_factor[i] * max_risk * eqlActive2 / data$nopActive[i]))
          
          # Check if SL or PT were updated
          data$eventSLShift[i] <- (new_stopLoss != stopLoss)
          
          stopLoss <- new_stopLoss
          
        } else if (data$position[i] == -1) {  # Short
          # new_stopLoss <- min(stopLoss, data$Close[i] + (data$equity_growth_factor[i] * max_risk * data$eqlActive[i] / data$nopActive[i]))
          new_stopLoss <- min(stopLoss, data$Close[i] + (data$equity_growth_factor[i] * max_risk * eqlActive2 / data$nopActive[i]))
          
          # Check if SL or PT were updated
          data$eventSLShift[i] <- (new_stopLoss != stopLoss)
          
          stopLoss <- new_stopLoss

        }
        
      }
    }
    
    data$stopLoss[i] <- stopLoss
    data$profitTake[i] <- profitTake
    
    # Check for stop-loss or profit-take events
    if (data$position[i] == 1) {
      data$eventSL[i] <- if (!is.na(stopLoss) && data$Close[i] <= stopLoss) TRUE else NA
      data$eventPT[i] <- if (!is.na(profitTake) && data$Close[i] >= profitTake) TRUE else NA
    } else if (data$position[i] == -1) {
      data$eventSL[i] <- if (!is.na(stopLoss) && data$Close[i] >= stopLoss) TRUE else NA
      data$eventPT[i] <- if (!is.na(profitTake) && data$Close[i] <= profitTake) TRUE else NA
    } else {
      data$eventSL[i] <- data$eventPT[i] <- NA
    }
    
    # Reverse position logic
    if (!flat) {
      if (!is.na(data$eventSL[i]) || !is.na(data$eventPT[i])) {
        reversed_position <- -data$position[i]  # Store for next period
        if (flat_after_event) {
          flat <- TRUE
        }
      }
    }
    
    # Rest stay_flat flag when signal changes and there are no pending reversals
    if (i > 2 && data$group[i] != data$group[i - 1] && is.na(data$eventSL[i]) && is.na(data$eventPT[i])) {
      # is.na(data$eventSL[i]) && is.na(data$eventPT[i]) : when signal changes and no pending reversals
      flat <- FALSE
    }
    
    # Set up when a reversal happens (position flat out)
    if (data$position[i] == 0) {
      # If position is flat, reset PnL type to "U"
      data$pnlActiveType[i] <- "U"
      pnlActiveR <- FALSE  # Reset reversal flag on flat
    } else {
      # Check if a position reversal happened (1 to -1 or -1 to 1)
      if (!pnlActiveR && 
          ((data$position[i - 1] == 1 && data$position[i] == -1) || 
           (data$position[i - 1] == -1 && data$position[i] == 1))) {
        data$pnlActiveType[i] <- "R"  # Set PnL type to "R" on reversal
        pnlActiveR <- TRUE  # Mark that a reversal happened
      } else if (pnlActiveR) {
        # After reversal, reset to "U" for the next period
        data$pnlActiveType[i] <- "U"
        pnlActiveR <- FALSE  # Reset reversal flag
      } else {
        # Default to "U" when no reversal has happened
        data$pnlActiveType[i] <- "U"
      }
    }
    
    # Active strategy portfolio value
    
    # Post reversal PnL calculation
    if (next_day_zero_pnl) {
      data$pnlActive[i] <- 0
      next_day_zero_pnl <- FALSE  # Reset flag
    } else {
      if (data$pnlActiveType[i] == "R") {
        next_day_zero_pnl <- TRUE  # Set flag for the next period
        data$nopActive[i] <- data$nopActive[i - 1]
        data$pnlActive[i] <- (data$Close[i - 1] - data$Close[i]) * data$position[i] * data$nopActive[i - 1]
      } else {
        data$pnlActive[i] <- if (data$position[i] == 0) 0 else round((data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1], 2)
      }
    }
    
    # Calculate PnL and portfolio value (account size evolution)
    eqlActive <- round(eqlActive + data$pnlActive[i], 2)
    data$eqlActive[i] <- if (eqlActive < 0) 0 else eqlActive
    data$eqlActive[i] <- eqlActive
    
    # Passive strategy portfolio value
    data$nopPassive[i] <- max(0, eqlPassive * leverage / data$Close[i])
    data$pnlPassive[i] <- round((data$Close[i] - data$Close[i - 1]) * data$nopPassive[i - 1], 2)
    eqlPassive <- max(0, eqlPassive + data$pnlPassive[i])
    data$eqlPassive[i] <- eqlPassive
    
  }
  
  data <- data %>%
    mutate(
      pnlActiveCumulative = cumsum(replace_na(pnlActive, 0)),
      pnlPassiveCumulative = cumsum(replace_na(pnlPassive, 0))
    )
  
  return(data)
},

# Estimate trading profile of a strategy
estimate_trading_profile = function(data_subset, strategy_type, symbol) {

  data_subset$Date <- as.Date(data_subset$Date)

  # Select appropriate columns based on strategy type
  pnl_col <- ifelse(strategy_type == "Active", "pnlActive", "pnlPassive")
  eql_col <- ifelse(strategy_type == "Active", "eqlActive", "eqlPassive")
  r_col <- ifelse(strategy_type == "Active", "r_eqlActive", "r_eqlPassive")
  
  # Generate a trade_id based on changes in position
  data_subset <- data_subset %>% mutate(trade_id = cumsum(position != lag(position, default = 1)))

  GrossProfit <- round(GrossProfit <- sum(na.omit(tail(data_subset[[eql_col]], 1)) - na.omit(data_subset[[eql_col]][1])), 0)

  # 1. Annualized Profit
  AnnualizedProfit <- round(as.numeric(Return.annualized(as.numeric(na.omit(data_subset[[r_col]])), scale = 365, geometric = TRUE) * 100), 2)

  # profit_scale <- ifelse(data_subset$cryptoClass, 365, 252)

  # # 1. Annualized Profit
  # AnnualizedProfit <- round(as.numeric(Return.annualized(as.numeric(na.omit(data_subset[[r_col]])), scale = profit_scale, geometric = TRUE) * 100), 2)

  # 2. Number of Trades per Year
  NumberOfTradesPerYear <- round((if (strategy_type == "Active") sum(diff(data_subset$trade_id_m) != 0) + 1 else 1) / 
                                length(unique(format(data_subset$Date, "%Y"))), 0)

  # 3. Percentage of Winning Trades
  PercentageOfWinningTrades <- round(
    sum(aggregate(data_subset[[pnl_col]], by = list(cumsum(c(1, diff(data_subset$position) != 0))), sum, na.rm = TRUE)$x > 0) / 
    nrow(aggregate(data_subset[[pnl_col]], by = list(cumsum(c(1, diff(data_subset$position) != 0))), sum, na.rm = TRUE)) * 100, 2)

  # 4. Largest Win
  trades <- data_subset %>%
    group_by(trade_id_m2) %>%
    summarise(pnl = sum(!!sym(pnl_col), na.rm = TRUE))

  #LargestWin <- round(max(data_subset[[pnl_col]], na.rm = TRUE), 0)
  LargestWin <- round(max(trades$pnl, na.rm = TRUE), 0)

  # 5. Length of Largest Win
  LengthOfLargestWin <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.max(data_subset[[pnl_col]])], ], 
                              as.numeric(max(Date) - min(Date) + 1))

  # 6. Average Win
  AverageWin <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] > 0], na.rm = TRUE), 2)

  # 7. Length of Average Win
  AverageWinLength <- tryCatch({data_subset %>%
  transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
  aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
  subset(cum_pnl > 0) %>%
  {if (nrow(.) == 0) return(NA) else .} %>%
  merge(data_subset, by = "trade_id") %>%
  aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
  with(round(mean(Date, na.rm = TRUE)))}, error = function(e) NA)
  
  # 8. Largest Loss
  #LargestLoss <- round(min(data_subset[[pnl_col]], na.rm = TRUE),0)
  LargestLoss <- round(min(trades$pnl, na.rm = TRUE), 0)

  # 9. Length of Largest Loss
  LengthOfLargestLoss <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.min(data_subset[[pnl_col]])], ], 
                              as.numeric(max(Date) - min(Date) + 1))

  # 10. Average Loss
  AverageLoss <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] < 0], na.rm = TRUE), 2)

  # 11. Length of Average Loss
  AverageLossLength <- tryCatch({data_subset %>%
  transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
  aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
  subset(cum_pnl < 0) %>%
  {if (nrow(.) == 0) return(NA) else .} %>%
  merge(data_subset, by = "trade_id") %>%
  aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
  with(round(mean(Date, na.rm = TRUE)))}, error = function(e) NA)

  # 12-15: Winning Runs
  is_winning <- data_subset[[pnl_col]] > 0
  winning_runs <- rle(is_winning)$lengths[rle(is_winning)$values]
  winning_runs <- winning_runs[!is.na(winning_runs)]

  # 12. Average Winning Run
  AverageWinningRun <- round(mean(winning_runs), 2)

  # 13. Largest Winning Run
  LargestWinningRun <- max(winning_runs)

  # 14. Length of Time in Largest Winning Run
  largest_run_start <- tryCatch({sum(head(rle(is_winning)$lengths, which.max(winning_runs) - 1)) + 1}, error = function(e) NA)

  if (!is.na(largest_run_start)) {
    largest_run_end <- largest_run_start + LargestWinningRun - 1
    LengthOfTimeInLargestWinningRun <- as.numeric(diff(range(data_subset$Date[largest_run_start:largest_run_end]))) + 1
  } else {

    LengthOfTimeInLargestWinningRun <- NA
  }

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

  # Check if 'drawdown' column is empty or has valid values
  if (length(drawdown_data$drawdown) > 0 && !all(is.na(drawdown_data$drawdown))) {
    peak_idx <- which.max(drawdown_data[[eql_col]][1:which.min(drawdown_data$drawdown)])  # Peak before max drawdown
    trough_idx <- which.min(drawdown_data$drawdown)  # Trough for max drawdown

    StartDateMaxDrawdown <- as.Date(drawdown_data$Date[peak_idx])
    EndDateMaxDrawdown <- as.Date(drawdown_data$Date[trough_idx])
  } else {
    StartDateMaxDrawdown <- NA
    EndDateMaxDrawdown <- NA
  }

  # 22. Length of maximum drawdown period (in days)
  if (length(StartDateMaxDrawdown) == 0 || length(EndDateMaxDrawdown) == 0) {
    LengthOfMaxDrawdown <- NA
    StartDateMaxDrawdown <- NA
    EndDateMaxDrawdown <- NA
  } else {
    LengthOfMaxDrawdown <- as.numeric(EndDateMaxDrawdown - StartDateMaxDrawdown)
  }

  # 23. Maximum equity run-up (as a percentage)
  MaxRunUp <- round(max(data_subset %>%
                    mutate(cum_min_eql = cummin(get(eql_col)),
                          run_up = (get(eql_col) - cum_min_eql) / cum_min_eql) %>%
                    pull(run_up), na.rm = TRUE) * 100,2)

  # 24. Start and end dates of maximum run-up
  run_up_data <- data_subset %>%
    mutate(cum_min_eql = cummin(get(eql_col)),
          run_up = (get(eql_col) - cum_min_eql) / cum_min_eql)

  # Check if 'run_up' column is empty or has valid values
  if (length(run_up_data$run_up) > 0 && !all(is.na(run_up_data$run_up))) {
    # Identify the trough (start of run-up) and peak (maximum run-up)
    trough_idx_run_up <- which.min(run_up_data[[eql_col]])  # Trough before the run-up
    peak_idx_run_up <- which.max(run_up_data$run_up)  # Peak during the run-up

    # Ensure that the peak happens after the trough
    if (peak_idx_run_up < trough_idx_run_up) {
      peak_idx_run_up <- which.max(run_up_data$run_up[trough_idx_run_up:length(run_up_data$run_up)]) + trough_idx_run_up - 1
    }

    StartDateMaxRunUp <- as.Date(run_up_data$Date[trough_idx_run_up])
    EndDateMaxRunUp <- as.Date(run_up_data$Date[peak_idx_run_up])
  } else {
    StartDateMaxRunUp <- NA
    EndDateMaxRunUp <- NA
  }

  # 25. Length of maximum run-up period (in days)
  if (length(StartDateMaxRunUp) == 0 || length(EndDateMaxRunUp) == 0) {
    LengthOfMaxRunUp <- NA
    StartDateMaxRunUp <- NA
    EndDateMaxRunUp <- NA
  } else {
    LengthOfMaxRunUp <- as.numeric(EndDateMaxRunUp - StartDateMaxRunUp)
  }

  # 26. Trade expected return (absolute amount)
  ExpectedAbsoluteReturn = round((AverageWin + AverageLoss) * PercentageOfWinningTrades / 100, 2)

  # 27. Calmar Ratio
  CR = round(AnnualizedProfit / -MaxDrawdown, 4)

  # 28. Max sequence of losing trades
  trades$losing_trade <- trades$pnl < 0

  # Assign a unique group ID to each consecutive losing streak
  trades$group_id <- cumsum(c(1, diff(trades$losing_trade) != 0))

  # Filter only the losing trades and calculate the length of each losing streak
  losing_streaks <- trades %>%
    filter(losing_trade) %>%
    group_by(group_id) %>%
    summarise(losing_streak_length = n()) %>%
    ungroup()

  # Get the maximum length of consecutive losing trades
  MaxLosingStreak <- max(losing_streaks$losing_streak_length, na.rm = TRUE)

  # 29. Max sequence of winning trades
  trades$winning_trade <- trades$pnl > 0

  # Assign a unique group ID to each consecutive winning streak
  trades$group_id <- cumsum(c(1, diff(trades$winning_trade) != 0))

  # Filter only the winning trades and calculate the length of each winning streak
  winning_streaks <- trades %>%
    filter(winning_trade) %>%
    group_by(group_id) %>%
    summarise(winning_streak_length = n()) %>%
    ungroup()

  # Get the maximum length of consecutive winning trades
  MaxWinningStreak <- max(winning_streaks$winning_streak_length, na.rm = TRUE)

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
      LengthOfMaxRunUp = LengthOfMaxRunUp,
      ExpectedAbsoluteReturn = ExpectedAbsoluteReturn,
      CR = CR,
      MaxLosingStreak = MaxLosingStreak,
      MaxWinningStreak = MaxWinningStreak
    )
  )
},

# Risk and return performance metrics
compute_metrics = function(data_subset, symbol, run_via_cpp) {
    
    # Metrics for Active strategy
    active <- if (run_via_cpp) {
      estimate_trading_profile_cpp(data_subset, "Active")
    } else {
      private$estimate_trading_profile(data_subset, "Active", symbol)
    }

    # Metrics for Passive strategy
    passive <- if (run_via_cpp) {
      estimate_trading_profile_cpp(data_subset, "Passive")
    } else {
      private$estimate_trading_profile(data_subset, "Passive", symbol)
    }

  metrics_df <- data.frame(
    Strategy = c("Active", "Passive"),  
    ticker = symbol,

    # Return Metrics
    `Gross Profit` = c(active$GrossProfit, passive$GrossProfit),
    `Calmar Ratio` = c(active$CR, passive$CR),
    `Expected Absolute Return (per 1 trade)` = c(active$ExpectedAbsoluteReturn, "NotApplicable"),
    `Annualized Profit` = c(active$AnnualizedProfit, passive$AnnualizedProfit),
    `Largest Trade Win` = c(active$LargestWin, passive$LargestWin),
    #`Max Run Up` = c(active$MaxRunUp, passive$MaxRunUp),
    `Average Win` = c(active$AverageWin, passive$AverageWin),
    `Length of Average Win` = c(active$LengthOfAverageWin, passive$LengthOfAverageWin),
    `Max Winning Streak` = c(active$MaxWinningStreak, "NotApplicable"),

    # Risk Metrics
    `Max Drawdown` = c(active$MaxDrawdown, passive$MaxDrawdown),
    `Largest Trade Loss` = c(active$LargestLoss, passive$LargestLoss),
    `Average Loss` = c(active$AverageLoss, passive$AverageLoss),
    `Length of Average Loss` = c(active$LengthOfAverageLoss, passive$LengthOfAverageLoss),
    `Max Losing Streak` = c(active$MaxLosingStreak, "NotApplicable"),

    # Activity Metrics
    `Number of Trades Per Year` = c(active$NumberOfTradesPerYear, 0),
    `Percentage of Winning Trades` = c(active$PercentageOfWinningTrades, "NotApplicable"),
    check.names = FALSE

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

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ma_types, window_sizes, 
leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, output_df = FALSE,
run_via_cpp) {

  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (ma_type in ma_types) {
        for (flat_after_event in flats_after_event) {
          for (dynamic_limits in dynamics_limits) {
            for (max_risk in max_risks) {
              for(reward_ratio in reward_ratios) {
                for (leverage in leverages) {

      # Fetch data using DataFetcher for the current symbol and date range
      data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
      data <- data_fetcher$download_xts_data()

      # Create an instance of SMA1 strategy
      # gc()
      # sma_instance <- NULL
      sma_instance <- SMA1$new(data, window_size = window_size, ma_type = ma_type)

      # Ensure data is not empty
      if (nrow(data) == 0) {
        warning(paste("No data available for symbol:", symbol))
        next
      }
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sma_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- sma_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
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
        results[[paste(symbol, window_size, ma_type, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "SMA1",
          Window_Size = window_size,
          MA_Type = ma_type,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "strategy SMA1 | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size: ", window_size, 
          " | ma_type: ", ma_type, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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
    self$data <- self$data %>%
      mutate(
        ma1 = ma_func(Close, self$window_size1),
        ma2 = ma_func(Close, self$window_size2),
        signal = case_when(
          ma1 > ma2 ~ 1,    # Long signal when ma1 crosses above ma2
          ma1 < ma2 ~ -1,   # Short signal when ma1 crosses below ma2
          TRUE ~ 0          # No signal otherwise
        ),
        position = case_when(
          row_number() == 1 ~ 0,  # Ensure first row has no position
          TRUE ~ dplyr::lag(signal, default = 0) # Maintain previous position
        )
      )
},


run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ma_types, window_sizes1, window_sizes2, 
leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {

  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
        for (ma_type in ma_types) {
          for (flat_after_event in flats_after_event) {
            for(dynamic_limits in dynamics_limits) {
              for (max_risk in max_risks) {
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
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- sma2_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
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
        results[[paste(symbol, window_size1, window_size2, ma_type, flat_after_event, dynamic_limits, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "SMA2",
          Window_Size1 = window_size1,
          Window_Size2 = window_size2,
          MA_Type = ma_type,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: SMA2 | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size1: ", window_size1,
          " | window_size2: ", window_size2, 
          " | ma_type: ", ma_type, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
                  }
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size1 = x$Window_Size1,
        Window_Size2 = x$Window_Size2,
        MA_Type = x$MA_Type,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ma_types, window_sizes, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
      for (window_size in window_sizes) {
        for (ma_type in ma_types) {
          for (flat_after_event in flats_after_event) {
            for (dynamic_limits in dynamics_limits) {
              for (max_risk in max_risks) {
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
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- sma_instance$estimate_performance(
         symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
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
        results[[paste(symbol, window_size, ma_type, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "SMA1M",
          Window_Size = window_size,
          MA_Type = ma_type,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: SMA1M | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size: ", window_size, 
          " | ma_type: ", ma_type, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ma_types, window_sizes1, window_sizes2, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {

  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
        for (ma_type in ma_types) {
          for (flat_after_event in flats_after_event) {
            for(dynamic_limits in dynamics_limits) {
              for (max_risk in max_risks) {
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
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- sma2_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
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
        results[[paste(symbol, window_size1, window_size2, ma_type, flat_after_event, dynamic_limits, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "SMA2M",
          Window_Size1 = window_size1,
          Window_Size2 = window_size2,
          MA_Type = ma_type,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: SMA2M | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size1: ", window_size1,
          " | window_size2: ", window_size2, 
          " | ma_type: ", ma_type, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
                  }
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
        tibble(
          Symbol = x$Symbol,
          Class = x$Class,
          # Strategy specific:
          Methodology = x$Methodology,
          Window_Size1 = x$Window_Size1,
          Window_Size2 = x$Window_Size2,
          MA_Type = x$MA_Type,
          # RM:
          Flat = x$Flat,
          Dynamic_limits = x$Dynamic_limits
        ) %>%
          bind_cols(performance_data)
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

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ma_types, window_sizes1, window_sizes2, slines, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
        for (sline in slines) {
          for (ma_type in ma_types) {
            for (flat_after_event in flats_after_event) {
              for(dynamic_limits in dynamics_limits) {
                for (max_risk in max_risks) {
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
        # General:
        symbol = symbol,
        capital = capital,
        leverage = leverage,
        data_type = data_type,
        split_data = TRUE,
        cut_date = cut_date,
        window = slicing_years,
        # RM:
        apply_rm = apply_rm,
        flat_after_event = flat_after_event,
        dynamic_limits = dynamic_limits,
        max_risk = max_risk,
        reward_ratio = reward_ratio,
        run_via_cpp = run_via_cpp
      )
    } else {
      performance <- macd_instance$estimate_performance(
        # General:
        symbol = symbol,
        capital = capital,
        leverage = leverage,
        data_type = data_type,
        split_data = FALSE,
        cut_date = cut_date,
        window = slicing_years,
        # RM:
        apply_rm = apply_rm,
        flat_after_event = flat_after_event,
        dynamic_limits = dynamic_limits,
        max_risk = max_risk,
        reward_ratio = reward_ratio,
        run_via_cpp = run_via_cpp
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
    results[[paste(symbol, window_size1, window_size2, sline, ma_type, flat_after_event, dynamic_limits, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
      Symbol = symbol,
      Class = meta$assets[[symbol]]$class,
      Methodology = "MACD",
      Window_Size1 = window_size1,
      Window_Size2 = window_size2,
      Sline = sline,
      MA_Type = ma_type,
      Flat = flat_after_event,
      Dynamic_limits = dynamic_limits,
      Performance = performance
    )

    print(paste0(
      "Strategy: MACD | symbol: ", symbol, 
      " | class: ", meta$assets[[symbol]]$class, 
      " | window_size1: ", window_size1,
      " | window_size2: ", window_size2, 
      " | sline: ", sline, 
      " | ma_type: ", ma_type, 
      " | flat_after_event: ", flat_after_event,
      " | dynamic_limit: ", dynamic_limits,
      " | max_risk: ", max_risk, 
      " | reward_ratio: ", reward_ratio, 
      " | leverage: ", leverage,
      " |"
    ))
                    }
                  }
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
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size1 = x$Window_Size1,
        Window_Size2 = x$Window_Size2,
        Sline = x$Sline,
        MA_Type = x$MA_Type,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

# Define TurtleTrading (buy high, sell low)
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
},


run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, window_sizes1, window_sizes2, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {
          for (flat_after_event in flats_after_event) {
            for(dynamic_limits in dynamics_limits) {
              for (max_risk in max_risks) {
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
        tt_instance <- TurtleTrading$new(data, window_size1 = window_size1, window_size2 = window_size2)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- tt_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- tt_instance$estimate_performance(
           # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size1:", window_size1,
                        "window_size2:", window_size2
                        ))
          next
        }

        # Store the results
        results[[paste(symbol, window_size1, window_size2, flat_after_event, dynamic_limits, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "TurtleTrading",
          Window_Size1 = window_size1,
          Window_Size2 = window_size2,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: TurtleTrading | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size1: ", window_size1,
          " | window_size2: ", window_size2, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size1 = x$Window_Size1,
        Window_Size2 = x$Window_Size2,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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
},

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date,  window_sizes, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
        for (flat_after_event in flats_after_event) {
          for (dynamic_limits in dynamics_limits) {
            for (max_risk in max_risks) {
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
        dc_instance <- DonchianChannel$new(data, window_size = window_size)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- dc_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- dc_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size:", window_size
                        ))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "DonchianChannel:",
          Window_Size = window_size,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: DonchianChannel | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size: ", window_size, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

# Define Relative Strength Index class
RSI <- R6Class(
  "RSI",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    threshold_oversold = NULL,
    threshold_overbought = NULL,

initialize = function(data, window_size, threshold_oversold, threshold_overbought) {
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
},


run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, window_sizes, thresholds_oversold, thresholds_overbought, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {

  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (threshold_oversold in thresholds_oversold) {
        for (threshold_overbought in thresholds_overbought) {
          for (flat_after_event in flats_after_event) {
            for (dynamic_limits in dynamics_limits) {
              for (max_risk in max_risks) {
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
        rsi_instance <- RSI$new(data, window_size = window_size, threshold_oversold = threshold_oversold, threshold_overbought = threshold_overbought)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- rsi_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- rsi_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size:", window_size
                        ))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, threshold_oversold, threshold_overbought, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "RSI:",
          Window_Size = window_size,
          Threshold_Oversold = threshold_oversold,
          Threshold_Overbought = threshold_overbought,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: RSI | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size: ", window_size, 
          " | threshold_oversold: ", threshold_oversold,
          " | threshold_overbought: ", threshold_overbought,
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
                  }
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        Threshold_Oversold = x$Threshold_Oversold,
        Threshold_Overbought = x$Threshold_Overbought,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, accels, accels_max, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {

  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (accel in accels) {
      for (accel_max in accels_max) {
       for (flat_after_event in flats_after_event) {
          for (dynamic_limits in dynamics_limits) {
            for (max_risk in max_risks) {
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
        sar_instance <- StopAndReversal$new(data, accel = accel, accel_max = accel_max)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sar_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- sar_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "accel:", accel,
                        "accel_max:", accel_max
                        ))
          next
        }

        # Store the results
        results[[paste(symbol, accel, accel_max, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "SAR",
          Accel = accel,
          Accel_Max = accel_max,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: SAR | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | accel: ", accel,
          " | accel_max: ", accel_max, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Accel = x$Accel,
        Accel_max = x$Accel_max,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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
      DIp > lag(DIn) & lag(ADX) > self$trend_strength ~ 1, # lag
      lag(DIp) > lag(DIn) & lag(ADX) > self$trend_strength ~ 1, # lag
      DIp < lag(DIn) & lag(ADX) > self$trend_strength ~ -1,
      lag(DIp) < lag(DIn) & lag(ADX) > self$trend_strength ~ -1,
      TRUE ~ 0
    ),
    signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
    position = lag(signal, default = 0)
    ) %>%
    na.omit()
                    
},

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ndxs, trends_strength, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (ndx in ndxs) {
      for (trend_strength in trends_strength) {
       for (flat_after_event in flats_after_event) {
          for (dynamic_limits in dynamics_limits) {
            for (max_risk in max_risks) {
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
        adx_instance <- ADX$new(data, ndx = ndx, trend_strength = trend_strength)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- adx_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- adx_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol
                        ))
          next
        }

        # Store the results
        results[[paste(symbol, ndx, trend_strength, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "ADX:",
          Ndx = ndx,
          Trend_Strength = trend_strength,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: ADX | symbol: ", symbol, 
          " |class: ", meta$assets[[symbol]]$class, 
          " | ndx: ", ndx,
          " | trend_strength: ", trend_strength,
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Ndx = x$Ndx,
        Trend_Strength = x$Trend_Strength,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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
},

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, sd_mults, window_sizes, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (sd_mult in sd_mults) {
       for (flat_after_event in flats_after_event) {
          for (dynamic_limits in dynamics_limits) {
            for (max_risk in max_risks) {
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
        bb_instance <- BollingerBreakout$new(data, window_size = window_size, sd_mult = sd_mult)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- bb_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- bb_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol
                        ))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, sd_mult, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "BollingerBreakout",
          Window_Size = window_size,
          Sd_Mult = sd_mult,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: BB | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size: ", window_size, 
          " | sd_mult: ", sd_mult, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        Sd_Mult = x$Sd_Mult,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

# Define Volatility Mean Reversion class
VolatilityMeanReversion <- R6Class(
  "VolatilityMeanReversion",
  inherit = Strategy,
  public = list(
    window_size = NULL,

initialize = function(data, window_size, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
    },

generate_signals = function() {
  
  self$data <- super$estimate_range_potential(30)
  self$data <- self$data %>%
    mutate(
      signal = case_when(
        TR > lag(ATR) ~ -1,  # Increased volatility → Sell
        TR < lag(ATR) ~ 1,   # Reduced volatility → Buy
        TRUE ~ 0                # No signal
      ),
      position = lag(signal, default = 0)
    ) %>%
    na.omit()
},

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split, cut_date, ma_types, window_sizes, leverages, apply_rm, flats_after_event, dynamics_limits, max_risks, reward_ratios, run_via_cpp, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
     for (ma_type in ma_types) {
        for (flat_after_event in flats_after_event) {
          for (dynamic_limits in dynamics_limits) {
            for (max_risk in max_risks) {
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
        vmr_instance <- VolatilityMeanReversion$new(data, window_size = window_size)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- vmr_instance$estimate_performance(
          # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      } else {
        performance <- vmr_instance$estimate_performance(
        # General:
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          # RM:
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = dynamic_limits,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          run_via_cpp = run_via_cpp
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, ma_type, flat_after_event, dynamic_limits, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = "VMR:",
          Window_Size = window_size,
          MA_Type = ma_type,
          Flat = flat_after_event,
          Dynamic_limits = dynamic_limits,
          Performance = performance
        )

        print(paste0(
          "Strategy: VMR | symbol: ", symbol, 
          " | class: ", meta$assets[[symbol]]$class, 
          " | window_size: ", window_size,
          " | ma_type: ", ma_type, 
          " | flat_after_event: ", flat_after_event,
          " | dynamic_limit: ", dynamic_limits,
          " | max_risk: ", max_risk, 
          " | reward_ratio: ", reward_ratio, 
          " | leverage: ", leverage,
          " |"
          )
        )
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
    res_df <- bind_rows(lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      performance_data <- performance_data %>%
        mutate(Period = ifelse("from" %in% names(.), paste(from, "to", to), "Full Period")) %>%
        select(-from, -to, -ticker)  # Remove 'from', 'to', and 'ticker' columns

      # Add metadata columns
      tibble(
        Symbol = x$Symbol,
        Class = x$Class,
        # Strategy specific:
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        # RM:
        Flat = x$Flat,
        Dynamic_limits = x$Dynamic_limits
      ) %>%
        bind_cols(performance_data)
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

# Method to generate column of signals and positions
generate_signals = function() {

  histVolest <- private$estimate_realized_volatility(self$data)
  instr <- self$data %>% 
      as.data.frame() %>%
          rename_with(~ sub(".*\\.", "", .), everything()) %>%
              mutate(TradeDate = as.Date(rownames(.))) %>%
                  select(TradeDate, Open, High, Low, Close) %>%
                      mutate(value = as.numeric(log(Close/lag(Close)))) %>%
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
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = self$cluster,
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
  
  volForHistRoll <- private$set_signal_criteria(volForHistRoll) %>%
      mutate(Date = TradeDate) %>%
          as.tibble()

  self$data <- as.data.frame(self$data)
  self$data <- self$data %>% rename_with(~ sub(".*\\.", "", .), everything()) %>%
              mutate(Date = as.Date(rownames(.))) %>%
                  select(Date, High, Low, Open, value) %>%
                      left_join(select(volForHistRoll, Date, Close, signal, position)) %>%
                          na.omit %>%
                              as.tibble
}

  ),

private = list(

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
set_signal_criteria = function(volData) {
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

# Time series Analysis class to estimate time series patterns (stationarity, autocorrelation, seasonality, heteroscedasticity, ARCH effects, outliers, returns patterns)
TSA <- R6Class(
  "TSA",
  public = list(
    original_data = NULL,
    data = NULL,

initialize = function(data) {
      self$original_data <- data
      self$data <- private$preprocess_data(freq = "daily")
},
    
estimate_stationarity = function(freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      adf <- aTSA::adf.test(self$data$value, output = FALSE)[["type1"]] %>% data.frame()
      
      # Plot values
      if (plot_flag) {
        print(
          ggplot(self$data, aes(x = Date, y = value)) +
            geom_line() +
            labs(title = "Stationarity (ADF test)") +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme_minimal()
        )
      }
      return(adf)
},
    
estimate_autocorr_rets_vol = function(test, freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      switch(test,
             "rets" = {
               # ACF test for returns  
               acf <- acf(self$data$value, main = "Autocorrelation", plot = TRUE)
               return(list(acf = acf))
             },
             "vol" = {
               # ACF test for squared returns
               acf2 <- acf(self$data$value^2, main = "Volatility clustering", plot = TRUE)
               return(list(acf = acf2))
             }
      )
},
    
estimate_seasonality = function(freq = "daily") {
      self$data <- private$preprocess_data(freq)
      self$data <- ts(self$data$value, frequency = ifelse(freq == "daily", 26, 52))
      # Decompose the time series
      decomposed_data <- decompose(self$data) # decompose into 1) original ts, 2) trend component, 3) seasonal component, 4) residual component
      # Plot the decomposed components
      plot(decomposed_data)
},
    
estimate_heteroscedasticity = function(freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      
      # Fit a linear regression model to the squared log returns
      model <- lm(self$data$value^2 ~ self$data$value, data = data.frame(value = self$data$value))
      
      # Perform Breusch-Pagan test and print results
      bp_test <- bptest(model)
      print(bp_test)
      
      # Get the residuals from the linear regression model
      residuals <- residuals(model)
      
      # Create a data frame for the residuals
      residual_df <- data.frame(Residuals = residuals, Observation = 1:length(residuals))
      
      # Plot using ggplot
      if (plot_flag) {
        print(
          ggplot(residual_df, aes(x = Observation, y = Residuals)) +
            geom_point() +
            geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
            labs(title = "Residuals from Linear Regression Model",
                 x = "Observation Number",
                 y = "Residuals") +
            theme_minimal()
        )
      }
},

estimate_arch_effects = function(freq = "daily", p = 1, q = 1, plot_flag = TRUE) {
  
  self$data <- private$preprocess_data(freq)
  
  # Fit an ARIMA model
  fit <- arima(self$data$value, order=c(p,0,q))
  
  # Engle's ARCH test
  arch <- aTSA::arch.test(fit)
  return(arch)
},

estimate_outliers = function(test, freq = "daily", plot_flag = TRUE, q1 = NULL, q3 = NULL, threshold = 1.5) {
  self$data <- private$preprocess_data(freq)
  switch(
    test,
    # Super smoothing
    "supsmu" = {  
    ts_rets <- ts(self$data$value, frequency = 1)

    # Clean the time series using tsclean
    clean_ts <- tsclean(ts_rets)

    # Identify outliers using tsoutliers
    outliers <- tsoutliers(ts_rets)

    # Plot the original time series, cleaned time series, and outliers
    if (plot_flag) {
      print(
        autoplot(clean_ts, series = "Cleaned", color = 'red', lwd = 0.9) +
        autolayer(ts_rets, series = "Original", color = 'gray', lwd = 1) +
        geom_point(data = outliers %>% as.data.frame(),
        aes(x = index, y = replacements), col = 'blue') +
        labs(x = "Date", y = "Close Price", title = "Time Series with Outliers Highlighted (via SuperSmoothing 'tsoutliers' function)")
        )
      }
    
    # Create a data frame from the outliers list
    outliers_df <- data.frame(
      index = outliers$index,
      replacements = outliers$replacements
    )
    return(outliers_df)
    },

    "zscore" = {
    ts_rets <- ts(self$data$value, frequency = 1)
     # Calculate Modified Z-scores
     median_val <- median(ts_rets)
     mad_val <- mad(ts_rets)

     modified_z_scores <- abs(0.6745 * (ts_rets - median_val) / mad_val)

     # Identify outliers (e.g., Modified Z-score > 3.5)
     outliers_modified <- which(modified_z_scores > 3.5)

     # Plot outliers
      if (plot_flag) {
        print(
          ggplot(data.frame(Date = time(ts_rets), Value = as.numeric(ts_rets)), aes(x = Date, y = Value)) +
            geom_line() +
            geom_point(data = data.frame(Date = time(ts_rets)[outliers_modified], Value = ts_rets[outliers_modified]), aes(x = Date, y = Value), color = "red") +
            labs(title = "Time Series Data with Outliers Identified by Modified Z-Score")
        )
      }

      # Create a data frame from the outliers list
      outliers_df <- data.frame(
        index = time(ts_rets)[outliers_modified],
        replacements = ts_rets[outliers_modified]
      )
      return(outliers_df)
        },
      
    "fences" = {
    ts_rets <- ts(self$data$value, frequency = 1)
    # Calculate IQR
    Q1 <- quantile(ts_rets, q1)
    Q3 <- quantile(ts_rets, q3)
    IQR_val <- Q3 - Q1

    # Identify outliers (e.g., values < Q1 - 1.5 * IQR or values > Q3 + 1.5 * IQR)
    outliers_tukey <- which(ts_rets < (Q1 - threshold * IQR_val) | ts_rets > (Q3 + threshold * IQR_val))

    # Plot outliers
    if (plot_flag) {
      print(
    ggplot(data.frame(Date = time(ts_rets), Value = as.numeric(ts_rets)), aes(x = Date, y = Value)) +
        geom_line() +
        geom_point(data = data.frame(Date = time(ts_rets)[outliers_tukey], Value = ts_rets[outliers_tukey]), aes(x = Date, y = Value), color = "red", size = 2) +
        labs(title = "Time Series Data with Outliers Identified by Tukey's Fences")
            )   
          }
        }
      )
},

compute_wkd_rets = function(freq = "daily") {
  self$data <- private$preprocess_data(freq)
  self$data <- self$data %>%
    mutate(weekday = wday(Date, label = TRUE, abbr = TRUE)) 

  # Compute average return on weekdays
  avg_wkd_rets <- self$data %>%
    group_by(weekday) %>%
      summarize(avg_return = mean(value, na.rm = TRUE)) %>%
        arrange(weekday)
  
  # Compute longest consequtive streak of positive and negative weekday returns
  positive <- self$data %>%
    group_by(weekday) %>%
      summarise(longest_series_weekdays = max(rle(value > 0)$lengths * (rle(value > 0)$values))) %>%
          ungroup()

  negative <- self$data %>%
    mutate(weekday = wday(Date, label = TRUE, abbr = TRUE)) %>%
      group_by(weekday) %>%
        summarise(longest_series_weekdays = max(rle(value < 0)$lengths * (rle(value < 0)$values))) %>%
            ungroup()

  res_longest <- merge(positive, negative, by = "weekday", all = TRUE) %>%
    rename(longest_positive = longest_series_weekdays.x, longest_negative = longest_series_weekdays.y) %>% 
      arrange(weekday)

  # Test hypothesis if any weekday return is statistically different from rets mean return

  # Overall mean return
  overall_mean <- mean(self$data$value)

  # Perform Wilcoxon signed-rank test for each weekday
  avg_wkd_rets <- avg_wkd_rets %>%
    rowwise() %>%
      mutate(
    test_statistic = wilcox.test(self$data$value, mu = avg_return, alternative = "two.sided")$statistic,
    p_value = format(wilcox.test(self$data$value, mu = avg_return, alternative = "two.sided")$p.value, nsmall = 5)
  )

  return(list(avg_wkd_rets = avg_wkd_rets, res_longest = res_longest))
},

compute_summary_statistics = function(freq = "daily") {
  self$data <- private$preprocess_data(freq)
  summary_stats <- self$data %>%
    summarise(
      mean_return = mean(value),
      median_return = median(value),
      sd_return = sd(value),
      skewness = skewness(value),
      kurtosis = kurtosis(value)
    )
  
  return(summary_stats)
}

  ), # end of public list arguments
  
private = list(

preprocess_data = function(freq) {
      # Convert xts data into tibble 
      data <- data.frame(self$original_data)  
      data <- data %>%
        rename_with(~ sub(".*\\.", "", .), everything()) %>%
        mutate(Date = as.Date(rownames(data))) %>%
        select(Date, everything()) %>%
        na.omit() %>%
        as.tibble()
      
      # Choose values frequency: daily overlapping values or bi-weekly non-overlapping 
      switch(
        freq,
        "daily" = {
          data <- data
        },
        "biweekly" = {
          bdates <- seq.Date(
            from = (from_date + 0:6)[weekdays(from_date + 0:6) %in% "Wednesday"], 
            to = (to_date - 0:6)[weekdays(to_date - 0:6) %in% "Wednesday"], by = 14)
            data <- data %>% filter(Date %in% bdates)
        },
        stop("Invalid value for 'freq' argument. Choose 'daily' or 'biweekly'.")
      )
      return(data)
    }
  )
)