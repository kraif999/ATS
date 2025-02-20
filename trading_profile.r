# Copyright (c) 2024 Oleh Bilyk

source("backtesting_trading_strategies/libraries.R")
source("backtesting_trading_strategies/strategies.R")
options(scipen = 999)

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
estimate_performance = function(symbol, capital, leverage, data_type, split_data, cut_date, window, apply_rm, flat_after_event, dynamic_limits, max_risk, reward_ratio) {
  
  # Slice self$data using the private slicer method
  self$data <- private$slicer(self$data, cut_date, data_type)

  # Generate signals
  self$generate_signals()

  # Estimate volatility over the period of 30 days (after signal generation):
  self$estimate_range_potential(30)

  ##########################################################################################################################
  # Update position type (long/short) given risk management option; compute: number of positions, daily PnL, portfolio value
  ##########################################################################################################################
  if(apply_rm) {
    self$data <- private$apply_risk_management(self$data, max_risk, reward_ratio, leverage, capital, flat_after_event, dynamic_limits = FALSE)
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
    if (eqlActive == 0) {
      self$data$position[i] <- 0
      self$data$Liquidation[i] <- TRUE
    } else {
    self$data$Liquidation[i] <- self$data$Liquidation[i - 1]  # Carry forward liquidation status
  }
    
    prev_nop_Active <- self$data$nopActive[i - 1]
    
    # Handle position change (for active portfolio)
    if (self$data$position[i] != self$data$position[i - 1]) {
        self$data$nopActive[i] <- max(0, eqlActive * leverage / self$data$Close[i], 0)
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
  
  ########################################################################################################################
  # Adding additional metrics (annualized volatility, cumulative daily PnL, and portfolio daily return)
  ########################################################################################################################
  self$data <- self$data %>%
    mutate(
      annual_vol = rollapply(value, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(365),
      pnlActiveCumulative = cumsum(replace_na(pnlActive, 0)),
      pnlPassiveCumulative = cumsum(replace_na(pnlPassive, 0)),
      r_eqlActive = (eqlActive - lag(eqlActive)) / lag(eqlActive),
      r_eqlPassive = (eqlPassive - lag(eqlPassive)) / lag(eqlPassive)
    )

  ########################################################################################################################
  # Estimate trading profile
  ########################################################################################################################
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
get_trades = function(apply_rm) {

  self$data$trade_id <- cumsum(c(0, diff(self$data$position) != 0))
  
  # Copy trade_id to trade_id_m
  self$data$trade_id_m <- self$data$trade_id
  
  # Update trade_id_m where pnlActiveType == "R"
  self$data$trade_id_m <- ifelse(self$data$pnlActiveType == "R", dplyr::lag(self$data$trade_id_m, default = first(self$data$trade_id_m)), self$data$trade_id_m)
  
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
    entry_account_size = round(if_else(trade_id_m != lag(trade_id_m), eqlActive, NA_real_), 0),
    # Exits
    exit = if_else(trade_id_m != lead(trade_id_m), Date, as.Date(NA)),
    exit_price = if_else(trade_id_m != lead(trade_id_m), Close, NA_real_),
    exit_size = if_else(trade_id_m != lead(trade_id_m), nopActive, NA_real_),
    exit_account_size = round(if_else(trade_id_m != lead(trade_id_m), eqlActive, NA_real_), 0)
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
    BalanceEnd = BalanceStart + TradePnL
  ) %>%
  
  ungroup() %>%
  mutate(
    RunningPnL = round(cumsum(TradePnL), 0),
    Efficiency = round((TradePnL / abs(RunningPnL)) * 100, 0)
  ) %>%
  
  rename(`TradePnL/RunningPnL,%` = Efficiency) %>%
  
  select(
    Trade, ExitBy, Start, End, Size, EntryPrice, ExitPrice, BalanceStart, TradePnL, BalanceEnd, RunningPnL, `TradePnL/RunningPnL,%`
  )

  # Generate PnL histogram
  range_pnl <- range(trades$TradePnL[is.finite(trades$TradePnL) & trades$TradePnL != 0], na.rm = TRUE)
  x_breaks <- seq(range_pnl[1], range_pnl[2], length.out = 11)
  
  pnl_hist <- ggplot(data = data.frame(TradePnL = trades$TradePnL[is.finite(trades$TradePnL) & trades$TradePnL != 0]),
                     aes(x = TradePnL, fill = TradePnL < 0)) +
    geom_histogram(binwidth = diff(range_pnl) / 100, color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("green", "red")) +
    labs(title = "Trade Profit and Loss (PnL) Distribution", x = "Trade PnL", y = "Frequency") +
    scale_x_continuous(expand = c(0, 0), limits = range_pnl, breaks = x_breaks) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal()
  
  return(list(trades = trades, plot = pnl_hist))  

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
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
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
}

  ),

private = list(

# Apply stop loss and profit take
apply_risk_management = function(data, max_risk, reward_ratio, leverage, capital, flat_after_event, dynamic_limits) {
  
  data$position[1] <- 0
  eqlActive <- capital
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
      pnlActiveType = "U",
      Liquidation = FALSE # liquidation when account balance is equal to 0
    )
  
  # Iterate over each row in the data
  for (i in 2:nrow(data)) {

    # No positions after liquidation
    if (eqlActive == 0) {
      data$position[i] <- 0  # Force position to 0 if equity is depleted
      data$Liquidation[i] <- TRUE  # Mark liquidation
    } else {
      data$Liquidation[i] <- data$Liquidation[i - 1]  # Carry forward liquidation status
    }
    
    if (flat_after_event && flat) {
      data$position[i] <- 0  # Stay flat after reversal
    }
    
    if (!is.na(reversed_position)) {
      data$position[i] <- reversed_position
      reversed_position <- NA
    } 
    
      #if (data$position[i] != data$position[i - 1] || !is.na(data$pnlActiveType[i - 1]) && data$pnlActiveType[i - 1] == "R") {
      if (data$position[i] != previous_position || !is.na(data$pnlActiveType[i - 1]) && data$pnlActiveType[i - 1] == "R") {
      data$nopActive[i] <- max(0, eqlActive * leverage / data$Close[i])
      
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
      # Carry forward previous values
      data$nopActive[i] <- data$nopActive[i - 1]

      # Apply dynamic updates if enabled
      if (dynamic_limits) {

      # Compute rm_update dynamically
      rm_update <- data$eqlActive[i] / data$eqlActive[i - 1]

      # Ensure no division by zero or extreme values
      rm_update <- ifelse(is.na(rm_update) | rm_update <= 0, 1, rm_update)

      # Only adjust SL/PT if equity increased (favorable move)
      if (data$position[i] == 1) {  # Long
        stopLoss <- max(stopLoss, data$Close[i] - (rm_update * max_risk * eqlActive / data$nopActive[i]))
        profitTake <- max(profitTake, data$Close[i] + (reward_ratio * max_risk * eqlActive / data$nopActive[i]))  # PT always moves up
      } else if (data$position[i] == -1) {  # Short
        stopLoss <- min(stopLoss, data$Close[i] + (rm_update * max_risk * eqlActive / data$nopActive[i]))
        profitTake <- min(profitTake, data$Close[i] - (reward_ratio * max_risk * eqlActive / data$nopActive[i]))  # PT always moves down
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
        
    if (i > 2 && data$group[i] != data$group[i - 1] && is.na(data$eventSL[i]) && is.na(data$eventPT[i])) {
      # is.na(data$eventSL[i]) && is.na(data$eventPT[i]) : when signal changes and no pending reversals
      flat <- FALSE
    }
    
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
        data$pnlActive[i] <- (data$Close[i - 1] - data$Close[i]) * data$position[i] * data$nopActive[i - 1]
      } else {
        data$pnlActive[i] <- if (data$position[i] == 0) 0 else round((data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1], 2)
      }
    }

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
  
  # Count the number of unique year-month combinations
  # num_months <- length(unique(format(data$Date, "%Y-%m")))
  
  # # Print average stop-loss and profit-take events per month
  # print(paste0("Stop Losses occur every: ", round(1 / ((sum(data$eventSL, na.rm = TRUE) / num_months)),0), " month(s)"))
  # print(paste0("Average Profit Takes per Month: ", round(1 / ((sum(data$eventPT, na.rm = TRUE) / num_months)),0), " month(s)"))
  
  return(data)
},

# Estimate trading profile of a strategy
estimate_trading_profile = function(data_subset, strategy_type) {

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
  AverageWinLength <- tryCatch({data_subset %>%
  transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
  aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
  subset(cum_pnl > 0) %>%
  {if (nrow(.) == 0) return(NA) else .} %>%
  merge(data_subset, by = "trade_id") %>%
  aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
  with(round(mean(Date, na.rm = TRUE)))}, error = function(e) NA)
  
  # 8. Largest Loss
  LargestLoss <- round(min(data_subset[[pnl_col]], na.rm = TRUE),0)

  # 9. Length of Largest Loss
  LengthOfLargestLoss <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.min(data_subset[[pnl_col]])], ], 
                              as.numeric(max(Date) - min(Date) + 1))

  # 10. Average Loss
  AverageLoss <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] < 0], na.rm = TRUE),0)

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

  peak_idx <- which.max(drawdown_data[[eql_col]][1:which.min(drawdown_data$drawdown)])  # Peak before max drawdown
  trough_idx <- which.min(drawdown_data$drawdown)  # Trough for max drawdown

  StartDateMaxDrawdown <- as.Date(drawdown_data$Date[peak_idx])
  EndDateMaxDrawdown <- as.Date(drawdown_data$Date[trough_idx])

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
  if (length(StartDateMaxRunUp) == 0 || length(EndDateMaxRunUp) == 0) {
    LengthOfMaxRunUp <- NA
    StartDateMaxRunUp <- NA
    EndDateMaxRunUp <- NA
  } else {
    LengthOfMaxRunUp <- as.numeric(EndDateMaxRunUp - StartDateMaxRunUp)
  }

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
},

# Risk and return performance metrics
compute_metrics = function(data_subset, symbol) {
    
  # Metrics for Active strategy
  active <- private$estimate_trading_profile(data_subset, "Active")

  # Metrics for Passive strategy
  passive <- private$estimate_trading_profile(data_subset, "Passive")

  # Combine metrics into a dataframe
  metrics_df <- data.frame(
    Strategy = c("Active", "Passive"),
    ticker = symbol,
    `Gross Profit` = c(active$GrossProfit, passive$GrossProfit),
    `Annualized Profit` = c(active$AnnualizedProfit, passive$AnnualizedProfit),
    `Number of Trades Per Year` = c(active$NumberOfTradesPerYear, passive$NumberOfTradesPerYear),
    `Percentage of Winning Trades` = c(active$PercentageOfWinningTrades, "NotApplicable"),
    `Average Win` = c(active$AverageWin, passive$AverageWin),
    `Length of Average Win` = c(active$LengthOfAverageWin, passive$LengthOfAverageWin),
    `Largest Win` = c(active$LargestWin, passive$LargestWin),
    `Length of Largest Win` = c(active$LengthOfLargestWin, passive$LengthOfLargestWin),
    `Average Loss` = c(active$AverageLoss, passive$AverageLoss),
    `Length of Average Loss` = c(active$LengthOfAverageLoss, passive$LengthOfAverageLoss),
    `Largest Loss` = c(active$LargestLoss, passive$LargestLoss),
    `Length of Largest Loss` = c(active$LengthOfLargestLoss, passive$LengthOfLargestLoss),
    `Average Winning Run` = c(active$AverageWinningRun, passive$AverageWinningRun),
    `Length of Time in Average Winning Run` = c(active$LengthOfTimeInAverageWinningRun, passive$LengthOfTimeInAverageWinningRun),
    `Largest Winning Run` = c(active$LargestWinningRun, passive$LargestWinningRun),
    `Length of Time in Largest Winning Run` = c(active$LengthOfTimeInLargestWinningRun, passive$LengthOfTimeInLargestWinningRun),
    `Average Losing Run` = c(active$AverageLosingRun, passive$AverageLosingRun),
    `Length of Time in Average Losing Run` = c(active$LengthOfTimeInAverageLosingRun, passive$LengthOfTimeInAverageLosingRun),
    `Largest Losing Run` = c(active$LargestLosingRun, passive$LargestLosingRun),
    `Length of Time in Largest Losing Run` = c(active$LengthOfTimeInLargestLosingRun, passive$LengthOfTimeInLargestLosingRun),
    `Max Drawdown` = c(active$MaxDrawdown, passive$MaxDrawdown),
    `Length of Max Drawdown` = c(active$LengthOfMaxDrawdown, passive$LengthOfMaxDrawdown),
    `Start Date Max Drawdown` = c(as.Date(active$StartDateMaxDrawdown), as.Date(passive$StartDateMaxDrawdown)),
    `End Date Max Drawdown` = c(as.Date(active$EndDateMaxDrawdown), as.Date(passive$EndDateMaxDrawdown)),
    `Max Run-Up` = c(active$MaxRunUp, passive$MaxRunUp),
    `Length of Max Run-Up` = c(active$LengthOfMaxRunUp, passive$LengthOfMaxRunUp),
    `Start Date Max Run-Up` = c(as.Date(active$StartDateMaxRunUp), as.Date(passive$StartDateMaxRunUp)),
    `End Date Max Run-Up` = c(as.Date(active$EndDateMaxRunUp), as.Date(passive$EndDateMaxRunUp)),
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

run_backtest = function(symbols, from_date, to_date, slicing_years, data_type, split_data, cut_date,  ma_types,  window_sizes, leverages, apply_rm, flats_after_event, max_risks, reward_ratios, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (ma_type in ma_types) {
        for (flat_after_event in flats_after_event) {
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
          symbol = symbol,
          capital = capital,
          leverage = leverage,
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = FALSE,
          max_risk = max_risk,
          reward_ratio = reward_ratio
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
          apply_rm = apply_rm,
          flat_after_event = flat_after_event,
          dynamic_limits = FALSE,
          max_risk = max_risk,
          reward_ratio = reward_ratio
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
        results[[paste(symbol, window_size, ma_type, flat_after_event, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("SMA1:", window_size, ma_type),
          Window_Size = window_size,
          MA_Type = ma_type,
          Flat = flat_after_event,
          Max_Risk = max_risk,
          Reward_Ratio = reward_ratio,
          Performance = performance
        )

        print(paste0(
          "SMA1 strategy (symbol: ", symbol, 
          ", class: ", meta$assets[[symbol]]$class, 
          ", window_size: ", window_size, 
          ", ma_type: ", ma_type, 
          ", flat_after_event: ", flat_after_event,
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
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        Flat = x$Flat,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio
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

# Specify the following strategy parameters
from_date <- as.Date("2022-01-01") 
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
source("strategies.R")
# IN-SAMPLE (WITHOUT SPLIT)
sma1 <- SMA1$new(ts, window_size = 20, ma_type = 'EMA')
sma1$estimate_range_potential(n=14)

# in-sample:
sma1_res_in_sample <- t(
  sma1$estimate_performance(
  symbol = symbol,
  capital = capital,
  leverage = leverage,
  data_type = "in_sample", 
  split_data = FALSE, 
  cut_date = as.Date("2024-06-30"), 
  window = 0.5, 
  apply_rm = apply_rm, 
  flat_after_event = flat_after_event,
  dynamic_limits = dynamic_limits,
  max_risk = 0.1, 
  reward_ratio = 3
    )
  )

sma1_res_in_sample_dt <- cbind(Metric = rownames(sma1_res_in_sample), as.data.table(as.data.frame(sma1_res_in_sample, stringsAsFactors = FALSE)))

sma1_res_in_sample_dt[, units := ifelse(
  .I <= 5 | Metric %in% c("max_risk", "Strategy", "reward_ratio", "Number of Trades Per Year"), "",
  ifelse(
    Metric %in% c("Annualized Profit", "Percentage of Winning Trades", "Max Drawdown", "Max Run-Up"), "%",
    ifelse(
      Metric %in% c("Length of Largest Win", "Length of Largest Loss", "Length of Average Win", "Length of Average Loss", 
                    "Length of Max Drawdown", "Length of Max Run-Up", "Length of Time in Largest Winning Run", "Length of Time in Largest Losing Run", 
                    "Length of Time in Average Winning Run", "Length of Time in Average Losing Run", "Largest Winning Run", "Largest Losing Run",
                    "Average Winning Run", "Average Losing Run"), "days",
      ifelse(
        grepl("Date", Metric), "Date", 
        "USD"  # Default case for other rows
      )
    )
  )
)]

dataset <- sma1$data
dataset <- sma1$data %>% select(
    Date, Open, Close, position, stopLoss, profitTake, signal, position, pnlActiveType,
    eventSL, eventPT,
    nopActive, pnlActive, eqlActive, pnlActiveCumulative, Liquidation)

trades <- sma1$get_trades(apply_rm = apply_rm)$trades
View(trades)

sma1$plot_equity_lines("SMA1", signal_flag = FALSE, capital, symbol)
plots <- sma1$plot_close_vs_vol(30)
grid.arrange(plots$close, plots$n, ncol = 1)
sma1$plot_rm_levels(30, apply_rm = apply_rm)
sma1$get_trades(apply_rm = apply_rm)$plot

# IN-SAMPLE (WITHOUT SPLIT)

meta <- jsonlite::fromJSON("instr_config.json")

# Overall trading profile (NO SPLIT with stop-loss)
sma1 <- SMA1$new(ts, window_size = 20, ma_type = 'EMA')
btc_sma1_in_sample_no_split <- sma1$run_backtest(
  symbols = c("BTC-USD"),
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-01-01"),
  slicing_years = 4,
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  ma_types = c("SMA", "EMA"), 
  window_sizes = round(10 * (1.25 ^ (0:13))),
  leverages = seq(1, 2, by = 1),
  apply_rm = TRUE,
  flats_after_event = c(TRUE, FALSE),
  max_risks = seq(0.1, 0.3, by = 0.1),
  reward_ratios = seq(2,3, by = 1),
  output_df = TRUE
)

#fwrite(btc_sma1_in_sample_no_split, "/bin/res_sma1_btc.csv")
btc_sma1_in_sample_no_split <- fread("bin/res_sma1_btc.csv")

# Backtest visualization
ggplot(btc_sma1_in_sample_no_split %>% filter(leverage == 2), aes(x = Window_Size, y = `Annualized Profit`)) +
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
