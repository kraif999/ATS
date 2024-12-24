# Copyright (c) August 2024 Oleh Bilyk
# Price-time based strategies

# September the 3rd, 2024

# Apply historical time period split + 
# Add method to extract the list of all trades
# Add plot with equity line and raw timeseries (segmented: with vertical lines for start/end dates) +
# Modify what metrics is being computed (add trade list across date, % of winning signals, etc.)
# Review Search and Judgement section in Robert Pardo

# September the 4th, 2924
# Try more advanced optimization techniques (currently brute force = grid search is used only, see "run_backtest" function)
# Hill Climbing Search Algorithms

# December 12th, 2024
# GOAL: THE ENHANCED EVALUATION PROFILE
# Add more metrics in "estimate_performance" method in Strategy class, streamline the information, the output is df with Strategy name and all metrics across all tested periods
# Answer the question how the strategy behaves given different regimes (upward/downward trend, flatten conditions, etc.)
# MACROSCOPIC LEVEL (table with metrics for a Strategy) Vs MICROSCOPIC LEVEL (list of trades occurred at time t)
# GRAPHICAL Vs TABULAR

# Compare periodically a test profile with a trade profile (50% - 150%)

############### Strategy based on Gerchyk ideas (June 2024) ################

######################################################
# Specify trading strategy parameters
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()
symbol <- "BTC-USD" # bitcoin
capital <- 50000 # units of initial capital invested
leverage <- 1 # financial leverage used to calculate number of positions in estimate_performance in Strategy class
# Also, it is assumed 100% portfolio investment (number of positions =  capital / price per unit).
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()
data_fetcher$plot_close_or_rets(type = "close")

# Convert to tibble format which is further used in Strategy:
ts_df <- data.frame(Date = index(ts), coredata(ts)) %>%
  rename_with(~ sub(".*\\.", "", .), everything()) %>%
  na.omit() %>%
  as_tibble()

head(ts_df)
tail(ts)

# Historical levels
ts_df <- ts_df %>%
  mutate(
    # max
    global_max = cummax(ts_df$Close),
    local_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
    local_half_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
    local_month_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
    local_week_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE), # week
    
    # min
    global_min = cummin(ts_df$Close), # not important
    local_year_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
    local_half_year_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
    local_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
    local_week_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE) # week
    
  ) 


library(dplyr)
library(slider)
library(plotly)

# Define the function to plot candlestick chart with horizontal historical levels
plot_candles <- function(df, ndays) {
  
  # Get the last ndays of data
  df_tail <- tail(df, ndays)
  
  # Latest date in the tail data
  latest_date <- max(df_tail$Date)
  
  # Calculate historical levels
  ts_df <- df %>%
    mutate(
      # max
      global_max = cummax(Close),
      local_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
      local_half_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
      local_month_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
      local_week_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE), # week
      
      # min
      local_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
      local_week_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE) # week
    )
  
  # Get the latest levels for the horizontal lines
  latest_levels <- ts_df %>%
    filter(Date == latest_date) %>%
    select(local_year_max, local_half_year_max, local_month_max, local_week_max,
           local_month_min, local_week_min) %>%
    slice(1)
  
  # Plot the candlestick chart
  fig <- df_tail %>% plot_ly(x = ~Date, type= "candlestick",
                             open = ~Open, close = ~Close,
                             high = ~High, low = ~Low) 
  
  # Add horizontal lines for historical levels
  fig <- fig %>% 
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_year_max, nrow(df_tail)),
              line = list(color = 'blue', dash = 'dash', width = 1), name = 'Year Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_half_year_max, nrow(df_tail)),
              line = list(color = 'green', dash = 'dash', width = 1), name = 'Half Year Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_max, nrow(df_tail)),
              line = list(color = 'red', dash = 'dash', width = 1), name = 'Month Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_max, nrow(df_tail)),
              line = list(color = 'purple', dash = 'dash', width = 1), name = 'Week Max') %>%
    
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_min, nrow(df_tail)),
              line = list(color = 'red', dash = 'dot', width = 1), name = 'Month Min') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_min, nrow(df_tail)),
              line = list(color = 'purple', dash = 'dot', width = 1), name = 'Week Min')
  
  # Add layout title
  fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
  
  # Return the plot
  fig
}

plot_candles(ts_df, 30)

# Building blocks
# Risk management: 3R vs R
# Money management: position sizing
# Estimate performance (adjust estimate_performance in Strategy class or write new method in Nero class, the latter is preffered)


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
    # UseMethod("mutate") : no applicable method for 'mutate' applied to an object of class "c('xts', 'zoo')"
    #ts$value <-  log(ts[,paste0(self$symbol, ".Close")]) - log(lag(ts[,paste0(self$symbol, ".Close")]))
    ts$value <- log(ts[, grep("\\.Close$", colnames(ts))]) - log(lag(ts[, grep("\\.Close$", colnames(ts))]))
    ts <- na.omit(ts)
    attr(ts, "na.action") <- NULL

    return(ts)
},

# Cut the data into in-sample or out-of-sample
slicer = function(data, cut_date, sample_type) {
  # Ensure data is provided and is an xts object
  if (is.null(data) || !inherits(data, "xts")) {
    stop("A valid xts object must be provided as data.")
  }

  # Ensure cut_date is of class Date
  if (!inherits(cut_date, "Date")) {
    stop("cut_date must be of class Date.")
  }

  # Use switch to handle sample_type
  result <- switch(
    sample_type,
    "in-sample" = data[index(data) <= cut_date, ],
    "out-of-sample" = data[index(data) >= cut_date, ],
    stop("sample_type must be either 'in-sample' or 'out-of-sample'.")
  )

  return(result)
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

# Historical period
# Segments of historical period (for example, the consecutive periods of 2-years within 10-years entire period)
# Busket of instruments/markets (selected from instr_config.json file)

######################################################
# Specify trading strategy parameters
######################################################
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()
symbol <- "BTC-USD" # cryptocurrencies
capital <- 50000 # units of initial capital invested
leverage <- 1 # financial leverage used to calculate number of positions in estimate_performance in Strategy class
# Also, it is assumed 100% portfolio investment (number of positions =  capital / price per unit).
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()
ts_in_sample <- data_fetcher$slicer(ts, as.Date("2024-01-01"), "in-sample")
ts_in_sample %>% tail

data_fetcher$plot_close_or_rets(type = "close")
data_fetcher$plot_close_or_rets(type = "rets")
data_fetcher$plot_close_or_rets(type = "rets_hist")
data_fetcher$compute_NA_close_price_ratio()

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

# Performance estimation for Active and Passive strategies (annualized return, standard deviation, Information ratio, maximum drawdown, trades count, success prediction rate)
estimate_performance = function() {

  self$generate_signals()  # Call generate_signals dynamically

  self$data <- self$data %>% 
    mutate(
      pnlActive = c(0, diff(Close) * signal[-length(Close)]),
      pnlPassive = c(0, diff(Close)),
      nopActive = 0,
      nopPassive = 0,
      eqlActive = 0,
      eqlPassive = 0,
      From = as.Date(NA),
      To = as.Date(NA)
    )

  # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
  self$data$eqlActive[1] <- capital
  self$data$nopActive[1] <- floor(capital / (self$data$Close[1] / leverage))
  self$data$eqlPassive[1] <- capital
  self$data$nopPassive[1] <- floor(capital / (self$data$Close[1] / leverage))  

  # Check if sizing columns exist
  has_L <- "nop_sizing" %in% names(self$data)
  has_Vol <- "vol_nop_sizing" %in% names(self$data)

  for (i in 2:nrow(self$data)) {
    # Active
    pnlActive <- self$data$pnlActive[i]
    prev_nop_Active <- floor(self$data$nopActive[i - 1])
    current_nop_Active <- floor((self$data$eqlActive[i - 1]) / (self$data$Close[i] / leverage))
    self$data$eqlActive[i] <- self$data$eqlActive[i - 1] + prev_nop_Active * pnlActive

    # Calculate nopActive based on the presence of the "L" column
    if (has_L) {
      self$data$nopActive[i] <- ifelse(
        self$data$L[i], 
        current_nop_Active * self$data$nop_sizing[i], 
        current_nop_Active
      )
    } else if (has_Vol) {
      self$data$nopActive[i] <- ifelse(
        self$data$vol_nop_sizing[i], 
        current_nop_Active * self$data$vol_nop_sizing[i], 
        current_nop_Active
      )
    } else {
      self$data$nopActive[i] <- current_nop_Active
    }

    # Passive
    pnlPassive <- self$data$pnlPassive[i]
    prev_nop_Passive <- floor(self$data$nopPassive[i - 1])
    current_nop_Passive <- floor((self$data$eqlPassive[i - 1]) / (self$data$Close[i] / leverage))
    self$data$eqlPassive[i] <- self$data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
    self$data$nopPassive[i] <- current_nop_Passive
  }

    #   self$data <- self$data %>%
    #     mutate(
    #       r_eqlActive = quantmod::Delt(eqlActive),
    #       r_eqlPassive = quantmod::Delt(eqlPassive)
    #     )

    self$data <- self$data %>%
        mutate(
            r_eqlActive = (eqlActive - lag(eqlActive)) / lag(eqlActive),
            r_eqlPassive = (eqlPassive - lag(eqlPassive)) / lag(eqlPassive)
            )

  # Ensure Date is in Date format and sorted
  self$data <- self$data %>%
    mutate(Date = as.Date(Date)) %>%
    arrange(Date)

  # Define the start and end dates
  start_date <- min(self$data$Date)
  end_date <- max(self$data$Date)

  # Create a sequence of 2-year periods
  period_start <- start_date
  period_end <- period_start %m+% years(2) - days(1)  # 2-year period

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
      metrics <- private$compute_metrics(data_period)
      metrics$from <- period_start
      metrics$to <- current_end
      performance_list[[length(performance_list) + 1]] <- metrics
    }

    # Move to the next period
    period_start <- period_start %m+% years(2)
    period_end <- period_start %m+% years(2) - days(1)
  }

  # Combine all period performances into one data frame
  performance_df <- bind_rows(performance_list) %>%
    select(ticker, from, to, Strategy, everything())

  return(performance_df)
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

# Get the list of all trades
get_trades = function() {

# Original entry and reversal (if such)

},

# Calculate cumulative return (method to be removed)
calculate_cumulative_return = function() {
      #self$generate_signals()  # Call generate_signals dynamically
      cret = data.frame(
        Active = prod(1+self$data$value * self$data$position) - 1,
        Buy_and_hold = prod(1+self$data$value) - 1
      )
      return(cret)
},

# Visualize equity lines for active strategy and passive (buy and hold)
plot_equity_lines = function(strategy_name, signal_flag = FALSE) {
  # Line size
  active_line_size <- ifelse(signal_flag, 1, 0.8)
  passive_line_size <- ifelse(signal_flag, 1, 0.8)
  
  # Plot equity lines
  p <- ggplot(self$data, aes(x = Date)) +
    labs(title = paste0("Equity Lines for Active ", "(", as.character(strategy_name), ")", " and Passive (buy-and-hold) strategies"),
         x = "Date",
         y = "Equity line") +
    theme_minimal()

  # Add signals if signal_flag is TRUE
  if (signal_flag) {
    p <- p +
      geom_vline(data = self$data[self$data$signal == -1, ], aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$signal == 1, ], aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "green", alpha = 0.5) +
      scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "green"))
  }

  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")

  # Print or return the plot
  if (signal_flag) {
    print(p)
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
estimate_average_true_range = function(ts, n = 14) { # 14 days by default
  atr <- ATR(HLC(ts), n)
  atr_data <- data.frame(Date = index(atr), coredata(atr))
  ts_data <- data.frame(Date = index(ts), coredata(ts))
  combined_data <- merge(ts_data, atr_data, by = "Date") %>%
    #mutate(tr_reserve = 1 - tr/atr * 100)
    mutate(tr_reserve = tr/atr * 100)
  return(combined_data)
}

# Money management rules (risk per trade, position sizing, diversification, profit taking, risk tolerance level) are to be added

  ),
    
private = list(

compute_metrics = function(data_subset) {

    aR_active <- round(as.numeric(Return.annualized(as.numeric(data_subset$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
    aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data_subset$r_eqlActive), scale = 252) * 100), 3)
    IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
    MD_active <- round(as.numeric(maxDrawdown(as.numeric(data_subset$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
    trades_active <- sum(diff(data_subset$position) != 0) + 1
    #trades_active <- nrow(data_subset)

    aR_passive <- round(as.numeric(Return.annualized(as.numeric(data_subset$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
    aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data_subset$r_eqlPassive), scale = 252) * 100), 3)
    IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
    MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data_subset$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
    trades_passive <- 1

    unique_months <- length(unique(format(data_subset$Date, "%Y-%m")))

    # Calculate the number of winning and losing trades
    df <- data_subset[!is.na(data_subset$r_eqlActive) & !is.na(data_subset$position),]
    df$reversal <- c(0, diff(df$position))
    df$reversal[1] <- 1
    df2 <- df[df$reversal != 0,]

    num_winning_trades <- sum(df2$pnlActive > 0) # there are trades where PnL = 0
    num_losing_trades <- sum(df2$pnlActive < 0)
    avg_winning_trade <- ifelse(num_winning_trades > 0, round(mean(df[df$pnlActive > 0,]$pnlActive * df[df$pnlActive > 0,]$nopActive), 2), 0)
    avg_losing_trade <- ifelse(num_losing_trades > 0, round(mean(df[df$pnlActive < 0,]$pnlActive * df[df$pnlActive < 0,]$nopActive), 2), 0)

    # Create performance dataframe for the subset
    df_subset <- data.frame(
        #GrossProfit = round(c(tail(data_subset$eqlActive, 1) - data_subset$eqlActive[1], tail(data_subset$eqlPassive, 1) - data_subset$eqlPassive[1]), 0),
        GrossProfit = round(c(tail(data_subset$eqlActive, 1) - capital, tail(data_subset$eqlPassive, 1) - capital), 0),
        aR = c(aR_active, aR_passive),
        aSD = c(aSD_active, aSD_passive),
        IR = c(IR_active, IR_passive),
        MD = c(MD_active, MD_passive),
        trades = c(trades_active, trades_passive),
        avg_no_monthly_trades = round(c(trades_active / unique_months, 0), 2),
        num_winning_trades = c(num_winning_trades, NA),
        avg_winning_trade = c(avg_winning_trade, NA),
        num_losing_trades = c(num_losing_trades, NA),
        avg_losing_trade = c(avg_losing_trade, NA),
        win_ratio = c(num_winning_trades/trades_active, NA) * 100
    )

    df_subset$Strategy <- c("Active", "Passive")
    df_subset$ticker <- symbol

    return(df_subset)
}

  )
)

# e <- data[data$Date >= as.Date("2007-01-05") & data$Date <= as.Date("2009-01-04"),]
# sum(na.omit(e$r_eqlActive > 0))
# nrow(e)
# sum(diff(e$signal) != 0)
# e$reversal <- c(0, diff(e$position))
# e$reversal[1] <- 1
# e2 <- e[e$reversal != 0,]
# sum(e2$pnlActive > 0)
# sum(e2$pnlActive < 0)
# sum(e2$pnlActive == 0) # 18 cases when trade resulted in PnL being equal to 0

# range(as.numeric(e2$r_eqlActive), na.rm = TRUE)

Nero <- R6Class(
  "Nero",
  inherit = Strategy,
  public = list(
    
    initialize = function(data) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
    },
    
    # identify Close price's global min, global max, local min1, local max1, ...
identify_historical_levels = function() {
      self$data <- self$data %>%
        mutate(
          # max
          global_max = cummax(Close),
          local_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365, .complete = TRUE), # 1 year
          local_half_year_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/2, .complete = TRUE), # half-year
          local_month_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
          local_week_max = slider::slide_index_dbl(High, Date, ~ max(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE), # week
          
          # min
          local_three_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/4, .complete = TRUE), # month
          local_month_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/12, .complete = TRUE), # month
          local_week_min = slider::slide_index_dbl(Low, Date, ~ min(.x, na.rm = TRUE), .before = 365/52, .complete = TRUE) # week
        )
      #return(self$data)
},
    
    # identify strong price levels (refer to Chapter 2)
    # level, points from which new min or max is reached
    
    # Criteria to support identified strong price level
    # Entry point, stop value, luft, stop loss, take profit points
    
    # generate_signals = function() {
    
    #     # Split data into periods
    #     period_list <- super$split_data_into_periods()
        
    #     # Apply signal generation logic to each period
    #     period_list <- lapply(period_list, function(df) {
    #         df <- df %>%
    #             mutate(
    #                 # Generate signals: 1 for Buy, -1 for Sell, 0 for Hold
    #                 signal = ifelse(Close > lag(Close), 1, ifelse(Close < lag(Close), -1, 0)),
                    
    #                 # Lag the signal to determine position, avoiding using today's signal for today's position
    #                 position = lag(signal, default = 0)
    #             ) %>%
    #             na.omit()  # Remove any rows with NA values that may result from the lag function
    #         return(df)
    #     })
        
    #     # Combine all period data frames into one data frame
    #     self$data <- bind_rows(period_list)
      
    #   # Three main strategies (nearby strong price levels)
    #   # *identify strong price level, market is in range in ~ 3/4 time
    #   # Choose one of the following and trade
    #   # 1. Bounce off the level
    #   # 2. False breakout (simple, 2 bars, complex: 3 or more bars)
    #   # 3. Breakout the level (spring)
      
    #   #self$identify_historical_levels() # add historical levels to plot with candles
    #   #self$identify_reversals(50)
    # },

generate_signals = function() {

    # Ensure self$data is not empty
    if (nrow(self$data) == 0) {
        stop("No data available for signal generation.")
    }
    
    # Apply signal generation logic directly to self$data
    # self$data <- self$data %>%
    #     mutate(
    #         # Generate signals: 1 for Buy, -1 for Sell, 0 for Hold
    #         signal = ifelse(Close > lag(Close), 1, ifelse(Close < lag(Close), -1, 0)),
            
    #         # Lag the signal to determine position, avoiding using today's signal for today's position
    #         position = lag(signal, default = 0)
    #     ) %>% na.omit()  # Remove any rows with NA values that may result from the lag function

    self$data <- mutate(self$data, 
                    ma = EMA(Close, 20, align = "right", fill = NA),
                    signal = ifelse(Close > lag(ma), 1, ifelse(Close < lag(ma), -1, 0)),
                    position = lag(signal, default = 0)) %>% na.omit()
    
},
    # Estimate strategy performance
    # estimate_performance = function() {
    #   # 3R/R
    #   # Money management: position sizing, cascade/de-cascade
    #   # Risk management rules: 1% portfolio daily loss max, etc.
    #   # sequence of 3 losing trades - pause activity
    # },
    
# Identify levels based on reversal points given rolling window
identify_reversals = function(window ) {
  # Calculate the rolling minimum
  self$data$RollingMin <- rollapply(self$data$Close, width = window, FUN = min, by.column = TRUE, fill = NA, align = "right")
  
  # Initialize columns
  self$data$strong_min <- NA
  self$data$strong_max <- NA
  
  # Variables to track current minimum and current high
  current_min <- NA
  current_high <- -Inf
  
  # Iterate through each row in the data frame
  for (i in 1:nrow(self$data)) {
    if (!is.na(self$data$RollingMin[i])) {
      # Check if a new rolling window starts (every `window` days)
      if (i %% window == 1) {
        current_min <- self$data$RollingMin[i]  # Reset current_min
        current_high <- self$data$Close[i]      # Reset current_high
        self$data$strong_min[i] <- current_min
      } else {
        # If we encounter a new minimum within the window
        if (is.na(current_min) || self$data$RollingMin[i] < current_min) {
          current_min <- self$data$RollingMin[i]
          current_high <- self$data$Close[i]    # Reset current high
          self$data$strong_min[i] <- current_min
        }
        
        # If we encounter a new high within the window
        if (self$data$Close[i] > current_high) {
          current_high <- self$data$Close[i]
          if (!is.na(current_min)) {
            # Record the current minimum and maximum as strong
            self$data$strong_min[i] <- current_min
            self$data$strong_max[i] <- current_high
          }
        }
      }
    }
  }
  
  # Filter out rows with NA in strong_max
  data_filtered <- self$data[!is.na(self$data$strong_max), ]
  
  # Calculate strongest values
  strongest_values <- data_filtered %>%
    group_by(strong_min) %>%
    mutate(strongest = ifelse(strong_max == max(strong_max, na.rm = TRUE), max(strong_max, na.rm = TRUE), NA)) %>%
    ungroup()
  
  # Create levels data frame
  levels <- strongest_values %>%
    filter(!is.na(strongest)) %>%
    mutate(plot = TRUE)
  
  # Merge strongest column back to original data by Date
  # self$data <- merge(self$data, levels[, c("Date", "strongest", "plot")], by = "Date", all.x = TRUE)
  
  self$data <- self$data %>% left_join(levels[, c("Date", "strongest", "plot")], by = "Date")
  #self$data <- merge(self$data, levels[, c("Date", "strongest", "plot")], by = "Date", all.x = TRUE, suffixes = c("", ""))
  
  # Return self$data for chaining or other purposes
  #return(self$data)
},
    
# Plot levels based on reversal points
plot_reversals_lvl = function(from, to) {
  # Plotting using ggplot2
  ggplot(self$data[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to), ], aes(x = Date, y = Close)) +
    geom_line(color = "black", size = 1) +  # Close prices
    geom_hline(aes(yintercept = strong_min), data = subset(self$data[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & self$data$plot.x == TRUE, ]), linetype = "dashed", color = "green", size = 0.5) +  # strong_min levels
    geom_hline(aes(yintercept = strong_max), data = subset(self$data[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & self$data$plot.x == TRUE, ]), linetype = "longdash", color = "red", size = 0.5) +  # strong_max levels
    annotate("text", x = self$data$Date[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_min) & self$data$plot.x == TRUE], y = self$data$strong_min[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_min) & self$data$plot.x == TRUE], label = "strong_min", vjust = -0.5, color = "green", size = 3) +  # Annotation for strong_min
    annotate("text", x = self$data$Date[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_max) & self$data$plot.x == TRUE], y = self$data$strong_max[self$data$Date >= as.Date(from) & self$data$Date <= as.Date(to) & !is.na(self$data$strong_max) & self$data$plot.x == TRUE], label = "strong_max", vjust = 1, color = "red", size = 3) +  # Annotation for strong_max
    labs(
      title = "Close Prices with strong_min and strong_max Levels",
      x = "Date",
      y = "Price"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
},
    
# Plot Japanese candles (HLOC) given latest ndays
plot_candles = function(ndays) {
  
  # Get the last ndays of data
  df_tail <- tail(self$data, ndays)
  
  # Latest date in the tail data
  latest_date <- max(df_tail$Date)
  
  # Get the latest levels for the horizontal lines
  latest_levels <- self$data %>%
    filter(Date == latest_date) %>%
    select(local_year_max, local_half_year_max, local_month_max, local_week_max,
            local_three_month_min, local_month_min, local_week_min) %>%
    slice(1)
  
  # Plot the candlestick chart
  fig <- df_tail %>% plot_ly(x = ~Date, type= "candlestick",
                              open = ~Open, close = ~Close,
                              high = ~High, low = ~Low) 
  
  # Add horizontal lines for historical levels
  fig <- fig %>% 
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_year_max, nrow(df_tail)),
              line = list(color = 'blue', dash = 'dash', width = 1), name = 'Year Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_half_year_max, nrow(df_tail)),
              line = list(color = 'green', dash = 'dash', width = 1), name = 'Half Year Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_max, nrow(df_tail)),
              line = list(color = 'red', dash = 'dash', width = 1), name = 'Month Max') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_max, nrow(df_tail)),
              line = list(color = 'purple', dash = 'dash', width = 1), name = 'Week Max') %>%
    
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_three_month_min, nrow(df_tail)),
              line = list(color = 'brown', dash = 'dot', width = 1), name = '3 Month Min') %>%
    
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_month_min, nrow(df_tail)),
              line = list(color = 'red', dash = 'dot', width = 1), name = 'Month Min') %>%
    add_lines(x = df_tail$Date, y = rep(latest_levels$local_week_min, nrow(df_tail)),
              line = list(color = 'purple', dash = 'dot', width = 1), name = 'Week Min')
  
  # Add layout title
  fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
  
  # Return the plot
  fig
}

  )
)


# Instances of Nero strategy
nero <- Nero$new(ts)
nero$identify_historical_levels()
nero$identify_reversals(window = 50)
reversals <- nero$identify_reversals(window = 50)
nero$plot_reversals_lvl("2023-01-01", "2024-06-20")

per_res <- nero$estimate_performance() %>% filter(Strategy == "Active")
per_res
sum(per_res$GrossProfit)
data <- nero$data
nero$plot_performance()

data$reversal <- c(0, diff(data$signal))
data %>% filter(reversal != 0)

hist(data$pnlActive, breaks = 200)

#########################################################################################################


a$plot.x == a$plot.y

na.omit(a$plot.x)

nero$plot_candles(30)
nero$plot_candles(60)
nero$plot_candles(90) # optimal
nero$plot_candles(120)

atr_btc <- nero$estimate_average_true_range(ts, 14) %>% 
  tail(90)

mean(atr_btc$tr_reserve)

# Strong levels identification
identify_events = function(data, threshold) {
  # Initialize event vector
  data <- data %>%
    mutate(
      mid = (High + Open) / 2
    )
  
  events <- numeric(nrow(data))
  
  # Initialize highest high and lowest low
  highest_high <- 0
  lowest_low <- Inf
  
  # Initialize event flag
  event_flag <- 1  # 1: Event start, -1: Event end
  events[1] <- 1  # Set the first value to 1 as the starting point
  
  # Initialize column for combined overshoot events
  data$OS <- ""
  
  # Initialize columns for highest_high and lowest_low
  data$highest_high <- NA
  data$lowest_low <- NA
  
  # Loop through each row of the data
  for (i in 1:nrow(data)) {
    # Check if event flag is 1
    if (event_flag == 1) {
      # Check condition for Event = 1 (Upturn)
      if (data$High[i] > highest_high) {
        highest_high <- data$High[i]
      }
      if (data$mid[i] <= highest_high * (1 - threshold)) {
        events[i] <- -1
        event_flag <- -1
        #lowest_low <- data$Low[i]
        highest_high <- data$High[i]  # Reset highest_high
        lowest_low <- Inf  # Reset lowest_low to infinity
      }
    } else {
      # Check condition for Event = -1 (Downturn)
      if (data$Low[i] < lowest_low) {
        lowest_low <- data$Low[i]
      }
      if (data$mid[i] >= lowest_low * (1 + threshold)) {
        events[i] <- 1
        event_flag <- 1
        #highest_high <- data$High[i]
        lowest_low <- data$Low[i]  # Reset lowest_low
        highest_high <- Inf
      }
    }
    
    # Update highest_high and lowest_low in the dataframe
    data$highest_high[i] <- highest_high
    data$lowest_low[i] <- lowest_low
  }
  
  # Initialize current state
  current_state <- NA
  
  # Assign OS values
  for (i in seq_along(events)) {
    if (events[i] == 1) {
      current_state <- "UOS"
    } else if (events[i] == -1) {
      current_state <- "DOS"
    }
    if (!is.na(current_state)) {
      data$OS[i] <- current_state
    }
    if (is.na(data$OS[i]) && !is.na(current_state) && i > 1) {
      data$OS[i] <- data$OS[i - 1]
    }
  }
  
  # Lag OS column
  data$OS <- lag(data$OS)
  
  # Return dataframe with events column
  result <- data.frame(data, events = events)
  
  # Calculate dc column
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  
  # Set default values
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)
}
head(ts_df)

# timeframe 1 year
identify_events = function(data, threshold) {
  # Initialize event vector
  data <- data %>%
    mutate(mid = (High + Open) / 2)
  
  events <- numeric(nrow(data))
  
  # Initialize highest high and lowest low
  highest_high <- 0
  lowest_low <- Inf
  
  # Initialize event flag
  event_flag <- 1  # 1: Event start, -1: Event end
  events[1] <- 1  # Set the first value to 1 as the starting point
  
  # Initialize column for combined overshoot events
  data$OS <- ""
  
  # Initialize columns for highest_high and lowest_low
  data$highest_high <- NA
  data$lowest_low <- NA
  
  # Initialize columns for event_price and strong_lvl
  data$event_price <- NA
  data$strong_lvl <- FALSE
  
  # Loop through each row of the data
  for (i in 1:nrow(data)) {
    # Check if event flag is 1
    if (event_flag == 1) {
      # Check condition for Event = 1 (Upturn)
      if (data$High[i] > highest_high) {
        highest_high <- data$High[i]
      }
      if (data$mid[i] <= highest_high * (1 - threshold)) {
        events[i] <- -1
        event_flag <- -1
        highest_high <- data$High[i]  # Reset highest_high
        lowest_low <- Inf  # Reset lowest_low to infinity
        data$event_price[i] <- data$Close[i]
      }
    } else {
      # Check condition for Event = -1 (Downturn)
      if (data$Low[i] < lowest_low) {
        lowest_low <- data$Low[i]
      }
      if (data$mid[i] >= lowest_low * (1 + threshold)) {
        events[i] <- 1
        event_flag <- 1
        lowest_low <- data$Low[i]  # Reset lowest_low
        highest_high <- Inf
        data$event_price[i] <- data$Close[i]
      }
    }
    
    # Update highest_high and lowest_low in the dataframe
    data$highest_high[i] <- highest_high
    data$lowest_low[i] <- lowest_low
  }
  
  # Initialize current state
  current_state <- NA
  
  # Assign OS values
  for (i in seq_along(events)) {
    if (events[i] == 1) {
      current_state <- "UOS"
    } else if (events[i] == -1) {
      current_state <- "DOS"
    }
    if (!is.na(current_state)) {
      data$OS[i] <- current_state
    }
    if (is.na(data$OS[i]) && !is.na(current_state) && i > 1) {
      data$OS[i] <- data$OS[i - 1]
    }
  }
  
  # Lag OS column
  data$OS <- lag(data$OS)
  
  # Track strong level price
  for (i in 1:nrow(data)) {
    if (!is.na(data$event_price[i])) {
      event_start <- events[i]
      event_price <- data$event_price[i]
      for (j in i:nrow(data)) {
        if (event_start == 1) { # Upturn
          if (data$High[j] > data$High[i]) {
            break
          }
          if (data$Close[j] >= event_price) {
            data$strong_lvl[i] <- TRUE
            break
          }
        } else { # Downturn
          if (data$Low[j] < data$Low[i]) {
            break
          }
          if (data$Close[j] <= event_price) {
            data$strong_lvl[i] <- TRUE
            break
          }
        }
      }
    }
  }
  
  # Return dataframe with events column
  result <- data.frame(data, events = events)
  
  # Calculate dc column
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  
  # Set default values
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)
}

a <- identify_events(ts_df[ts_df$Date >= as.Date("2020-01-01"),], 0.075)
table(a$events)

############### Trend reversals levels ###################

identify_reversals <- function(data) {
  # Calculate the rolling minimum (100-day window)
  data$RollingMin <- rollapply(data$Close, width = 100, FUN = min, by.column = TRUE, fill = NA, align = "right")
  
  # Initialize columns
  data$strong_min <- NA
  data$strong_max <- NA
  
  # Variables to track current minimum and current high
  current_min <- NA
  current_high <- -Inf
  
  # Iterate through each row in the data frame
  for (i in 1:nrow(data)) {
    if (!is.na(data$RollingMin[i])) {
      # Check if a new rolling window starts (every 100 days)
      if (i %% 100 == 1) {
        current_min <- data$RollingMin[i]  # Reset current_min
        current_high <- data$Close[i]      # Reset current_high
        data$strong_min[i] <- current_min
      } else {
        # If we encounter a new minimum within the window
        if (is.na(current_min) || data$RollingMin[i] < current_min) {
          current_min <- data$RollingMin[i]
          current_high <- data$Close[i]    # Reset current high
          data$strong_min[i] <- current_min
        }
        
        # If we encounter a new high within the window
        if (data$Close[i] > current_high) {
          current_high <- data$Close[i]
          if (!is.na(current_min)) {
            # Record the current minimum and maximum as strong
            data$strong_min[i] <- current_min
            data$strong_max[i] <- current_high
          }
        }
      }
    }
  }
  
  
  data_filtered <- data[!is.na(data$strong_max), ]
  
  strongest_values <- data_filtered %>%
    group_by(strong_min) %>%
    mutate(strongest = ifelse(strong_max == max(strong_max, na.rm = TRUE), max(strong_max, na.rm = TRUE), NA)) %>%
    ungroup()
  
  levels <- strongest_values %>%
    filter(!strongest == 0) %>%
    mutate(plot = T)
  
  # Merge strongest column back to original data by Date
  merged_data <- merge(data, levels[, c("Date", "strongest", "plot")], by = "Date", all.x = TRUE)
  
  # Return the merged data
  return(merged_data)
  
}

a <- identify_reversals(ts_df, 100)
table(a$strongest)

plot_reversals_lvl <- function(merged_data) {
  # Plotting using ggplot2
  ggplot(merged_data, aes(x = Date, y = Close)) +
    geom_line(color = "black", size = 1) +  # Close prices
    # geom_hline(aes(yintercept = strong_min), data = subset(merged_data, !is.na(strong_min)), linetype = "dashed", color = "green", size = 0.5) +  # strong_min levels
    # geom_hline(aes(yintercept = strong_max), data = subset(merged_data, !is.na(strong_max)), linetype = "longdash", color = "red", size = 0.5) +  # strong_max levels
    
    geom_hline(aes(yintercept = strong_min), data = subset(merged_data, plot == TRUE), linetype = "dashed", color = "green", size = 0.5) +  # strong_min levels
    geom_hline(aes(yintercept = strong_max), data = subset(merged_data, plot == TRUE), linetype = "longdash", color = "red", size = 0.5) +  # strong_max levels
    
    annotate("text", x = merged_data$Date[!is.na(merged_data$strong_min) & merged_data$plot == TRUE], y = merged_data$strong_min[!is.na(merged_data$strong_min) & merged_data$plot == TRUE], label = "strong_min", vjust = -0.5, color = "green", size = 3) +  # Annotation for strong_min
    annotate("text", x = merged_data$Date[!is.na(merged_data$strong_max) & merged_data$plot == TRUE], y = merged_data$strong_max[!is.na(merged_data$strong_max) & merged_data$plot == TRUE], label = "strong_max", vjust = 1, color = "red", size = 3) +  # Annotation for strong_max
    labs(
      title = "Close Prices with strong_min and strong_max Levels",
      x = "Date",
      y = "Price"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_reversals_lvl(a[a$Date >= as.Date("2023-01-01") & a$Date <= Sys.Date(),])

b <- a[a$Date >= as.Date("2023-01-01") & a$Date <= Sys.Date(),]

lvl <- b[!is.na(b$strongest),]
lvl

############### Mirror levels ###################
