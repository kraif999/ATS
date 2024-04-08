#source("helper_indicators.r")

# Dates to download ts data
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2024-10-03", format = "%Y-%m-%d")

symbols <- c("GC=F", "BZ=F", "DX-Y.NYB", "^GSPC")  # list of all symbols
#symbols <- c("EUR=X") 

# Download data
wide_df_rets <- convert_xts_to_wide_df(symbols, from_date, to_date, type = "rets")

# Define Data parent class (DataFetcher)
DataFetcher <- R6Class(
  "DataFetcher",
  public = list(
    symbols = NULL,
    from_date = NULL,
    to_date = NULL,
    type = NULL,
    initialize = function(symbols, from_date, to_date, type = "rets") {
      self$symbols <- symbols
      self$from_date <- from_date
      self$to_date <- to_date
      self$type <- type
    },
    convert_xts_to_wide_df = function() {
      dfs <- list()
      # Iterate through symbols to retrieve data and create data frames
      for (symbol in self$symbols) {
        # Fetch data for symbol
        ts <- getSymbols(symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
        ts <- na.omit(ts)
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
    }
  )
)

# Create an instance of DataFetcher
fetcher_rets <- DataFetcher$new(symbols, from_date, to_date, type = "rets")
wide_df_rets <- fetcher_rets$convert_xts_to_wide_df()

# wide_df_rets[, c("Date", paste0("rets_", symbols[1]))]

# Define parent Strategy class
Strategy <- R6Class(
  "Strategy",
  public = list(
    data = NULL,

    initialize = function(data) {
      # Change second column to 'value'
      colnames(data)[2] <- "value"
      self$data <- data
    },

    # Signal generation, specific to each sub-class (generic method)
    generate_signals = function() {
    },

    # Calculate portfolio value for each trading day
    calculate_positions_and_equity_lines = function() {
      self$generate_signals()  # Call generate_signals dynamically

      # Active strategy
      self$data <- mutate(self$data, equity_line = cumsum(position * value))
      
      # Buy and hold
      self$data <- mutate(self$data, 
                          position_bh = 1,
                          equity_line_bh = cumsum(position_bh * value))
      
      return(self$data)
    },

    # Calculate cumulative return
    calculate_cumulative_return = function() {
      self$generate_signals()  # Call generate_signals dynamically
      cret = data.frame(
        Active = prod(1+self$data$value * self$data$position) - 1,
        Buy_and_hold = prod(1+self$data$value) - 1
      )
      return(cret)
    },

    # Performance measurement (Information Ratio) for active and buy and hold strategies
    calculate_information_ratio = function() {

      self$data <- na.omit(self$data)
      # Calculate annualized returns
      aR <- (self$data$equity_line[length(self$data$equity_line)] / self$data$equity_line[1]) ^ (252 / length(self$data$equity_line)) - 1
      aR_bh <- (self$data$equity_line_bh[length(self$data$equity_line_bh)] / self$data$equity_line_bh[1]) ^ (252 / length(self$data$equity_line_bh)) - 1

      # Calculate annualized standard deviation
      aSD <- sd(self$data$equity_line) * sqrt(252)
      aSD_bh <- sd(self$data$equity_line_bh) * sqrt(252)

      # Create a data frame with information ratios for both strategies
      ir_df <- data.frame(
        Active_strategy = aR / aSD,
        Buy_and_Hold_Strategy_IR = aR_bh / aSD_bh)
    
      return(ir_df)

    },

    plot_equity_lines = function() {
      # Plot equity lines
      ggplot(self$data, aes(x = Date)) +
        geom_line(aes(y = equity_line, color = "Active Strategy")) +
        geom_line(aes(y = equity_line_bh, color = "Buy and Hold Strategy")) +
        labs(title = "Equity Lines for Active and Buy and Hold Strategies",
             x = "Date",
             y = "Equity line") +
        scale_color_manual(values = c("Active Strategy" = "blue", "Buy and Hold Strategy" = "red")) +
        theme_minimal()
    }
  )
)

# Define SMA1 class
SMA1 <- R6Class(
  "SMA1",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = "simple",
    initialize = function(data, window_size, ma_type = "simple") {
      super$initialize(data)
      self$window_size <- window_size
      self$ma_type <- ma_type
    },
    generate_signals = function(ma_type = self$ma_type) {
      ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
      self$data <- mutate(self$data, 
                          ma = ma_func(value, k = self$window_size, align = "right", fill = NA),
                          signal = ifelse(value > ma, 1, ifelse(value < ma, -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
    }
  )
)

sma1 <- SMA1$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size = 10, ma_type = 'exp')
sma1$calculate_positions_and_equity_lines()
sma1$plot_equity_lines()
sma1$calculate_cumulative_return()
# Create instances of SMA1 and SMA2
sma1 <- SMA1$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size = 10)
#sma1 <- SMA1$new(wide_df_rets %>% select(Date, `rets_EUR=X`), window_size = 10)
sma1$calculate_positions_and_equity_lines()
sma1$plot_equity_lines()
sma1$calculate_information_ratio()

# Define SMA2 class
SMA2 <- R6Class(
  "SMA2",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    initialize = function(data, window_size1, window_size2) {
      super$initialize(data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
    },

    generate_signals = function() {
      # Calculate first and second moving averages
      self$data <- mutate(self$data, 
                          ma1 = rollmean(value, k = self$window_size1, align = "right", fill = NA),
                          ma2 = rollmean(value, k = self$window_size2, align = "right", fill = NA),
                          signal = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit

    }
  )
)

# Create instances SMA2
sma2 <- SMA2$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size1 = 50, window_size2 = 150)
sma2 <- SMA2$new(wide_df_rets %>% select(Date, `rets_EUR=X`), window_size1 = 100, window_size2 = 300)
sma2$calculate_positions_and_equity_lines()
sma2$plot_equity_lines()
sma2$calculate_information_ratio()

# IR has drawbacks: negative values to non-integer power and misleading  interpretation of the information ratio in case last and first values are negative
# therefore, to draw a conclusion IR should be considered with equity line plots

window_sizes <- seq(from = 5, to = 150, by = 5)
# Apply the function to each window_size value
ir_results <- lapply(window_sizes, function(ws) calculate_ir(wide_df_rets %>% select(Date, `rets_^GSPC`), ws))
ir_results_df <- do.call(rbind, ir_results)

#######################################################################################
# modified SMA1 and SMA2
# Modification consists of a rule that relates the current price of an asset with the price of the last ‘buy’ signal issued by a moving average strategy 
# (making this latter price a dynamic threshold) and it works as a dynamic trailing stop
#######################################################################################

# Define SMA1 class with modified signals
SMA1_modified <- R6Class(

  "SMA1_modified",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = "simple",
    initialize = function(data, window_size, ma_type = "simple") {
      super$initialize(data)
      self$window_size <- window_size
      self$ma_type
    },

    # Generate modified signals
    generate_signals = function(ma_type = self$ma_type) {

    ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
    self$data <- mutate(
    self$data, 
    ma = ma_func(value, k = self$window_size, align = "right", fill = NA),
    signal1 = ifelse(value > ma, 1, ifelse(value < ma, -1, 0)),
    position = lag(signal1, default = 0)) %>% 
    na.omit
    
    # Initialize last_long_value and last_short_value
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
            last_long_value <- self$data$value[first_previous_index]
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
            last_short_value <- self$data$value[first_previous_index]
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
    self$data$signal <- ifelse((self$data$value > self$data$last_long_value) & (self$data$value > self$data$ma), 1,
                        ifelse((self$data$value < self$data$last_short_value) & (self$data$value < self$data$ma), -1, 0))
    self$data <- self$data 
    # %>% select(-c(signal1, last_long_value, last_short_value))
        }
    )
)

sma1_modified <- SMA1_modified$new(wide_df_rets %>% select(Date, `rets_BZ=F`), window_size = 50, ma_type = 'exp')
sma1_modified$calculate_positions_and_equity_lines()
sma1_modified$plot_equity_lines()

# SMA2 (modified by dynamic trailing stop)
SMA2_modified <- R6Class(
  "SMA2_modified",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = "simple",
    initialize = function(data, window_size1, window_size2, ma_type = "simple") {
      super$initialize(data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
    },

    generate_signals = function(ma_type = self$ma_type) {
      
      ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
      self$data <- mutate(self$data, 
                          ma1 = rollmean(value, k = self$window_size1, align = "right", fill = NA),
                          ma2 = rollmean(value, k = self$window_size2, align = "right", fill = NA),
                          signal1 = ifelse(ma1 > ma2, 1, ifelse(ma1 < ma2, -1, 0)),
                          position = lag(signal1, default = 0)) %>%
                            na.omit
      
      # Initialize last_long_value and last_short_value
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
              last_short_value <- self$data$ma2[first_previous_index]
            }
            # Assign last short value to the corresponding row in the data frame
            self$data$last_short_value[i] <- last_short_value
          }

          # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
          self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
          self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)
      }

      # Compare data$value[i] with the first previous value and update data$s2
      self$data$signal <- ifelse((self$data$value > self$data$last_long_value) & (self$data$value > self$data$ma1), 1,
                                 ifelse((self$data$value < self$data$last_short_value) & (self$data$value < self$data$ma1), -1, 0))
      self$data <- self$data 
      # %>% select(-c(last_long_value, last_short_value))
    }
  )
)

sma2_modified <- SMA2_modified$new(wide_df_rets %>% select(Date, `rets_BZ=F`), window_size1 = 10, window_size2 = 200, ma_type = "exp")
sma2_modified$calculate_positions_and_equity_lines()
sma2_modified$plot_equity_lines()

# Define Parameter Grids
symbols <- c("rets_BZ=F", "rets_GC=F", "rets_EUR=X")  # Example symbols
window_sizes <- list(c(50, 200), c(20, 50), c(100, 150))  # Example window size combinations
ma_types <- c("simple", "exp")  # Example MA types

#######################################################################################
# Sensitivity analysis given symbol, moving average type, moving average window
#######################################################################################

# Iterate Over Parameter Combinations
results <- list()
for (symbol in symbols) {
  for (window_size_pair in window_sizes) {
    for (ma_type in ma_types) {
      # Create instance of SMA2_modified
      sma_instance <- SMA2_modified$new(wide_df_rets %>% select(Date, !!sym(symbol)), 
                                         window_size1 = window_size_pair[1], 
                                         window_size2 = window_size_pair[2], 
                                         ma_type = ma_type)
      
      # Calculate positions and equity lines
      sma_instance$calculate_positions_and_equity_lines()
      
      # Store results
      results[[paste0("Symbol_", symbol, "_Window_", window_size_pair[1], "_", window_size_pair[2], "_MA_", ma_type)]] <- sma_instance
    }
  }
}


# Define Relative Strength Index class
RSI <- R6Class(
  "RSI",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    threshold_oversold = 30,
    threshold_overbought = 70,
    initialize = function(data, window_size, threshold_oversold = 30, threshold_overbought = 70) {
      super$initialize(data)
      self$window_size <- window_size
      self$threshold_oversold <- threshold_oversold
      self$threshold_overbought <- threshold_overbought
    },
    generate_signals = function() {
      self$data <- mutate(self$data,
                          avg_gain = rollmean(ifelse(value > 0, value, 0), k = self$window_size, align = "right", fill = NA),
                          avg_loss = rollmean(ifelse(value < 0, abs(value), 0), k = self$window_size, align = "right", fill = NA),
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

# Create an instance of RSI class
rsi <- RSI$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size = 14, threshold_oversold = 40, threshold_overbought = 60)
rsi$generate_signals()  # Generate signals
rsi$calculate_positions_and_equity_lines()  # Calculate positions and equity lines
rsi$plot_avg_gain_loss_with_equity_lines()  # Plot average gain, average loss, and equity line
rsi$plot_equity_lines()

# Define Bollinger Bands Breakout class
BollingerBreakout <- R6Class(
  "BollingerBreakout",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    sd_multiplier = 2,  # Multiplier for standard deviation
    initialize = function(data, window_size, sd_multiplier = 2) {
      super$initialize(data)
      self$window_size <- window_size
      self$sd_multiplier <- sd_multiplier
    },
    generate_signals = function() {
      self$data <- mutate(self$data, 
                          ma = rollmean(value, k = self$window_size, align = "right", fill = NA),
                          sd = rollapply(value, width = self$window_size, sd, align = "right", fill = NA),
                          upper_band = ma + self$sd_multiplier * sd,
                          lower_band = ma - self$sd_multiplier * sd,
                          signal = case_when(
                            value > upper_band ~ 1,
                            value < lower_band ~ -1,
                            TRUE ~ 0
                          ),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
    }
  )
)

# Create an instance of BollingerBreakout class
bol_br <- BollingerBreakout$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size = 20, sd_multiplier = 2)
bol_br$generate_signals()  # Generate signals
bol_br$calculate_positions_and_equity_lines()  # Calculate positions and equity lines

# Plot equity lines for Bollinger Breakout
bol_br$plot_equity_lines()
bol_br$calculate_cumulative_return()

# Define Volatility Mean Reversion class
VolatilityMeanReversion <- R6Class(
  "VolatilityMeanReversion",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    initialize = function(data, window_size) {
      super$initialize(data)
      self$window_size <- window_size
    },
    generate_signals = function() {
      # Calculate historical volatility
      hist_vol <- rollapply(self$data$value, width = self$window_size, sd, align = "right", fill = NA)
      
      # Calculate rolling mean of historical volatility
      mean_vol <- rollmean(hist_vol, k = self$window_size, align = "right", fill = NA)
      
      # Generate signals
      self$data <- mutate(self$data,
                          historical_volatility = hist_vol,
                          mean_volatility = mean_vol,
                          signal = ifelse(hist_vol > mean_vol, -1, 1),
                          position = lag(signal, default = 0)) %>%
                        na.omit
    }
  )
)


# Create an instance of VolatilityMeanReversion class
vol_mean_rev <- VolatilityMeanReversion$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size = 20)
# Generate signals
vol_mean_rev$generate_signals()
# Calculate positions and equity lines
vol_mean_rev$calculate_positions_and_equity_lines()
# Plot equity lines for Volatility Mean Reversion
vol_mean_rev$plot_equity_lines()
# Calculate cumulative return
vol_mean_rev$calculate_cumulative_return()
