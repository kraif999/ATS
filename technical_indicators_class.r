source("helper_indicators.r")

# Dates to download ts data
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2024-10-03", format = "%Y-%m-%d")

symbols <- c("GC=F", "BZ=F", "DX-Y.NYB", "^GSPC")  # list of all symbols
symbols <- c("EUR=X") 

# Download data
wide_df_rets <- convert_xts_to_wide_df(symbols, from_date, to_date, type = "rets")

# Consider 'Close' ts case
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
# modified SMA1
# It consists of a rule that relates the current price of an asset with the price of the last ‘buy’ signal issued by a moving average strategy 
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
    self$data <- self$data %>%
        select(-c(signal1, last_long_value, last_short_value))
        }
    )
)

sma1_modified <- SMA1_modified$new(wide_df_rets %>% select(Date, `rets_BZ=F`), window_size = 50, ma_type = 'exp')
sma1_modified$calculate_positions_and_equity_lines()
sma1_modified$plot_equity_lines()


