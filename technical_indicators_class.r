# Define Strategy class

Strategy <- R6Class(
  "Strategy",
  public = list(
    data = NULL,
    
    initialize = function(data) {
      # Change second column to 'rets'
      colnames(data)[2] <- "rets"
      self$data <- data
    },

    # Signal generation, specific to each sub-class (generic method)
    generate_signals = function() {
    },

    # Calculate portfolio value for each trading day
    calculate_positions_and_equity_lines = function() {
      self$generate_signals()  # Call generate_signals dynamically

      # Active strategy
      self$data <- mutate(self$data, equity_line = cumsum(position * rets))
      
      # Buy and hold
      self$data <- mutate(self$data, 
                          position_bh = 1,
                          equity_line_bh = cumsum(position_bh * rets))
      
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
    initialize = function(data, window_size) {
      super$initialize(data)
      self$window_size <- window_size
    },
    generate_signals = function() {
      # Calculate moving average and generate signal
      self$data <- mutate(self$data, 
                          ma = rollmean(rets, k = self$window_size, align = "right", fill = NA),
                          signal = ifelse(ma > lag(ma), 1, ifelse(ma < lag(ma), -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
    }
  )
)

# Create instances of SMA1 and SMA2
sma1 <- SMA1$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size = 10)
sma1 <- SMA1$new(wide_df_rets %>% select(Date, `rets_EUR=X`), window_size = 10)
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
                          ma1 = rollmean(rets, k = self$window_size1, align = "right", fill = NA),
                          ma2 = rollmean(rets, k = self$window_size2, align = "right", fill = NA),
                          signal = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit

    }
  )
)

# Create instances SMA2
sma2 <- SMA2$new(wide_df_rets %>% select(Date, `rets_GC=F`), window_size1 = 50, window_size2 = 150)
sma2 <- SMA2$new(wide_df_rets %>% select(Date, `rets_^GSPC`), window_size1 = 100, window_size2 = 300)
sma2$calculate_positions_and_equity_lines()
sma2$plot_equity_lines()
sma2$calculate_information_ratio()

# IR has drawbacks: negative values to non-integer power and misleading  interpretation of the information ratio in case last and first values are negative
# therefore, to draw a conclusion IR should be considered with equity line plots

# Define a function to calculate Information Ratio for a given window_size
window_sizes <- seq(from = 5, to = 150, by = 5)

calculate_ir <- function(data, window_size) {
  # Create an instance of SMA1 with the current window_size
  strategy <- SMA1$new(data, window_size)
  
  # Calculate positions and equity lines
  strategy$calculate_positions_and_equity_lines()
  
  # Calculate Information Ratio
  ir_df <- strategy$calculate_information_ratio()
  
  # Add window_size column to the result dataframe
  ir_df$window_size <- window_size
  
  # Return Information Ratio for the current window_size
  return(ir_df)
}

# Apply the function to each window_size value
ir_results <- lapply(window_sizes, function(ws) calculate_ir(wide_df_rets %>% select(Date, `rets_^GSPC`), ws))

# Combine Information Ratio results into a single data frame
ir_results_df <- do.call(rbind, ir_results)



