#' @title Simple Moving Average Strategy
#' @description Implements a trading strategy based on Simple Moving Average crossovers
#' @export
SMAStrategy <- R6Class(
  "SMAStrategy",
  inherit = Strategy,
  public = list(
    #' @description Initialize a new SMA Strategy
    #' @param fast_window The window size for the fast SMA
    #' @param slow_window The window size for the slow SMA
    initialize = function(fast_window = 20, slow_window = 50) {
      super$initialize(
        name = "SMA Strategy",
        parameters = list(
          fast_window = fast_window,
          slow_window = slow_window
        )
      )
    },
    
    #' @description Generate trading signals based on SMA crossovers
    #' @param data The price data to generate signals from
    #' @return A data frame containing the signals
    generate_signals = function(data) {
      # Extract close prices
      close_prices <- data[, grep("\\.Close$", colnames(data))]
      
      # Calculate SMAs
      fast_sma <- TTR::SMA(close_prices, n = self$parameters$fast_window)
      slow_sma <- TTR::SMA(close_prices, n = self$parameters$slow_window)
      
      # Generate signals
      signals <- data.frame(
        Date = index(data),
        Close = as.numeric(close_prices),
        Fast_SMA = as.numeric(fast_sma),
        Slow_SMA = as.numeric(slow_sma)
      )
      
      # Calculate signal
      signals$Signal <- 0
      signals$Signal[signals$Fast_SMA > signals$Slow_SMA] <- 1  # Buy signal
      signals$Signal[signals$Fast_SMA < signals$Slow_SMA] <- -1 # Sell signal
      
      # Calculate position changes
      signals$Position <- c(0, diff(signals$Signal))
      
      # Store signals
      self$signals <- signals
      
      return(signals)
    },
    
    #' @description Get the strategy's description
    #' @return A string describing the strategy
    get_description = function() {
      paste(
        "Simple Moving Average Strategy with",
        self$parameters$fast_window,
        "and",
        self$parameters$slow_window,
        "period windows"
      )
    }
  )
) 