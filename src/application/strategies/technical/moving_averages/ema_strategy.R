#' @title Exponential Moving Average Strategy
#' @description Implements a trading strategy based on Exponential Moving Average crossovers
#' @export
EMAStrategy <- R6Class(
  "EMAStrategy",
  inherit = Strategy,
  public = list(
    #' @description Initialize a new EMA Strategy
    #' @param fast_window The window size for the fast EMA
    #' @param slow_window The window size for the slow EMA
    initialize = function(fast_window = 12, slow_window = 26) {
      super$initialize(
        name = "EMA Strategy",
        parameters = list(
          fast_window = fast_window,
          slow_window = slow_window
        )
      )
    },
    
    #' @description Generate trading signals based on EMA crossovers
    #' @param data The price data to generate signals from
    #' @return A data frame containing the signals
    generate_signals = function(data) {
      # Extract close prices
      close_prices <- data[, grep("\\.Close$", colnames(data))]
      
      # Calculate EMAs
      fast_ema <- TTR::EMA(close_prices, n = self$parameters$fast_window)
      slow_ema <- TTR::EMA(close_prices, n = self$parameters$slow_window)
      
      # Generate signals
      signals <- data.frame(
        Date = index(data),
        Close = as.numeric(close_prices),
        Fast_EMA = as.numeric(fast_ema),
        Slow_EMA = as.numeric(slow_ema)
      )
      
      # Calculate signal
      signals$Signal <- 0
      signals$Signal[signals$Fast_EMA > signals$Slow_EMA] <- 1  # Buy signal
      signals$Signal[signals$Fast_EMA < signals$Slow_EMA] <- -1 # Sell signal
      
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
        "Exponential Moving Average Strategy with",
        self$parameters$fast_window,
        "and",
        self$parameters$slow_window,
        "period windows"
      )
    }
  )
) 