#' @title Relative Strength Index Strategy
#' @description Implements a trading strategy based on RSI overbought/oversold levels
#' @export
RSIStrategy <- R6Class(
  "RSIStrategy",
  inherit = Strategy,
  public = list(
    #' @description Initialize a new RSI Strategy
    #' @param period The period for RSI calculation
    #' @param overbought The overbought threshold
    #' @param oversold The oversold threshold
    initialize = function(period = 14, overbought = 70, oversold = 30) {
      super$initialize(
        name = "RSI Strategy",
        parameters = list(
          period = period,
          overbought = overbought,
          oversold = oversold
        )
      )
    },
    
    #' @description Generate trading signals based on RSI levels
    #' @param data The price data to generate signals from
    #' @return A data frame containing the signals
    generate_signals = function(data) {
      # Extract close prices
      close_prices <- data[, grep("\\.Close$", colnames(data))]
      
      # Calculate RSI
      rsi <- TTR::RSI(close_prices, n = self$parameters$period)
      
      # Generate signals
      signals <- data.frame(
        Date = index(data),
        Close = as.numeric(close_prices),
        RSI = as.numeric(rsi)
      )
      
      # Calculate signal
      signals$Signal <- 0
      signals$Signal[signals$RSI < self$parameters$oversold] <- 1   # Buy signal
      signals$Signal[signals$RSI > self$parameters$overbought] <- -1 # Sell signal
      
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
        "Relative Strength Index Strategy with",
        self$parameters$period,
        "period, overbought at",
        self$parameters$overbought,
        "and oversold at",
        self$parameters$oversold
      )
    }
  )
) 