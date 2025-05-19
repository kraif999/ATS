#' @title Average Directional Index Strategy
#' @description Implements a trading strategy based on ADX trend strength and directional movement
#' @export
ADXStrategy <- R6Class(
  "ADXStrategy",
  inherit = Strategy,
  public = list(
    #' @description Initialize a new ADX Strategy
    #' @param period The period for ADX calculation
    #' @param threshold The ADX threshold for trend strength
    initialize = function(period = 14, threshold = 25) {
      super$initialize(
        name = "ADX Strategy",
        parameters = list(
          period = period,
          threshold = threshold
        )
      )
    },
    
    #' @description Generate trading signals based on ADX and DI+/DI-
    #' @param data The price data to generate signals from
    #' @return A data frame containing the signals
    generate_signals = function(data) {
      # Extract OHLC data
      high_prices <- data[, grep("\\.High$", colnames(data))]
      low_prices <- data[, grep("\\.Low$", colnames(data))]
      close_prices <- data[, grep("\\.Close$", colnames(data))]
      
      # Calculate ADX
      adx <- TTR::ADX(
        HLC = cbind(high_prices, low_prices, close_prices),
        n = self$parameters$period
      )
      
      # Generate signals
      signals <- data.frame(
        Date = index(data),
        Close = as.numeric(close_prices),
        ADX = as.numeric(adx$ADX),
        DIp = as.numeric(adx$DIp),
        DIn = as.numeric(adx$DIn)
      )
      
      # Calculate signal
      signals$Signal <- 0
      
      # Strong trend conditions
      strong_trend <- signals$ADX > self$parameters$threshold
      
      # Buy signal: Strong uptrend (ADX > threshold and DI+ > DI-)
      signals$Signal[strong_trend & signals$DIp > signals$DIn] <- 1
      
      # Sell signal: Strong downtrend (ADX > threshold and DI- > DI+)
      signals$Signal[strong_trend & signals$DIn > signals$DIp] <- -1
      
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
        "Average Directional Index Strategy with",
        self$parameters$period,
        "period and",
        self$parameters$threshold,
        "trend strength threshold"
      )
    }
  )
) 