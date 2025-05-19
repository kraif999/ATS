#' @title Stochastic Oscillator Strategy
#' @description Implements a trading strategy based on Stochastic Oscillator overbought/oversold levels
#' @export
StochasticStrategy <- R6Class(
  "StochasticStrategy",
  inherit = Strategy,
  public = list(
    #' @description Initialize a new Stochastic Strategy
    #' @param n The period for %K calculation
    #' @param m The period for %D calculation
    #' @param overbought The overbought threshold
    #' @param oversold The oversold threshold
    initialize = function(n = 14, m = 3, overbought = 80, oversold = 20) {
      super$initialize(
        name = "Stochastic Strategy",
        parameters = list(
          n = n,
          m = m,
          overbought = overbought,
          oversold = oversold
        )
      )
    },
    
    #' @description Generate trading signals based on Stochastic Oscillator
    #' @param data The price data to generate signals from
    #' @return A data frame containing the signals
    generate_signals = function(data) {
      # Extract OHLC data
      high_prices <- data[, grep("\\.High$", colnames(data))]
      low_prices <- data[, grep("\\.Low$", colnames(data))]
      close_prices <- data[, grep("\\.Close$", colnames(data))]
      
      # Calculate Stochastic Oscillator
      stoch <- TTR::stoch(
        HLC = cbind(high_prices, low_prices, close_prices),
        n = self$parameters$n,
        nFastK = self$parameters$n,
        nFastD = self$parameters$m,
        nSlowD = self$parameters$m
      )
      
      # Generate signals
      signals <- data.frame(
        Date = index(data),
        Close = as.numeric(close_prices),
        FastK = as.numeric(stoch$fastK),
        FastD = as.numeric(stoch$fastD)
      )
      
      # Calculate signal
      signals$Signal <- 0
      signals$Signal[signals$FastK < self$parameters$oversold] <- 1    # Buy signal
      signals$Signal[signals$FastK > self$parameters$overbought] <- -1 # Sell signal
      
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
        "Stochastic Oscillator Strategy with",
        self$parameters$n,
        "period %K,",
        self$parameters$m,
        "period %D, overbought at",
        self$parameters$overbought,
        "and oversold at",
        self$parameters$oversold
      )
    }
  )
) 