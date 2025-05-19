#' RSI Strategy
#' @description Implements a mean reversion strategy based on the Relative Strength Index (RSI)
#' @export
RSIStrategy <- R6::R6Class("RSIStrategy",
  inherit = Strategy,
  public = list(
    #' @field period RSI calculation period
    period = NULL,
    
    #' @field overbought Overbought threshold
    overbought = NULL,
    
    #' @field oversold Oversold threshold
    oversold = NULL,
    
    #' @description Initialize RSI strategy
    #' @param period RSI calculation period (default: 14)
    #' @param overbought Overbought threshold (default: 70)
    #' @param oversold Oversold threshold (default: 30)
    initialize = function(period = 14, overbought = 70, oversold = 30) {
      super$initialize("RSI Strategy")
      self$period <- period
      self$overbought <- overbought
      self$oversold <- oversold
    },
    
    #' @description Generate trading signals based on RSI
    #' @param data Data frame with price data
    #' @return Data frame with signals
    generate_signals = function(data) {
      # Calculate RSI
      rsi <- TTR::RSI(data$Close, n = self$period)
      
      # Generate signals
      signals <- data.frame(
        Date = data$Date,
        Close = data$Close,
        RSI = rsi,
        Signal = 0,
        Position = 0
      )
      
      # Buy signal when RSI crosses below oversold
      signals$Signal[signals$RSI < self$oversold] <- 1
      
      # Sell signal when RSI crosses above overbought
      signals$Signal[signals$RSI > self$overbought] <- -1
      
      # Calculate positions
      signals$Position <- cumsum(signals$Signal)
      
      self$signals <- signals
      return(signals)
    },
    
    #' @description Get strategy description
    #' @return String description
    get_description = function() {
      return(sprintf(
        "RSI Strategy (Period: %d, Overbought: %d, Oversold: %d)",
        self$period, self$overbought, self$oversold
      ))
    }
  )
) 