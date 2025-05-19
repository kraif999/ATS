#' Bollinger Bands Strategy
#' @description Implements a mean reversion strategy based on Bollinger Bands
#' @export
BollingerBandsStrategy <- R6::R6Class("BollingerBandsStrategy",
  inherit = Strategy,
  public = list(
    #' @field period Moving average period
    period = NULL,
    
    #' @field sd Standard deviation multiplier
    sd = NULL,
    
    #' @description Initialize Bollinger Bands strategy
    #' @param period Moving average period (default: 20)
    #' @param sd Standard deviation multiplier (default: 2)
    initialize = function(period = 20, sd = 2) {
      super$initialize("Bollinger Bands Strategy")
      self$period <- period
      self$sd <- sd
    },
    
    #' @description Generate trading signals based on Bollinger Bands
    #' @param data Data frame with price data
    #' @return Data frame with signals
    generate_signals = function(data) {
      # Calculate Bollinger Bands
      bb <- TTR::BBands(data$Close, n = self$period, sd = self$sd)
      
      # Generate signals
      signals <- data.frame(
        Date = data$Date,
        Close = data$Close,
        Middle = bb$mavg,
        Upper = bb$up,
        Lower = bb$dn,
        Signal = 0,
        Position = 0
      )
      
      # Buy signal when price crosses below lower band
      signals$Signal[data$Close < signals$Lower] <- 1
      
      # Sell signal when price crosses above upper band
      signals$Signal[data$Close > signals$Upper] <- -1
      
      # Calculate positions
      signals$Position <- cumsum(signals$Signal)
      
      self$signals <- signals
      return(signals)
    },
    
    #' @description Get strategy description
    #' @return String description
    get_description = function() {
      return(sprintf(
        "Bollinger Bands Strategy (Period: %d, SD: %.1f)",
        self$period, self$sd
      ))
    }
  )
) 