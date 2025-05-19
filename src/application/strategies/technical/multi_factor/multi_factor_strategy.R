#' Multi-factor Strategy
#' @description Implements a strategy that combines multiple technical indicators
#' @export
MultiFactorStrategy <- R6::R6Class("MultiFactorStrategy",
  inherit = Strategy,
  public = list(
    #' @field rsi_period RSI calculation period
    rsi_period = NULL,
    
    #' @field rsi_overbought RSI overbought threshold
    rsi_overbought = NULL,
    
    #' @field rsi_oversold RSI oversold threshold
    rsi_oversold = NULL,
    
    #' @field bb_period Bollinger Bands period
    bb_period = NULL,
    
    #' @field bb_sd Bollinger Bands standard deviation
    bb_sd = NULL,
    
    #' @field obv_period OBV moving average period
    obv_period = NULL,
    
    #' @field obv_threshold OBV momentum threshold
    obv_threshold = NULL,
    
    #' @description Initialize Multi-factor strategy
    #' @param rsi_period RSI period (default: 14)
    #' @param rsi_overbought RSI overbought level (default: 70)
    #' @param rsi_oversold RSI oversold level (default: 30)
    #' @param bb_period BB period (default: 20)
    #' @param bb_sd BB standard deviation (default: 2)
    #' @param obv_period OBV period (default: 20)
    #' @param obv_threshold OBV threshold (default: 0.02)
    initialize = function(
      rsi_period = 14, rsi_overbought = 70, rsi_oversold = 30,
      bb_period = 20, bb_sd = 2,
      obv_period = 20, obv_threshold = 0.02
    ) {
      super$initialize("Multi-factor Strategy")
      self$rsi_period <- rsi_period
      self$rsi_overbought <- rsi_overbought
      self$rsi_oversold <- rsi_oversold
      self$bb_period <- bb_period
      self$bb_sd <- bb_sd
      self$obv_period <- obv_period
      self$obv_threshold <- obv_threshold
    },
    
    #' @description Generate trading signals based on multiple factors
    #' @param data Data frame with price and volume data
    #' @return Data frame with signals
    generate_signals = function(data) {
      # Calculate RSI
      rsi <- TTR::RSI(data$Close, n = self$rsi_period)
      
      # Calculate Bollinger Bands
      bb <- TTR::BBands(data$Close, n = self$bb_period, sd = self$bb_sd)
      
      # Calculate OBV
      obv <- TTR::OBV(data$Close, data$Volume)
      obv_ma <- TTR::SMA(obv, n = self$obv_period)
      obv_momentum <- (obv - obv_ma) / obv_ma
      
      # Generate signals
      signals <- data.frame(
        Date = data$Date,
        Close = data$Close,
        RSI = rsi,
        BB_Upper = bb$up,
        BB_Lower = bb$dn,
        OBV_Momentum = obv_momentum,
        Signal = 0,
        Position = 0
      )
      
      # RSI signals
      rsi_buy <- signals$RSI < self$rsi_oversold
      rsi_sell <- signals$RSI > self$rsi_overbought
      
      # Bollinger Bands signals
      bb_buy <- data$Close < signals$BB_Lower
      bb_sell <- data$Close > signals$BB_Upper
      
      # OBV signals
      obv_buy <- signals$OBV_Momentum > self$obv_threshold
      obv_sell <- signals$OBV_Momentum < -self$obv_threshold
      
      # Combine signals (buy when at least 2 indicators agree)
      signals$Signal[rsi_buy & bb_buy] <- 1
      signals$Signal[rsi_buy & obv_buy] <- 1
      signals$Signal[bb_buy & obv_buy] <- 1
      
      signals$Signal[rsi_sell & bb_sell] <- -1
      signals$Signal[rsi_sell & obv_sell] <- -1
      signals$Signal[bb_sell & obv_sell] <- -1
      
      # Calculate positions
      signals$Position <- cumsum(signals$Signal)
      
      self$signals <- signals
      return(signals)
    },
    
    #' @description Get strategy description
    #' @return String description
    get_description = function() {
      return(sprintf(
        "Multi-factor Strategy (RSI: %d/%d/%d, BB: %d/%.1f, OBV: %d/%.2f)",
        self$rsi_period, self$rsi_overbought, self$rsi_oversold,
        self$bb_period, self$bb_sd,
        self$obv_period, self$obv_threshold
      ))
    }
  )
) 