#' OBV Strategy
#' @description Implements a volume-based strategy using On-Balance Volume (OBV)
#' @export
OBVStrategy <- R6::R6Class("OBVStrategy",
  inherit = Strategy,
  public = list(
    #' @field period Moving average period for OBV
    period = NULL,
    
    #' @field threshold Signal threshold
    threshold = NULL,
    
    #' @description Initialize OBV strategy
    #' @param period Moving average period (default: 20)
    #' @param threshold Signal threshold (default: 0.02)
    initialize = function(period = 20, threshold = 0.02) {
      super$initialize("OBV Strategy")
      self$period <- period
      self$threshold <- threshold
    },
    
    #' @description Generate trading signals based on OBV
    #' @param data Data frame with price and volume data
    #' @return Data frame with signals
    generate_signals = function(data) {
      # Calculate OBV
      obv <- TTR::OBV(data$Close, data$Volume)
      
      # Calculate OBV moving average
      obv_ma <- TTR::SMA(obv, n = self$period)
      
      # Calculate OBV momentum
      obv_momentum <- (obv - obv_ma) / obv_ma
      
      # Generate signals
      signals <- data.frame(
        Date = data$Date,
        Close = data$Close,
        OBV = obv,
        OBV_MA = obv_ma,
        OBV_Momentum = obv_momentum,
        Signal = 0,
        Position = 0
      )
      
      # Buy signal when OBV momentum crosses above threshold
      signals$Signal[signals$OBV_Momentum > self$threshold] <- 1
      
      # Sell signal when OBV momentum crosses below -threshold
      signals$Signal[signals$OBV_Momentum < -self$threshold] <- -1
      
      # Calculate positions
      signals$Position <- cumsum(signals$Signal)
      
      self$signals <- signals
      return(signals)
    },
    
    #' @description Get strategy description
    #' @return String description
    get_description = function() {
      return(sprintf(
        "OBV Strategy (Period: %d, Threshold: %.2f)",
        self$period, self$threshold
      ))
    }
  )
) 