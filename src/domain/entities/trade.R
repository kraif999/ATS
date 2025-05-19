#' @title Trade Entity
#' @description Represents a single trade in the system
#' @export
Trade <- R6Class(
  "Trade",
  public = list(
    #' @field id Unique identifier for the trade
    id = NULL,
    
    #' @field instrument The instrument being traded
    instrument = NULL,
    
    #' @field strategy The strategy that generated the trade
    strategy = NULL,
    
    #' @field entry_date Date of trade entry
    entry_date = NULL,
    
    #' @field exit_date Date of trade exit
    exit_date = NULL,
    
    #' @field entry_price Price at entry
    entry_price = NULL,
    
    #' @field exit_price Price at exit
    exit_price = NULL,
    
    #' @field position_size Size of the position
    position_size = NULL,
    
    #' @field direction Direction of the trade ("long" or "short")
    direction = NULL,
    
    #' @field pnl Profit and loss of the trade
    pnl = NULL,
    
    #' @description Initialize a new Trade
    #' @param instrument The instrument being traded
    #' @param strategy The strategy that generated the trade
    #' @param entry_date Date of trade entry
    #' @param entry_price Price at entry
    #' @param position_size Size of the position
    #' @param direction Direction of the trade
    initialize = function(instrument, strategy, entry_date, entry_price, position_size, direction) {
      self$id <- uuid::UUIDgenerate()
      self$instrument <- instrument
      self$strategy <- strategy
      self$entry_date <- entry_date
      self$entry_price <- entry_price
      self$position_size <- position_size
      self$direction <- direction
      self$exit_date <- NULL
      self$exit_price <- NULL
      self$pnl <- NULL
    },
    
    #' @description Close the trade
    #' @param exit_date Date of trade exit
    #' @param exit_price Price at exit
    close = function(exit_date, exit_price) {
      self$exit_date <- exit_date
      self$exit_price <- exit_price
      self$calculate_pnl()
    },
    
    #' @description Calculate the profit and loss
    calculate_pnl = function() {
      if (is.null(self$exit_price) || is.null(self$entry_price)) {
        stop("Cannot calculate PnL: missing entry or exit price")
      }
      
      price_diff <- self$exit_price - self$entry_price
      self$pnl <- if (self$direction == "long") {
        price_diff * self$position_size
      } else {
        -price_diff * self$position_size
      }
    },
    
    #' @description Get the trade's duration in days
    #' @return The duration in days
    get_duration = function() {
      if (is.null(self$exit_date)) {
        return(NA)
      }
      as.numeric(self$exit_date - self$entry_date)
    },
    
    #' @description Get the trade's return
    #' @return The return as a decimal
    get_return = function() {
      if (is.null(self$pnl)) {
        return(NA)
      }
      self$pnl / (self$entry_price * self$position_size)
    }
  )
) 