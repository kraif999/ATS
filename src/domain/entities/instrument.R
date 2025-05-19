#' @title Instrument Entity
#' @description Represents a financial instrument with its properties and data
#' @export
Instrument <- R6Class(
  "Instrument",
  public = list(
    #' @field symbol The instrument's symbol
    symbol = NULL,
    
    #' @field name The instrument's name
    name = NULL,
    
    #' @field type The instrument's type (e.g., "stock", "crypto", "forex")
    type = NULL,
    
    #' @field data The instrument's price data
    data = NULL,
    
    #' @description Initialize a new Instrument
    #' @param symbol The instrument's symbol
    #' @param name The instrument's name
    #' @param type The instrument's type
    initialize = function(symbol, name = NULL, type = NULL) {
      self$symbol <- symbol
      self$name <- name
      self$type <- type
    },
    
    #' @description Set the instrument's price data
    #' @param data The price data as an xts object
    set_data = function(data) {
      self$data <- data
    },
    
    #' @description Get the instrument's price data
    #' @return The price data as an xts object
    get_data = function() {
      return(self$data)
    }
  )
) 