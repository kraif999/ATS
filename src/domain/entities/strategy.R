#' @title Strategy Entity
#' @description Base class for all trading strategies
#' @export
Strategy <- R6Class(
  "Strategy",
  public = list(
    #' @field name The strategy's name
    name = NULL,
    
    #' @field parameters The strategy's parameters
    parameters = NULL,
    
    #' @field signals The generated trading signals
    signals = NULL,
    
    #' @description Initialize a new Strategy
    #' @param name The strategy's name
    #' @param parameters The strategy's parameters
    initialize = function(name, parameters = list()) {
      self$name <- name
      self$parameters <- parameters
      self$signals <- NULL
    },
    
    #' @description Generate trading signals
    #' @param data The price data to generate signals from
    #' @return A data frame containing the signals
    generate_signals = function(data) {
      stop("Method must be implemented by subclasses")
    },
    
    #' @description Get the strategy's parameters
    #' @return The strategy's parameters
    get_parameters = function() {
      return(self$parameters)
    },
    
    #' @description Set the strategy's parameters
    #' @param parameters The new parameters
    set_parameters = function(parameters) {
      self$parameters <- parameters
    },
    
    #' @description Get the generated signals
    #' @return The generated signals
    get_signals = function() {
      return(self$signals)
    }
  )
) 