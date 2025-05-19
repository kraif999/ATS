#' Machine Learning Strategy
#' @description Implements a strategy using machine learning for signal generation
#' @export
MLStrategy <- R6::R6Class("MLStrategy",
  inherit = Strategy,
  public = list(
    #' @field lookback_period Number of periods to look back for features
    lookback_period = NULL,
    
    #' @field prediction_horizon Number of periods to predict ahead
    prediction_horizon = NULL,
    
    #' @field model The trained machine learning model
    model = NULL,
    
    #' @field features List of feature calculation functions
    features = NULL,
    
    #' @description Initialize ML strategy
    #' @param lookback_period Number of periods to look back (default: 20)
    #' @param prediction_horizon Number of periods to predict ahead (default: 5)
    initialize = function(lookback_period = 20, prediction_horizon = 5) {
      super$initialize("Machine Learning Strategy")
      self$lookback_period <- lookback_period
      self$prediction_horizon <- prediction_horizon
      self$model <- NULL
      self$features <- list(
        # Price-based features
        returns = function(x) diff(log(x)),
        volatility = function(x) TTR::volatility(x, n = 10),
        rsi = function(x) TTR::RSI(x, n = 14),
        macd = function(x) TTR::MACD(x)$macd,
        
        # Volume-based features
        volume_ma = function(x, v) TTR::SMA(v, n = 20),
        obv = function(x, v) TTR::OBV(x, v),
        
        # Technical indicators
        bb_upper = function(x) TTR::BBands(x)$up,
        bb_lower = function(x) TTR::BBands(x)$dn
      )
    },
    
    #' @description Train the machine learning model
    #' @param data Training data
    train = function(data) {
      # Prepare features
      X <- self$prepare_features(data)
      
      # Prepare target (future returns)
      y <- self$prepare_target(data)
      
      # Train model (using random forest as an example)
      self$model <- randomForest::randomForest(
        x = X,
        y = y,
        ntree = 100,
        importance = TRUE
      )
    },
    
    #' @description Prepare features for model training/prediction
    #' @param data Price and volume data
    #' @return Matrix of features
    prepare_features = function(data) {
      features_list <- list()
      
      # Calculate each feature
      for (name in names(self$features)) {
        if (name %in% c("obv", "volume_ma")) {
          features_list[[name]] <- self$features[[name]](data$Close, data$Volume)
        } else {
          features_list[[name]] <- self$features[[name]](data$Close)
        }
      }
      
      # Combine features into matrix
      X <- do.call(cbind, features_list)
      return(X)
    },
    
    #' @description Prepare target variable (future returns)
    #' @param data Price data
    #' @return Vector of future returns
    prepare_target = function(data) {
      # Calculate future returns
      future_returns <- c(
        diff(log(data$Close), lag = self$prediction_horizon),
        rep(NA, self$prediction_horizon)
      )
      
      # Convert to binary classification (1 for positive returns, 0 for negative)
      y <- as.factor(ifelse(future_returns > 0, 1, 0))
      
      return(y)
    },
    
    #' @description Generate trading signals using the trained model
    #' @param data Data frame with price and volume data
    #' @return Data frame with signals
    generate_signals = function(data) {
      if (is.null(self$model)) {
        stop("Model must be trained before generating signals")
      }
      
      # Prepare features
      X <- self$prepare_features(data)
      
      # Make predictions
      predictions <- predict(self$model, X, type = "prob")
      
      # Generate signals
      signals <- data.frame(
        Date = data$Date,
        Close = data$Close,
        Prediction = predictions[, 2],
        Signal = 0,
        Position = 0
      )
      
      # Buy signal when probability > 0.6
      signals$Signal[signals$Prediction > 0.6] <- 1
      
      # Sell signal when probability < 0.4
      signals$Signal[signals$Prediction < 0.4] <- -1
      
      # Calculate positions
      signals$Position <- cumsum(signals$Signal)
      
      self$signals <- signals
      return(signals)
    },
    
    #' @description Get strategy description
    #' @return String description
    get_description = function() {
      return(sprintf(
        "Machine Learning Strategy (Lookback: %d, Horizon: %d)",
        self$lookback_period, self$prediction_horizon
      ))
    }
  )
) 