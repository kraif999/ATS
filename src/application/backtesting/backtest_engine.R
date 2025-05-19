#' @title Backtest Engine
#' @description Executes and evaluates trading strategies on historical data
#' @export
BacktestEngine <- R6Class(
  "BacktestEngine",
  public = list(
    #' @field strategy The strategy to backtest
    strategy = NULL,
    
    #' @field instrument The instrument to trade
    instrument = NULL,
    
    #' @field initial_capital Initial capital for the backtest
    initial_capital = NULL,
    
    #' @field trades List of trades executed during the backtest
    trades = NULL,
    
    #' @field portfolio_value Portfolio value over time
    portfolio_value = NULL,
    
    #' @field risk_manager Risk manager for position sizing and risk control
    risk_manager = NULL,
    
    #' @field returns_matrix Matrix of returns for correlation tracking
    returns_matrix = NULL,
    
    #' @field instrument_ids Vector of instrument identifiers
    instrument_ids = NULL,
    
    #' @description Initialize a new Backtest Engine
    #' @param strategy The strategy to backtest
    #' @param instrument The instrument to trade
    #' @param initial_capital Initial capital for the backtest
    #' @param risk_manager Risk manager for position sizing and risk control
    initialize = function(strategy, instrument, initial_capital = 100000, risk_manager = NULL) {
      self$strategy <- strategy
      self$instrument <- instrument
      self$initial_capital <- initial_capital
      self$trades <- list()
      self$portfolio_value <- NULL
      self$risk_manager <- risk_manager
      self$returns_matrix <- NULL
      self$instrument_ids <- NULL
    },
    
    #' @description Run the backtest
    #' @param data The price data to backtest on
    #' @param position_size The position size to use (if risk_manager is NULL)
    #' @return A list of backtest results
    run = function(data, position_size = 100) {
      # Generate signals
      signals <- self$strategy$generate_signals(data)
      
      # Initialize portfolio tracking
      portfolio_value <- numeric(nrow(signals))
      portfolio_value[1] <- self$initial_capital
      current_position <- 0
      current_trade <- NULL
      peak_value <- self$initial_capital
      
      # Calculate returns for volatility and correlation
      if (!is.null(self$risk_manager)) {
        returns <- diff(signals$Close) / signals$Close[-nrow(signals)]
        self$returns_matrix <- matrix(returns, ncol = 1)
        self$instrument_ids <- c(self$instrument$symbol)
        self$risk_manager$update_correlations(self$returns_matrix, self$instrument_ids)
      }
      
      # Process each signal
      for (i in 2:nrow(signals)) {
        # Update portfolio value
        if (current_position != 0) {
          price_change <- (signals$Close[i] - signals$Close[i-1]) / signals$Close[i-1]
          portfolio_value[i] <- portfolio_value[i-1] * (1 + price_change * current_position)
        } else {
          portfolio_value[i] <- portfolio_value[i-1]
        }
        
        # Update peak value
        peak_value <- max(peak_value, portfolio_value[i])
        
        # Check risk management conditions
        if (!is.null(self$risk_manager)) {
          # Check drawdown
          if (self$risk_manager$update_drawdown(peak_value, portfolio_value[i])) {
            # Close all positions if max drawdown is exceeded
            if (current_position != 0) {
              trade <- self$close_trade(signals$Date[i], signals$Close[i], current_position)
              self$trades <- c(self$trades, list(trade))
              current_position <- 0
              current_trade <- NULL
            }
            next
          }
          
          # Check stop loss and take profit
          if (!is.null(current_trade) && self$risk_manager$check_exit_conditions(current_trade, signals$Close[i])) {
            trade <- self$close_trade(signals$Date[i], signals$Close[i], current_position)
            self$trades <- c(self$trades, list(trade))
            current_position <- 0
            current_trade <- NULL
            next
          }
        }
        
        # Process new signals
        if (signals$Position[i] != 0) {
          # Close existing position if any
          if (current_position != 0) {
            trade <- self$close_trade(signals$Date[i], signals$Close[i], current_position)
            self$trades <- c(self$trades, list(trade))
            current_position <- 0
            current_trade <- NULL
          }
          
          # Open new position
          if (signals$Signal[i] != 0) {
            # Calculate position size
            if (!is.null(self$risk_manager)) {
              # Get historical returns for volatility calculation
              returns <- NULL
              if (i > self$risk_manager$volatility_window) {
                returns <- diff(signals$Close[(i-self$risk_manager$volatility_window):i]) / 
                          signals$Close[(i-self$risk_manager$volatility_window):(i-1)]
              }
              
              position_size <- self$risk_manager$calculate_position_size(
                portfolio_value[i],
                signals$Close[i],
                returns = returns,
                instrument_id = self$instrument$symbol
              )
            }
            
            current_position <- signals$Signal[i] * position_size
            current_trade <- list(
              direction = ifelse(signals$Signal[i] > 0, "long", "short"),
              entry_price = signals$Close[i],
              entry_date = signals$Date[i]
            )
          }
        }
      }
      
      # Close any open position at the end
      if (current_position != 0) {
        trade <- self$close_trade(signals$Date[nrow(signals)], signals$Close[nrow(signals)], current_position)
        self$trades <- c(self$trades, list(trade))
      }
      
      # Store portfolio value
      self$portfolio_value <- data.frame(
        Date = signals$Date,
        Value = portfolio_value
      )
      
      return(self$get_results())
    },
    
    #' @description Close a trade
    #' @param date The date to close the trade
    #' @param price The price to close the trade at
    #' @param position The position size
    #' @return A Trade object
    close_trade = function(date, price, position) {
      trade <- Trade$new(
        instrument = self$instrument,
        direction = ifelse(position > 0, "long", "short"),
        entry_price = current_trade$entry_price,
        entry_date = current_trade$entry_date,
        exit_price = price,
        exit_date = date,
        size = abs(position)
      )
      return(trade)
    },
    
    #' @description Get backtest results
    #' @return A list of performance metrics
    get_results = function() {
      if (is.null(self$portfolio_value)) {
        return(NULL)
      }
      
      # Calculate returns
      returns <- diff(self$portfolio_value$Value) / self$portfolio_value$Value[-nrow(self$portfolio_value)]
      
      # Calculate metrics
      total_return <- (self$portfolio_value$Value[nrow(self$portfolio_value)] - self$initial_capital) / self$initial_capital
      annualized_return <- (1 + total_return) ^ (252 / nrow(self$portfolio_value)) - 1
      sharpe_ratio <- mean(returns) / sd(returns) * sqrt(252)
      max_drawdown <- min(returns)
      
      # Calculate trade statistics
      num_trades <- length(self$trades)
      winning_trades <- sum(sapply(self$trades, function(t) t$pnl > 0))
      win_rate <- winning_trades / num_trades if num_trades > 0 else 0
      
      # Calculate risk metrics
      volatility <- sd(returns) * sqrt(252)
      var_95 <- quantile(returns, 0.05)
      
      return(list(
        total_return = total_return,
        annualized_return = annualized_return,
        sharpe_ratio = sharpe_ratio,
        max_drawdown = max_drawdown,
        num_trades = num_trades,
        win_rate = win_rate,
        volatility = volatility,
        var_95 = var_95,
        portfolio_value = self$portfolio_value
      ))
    }
  )
) 