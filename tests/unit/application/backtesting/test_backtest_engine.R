test_that("BacktestEngine initialization works", {
  # Create strategy and instrument
  strategy <- SMAStrategy$new()
  instrument <- Instrument$new("AAPL", "Apple Inc.")
  
  # Create a new engine with default parameters
  engine <- BacktestEngine$new(strategy, instrument)
  
  # Test properties
  expect_equal(engine$strategy, strategy)
  expect_equal(engine$instrument, instrument)
  expect_equal(engine$initial_capital, 100000)
  expect_equal(length(engine$trades), 0)
  expect_null(engine$portfolio_value)
  expect_null(engine$risk_manager)
  expect_null(engine$returns_matrix)
  expect_null(engine$instrument_ids)
  
  # Create a new engine with custom parameters and risk manager
  risk_manager <- RiskManager$new(
    max_position_size = 0.2,
    stop_loss = 0.03,
    take_profit = 0.06,
    max_drawdown = 0.15,
    volatility_window = 30,
    target_volatility = 0.12,
    correlation_threshold = 0.6
  )
  engine <- BacktestEngine$new(
    strategy = strategy,
    instrument = instrument,
    initial_capital = 200000,
    risk_manager = risk_manager
  )
  expect_equal(engine$initial_capital, 200000)
  expect_equal(engine$risk_manager, risk_manager)
})

test_that("BacktestEngine run works with risk management", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- cumsum(rnorm(100, 0, 1)) + 100  # Random walk
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy and instrument
  strategy <- SMAStrategy$new(fast_window = 5, slow_window = 10)
  instrument <- Instrument$new("AAPL", "Apple Inc.")
  
  # Create risk manager
  risk_manager <- RiskManager$new(
    max_position_size = 0.2,
    stop_loss = 0.02,
    take_profit = 0.04,
    max_drawdown = 0.15,
    volatility_window = 20,
    target_volatility = 0.15,
    correlation_threshold = 0.7
  )
  
  # Create engine
  engine <- BacktestEngine$new(
    strategy = strategy,
    instrument = instrument,
    initial_capital = 100000,
    risk_manager = risk_manager
  )
  
  # Run backtest
  results <- engine$run(data)
  
  # Test results
  expect_type(results, "list")
  expect_true(all(c("total_return", "annualized_return", "sharpe_ratio", "max_drawdown", 
                    "num_trades", "win_rate", "volatility", "var_95", "portfolio_value") %in% names(results)))
  expect_true(is.data.frame(results$portfolio_value))
  expect_equal(nrow(results$portfolio_value), 100)
  
  # Test risk metrics
  expect_true(is.numeric(results$volatility))
  expect_true(is.numeric(results$var_95))
  expect_true(results$volatility > 0)
  expect_true(results$var_95 < 0)
})

test_that("BacktestEngine respects risk management rules", {
  # Create sample data with a clear trend
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- seq(100, 200, length.out = 100)  # Clear uptrend
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy and instrument
  strategy <- SMAStrategy$new(fast_window = 5, slow_window = 10)
  instrument <- Instrument$new("AAPL", "Apple Inc.")
  
  # Create risk manager with tight stop loss
  risk_manager <- RiskManager$new(
    max_position_size = 0.2,
    stop_loss = 0.01,  # 1% stop loss
    take_profit = 0.02,  # 2% take profit
    max_drawdown = 0.05,  # 5% max drawdown
    volatility_window = 20,
    target_volatility = 0.15,
    correlation_threshold = 0.7
  )
  
  # Create engine
  engine <- BacktestEngine$new(
    strategy = strategy,
    instrument = instrument,
    initial_capital = 100000,
    risk_manager = risk_manager
  )
  
  # Run backtest
  results <- engine$run(data)
  
  # Test that risk management rules were followed
  expect_true(results$max_drawdown >= -0.05)  # Max drawdown should not exceed 5%
  
  # Check that position sizes were limited
  if (length(engine$trades) > 0) {
    max_position_value <- max(sapply(engine$trades, function(t) t$size * t$entry_price))
    expect_true(max_position_value <= 100000 * 0.2)  # Position size should not exceed 20% of capital
  }
  
  # Test volatility-based position sizing
  expect_true(is.numeric(results$volatility))
  expect_true(results$volatility > 0)
  
  # Test correlation tracking
  expect_true(is.matrix(engine$returns_matrix))
  expect_equal(ncol(engine$returns_matrix), 1)
  expect_equal(engine$instrument_ids, "AAPL")
})

test_that("BacktestEngine handles drawdown limits", {
  # Create sample data with a sharp decline
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- c(
    rep(100, 50),  # Stable price
    seq(100, 50, length.out = 50)  # Sharp decline
  )
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy and instrument
  strategy <- SMAStrategy$new(fast_window = 5, slow_window = 10)
  instrument <- Instrument$new("AAPL", "Apple Inc.")
  
  # Create risk manager with strict drawdown limit
  risk_manager <- RiskManager$new(
    max_position_size = 0.2,
    stop_loss = 0.02,
    take_profit = 0.04,
    max_drawdown = 0.1,  # 10% max drawdown
    volatility_window = 20,
    target_volatility = 0.15,
    correlation_threshold = 0.7
  )
  
  # Create engine
  engine <- BacktestEngine$new(
    strategy = strategy,
    instrument = instrument,
    initial_capital = 100000,
    risk_manager = risk_manager
  )
  
  # Run backtest
  results <- engine$run(data)
  
  # Test that drawdown limit was respected
  expect_true(results$max_drawdown >= -0.1)  # Max drawdown should not exceed 10%
  
  # Check that positions were closed when drawdown limit was hit
  if (length(engine$trades) > 0) {
    last_trade_date <- max(sapply(engine$trades, function(t) t$exit_date))
    expect_true(last_trade_date < dates[100])  # Should have closed positions before the end
  }
  
  # Test risk metrics
  expect_true(is.numeric(results$volatility))
  expect_true(is.numeric(results$var_95))
  expect_true(results$volatility > 0)
  expect_true(results$var_95 < 0)
})

test_that("BacktestEngine trade execution works", {
  # Create sample data with clear trend
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- seq(100, 200, length.out = 100)  # Upward trend
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create test objects
  instrument <- Instrument$new("AAPL")
  strategy <- SMAStrategy$new(fast_window = 5, slow_window = 10)
  engine <- BacktestEngine$new(strategy, instrument)
  
  # Run backtest
  results <- engine$run(data, position_size = 1)
  
  # Test trade execution
  expect_true(length(results$trades) > 0)
  expect_true(all(sapply(results$trades, function(t) t$pnl != 0)))
  expect_true(results$total_return > 0)  # Should be profitable in uptrend
  
  # Test risk metrics
  expect_true(is.numeric(results$volatility))
  expect_true(is.numeric(results$var_95))
  expect_true(results$volatility > 0)
  expect_true(results$var_95 < 0)
}) 