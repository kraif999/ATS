test_that("RiskManager initialization works", {
  # Create a new risk manager with default parameters
  risk_manager <- RiskManager$new()
  
  # Test properties
  expect_equal(risk_manager$max_position_size, 0.1)
  expect_equal(risk_manager$stop_loss, 0.02)
  expect_equal(risk_manager$take_profit, 0.04)
  expect_equal(risk_manager$max_drawdown, 0.2)
  expect_equal(risk_manager$current_drawdown, 0)
  expect_equal(risk_manager$portfolio_value, 0)
  expect_equal(risk_manager$volatility_window, 20)
  expect_equal(risk_manager$target_volatility, 0.15)
  expect_equal(risk_manager$correlation_threshold, 0.7)
  expect_null(risk_manager$position_correlations)
  
  # Create a new risk manager with custom parameters
  risk_manager <- RiskManager$new(
    max_position_size = 0.2,
    stop_loss = 0.03,
    take_profit = 0.06,
    max_drawdown = 0.15,
    volatility_window = 30,
    target_volatility = 0.12,
    correlation_threshold = 0.6
  )
  expect_equal(risk_manager$max_position_size, 0.2)
  expect_equal(risk_manager$stop_loss, 0.03)
  expect_equal(risk_manager$take_profit, 0.06)
  expect_equal(risk_manager$max_drawdown, 0.15)
  expect_equal(risk_manager$volatility_window, 30)
  expect_equal(risk_manager$target_volatility, 0.12)
  expect_equal(risk_manager$correlation_threshold, 0.6)
})

test_that("RiskManager position sizing works", {
  risk_manager <- RiskManager$new(max_position_size = 0.1)
  
  # Test basic position sizing
  portfolio_value <- 100000
  price <- 100
  
  # Calculate position size
  units <- risk_manager$calculate_position_size(portfolio_value, price)
  
  # Test position size calculation
  expect_equal(units, 100)  # 10% of portfolio = 10000, divided by price = 100 units
  expect_equal(risk_manager$portfolio_value, portfolio_value)
  
  # Test volatility-based position sizing
  returns <- rnorm(30, 0, 0.02)  # Generate returns with 20% annualized volatility
  units_vol <- risk_manager$calculate_position_size(portfolio_value, price, returns = returns)
  
  # Position size should be reduced due to high volatility
  expect_true(units_vol < units)
  
  # Test correlation-based position sizing
  # Create correlation matrix
  returns_matrix <- matrix(rnorm(100), ncol = 2)
  instrument_ids <- c("AAPL", "MSFT")
  risk_manager$update_correlations(returns_matrix, instrument_ids)
  
  # Calculate position size with correlation check
  units_corr <- risk_manager$calculate_position_size(portfolio_value, price, instrument_id = "AAPL")
  
  # Position size should be reduced if correlation is high
  expect_true(units_corr <= units)
})

test_that("RiskManager correlation tracking works", {
  risk_manager <- RiskManager$new(correlation_threshold = 0.7)
  
  # Create sample returns data
  returns_matrix <- matrix(rnorm(100), ncol = 2)
  instrument_ids <- c("AAPL", "MSFT")
  
  # Update correlations
  risk_manager$update_correlations(returns_matrix, instrument_ids)
  
  # Test correlation matrix
  expect_true(is.matrix(risk_manager$position_correlations))
  expect_equal(dim(risk_manager$position_correlations), c(2, 2))
  expect_equal(rownames(risk_manager$position_correlations), instrument_ids)
  expect_equal(colnames(risk_manager$position_correlations), instrument_ids)
  
  # Test position sizing with correlation
  portfolio_value <- 100000
  price <- 100
  
  # Calculate position size for first instrument
  units1 <- risk_manager$calculate_position_size(portfolio_value, price, instrument_id = "AAPL")
  
  # Calculate position size for second instrument
  units2 <- risk_manager$calculate_position_size(portfolio_value, price, instrument_id = "MSFT")
  
  # If correlation is high, position sizes should be reduced
  expect_true(units1 <= portfolio_value * 0.1 / price)
  expect_true(units2 <= portfolio_value * 0.1 / price)
})

test_that("RiskManager exit conditions work", {
  risk_manager <- RiskManager$new(stop_loss = 0.02, take_profit = 0.04)
  
  # Create a long trade
  long_trade <- list(
    direction = "long",
    entry_price = 100
  )
  
  # Test stop loss
  expect_true(risk_manager$check_exit_conditions(long_trade, 97))  # 3% loss > 2% stop loss
  
  # Test take profit
  expect_true(risk_manager$check_exit_conditions(long_trade, 105))  # 5% gain > 4% take profit
  
  # Test no exit
  expect_false(risk_manager$check_exit_conditions(long_trade, 101))  # 1% gain < 4% take profit
  
  # Create a short trade
  short_trade <- list(
    direction = "short",
    entry_price = 100
  )
  
  # Test stop loss
  expect_true(risk_manager$check_exit_conditions(short_trade, 103))  # 3% loss > 2% stop loss
  
  # Test take profit
  expect_true(risk_manager$check_exit_conditions(short_trade, 95))  # 5% gain > 4% take profit
  
  # Test no exit
  expect_false(risk_manager$check_exit_conditions(short_trade, 99))  # 1% gain < 4% take profit
})

test_that("RiskManager drawdown tracking works", {
  risk_manager <- RiskManager$new(max_drawdown = 0.2)
  
  # Test drawdown calculation
  peak_value <- 100000
  current_value <- 90000
  
  # Update drawdown
  exceeded <- risk_manager$update_drawdown(peak_value, current_value)
  
  # Test drawdown
  expect_equal(risk_manager$current_drawdown, 0.1)  # 10% drawdown
  expect_false(exceeded)  # 10% < 20% max drawdown
  
  # Test max drawdown exceeded
  current_value <- 75000
  exceeded <- risk_manager$update_drawdown(peak_value, current_value)
  
  expect_equal(risk_manager$current_drawdown, 0.25)  # 25% drawdown
  expect_true(exceeded)  # 25% > 20% max drawdown
})

test_that("RiskManager parameters retrieval works", {
  risk_manager <- RiskManager$new(
    max_position_size = 0.2,
    stop_loss = 0.03,
    take_profit = 0.06,
    max_drawdown = 0.15,
    volatility_window = 30,
    target_volatility = 0.12,
    correlation_threshold = 0.6
  )
  
  # Get parameters
  params <- risk_manager$get_parameters()
  
  # Test parameters
  expect_equal(params$max_position_size, 0.2)
  expect_equal(params$stop_loss, 0.03)
  expect_equal(params$take_profit, 0.06)
  expect_equal(params$max_drawdown, 0.15)
  expect_equal(params$current_drawdown, 0)
  expect_equal(params$volatility_window, 30)
  expect_equal(params$target_volatility, 0.12)
  expect_equal(params$correlation_threshold, 0.6)
}) 