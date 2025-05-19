test_that("Stochastic Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- StochasticStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "Stochastic Strategy")
  expect_equal(strategy$parameters$n, 14)
  expect_equal(strategy$parameters$m, 3)
  expect_equal(strategy$parameters$overbought, 80)
  expect_equal(strategy$parameters$oversold, 20)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- StochasticStrategy$new(n = 10, m = 5, overbought = 75, oversold = 25)
  expect_equal(strategy$parameters$n, 10)
  expect_equal(strategy$parameters$m, 5)
  expect_equal(strategy$parameters$overbought, 75)
  expect_equal(strategy$parameters$oversold, 25)
})

test_that("Stochastic Strategy signal generation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  high_prices <- cumsum(rnorm(100, 0.5, 1)) + 100  # Random walk
  low_prices <- high_prices - abs(rnorm(100, 0, 0.5))
  close_prices <- (high_prices + low_prices) / 2
  data <- xts::xts(
    cbind(high_prices, low_prices, close_prices),
    order.by = dates
  )
  colnames(data) <- c("AAPL.High", "AAPL.Low", "AAPL.Close")
  
  # Create strategy
  strategy <- StochasticStrategy$new(n = 5, m = 3, overbought = 80, oversold = 20)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "FastK", "FastD", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  expect_equal(signals$Signal[signals$FastK < 20], 1)   # Buy signal
  expect_equal(signals$Signal[signals$FastK > 80], -1)  # Sell signal
  expect_equal(signals$Signal[signals$FastK >= 20 & signals$FastK <= 80], 0)
})

test_that("Stochastic Strategy description works", {
  strategy <- StochasticStrategy$new(n = 10, m = 5, overbought = 75, oversold = 25)
  description <- strategy$get_description()
  expect_true(grepl("10", description))
  expect_true(grepl("5", description))
  expect_true(grepl("75", description))
  expect_true(grepl("25", description))
}) 