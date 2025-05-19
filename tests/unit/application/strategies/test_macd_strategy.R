test_that("MACD Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- MACDStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "MACD Strategy")
  expect_equal(strategy$parameters$fast_period, 12)
  expect_equal(strategy$parameters$slow_period, 26)
  expect_equal(strategy$parameters$signal_period, 9)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- MACDStrategy$new(fast_period = 8, slow_period = 21, signal_period = 5)
  expect_equal(strategy$parameters$fast_period, 8)
  expect_equal(strategy$parameters$slow_period, 21)
  expect_equal(strategy$parameters$signal_period, 5)
})

test_that("MACD Strategy signal generation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- cumsum(rnorm(100, 0, 1)) + 100  # Random walk
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy
  strategy <- MACDStrategy$new(fast_period = 5, slow_period = 10, signal_period = 3)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "MACD", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  expect_equal(signals$Signal[signals$MACD > signals$Signal], 1)   # Buy signal
  expect_equal(signals$Signal[signals$MACD < signals$Signal], -1)  # Sell signal
  expect_equal(signals$Signal[signals$MACD == signals$Signal], 0)
})

test_that("MACD Strategy description works", {
  strategy <- MACDStrategy$new(fast_period = 8, slow_period = 21, signal_period = 5)
  description <- strategy$get_description()
  expect_true(grepl("8", description))
  expect_true(grepl("21", description))
  expect_true(grepl("5", description))
}) 