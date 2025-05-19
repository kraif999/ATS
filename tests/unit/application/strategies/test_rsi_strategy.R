test_that("RSI Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- RSIStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "RSI Strategy")
  expect_equal(strategy$parameters$period, 14)
  expect_equal(strategy$parameters$overbought, 70)
  expect_equal(strategy$parameters$oversold, 30)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- RSIStrategy$new(period = 10, overbought = 75, oversold = 25)
  expect_equal(strategy$parameters$period, 10)
  expect_equal(strategy$parameters$overbought, 75)
  expect_equal(strategy$parameters$oversold, 25)
})

test_that("RSI Strategy signal generation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- cumsum(rnorm(100, 0, 1)) + 100  # Random walk
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy
  strategy <- RSIStrategy$new(period = 5, overbought = 70, oversold = 30)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "RSI", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  expect_equal(signals$Signal[signals$RSI < 30], 1)   # Buy signal
  expect_equal(signals$Signal[signals$RSI > 70], -1)  # Sell signal
  expect_equal(signals$Signal[signals$RSI >= 30 & signals$RSI <= 70], 0)
})

test_that("RSI Strategy description works", {
  strategy <- RSIStrategy$new(period = 10, overbought = 75, oversold = 25)
  description <- strategy$get_description()
  expect_true(grepl("10", description))
  expect_true(grepl("75", description))
  expect_true(grepl("25", description))
}) 