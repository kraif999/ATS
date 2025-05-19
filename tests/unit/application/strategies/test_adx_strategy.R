test_that("ADX Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- ADXStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "ADX Strategy")
  expect_equal(strategy$parameters$period, 14)
  expect_equal(strategy$parameters$threshold, 25)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- ADXStrategy$new(period = 10, threshold = 30)
  expect_equal(strategy$parameters$period, 10)
  expect_equal(strategy$parameters$threshold, 30)
})

test_that("ADX Strategy signal generation works", {
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
  strategy <- ADXStrategy$new(period = 5, threshold = 25)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "ADX", "DIp", "DIn", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  strong_trend <- signals$ADX > 25
  expect_equal(signals$Signal[strong_trend & signals$DIp > signals$DIn], 1)   # Buy signal
  expect_equal(signals$Signal[strong_trend & signals$DIn > signals$DIp], -1)  # Sell signal
  expect_equal(signals$Signal[!strong_trend | (signals$DIp == signals$DIn)], 0)
})

test_that("ADX Strategy description works", {
  strategy <- ADXStrategy$new(period = 10, threshold = 30)
  description <- strategy$get_description()
  expect_true(grepl("10", description))
  expect_true(grepl("30", description))
}) 