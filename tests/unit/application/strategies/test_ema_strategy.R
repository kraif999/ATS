test_that("EMA Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- EMAStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "EMA Strategy")
  expect_equal(strategy$parameters$fast_window, 12)
  expect_equal(strategy$parameters$slow_window, 26)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- EMAStrategy$new(fast_window = 8, slow_window = 21)
  expect_equal(strategy$parameters$fast_window, 8)
  expect_equal(strategy$parameters$slow_window, 21)
})

test_that("EMA Strategy signal generation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- cumsum(rnorm(100, 0, 1)) + 100  # Random walk
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy
  strategy <- EMAStrategy$new(fast_window = 5, slow_window = 10)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "Fast_EMA", "Slow_EMA", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  expect_equal(signals$Signal[signals$Fast_EMA > signals$Slow_EMA], 1)
  expect_equal(signals$Signal[signals$Fast_EMA < signals$Slow_EMA], -1)
  expect_equal(signals$Signal[signals$Fast_EMA == signals$Slow_EMA], 0)
})

test_that("EMA Strategy description works", {
  strategy <- EMAStrategy$new(fast_window = 8, slow_window = 21)
  description <- strategy$get_description()
  expect_true(grepl("8", description))
  expect_true(grepl("21", description))
}) 