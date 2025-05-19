test_that("SMA Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- SMAStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "SMA Strategy")
  expect_equal(strategy$parameters$fast_window, 20)
  expect_equal(strategy$parameters$slow_window, 50)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- SMAStrategy$new(fast_window = 10, slow_window = 30)
  expect_equal(strategy$parameters$fast_window, 10)
  expect_equal(strategy$parameters$slow_window, 30)
})

test_that("SMA Strategy signal generation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- cumsum(rnorm(100, 0, 1)) + 100  # Random walk
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy
  strategy <- SMAStrategy$new(fast_window = 5, slow_window = 10)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "Fast_SMA", "Slow_SMA", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  expect_equal(signals$Signal[signals$Fast_SMA > signals$Slow_SMA], 1)
  expect_equal(signals$Signal[signals$Fast_SMA < signals$Slow_SMA], -1)
  expect_equal(signals$Signal[signals$Fast_SMA == signals$Slow_SMA], 0)
})

test_that("SMA Strategy description works", {
  strategy <- SMAStrategy$new(fast_window = 10, slow_window = 30)
  description <- strategy$get_description()
  expect_true(grepl("10", description))
  expect_true(grepl("30", description))
}) 