test_that("Bollinger Bands Strategy initialization works", {
  # Create a new strategy with default parameters
  strategy <- BollingerBandsStrategy$new()
  
  # Test properties
  expect_equal(strategy$name, "Bollinger Bands Strategy")
  expect_equal(strategy$parameters$period, 20)
  expect_equal(strategy$parameters$sd, 2)
  expect_null(strategy$signals)
  
  # Create a new strategy with custom parameters
  strategy <- BollingerBandsStrategy$new(period = 14, sd = 2.5)
  expect_equal(strategy$parameters$period, 14)
  expect_equal(strategy$parameters$sd, 2.5)
})

test_that("Bollinger Bands Strategy signal generation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  prices <- cumsum(rnorm(100, 0, 1)) + 100  # Random walk
  data <- xts::xts(prices, order.by = dates)
  colnames(data) <- "AAPL.Close"
  
  # Create strategy
  strategy <- BollingerBandsStrategy$new(period = 10, sd = 2)
  
  # Generate signals
  signals <- strategy$generate_signals(data)
  
  # Test signal properties
  expect_equal(nrow(signals), 100)
  expect_true(all(c("Date", "Close", "Middle", "Upper", "Lower", "Signal", "Position") %in% colnames(signals)))
  expect_true(all(signals$Signal %in% c(-1, 0, 1)))
  
  # Test signal calculation
  expect_equal(signals$Signal[signals$Close < signals$Lower], 1)   # Buy signal
  expect_equal(signals$Signal[signals$Close > signals$Upper], -1)  # Sell signal
  expect_equal(signals$Signal[signals$Close >= signals$Lower & signals$Close <= signals$Upper], 0)
})

test_that("Bollinger Bands Strategy description works", {
  strategy <- BollingerBandsStrategy$new(period = 14, sd = 2.5)
  description <- strategy$get_description()
  expect_true(grepl("14", description))
  expect_true(grepl("2.5", description))
}) 