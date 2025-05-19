test_that("Trade initialization works", {
  # Create test objects
  instrument <- Instrument$new("AAPL")
  strategy <- Strategy$new("SMA Strategy")
  
  # Create a new trade
  trade <- Trade$new(
    instrument = instrument,
    strategy = strategy,
    entry_date = as.Date("2024-01-01"),
    entry_price = 100,
    position_size = 10,
    direction = "long"
  )
  
  # Test properties
  expect_false(is.null(trade$id))
  expect_equal(trade$instrument, instrument)
  expect_equal(trade$strategy, strategy)
  expect_equal(trade$entry_date, as.Date("2024-01-01"))
  expect_equal(trade$entry_price, 100)
  expect_equal(trade$position_size, 10)
  expect_equal(trade$direction, "long")
  expect_null(trade$exit_date)
  expect_null(trade$exit_price)
  expect_null(trade$pnl)
})

test_that("Trade closing works", {
  # Create test objects
  instrument <- Instrument$new("AAPL")
  strategy <- Strategy$new("SMA Strategy")
  
  # Create a new trade
  trade <- Trade$new(
    instrument = instrument,
    strategy = strategy,
    entry_date = as.Date("2024-01-01"),
    entry_price = 100,
    position_size = 10,
    direction = "long"
  )
  
  # Close the trade
  trade$close(as.Date("2024-01-10"), 110)
  
  # Test properties after closing
  expect_equal(trade$exit_date, as.Date("2024-01-10"))
  expect_equal(trade$exit_price, 110)
  expect_equal(trade$pnl, 100)  # (110 - 100) * 10
  expect_equal(trade$get_duration(), 9)
  expect_equal(trade$get_return(), 0.1)  # 100 / (100 * 10)
})

test_that("Trade PnL calculation works for short positions", {
  # Create test objects
  instrument <- Instrument$new("AAPL")
  strategy <- Strategy$new("SMA Strategy")
  
  # Create a new short trade
  trade <- Trade$new(
    instrument = instrument,
    strategy = strategy,
    entry_date = as.Date("2024-01-01"),
    entry_price = 100,
    position_size = 10,
    direction = "short"
  )
  
  # Close the trade
  trade$close(as.Date("2024-01-10"), 90)
  
  # Test PnL calculation for short position
  expect_equal(trade$pnl, 100)  # (100 - 90) * 10
  expect_equal(trade$get_return(), 0.1)  # 100 / (100 * 10)
}) 