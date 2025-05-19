test_that("Strategy initialization works", {
  # Create a new strategy
  strategy <- Strategy$new("SMA Strategy", list(window = 20))
  
  # Test properties
  expect_equal(strategy$name, "SMA Strategy")
  expect_equal(strategy$parameters, list(window = 20))
  expect_null(strategy$signals)
})

test_that("Strategy parameter methods work", {
  # Create a new strategy
  strategy <- Strategy$new("SMA Strategy", list(window = 20))
  
  # Test get_parameters
  expect_equal(strategy$get_parameters(), list(window = 20))
  
  # Test set_parameters
  new_params <- list(window = 50, threshold = 0.02)
  strategy$set_parameters(new_params)
  expect_equal(strategy$get_parameters(), new_params)
})

test_that("Strategy signal methods work", {
  # Create a new strategy
  strategy <- Strategy$new("SMA Strategy", list(window = 20))
  
  # Test generate_signals (should throw error as it's abstract)
  expect_error(strategy$generate_signals(NULL), "Method must be implemented by subclasses")
  
  # Test get_signals
  expect_null(strategy$get_signals())
}) 