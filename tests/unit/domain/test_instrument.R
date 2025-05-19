test_that("Instrument initialization works", {
  # Create a new instrument
  instrument <- Instrument$new("AAPL", "Apple Inc.", "stock")
  
  # Test properties
  expect_equal(instrument$symbol, "AAPL")
  expect_equal(instrument$name, "Apple Inc.")
  expect_equal(instrument$type, "stock")
  expect_null(instrument$data)
})

test_that("Instrument data methods work", {
  # Create a new instrument
  instrument <- Instrument$new("AAPL")
  
  # Create sample data
  sample_data <- xts::xts(
    matrix(1:10, ncol = 1),
    order.by = as.Date("2024-01-01") + 0:9
  )
  
  # Test set_data
  instrument$set_data(sample_data)
  expect_equal(instrument$data, sample_data)
  
  # Test get_data
  retrieved_data <- instrument$get_data()
  expect_equal(retrieved_data, sample_data)
}) 