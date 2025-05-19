test_that("Risk dashboard creation works", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  portfolio_value <- data.frame(
    Date = dates,
    Value = cumsum(rnorm(100, 0.001, 0.02)) + 100000
  )
  
  # Create sample trades
  trades <- list(
    list(
      entry_price = 100,
      exit_price = 105,
      size = 100,
      pnl = 500
    ),
    list(
      entry_price = 105,
      exit_price = 102,
      size = 100,
      pnl = -300
    )
  )
  
  # Create sample risk metrics
  risk_metrics <- list(
    total_return = 0.15,
    annualized_return = 0.12,
    sharpe_ratio = 1.5,
    max_drawdown = -0.1,
    win_rate = 0.6,
    volatility = 0.2,
    var_95 = -0.05
  )
  
  # Create dashboard
  dashboard <- create_risk_dashboard(portfolio_value, trades, risk_metrics)
  
  # Test that dashboard is a Shiny app
  expect_s3_class(dashboard, "shiny.appobj")
})

test_that("Risk dashboard handles edge cases", {
  # Create sample data with constant portfolio value
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  portfolio_value <- data.frame(
    Date = dates,
    Value = rep(100000, 100)
  )
  
  # Create empty trades list
  trades <- list()
  
  # Create sample risk metrics
  risk_metrics <- list(
    total_return = 0,
    annualized_return = 0,
    sharpe_ratio = 0,
    max_drawdown = 0,
    win_rate = 0,
    volatility = 0,
    var_95 = 0
  )
  
  # Create dashboard
  dashboard <- create_risk_dashboard(portfolio_value, trades, risk_metrics)
  
  # Test that dashboard is a Shiny app
  expect_s3_class(dashboard, "shiny.appobj")
})

test_that("Risk dashboard handles single trade", {
  # Create sample data
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  portfolio_value <- data.frame(
    Date = dates,
    Value = cumsum(rnorm(100, 0.001, 0.02)) + 100000
  )
  
  # Create single trade
  trades <- list(
    list(
      entry_price = 100,
      exit_price = 105,
      size = 100,
      pnl = 500
    )
  )
  
  # Create sample risk metrics
  risk_metrics <- list(
    total_return = 0.15,
    annualized_return = 0.12,
    sharpe_ratio = 1.5,
    max_drawdown = -0.1,
    win_rate = 1.0,
    volatility = 0.2,
    var_95 = -0.05
  )
  
  # Create dashboard
  dashboard <- create_risk_dashboard(portfolio_value, trades, risk_metrics)
  
  # Test that dashboard is a Shiny app
  expect_s3_class(dashboard, "shiny.appobj")
}) 