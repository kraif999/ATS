test_that("RiskVisualizer initialization works", {
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
  
  # Create visualizer
  visualizer <- RiskVisualizer$new(portfolio_value, trades, risk_metrics)
  
  # Test properties
  expect_equal(visualizer$portfolio_value, portfolio_value)
  expect_equal(visualizer$trades, trades)
  expect_equal(visualizer$risk_metrics, risk_metrics)
})

test_that("RiskVisualizer plot methods work", {
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
  
  # Create visualizer
  visualizer <- RiskVisualizer$new(portfolio_value, trades, risk_metrics)
  
  # Test portfolio value plot
  p1 <- visualizer$plot_portfolio_value()
  expect_s3_class(p1, "ggplot")
  
  # Test drawdown plot
  p2 <- visualizer$plot_drawdown()
  expect_s3_class(p2, "ggplot")
  
  # Test trade PnL plot
  p3 <- visualizer$plot_trade_pnl()
  expect_s3_class(p3, "ggplot")
  
  # Test rolling volatility plot
  p4 <- visualizer$plot_rolling_volatility()
  expect_s3_class(p4, "ggplot")
  
  # Test risk metrics plot
  p5 <- visualizer$plot_risk_metrics()
  expect_s3_class(p5, "ggplot")
})

test_that("RiskVisualizer dashboard creation works", {
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
  
  # Create visualizer
  visualizer <- RiskVisualizer$new(portfolio_value, trades, risk_metrics)
  
  # Test dashboard creation
  dashboard <- visualizer$create_dashboard()
  expect_s3_class(dashboard, "gtable")
})

test_that("RiskVisualizer handles edge cases", {
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
  
  # Create visualizer
  visualizer <- RiskVisualizer$new(portfolio_value, trades, risk_metrics)
  
  # Test plots with constant portfolio value
  p1 <- visualizer$plot_portfolio_value()
  expect_s3_class(p1, "ggplot")
  
  p2 <- visualizer$plot_drawdown()
  expect_s3_class(p2, "ggplot")
  
  # Test plot with empty trades
  p3 <- visualizer$plot_trade_pnl()
  expect_s3_class(p3, "ggplot")
  
  # Test rolling volatility with constant value
  p4 <- visualizer$plot_rolling_volatility()
  expect_s3_class(p4, "ggplot")
  
  # Test risk metrics with zero values
  p5 <- visualizer$plot_risk_metrics()
  expect_s3_class(p5, "ggplot")
}) 