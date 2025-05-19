#' @title Risk Visualizer
#' @description Visualizes risk metrics and portfolio performance
#' @export
RiskVisualizer <- R6::R6Class("RiskVisualizer",
  public = list(
    #' @field portfolio_value Portfolio value over time
    portfolio_value = NULL,
    
    #' @field trades List of trades
    trades = NULL,
    
    #' @field risk_metrics Risk metrics
    risk_metrics = NULL,
    
    #' @description Initialize risk visualizer
    #' @param portfolio_value Portfolio value data frame
    #' @param trades List of trades
    #' @param risk_metrics Risk metrics list
    initialize = function(portfolio_value, trades, risk_metrics) {
      self$portfolio_value <- portfolio_value
      self$trades <- trades
      self$risk_metrics <- risk_metrics
    },
    
    #' @description Plot portfolio value over time
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_portfolio_value = function(theme = "light") {
      p <- ggplot2::ggplot(self$portfolio_value, ggplot2::aes(x = Date, y = Value)) +
        ggplot2::geom_line(color = "steelblue", size = 1) +
        ggplot2::labs(
          title = "Portfolio Value Over Time",
          x = "Date",
          y = "Portfolio Value"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Plot drawdown over time
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_drawdown = function(theme = "light") {
      # Calculate drawdown
      peak <- cummax(self$portfolio_value$Value)
      drawdown <- (self$portfolio_value$Value - peak) / peak
      
      p <- ggplot2::ggplot(data.frame(
        Date = self$portfolio_value$Date,
        Drawdown = drawdown
      ), ggplot2::aes(x = Date, y = Drawdown)) +
        ggplot2::geom_area(fill = "red", alpha = 0.3) +
        ggplot2::geom_line(color = "darkred", size = 1) +
        ggplot2::labs(
          title = "Portfolio Drawdown",
          x = "Date",
          y = "Drawdown"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Plot trade PnL distribution
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_pnl_distribution = function(theme = "light") {
      pnl <- sapply(self$trades, function(t) t$pnl)
      
      p <- ggplot2::ggplot(data.frame(PnL = pnl), ggplot2::aes(x = PnL)) +
        ggplot2::geom_histogram(
          bins = 30,
          fill = "steelblue",
          color = "white",
          alpha = 0.7
        ) +
        ggplot2::geom_density(ggplot2::aes(y = ..count.. * 30), color = "red") +
        ggplot2::labs(
          title = "Trade PnL Distribution",
          x = "PnL",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Plot rolling volatility
    #' @param window Rolling window size (default: 20)
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_rolling_volatility = function(window = 20, theme = "light") {
      returns <- diff(log(self$portfolio_value$Value))
      vol <- TTR::runSD(returns, n = window) * sqrt(252)
      
      p <- ggplot2::ggplot(data.frame(
        Date = self$portfolio_value$Date[-1],
        Volatility = vol
      ), ggplot2::aes(x = Date, y = Volatility)) +
        ggplot2::geom_line(color = "purple", size = 1) +
        ggplot2::labs(
          title = sprintf("Rolling Volatility (%d-day window)", window),
          x = "Date",
          y = "Annualized Volatility"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Plot monthly returns heatmap
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_monthly_returns_heatmap = function(theme = "light") {
      # Calculate monthly returns
      monthly_returns <- self$portfolio_value %>%
        dplyr::mutate(
          Year = lubridate::year(Date),
          Month = lubridate::month(Date)
        ) %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarise(
          Return = (last(Value) - first(Value)) / first(Value),
          .groups = "drop"
        )
      
      p <- ggplot2::ggplot(monthly_returns, ggplot2::aes(x = Month, y = Year, fill = Return)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(
          low = "red",
          mid = "white",
          high = "green",
          midpoint = 0,
          labels = scales::percent
        ) +
        ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb) +
        ggplot2::labs(
          title = "Monthly Returns Heatmap",
          x = "Month",
          y = "Year",
          fill = "Return"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Plot trade duration analysis
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_trade_duration = function(theme = "light") {
      # Calculate trade durations
      durations <- sapply(self$trades, function(t) {
        as.numeric(difftime(t$exit_time, t$entry_time, units = "days"))
      })
      
      p <- ggplot2::ggplot(data.frame(Duration = durations), ggplot2::aes(x = Duration)) +
        ggplot2::geom_histogram(
          bins = 30,
          fill = "steelblue",
          color = "white",
          alpha = 0.7
        ) +
        ggplot2::labs(
          title = "Trade Duration Distribution",
          x = "Duration (Days)",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Plot win rate by time of day
    #' @param theme Plot theme (default: "light")
    #' @return ggplot object
    plot_win_rate_by_time = function(theme = "light") {
      # Calculate win rate by hour
      hourly_stats <- self$trades %>%
        dplyr::mutate(
          Hour = lubridate::hour(entry_time),
          Win = pnl > 0
        ) %>%
        dplyr::group_by(Hour) %>%
        dplyr::summarise(
          WinRate = mean(Win),
          Count = n(),
          .groups = "drop"
        )
      
      p <- ggplot2::ggplot(hourly_stats, ggplot2::aes(x = Hour, y = WinRate, size = Count)) +
        ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
        ggplot2::geom_line(color = "steelblue", alpha = 0.5) +
        ggplot2::scale_x_continuous(breaks = 0:23) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(
          title = "Win Rate by Hour of Day",
          x = "Hour",
          y = "Win Rate",
          size = "Number of Trades"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 10)
        )
      
      if (theme == "dark") {
        p <- p + ggplot2::theme_dark()
      }
      
      return(p)
    },
    
    #' @description Create a dashboard with all plots
    #' @param theme Plot theme (default: "light")
    #' @return grid object
    create_dashboard = function(theme = "light") {
      # Create all plots
      p1 <- self$plot_portfolio_value(theme)
      p2 <- self$plot_drawdown(theme)
      p3 <- self$plot_pnl_distribution(theme)
      p4 <- self$plot_rolling_volatility(theme)
      p5 <- self$plot_monthly_returns_heatmap(theme)
      p6 <- self$plot_trade_duration(theme)
      p7 <- self$plot_win_rate_by_time(theme)
      
      # Arrange plots in a grid
      gridExtra::grid.arrange(
        p1, p2, p3, p4, p5, p6, p7,
        ncol = 2,
        top = grid::textGrob(
          "Portfolio Analysis Dashboard",
          gp = grid::gpar(fontsize = 20, fontface = "bold")
        )
      )
    }
  )
) 