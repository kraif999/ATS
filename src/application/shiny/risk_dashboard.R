#' @title Risk Dashboard Shiny App
#' @description Interactive dashboard for visualizing risk metrics and portfolio performance
#' @export
create_risk_dashboard <- function(portfolio_value, trades, risk_metrics) {
  # Create UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Trading System Risk Dashboard"),
    
    # Sidebar with controls
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Date range selector
        shiny::dateRangeInput(
          "date_range",
          "Date Range",
          start = min(portfolio_value$Date),
          end = max(portfolio_value$Date),
          min = min(portfolio_value$Date),
          max = max(portfolio_value$Date)
        ),
        
        # Volatility window selector
        shiny::sliderInput(
          "vol_window",
          "Volatility Window (days)",
          min = 5,
          max = 60,
          value = 20,
          step = 5
        ),
        
        # Plot customization
        shiny::selectInput(
          "plot_theme",
          "Plot Theme",
          choices = c("Minimal", "Classic", "Dark", "Light"),
          selected = "Minimal"
        ),
        
        # Risk metrics display options
        shiny::checkboxGroupInput(
          "show_metrics",
          "Show Metrics",
          choices = c(
            "Total Return",
            "Annualized Return",
            "Sharpe Ratio",
            "Max Drawdown",
            "Win Rate",
            "Volatility",
            "VaR (95%)"
          ),
          selected = c(
            "Total Return",
            "Annualized Return",
            "Sharpe Ratio",
            "Max Drawdown",
            "Win Rate",
            "Volatility",
            "VaR (95%)"
          )
        ),
        
        # Download button
        shiny::downloadButton("download_plots", "Download Plots")
      ),
      
      # Main panel with plots
      shiny::mainPanel(
        shiny::tabsetPanel(
          # Portfolio Overview tab
          shiny::tabPanel(
            "Portfolio Overview",
            shiny::plotOutput("portfolio_plot", height = "400px"),
            shiny::plotOutput("drawdown_plot", height = "400px")
          ),
          
          # Risk Metrics tab
          shiny::tabPanel(
            "Risk Metrics",
            shiny::plotOutput("volatility_plot", height = "400px"),
            shiny::plotOutput("metrics_table", height = "400px")
          ),
          
          # Trade Analysis tab
          shiny::tabPanel(
            "Trade Analysis",
            shiny::plotOutput("pnl_plot", height = "400px"),
            shiny::plotOutput("trade_stats", height = "400px")
          )
        )
      )
    )
  )
  
  # Create server
  server <- function(input, output, session) {
    # Create visualizer
    visualizer <- RiskVisualizer$new(portfolio_value, trades, risk_metrics)
    
    # Filter data based on date range
    filtered_data <- shiny::reactive({
      portfolio_value[portfolio_value$Date >= input$date_range[1] & 
                      portfolio_value$Date <= input$date_range[2], ]
    })
    
    # Get theme function
    get_theme <- shiny::reactive({
      switch(input$plot_theme,
        "Minimal" = ggplot2::theme_minimal(),
        "Classic" = ggplot2::theme_classic(),
        "Dark" = ggplot2::theme_dark(),
        "Light" = ggplot2::theme_light()
      )
    })
    
    # Portfolio value plot
    output$portfolio_plot <- shiny::renderPlot({
      p <- ggplot2::ggplot(filtered_data(), ggplot2::aes(x = Date, y = Value)) +
        ggplot2::geom_line(color = "blue") +
        ggplot2::labs(
          title = "Portfolio Value Over Time",
          x = "Date",
          y = "Portfolio Value"
        ) +
        get_theme()
      p
    })
    
    # Drawdown plot
    output$drawdown_plot <- shiny::renderPlot({
      # Calculate drawdown
      peak <- cummax(filtered_data()$Value)
      drawdown <- (peak - filtered_data()$Value) / peak
      
      # Create data frame for plotting
      drawdown_df <- data.frame(
        Date = filtered_data()$Date,
        Drawdown = drawdown
      )
      
      p <- ggplot2::ggplot(drawdown_df, ggplot2::aes(x = Date, y = Drawdown)) +
        ggplot2::geom_area(fill = "red", alpha = 0.3) +
        ggplot2::geom_line(color = "red") +
        ggplot2::labs(
          title = "Portfolio Drawdown",
          x = "Date",
          y = "Drawdown"
        ) +
        get_theme()
      p
    })
    
    # Volatility plot
    output$volatility_plot <- shiny::renderPlot({
      # Calculate returns
      returns <- diff(filtered_data()$Value) / filtered_data()$Value[-nrow(filtered_data())]
      
      # Calculate rolling volatility
      rolling_vol <- sapply(input$vol_window:length(returns), function(i) {
        sd(returns[(i-input$vol_window+1):i]) * sqrt(252)
      })
      
      # Create data frame for plotting
      vol_df <- data.frame(
        Date = filtered_data()$Date[input$vol_window:length(returns)],
        Volatility = rolling_vol
      )
      
      p <- ggplot2::ggplot(vol_df, ggplot2::aes(x = Date, y = Volatility)) +
        ggplot2::geom_line(color = "purple") +
        ggplot2::labs(
          title = "Rolling Volatility",
          x = "Date",
          y = "Annualized Volatility"
        ) +
        get_theme()
      p
    })
    
    # Risk metrics table
    output$metrics_table <- shiny::renderPlot({
      # Create data frame for metrics
      metrics_df <- data.frame(
        Metric = c("Total Return", "Annualized Return", "Sharpe Ratio", "Max Drawdown", 
                  "Win Rate", "Volatility", "VaR (95%)"),
        Value = c(
          risk_metrics$total_return,
          risk_metrics$annualized_return,
          risk_metrics$sharpe_ratio,
          risk_metrics$max_drawdown,
          risk_metrics$win_rate,
          risk_metrics$volatility,
          risk_metrics$var_95
        )
      )
      
      # Filter metrics based on selection
      metrics_df <- metrics_df[metrics_df$Metric %in% input$show_metrics, ]
      
      # Format values
      metrics_df$Value <- sprintf("%.2f%%", metrics_df$Value * 100)
      
      # Create table plot
      p <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = 1, y = Metric, label = Value)) +
        ggplot2::geom_text(hjust = 0) +
        ggplot2::labs(
          title = "Risk Metrics Summary",
          x = "",
          y = ""
        ) +
        get_theme() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
      p
    })
    
    # PnL distribution plot
    output$pnl_plot <- shiny::renderPlot({
      # Extract PnL values
      pnl_values <- sapply(trades, function(t) t$pnl)
      
      # Create data frame for plotting
      pnl_df <- data.frame(PnL = pnl_values)
      
      p <- ggplot2::ggplot(pnl_df, ggplot2::aes(x = PnL)) +
        ggplot2::geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
        ggplot2::geom_density(alpha = 0.2, fill = "blue") +
        ggplot2::labs(
          title = "Trade PnL Distribution",
          x = "Profit/Loss",
          y = "Frequency"
        ) +
        get_theme()
      p
    })
    
    # Trade statistics plot
    output$trade_stats <- shiny::renderPlot({
      # Calculate trade statistics
      pnl_values <- sapply(trades, function(t) t$pnl)
      win_rate <- mean(pnl_values > 0)
      avg_win <- mean(pnl_values[pnl_values > 0])
      avg_loss <- mean(pnl_values[pnl_values < 0])
      
      # Create data frame for plotting
      stats_df <- data.frame(
        Metric = c("Win Rate", "Average Win", "Average Loss"),
        Value = c(win_rate, avg_win, avg_loss)
      )
      
      # Format values
      stats_df$Value <- sprintf("%.2f%%", stats_df$Value * 100)
      
      # Create table plot
      p <- ggplot2::ggplot(stats_df, ggplot2::aes(x = 1, y = Metric, label = Value)) +
        ggplot2::geom_text(hjust = 0) +
        ggplot2::labs(
          title = "Trade Statistics",
          x = "",
          y = ""
        ) +
        get_theme() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
      p
    })
    
    # Download plots
    output$download_plots <- shiny::downloadHandler(
      filename = function() {
        paste("risk_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep = "")
      },
      content = function(file) {
        # Create PDF
        pdf(file, width = 12, height = 8)
        
        # Plot portfolio value
        print(ggplot2::ggplot(filtered_data(), ggplot2::aes(x = Date, y = Value)) +
          ggplot2::geom_line(color = "blue") +
          ggplot2::labs(title = "Portfolio Value Over Time") +
          get_theme())
        
        # Plot drawdown
        peak <- cummax(filtered_data()$Value)
        drawdown <- (peak - filtered_data()$Value) / peak
        drawdown_df <- data.frame(Date = filtered_data()$Date, Drawdown = drawdown)
        print(ggplot2::ggplot(drawdown_df, ggplot2::aes(x = Date, y = Drawdown)) +
          ggplot2::geom_area(fill = "red", alpha = 0.3) +
          ggplot2::geom_line(color = "red") +
          ggplot2::labs(title = "Portfolio Drawdown") +
          get_theme())
        
        # Plot volatility
        returns <- diff(filtered_data()$Value) / filtered_data()$Value[-nrow(filtered_data())]
        rolling_vol <- sapply(input$vol_window:length(returns), function(i) {
          sd(returns[(i-input$vol_window+1):i]) * sqrt(252)
        })
        vol_df <- data.frame(
          Date = filtered_data()$Date[input$vol_window:length(returns)],
          Volatility = rolling_vol
        )
        print(ggplot2::ggplot(vol_df, ggplot2::aes(x = Date, y = Volatility)) +
          ggplot2::geom_line(color = "purple") +
          ggplot2::labs(title = "Rolling Volatility") +
          get_theme())
        
        # Plot PnL distribution
        pnl_values <- sapply(trades, function(t) t$pnl)
        pnl_df <- data.frame(PnL = pnl_values)
        print(ggplot2::ggplot(pnl_df, ggplot2::aes(x = PnL)) +
          ggplot2::geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
          ggplot2::geom_density(alpha = 0.2, fill = "blue") +
          ggplot2::labs(title = "Trade PnL Distribution") +
          get_theme())
        
        dev.off()
      }
    )
  }
  
  # Run the app
  shiny::shinyApp(ui = ui, server = server)
} 