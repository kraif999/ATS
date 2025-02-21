# Copyright (c) 2025 Oleh Bilyk

# Load libraries
source("libraries.R")

# Load strategies
source("strategies.R")
options(scipen = 999)

options(shiny.maxRequestSize = 30 * 1024^2)  # Adjust the maximum file upload size
options(shiny.timeout = 1200)  # Timeout for 20 minutes (1200 seconds)

# UI for multiple strategies
ui <- fluidPage(
  
  shinyjs::useShinyjs(),  # Enable shinyjs
  theme = shinytheme("cosmo"),  # Apply the Cosmo theme
  fluidRow(
    column(
      12, 
      tags$div(
        style = "margin-bottom: 10px; font-size: 16px; font-weight: bold;",
        tags$a(
          href = "https://github.com/kraif999/ATS",
          "GitHub repository: https://github.com/kraif999/ATS",
          target = "_blank" # Opens the link in a new tab
        )
      )
    )
  ),
  
  titlePanel("Backtesting Trading Strategies"),
  tags$p(style = "font-style: italic; color: #555;", 
             "*If the display appears cluttered, try reloading the page for a better layout.*"),
  sidebarLayout(
    sidebarPanel(
      # User input controls
      textInput("symbol", "Symbol", value = "BTC-USD"),
      
      # Dropdown menu to select a strategy
      selectInput(
        inputId = "strategy",
        label = "Select a Trading Strategy:",
        choices = c("ADX" = "adx", "BollingerBreakout" = "bollinger_breakout", "DonchianChannel" = "donchian_channel",
         "GARCH" = "garch", "MACD" = "macd", "RSI" = "rsi", "SMA1" = "sma1", "SMA1M" = "sma1m", "SMA2" = "sma2", "SMA2M" = "sma2m",
         "StopAndReversal" = "sar", "TurtleTrading" = "turtle_trading", "VolatilityMeanReversion" = "vol_mean_rev",
         "ARIMA" = "arima"
          ),
        selected = "sma1"
      ),
      
      dateRangeInput("date_range", "Date Range", start = as.Date("2022-01-01"), end = Sys.Date()),
      numericInput("capital", "Capital", value = 1000),
      numericInput("leverage", "Leverage", value = 1),
      selectInput("data_type", "Data Type", choices = c("in_sample", "out_of_sample")),
      dateInput("cut_date", "Cut-off Date", value = as.Date("2024-06-30")),
      
      # Specific parameters for ADX strategy
      conditionalPanel(
        condition = "input.strategy == 'adx'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for ADX"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("ndx", "NDX (Number of Periods)", value = 14),
        numericInput("trend_strength", "Trend Strength Threshold", value = 25)
      ),

      # Specific parameters for Bollinger Breakout strategy
      conditionalPanel(
        condition = "input.strategy == 'bollinger_breakout'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for Bollinger Breakout"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Window Size", value = 20),
        numericInput("sd_mult", "Standard Deviation multiplicator", value = 0.5)
      ),

      # Specific parameters for DonchianChannel strategy
      conditionalPanel(
        condition = "input.strategy == 'donchian_channel'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for Donchian Channel"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Window Size", value = 20)
      ),

      # Specific parameters for MACD strategy
      conditionalPanel(
        condition = "input.strategy == 'macd'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for MACD"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size1", "Fast Period", value = 10),
        numericInput("window_size2", "Slow Period", value = 20),
        numericInput("sline", "Signal Period", value = 7),
        selectInput("ma_type", "MA Type", choices = c("EMA", "SMA", "HMA", "WMA"))
      ),

      # Specific parameters for RSI strategy
      conditionalPanel(
        condition = "input.strategy == 'rsi'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for RSI"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Fast Period", value = 7),
        numericInput("threshold_oversold", "Oversold", value = 30),
        numericInput("threshold_overbought", "Overbought", value = 70)
      ),

      # Specific parameters for SMA1 strategy
      conditionalPanel(
        condition = "input.strategy == 'sma1'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for SMA1"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Window Size", value = 20),
        selectInput("ma_type", "MA Type", choices = c("EMA", "SMA", "HMA", "WMA"))
      ),

      # Specific parameters for SMA1M strategy
      conditionalPanel(
        condition = "input.strategy == 'sma1m'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for SMA1M"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Window Size", value = 20),
        selectInput("ma_type", "MA Type", choices = c("EMA", "SMA", "HMA", "WMA"))
      ),

      # Specific parameters for SMA2 strategy
      conditionalPanel(
        condition = "input.strategy == 'sma2'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for SMA2"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size1", "Window Size1", value = 20),
        numericInput("window_size2", "Window Size2", value = 60),
        selectInput("ma_type", "MA Type", choices = c("EMA", "SMA", "HMA", "WMA"))
      ),

      # Specific parameters for SMA2M strategy
      conditionalPanel(
        condition = "input.strategy == 'sma2m'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for SMA2M"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size1", "Window Size1", value = 10),
        numericInput("window_size2", "Window Size2", value = 200),
        selectInput("ma_type", "MA Type", choices = c("SMA", "EMA", "HMA", "WMA"))
      ),

    # Specific parameters for StopAndReversal strategy
      conditionalPanel(
        condition = "input.strategy == 'sar'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for SAR"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("accel", "Acceleration", value = 0.02),
        numericInput("accel_max", "AccelerationMax", value = 0.2)
      ),

    # Specific parameters for TurtleTrading strategy
      conditionalPanel(
        condition = "input.strategy == 'turtle_trading'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for Turtle Trading"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size1", "Window Size1", value = 4*7),
        numericInput("window_size2", "Window Size2", value = 2*7)
      ),

    # Specific parameters for VolatilityMeanReverting strategy
      conditionalPanel(
        condition = "input.strategy == 'vol_mean_rev'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for Volatility Mean Revertung"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Window Size1", value = 20)
      ),

    # Specific parameters for GARCH based strategy
      conditionalPanel(
        condition = "input.strategy == 'garch'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for GARCH based strategy"
        ),
        tags$br(),
        selectInput("specification", "Specification", choices = c("sGARCH", "eGARCH", "gjrGARCH")),
        numericInput("n_start", "Start of the window", value = 126),
        numericInput("refit_every", "The frequency of the refit", value = 126/2),
        selectInput("refit_window", "Refit window type", choices = c("moving", "expanding")),
        selectInput("distribution_model", "Distribution of residuals", choices = c("snorm", "norm", "nig")),
        selectInput("realized_vol", "Realized volatility approach", choices = c("close", "garman", "gk.yz", "yang.zhang"))
      ),

    # Specific parameters for ARIMA strategy
      conditionalPanel(
        condition = "input.strategy == 'arima'",
        tags$div(
          style = "margin-top: 10px; font-weight: bold;",
          "Specific Strategy Parameters for ARIMA"
        ),
        tags$br(), # Adds a blank line for spacing
        numericInput("window_size", "Window Size", value = 21),
        selectInput("window_type", "Window Type", choices = c("expanding", "moving")),
        checkboxInput("best_arima", "Best (auto) ARIMA ", value = FALSE),
        selectInput("p1", "AR lag", choices = 1:10, selected = 1),
        selectInput("d1", "Integration", choices = 1:2, selected = 2),
        selectInput("q1", "MA lag", choices = 1:10, selected = 1)
      ),

      # Other parameters

      # Data Granularity
      tags$div(
        style = "margin-top: 10px; font-weight: bold;",
        "Data Granularity"
      ),
      checkboxInput("split_data", "Split Data for Backtest", value = FALSE),
      numericInput("window", "Slice Data Into Windows (in years)", value = 1),

      # Risk Management
      tags$div(
        style = "margin-top: 10px; font-weight: bold;",
        "Risk Management"
      ),
      checkboxInput("apply_rm", "Apply risk management", value = TRUE),
      checkboxInput("flat_after_event", "Stay flat after stop loss or profit take happen until new signal", value = FALSE),
      checkboxInput("dynamic_limits", "Adjust stop loss and take profit limits in case price evoles in a favourable direction", value = TRUE),
      numericInput("max_risk", "Maximum risk", value = 0.1),
      numericInput("reward_ratio", "Reward/Maximum risk ratio", value = 3),

      # Plot Setup
      tags$div(
        style = "margin-top: 10px; font-weight: bold;",
        "Plot Setup"
      ),
      checkboxInput("signal_flag", "Show Signal Lines?", value = TRUE),
    
      # Backtest button
      actionButton("backtest_run", "Run Backtest") 
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trading Profile", DTOutput("trading_profile")),
        tabPanel("Performance Plot", plotOutput("performance_plot")),
        tabPanel("List of Trades", DTOutput("trades")),
        tabPanel("Trades PnL distribution", plotOutput("pnl_hist"))
      )
    )
  )
)

# Server for multiple strategies
server <- function(input, output, session) {
  
  # Reactive to fetch price data
  price_data <- reactive({
    req(input$symbol, input$date_range)  # Ensure inputs are available
    
    symbol <- input$symbol  # Use symbol from input
    print(paste("Fetching data for symbol:", symbol))  # Debug print
    
    # Fetch price data
    fetcher <- DataFetcher$new(symbol, input$date_range[1], input$date_range[2])
    ts <- fetcher$download_xts_data()
    return(ts)
  })
  
  # Reactive expression for strategy instance and plot
  strategy_reactive <- eventReactive(input$backtest_run, {
    req(price_data(), input$window_size, input$ma_type, input$cut_date)
    
    symbol <- input$symbol
    print(paste("Using symbol in strategy:", symbol))  # Debug print
    
    # Use switch to handle different strategies
    strategy_instance <- switch(input$strategy,

      "adx" = ADX$new(
        data = price_data(),
        ndx = input$ndx,
        trend_strength = input$trend_strength
      ),

      "bollinger_breakout" = BollingerBreakout$new(
        data = price_data(),
        window_size = input$window_size,
        sd_mult = input$sd_mult
      ),

      "donchian_channel" = DonchianChannel$new(
        data = price_data(),
        window_size = input$window_size
      ),

      "macd" = MACD$new(
        data = price_data(),
        window_size1 = input$window_size1,
        window_size2 = input$window_size2,
        sline = input$sline,
        ma_type = input$ma_type
      ),

      "rsi" = RSI$new(
        data = price_data(),
        window_size = input$window_size,
        threshold_oversold = input$threshold_oversold,
        threshold_overbought = input$threshold_overbought
      ),

      "sma1" = SMA1$new(
        data = price_data(),
        window_size = input$window_size,
        ma_type = input$ma_type
      ),

      "sma1m" = SMA1M$new(
        data = price_data(),
        window_size = input$window_size,
        ma_type = input$ma_type
      ),

      "sma2" = SMA2$new(
        data = price_data(),
        window_size1 = input$window_size1,
        window_size2 = input$window_size2,
        ma_type = input$ma_type
      ),

      "sma2m" = SMA2M$new(
        data = price_data(),
        window_size1 = input$window_size1,
        window_size2 = input$window_size2,
        ma_type = input$ma_type
      ),


      "sar" = StopAndReversal$new(
        data = price_data(),
        accel = input$accel,
        accel_max = input$accel_max
      ),

      "turtle_trading" = TurtleTrading$new(
        data = price_data(), 
        window_size1 = input$window_size1, 
        window_size2 = input$window_size2
        ),

       "vol_mean_rev" = VolatilityMeanReversion$new(
        data = price_data(),
        window_size = input$window_size,
        ma_type = input$ma_type
      ),

      "arima" = ARIMA$new(
        data = price_data(),
        window_size = input$window_size,
        window_type = input$window_type,
        best_arima = input$best_arima,
        p1 = ifelse(input$best_arima, NULL, as.numeric(input$p1)),
        d1 = ifelse(input$best_arima, NULL, as.numeric(input$d1)),
        q1 = ifelse(input$best_arima, NULL, as.numeric(input$q1))
      ),

       "garch" = GARCH$new(
        data = price_data(),
        specification = input$specification,
        n_start = input$n_start,
        refit_every = input$refit_every,
        refit_window = input$refit_window,
        distribution_model = input$distribution_model,
        realized_vol = input$realized_vol,
        #cluster = makePSOCKcluster(parallel::detectCores(logical = FALSE))
        cluster = NULL
      )
    )

    #strategy_instance$estimate_average_true_range(n=14)

    # Print the selected strategy name
    print(paste("Selected strategy/instance:", input$strategy))
    
    # Estimate performance
    performance_result <- strategy_instance$estimate_performance(
      symbol = input$symbol,
      capital = input$capital,
      leverage = input$leverage,
      data_type = input$data_type,
      split_data = input$split_data,
      cut_date = input$cut_date,
      window = input$window,
      apply_rm = input$apply_rm,
      flat_after_event = input$flat_after_event,
      dynamic_limits = input$dynamic_limits,
      max_risk = input$max_risk,
      reward_ratio = input$reward_ratio
    )
    
    print("Tail view:")
    print(
      if(input$apply_rm) {
      strategy_instance$data %>% 
        #select(Date, Close, signal, position, nopActive, nopPassive, eqlActive, eqlPassive, pnlActiveCumulative, pnlPassiveCumulative, tr, atr, tr_reserve, annual_vol) %>%
        select(Date, Close, signal, position, nopActive, nopPassive, eqlActive, eqlPassive, pnlActiveCumulative, pnlPassiveCumulative, ATR, N, annual_vol) %>%
        tail(10)
      } else {
      strategy_instance$data %>% 
        #select(Date, Close, signal, position, nopActive, nopPassive, eqlActive, eqlPassive, pnlActiveCumulative, pnlPassiveCumulative, tr, atr, tr_reserve, annual_vol) %>%
        select(Date, Close, signal, position, nopActive, nopPassive, eqlActive, eqlPassive, pnlActiveCumulative, pnlPassiveCumulative, ATR, N, annual_vol) %>%
        
        tail(10)
      }
    )

    # Count the number of unique year-month combinations
    num_months <- length(unique(format(strategy_instance$data$Date, "%Y-%m")))

    # Print average stop-loss and profit-take events per month
    print(paste0("Stop Losses occur every: ", 1 / round(sum(strategy_instance$data$eventSL, na.rm = TRUE) / num_months, 2), " month(s)"))
    print(paste0("Average Profit Takes per Month: ", 1 / round(sum(strategy_instance$data$eventPT, na.rm = TRUE) / num_months, 2), " month(s)"))

    trading_profile <- t(performance_result)
    
    # Convert to data.table for further processing
    trading_profile <- cbind(Metric = rownames(trading_profile), as.data.table(as.data.frame(trading_profile, stringsAsFactors = FALSE)))

    trading_profile[, units := ifelse(
      .I <= 5 | Metric %in% c("max_risk", "Strategy", "Number of Trades Per Year", "reward_ratio"), "",
      ifelse(
        Metric %in% c("Annualized Profit", "Percentage of Winning Trades", "Max Drawdown", "Max Run-Up"), "%",
        ifelse(
          Metric %in% c("Length of Largest Win", "Length of Largest Loss", "Length of Average Win", "Length of Average Loss", 
                        "Length of Max Drawdown", "Length of Max Run-Up", "Length of Time in Largest Winning Run", "Length of Time in Largest Losing Run", 
                        "Length of Time in Average Winning Run", "Length of Time in Average Losing Run", "Largest Winning Run", "Largest Losing Run", "Average Winning Run", "Average Losing Run"), "days",
          ifelse(
            grepl("Date", Metric), "Date", 
            "USD"  # Default case for other rows
          )
        )
      )
    )]

    trades_lst <- strategy_instance$get_trades(input$apply_rm)

    print("Cumulative PnL by position:")
    print(trades_lst$trades %>% group_by(Trade) %>% summarize(PnL = sum(TradePnL)))

    # Generate and return the plot
    p <- strategy_instance$plot_equity_lines(
      strategy_name = toupper(input$strategy),
      signal_flag = input$signal_flag,
      symbol = input$symbol,
      capital = input$capital
    )

    return(list(strategy = strategy_instance, plot = p, profile = trading_profile, trades = trades_lst$trades, pnl_hist = trades_lst$plot))
  })
  
  # Reactive expression to generate performance metrics
  performance_metrics <- reactive({
    req(strategy_reactive())
    performance_data <- strategy_reactive()$profile  # Correctly accessing the profile from strategy_reactive
    return(performance_data)
  })
  
  # Render trading profile data table
  output$trading_profile <- renderDT({
    req(performance_metrics())
    performance <- performance_metrics()
    
    datatable(
      as.data.frame(performance), 
      options = list(pageLength = 100)  # Set the number of rows per page to 100
    )
  })

  # Render performance plot
  output$performance_plot <- renderPlot({
    req(strategy_reactive())
    strategy_plot <- strategy_reactive()$plot  # Correctly accessing the plot from strategy_reactive
    print(strategy_plot)  # Render the plot
  })

  # Render trades data table
  output$trades <- renderDT({
    req(strategy_reactive())
    trades <- strategy_reactive()$trades
    datatable(
      trades %>% data.table,
      options = list(pageLength = 100)  # Set rows per page
    )
  })
 
 # Render trades pnl histogram
  output$pnl_hist <- renderPlot({
    req(strategy_reactive())
    pnl_hist <- strategy_reactive()$pnl_hist  # Correctly accessing the pnl_hist plot
    print(pnl_hist)
  })

}

# Run the app
shinyApp(ui = ui, server = server)
