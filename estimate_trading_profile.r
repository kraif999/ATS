# Copyright (c) 2024 Oleh Bilyk

# List of required libraries
required_libraries <-  c("PerformanceAnalytics", "ggplot2", "lmtest", "fBasics", "urca", "forecast", "quantmod", "tseries", "fUnitRoots", "xts",  "fBasics", "tseries",
 "car", "FinTS", "fGarch",  "psych", "rugarch", "parallel", "caTools", "plyr", "expss", "base", "tidyr", "dplyr", "MLmetrics", "tibble", "gridExtra", "writexl",
 "doParallel", "parallel", "lubridate", "reshape2", "R6", "stringr", "aTSA", "TTR", "purrr", "slider", "plotly", "vscDebugger", "lubridate", "patchwork")

invisible(lapply(required_libraries, library, character.only = TRUE))

# Function to load/install libraries
load_libraries <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

# Load the required libraries
load_libraries(required_libraries)

options(scipen = 999)

# Asset meta data (asset, symbol, class, description)
meta <- jsonlite::fromJSON("instr_config.json")

# List of assets
assets <- c(
  "EURUSD=X", "GBPUSD=X", "JPY=X", "AUDUSD=X", # FX most tradable
  "USDPLN=X", "MXN=X", # FX emerging
  "^GSPC", "^IXIC",  # US Equtities
  "^FTSE", "^GDAXI", "^N100", # Europe Equities
  # "WIG20.WA", data is not available
  "EPOL", # ishares MCSI Poland
  "^N225", "^HSI", "000001.SS", # Asia Equities
  "AGG", # Fixed Income
  "GC=F", # Commodities (gold)
  "BZ=F", # Commodities (oil)
  "SI=F", # Commodities (silver)
  "NG=F", # Commodities (natural gas)
  "HG=F", # Commodities (copper)
  "BTC-USD", # Cryptocurrency (BTC)
  "ETH-USD" # Cryptocurrency (Ethereum)
)

# Define Data parent class (DataFetcher)
DataFetcher <- R6Class(
  "DataFetcher",
  public = list(
    symbol = NULL,
    from_date = NULL,
    to_date = NULL,
    type = NULL,

initialize = function(symbol, from_date, to_date, type = "rets") {
      self$symbol <- symbol
      self$from_date <- from_date
      self$to_date <- to_date
      self$type <- type
},
  
# Download data for multiple symbols and save tibble data frame in wide format    
convert_xts_to_wide_df = function() {
    dfs <- list()
    # Iterate through symbols to retrieve data and create data frames
    for (symbol in self$symbol) {
    # Fetch data for symbol
    tsRaw <- getSymbols(symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    data <- coredata(ts)
    dates <- index(ts)
    close_price <- as.numeric(data[, 4])
    # Create data frame for the symbol
    switch(
        self$type,
        "rets" = {
        # Combine Date and log returns into a data.frame
        df <- data.frame(Date = as.Date(dates), 
                            rets = as.numeric(log(close_price / lag(close_price)))) %>%
            na.omit %>%
            unnest(rets)
        },
        "Close" = {
        df <- data.frame(Date = as.Date(dates), Close = close_price) %>% 
            na.omit %>%
            unnest
        },
        stop("Invalid value for 'type' argument. Choose 'rets' or 'Close'.")
    )
    # Store the data frame in the list
    dfs[[symbol]] <- df
    }
    # Combine all data frames into a single wide data frame
    wide_df <- bind_rows(dfs, .id = "symbol") %>%
    pivot_wider(names_from = "symbol", values_from = ifelse(self$type == "rets", "rets", "Close"), 
                names_prefix = ifelse(self$type == "rets", "rets_", "Close_")) %>%
                    na.omit()
    return(wide_df)
},

# Download xts data and compute log returns
download_xts_data = function() {
    tsRaw <- quantmod::getSymbols(self$symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    ts$value <- log(ts[, grep("\\.Close$", colnames(ts))]) - log(lag(ts[, grep("\\.Close$", colnames(ts))]))
    ts <- na.omit(ts)
    attr(ts, "na.action") <- NULL
    return(ts)
},

# Visualize Close price or returns
plot_close_or_rets = function(type = "close") {
  colnames(ts) <- sub(".*\\.", "", colnames(ts))
  switch(type,
        "close" = {
          # Plot Close
          ggplot(ts, aes(x = as.Date(index(ts)))) +
            geom_line(aes(y = Close, color = "Active Strategy"), color = "black") +
            geom_hline(yintercept = mean(ts$Close), linetype = "dashed", color = "blue") +
            labs(title = "Close price",
                  x = "Date",
                  y = "Close price") +
            scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
            theme_minimal()
        },

        "rets" = {
         ggplot(ts, aes(x = as.Date(index(ts)))) +
            geom_line(aes(y = value, color = "Active Strategy"), color = "black") +
            geom_hline(yintercept = mean(ts$value), linetype = "dashed", color = "blue") +
            labs(title = "Log returns",
                  x = "Date",
                  y = "log return") +
            scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +
            theme_minimal()         
        },

        "rets_hist" = {

          ggplot(ts, aes(value)) +
            geom_histogram(aes(y = after_stat(density)), binwidth = 0.001, fill = "lightblue", color = "black", boundary = 0.5) +
            stat_function(fun = dnorm, args = list(mean = mean(ts$value), sd = sd(ts$value)), color = "red", size = 1) +
            labs(title = "Histogram with Normal Distribution Curve", 
                x = "Numeric Vector", 
                y = "Density") +
            theme_minimal()
        },
        stop("Invalid type. Choose either 'close' or 'rets'.")
  )
},

# Compute missing ratio of values that are not available for Close price (US public holidays are not considered)
compute_NA_close_price_ratio = function() {

  dates <- data.frame(Date = as.Date(seq(from = from_date, to = to_date, by = "day") %>%
    `[`(., !weekdays(.) %in% c("Saturday", "Sunday"))))

  ts_df <- data.frame(ts)
  ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything())

  df <- merge(ts_df %>% select(Date, Close), dates, by = "Date", all.y = TRUE)
  print(paste("Missing Close values ratio considering all dates is:", sum(is.na(df$Close))/nrow(df)))

  # Compute missing ratio across business weekdays
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") 
  NA_weekdays <- lapply(days, function(day) {
  missing_ratio <- sum(is.na(df[df$Date %in% df$Date[weekdays(df$Date) %in% day],]$Close)) / nrow(df[df$Date %in% df$Date[weekdays(df$Date) %in% day],])
  return(missing_ratio)
  })

  names(NA_weekdays) <- days
  # Convert the list of missing ratios to a data frame
  NA_weekdays <- data.frame(Weekday = names(NA_weekdays), Missing_Ratio = unlist(NA_weekdays))

  return(NA_weekdays)
}
  )
)

TSA <- R6Class(
  "TSA",
  public = list(
    original_data = NULL,
    data = NULL,

initialize = function(data) {
      self$original_data <- data
      self$data <- private$preprocess_data(freq = "daily")
},
    
estimate_stationarity = function(freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      adf <- aTSA::adf.test(self$data$value, output = FALSE)[["type1"]] %>% data.frame()
      
      # Plot values
      if (plot_flag) {
        print(
          ggplot(self$data, aes(x = Date, y = value)) +
            geom_line() +
            labs(title = "Stationarity (ADF test)") +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme_minimal()
        )
      }
      return(adf)
},
    
estimate_autocorr_rets_vol = function(test, freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      switch(test,
             "rets" = {
               # ACF test for returns  
               acf <- acf(self$data$value, main = "Autocorrelation", plot = TRUE)
               return(list(acf = acf))
             },
             "vol" = {
               # ACF test for squared returns
               acf2 <- acf(self$data$value^2, main = "Volatility clustering", plot = TRUE)
               return(list(acf = acf2))
             }
      )
},
    
estimate_seasonality = function(freq = "daily") {
      self$data <- private$preprocess_data(freq)
      self$data <- ts(self$data$value, frequency = ifelse(freq == "daily", 26, 52))
      # Decompose the time series
      decomposed_data <- decompose(self$data) # decompose into 1) original ts, 2) trend component, 3) seasonal component, 4) residual component
      # Plot the decomposed components
      plot(decomposed_data)
},
    
estimate_heteroscedasticity = function(freq = "daily", plot_flag = TRUE) {
      self$data <- private$preprocess_data(freq)
      
      # Fit a linear regression model to the squared log returns
      model <- lm(self$data$value^2 ~ self$data$value, data = data.frame(value = self$data$value))
      
      # Perform Breusch-Pagan test and print results
      bp_test <- bptest(model)
      print(bp_test)
      
      # Get the residuals from the linear regression model
      residuals <- residuals(model)
      
      # Create a data frame for the residuals
      residual_df <- data.frame(Residuals = residuals, Observation = 1:length(residuals))
      
      # Plot using ggplot
      if (plot_flag) {
        print(
          ggplot(residual_df, aes(x = Observation, y = Residuals)) +
            geom_point() +
            geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
            labs(title = "Residuals from Linear Regression Model",
                 x = "Observation Number",
                 y = "Residuals") +
            theme_minimal()
        )
      }
},

estimate_arch_effects = function(freq = "daily", p = 1, q = 1, plot_flag = TRUE) {
  
  self$data <- private$preprocess_data(freq)
  
  # Fit an ARIMA model
  fit <- arima(self$data$value, order=c(p,0,q))
  
  # Engle's ARCH test
  arch <- aTSA::arch.test(fit)
  return(arch)
},

estimate_outliers = function(test, freq = "daily", plot_flag = TRUE, q1 = NULL, q3 = NULL, threshold = 1.5) {
  self$data <- private$preprocess_data(freq)
  switch(
    test,
    # Super smoothing
    "supsmu" = {  
    ts_rets <- ts(self$data$value, frequency = 1)

    # Clean the time series using tsclean
    clean_ts <- tsclean(ts_rets)

    # Identify outliers using tsoutliers
    outliers <- tsoutliers(ts_rets)

    # Plot the original time series, cleaned time series, and outliers
    if (plot_flag) {
      print(
        autoplot(clean_ts, series = "Cleaned", color = 'red', lwd = 0.9) +
        autolayer(ts_rets, series = "Original", color = 'gray', lwd = 1) +
        geom_point(data = outliers %>% as.data.frame(),
        aes(x = index, y = replacements), col = 'blue') +
        labs(x = "Date", y = "Close Price", title = "Time Series with Outliers Highlighted (via SuperSmoothing 'tsoutliers' function)")
        )
      }
    
    # Create a data frame from the outliers list
    outliers_df <- data.frame(
      index = outliers$index,
      replacements = outliers$replacements
    )
    return(outliers_df)
    },

    "zscore" = {
    ts_rets <- ts(self$data$value, frequency = 1)
     # Calculate Modified Z-scores
     median_val <- median(ts_rets)
     mad_val <- mad(ts_rets)

     modified_z_scores <- abs(0.6745 * (ts_rets - median_val) / mad_val)

     # Identify outliers (e.g., Modified Z-score > 3.5)
     outliers_modified <- which(modified_z_scores > 3.5)

     # Plot outliers
      if (plot_flag) {
        print(
          ggplot(data.frame(Date = time(ts_rets), Value = as.numeric(ts_rets)), aes(x = Date, y = Value)) +
            geom_line() +
            geom_point(data = data.frame(Date = time(ts_rets)[outliers_modified], Value = ts_rets[outliers_modified]), aes(x = Date, y = Value), color = "red") +
            labs(title = "Time Series Data with Outliers Identified by Modified Z-Score")
        )
      }

      # Create a data frame from the outliers list
      outliers_df <- data.frame(
        index = time(ts_rets)[outliers_modified],
        replacements = ts_rets[outliers_modified]
      )
      return(outliers_df)
        },
      
    "fences" = {
    ts_rets <- ts(self$data$value, frequency = 1)
    # Calculate IQR
    Q1 <- quantile(ts_rets, q1)
    Q3 <- quantile(ts_rets, q3)
    IQR_val <- Q3 - Q1

    # Identify outliers (e.g., values < Q1 - 1.5 * IQR or values > Q3 + 1.5 * IQR)
    outliers_tukey <- which(ts_rets < (Q1 - threshold * IQR_val) | ts_rets > (Q3 + threshold * IQR_val))

    # Plot outliers
    if (plot_flag) {
      print(
    ggplot(data.frame(Date = time(ts_rets), Value = as.numeric(ts_rets)), aes(x = Date, y = Value)) +
        geom_line() +
        geom_point(data = data.frame(Date = time(ts_rets)[outliers_tukey], Value = ts_rets[outliers_tukey]), aes(x = Date, y = Value), color = "red", size = 2) +
        labs(title = "Time Series Data with Outliers Identified by Tukey's Fences")
            )   
          }
        }
      )
},

compute_wkd_rets = function(freq = "daily") {
  self$data <- private$preprocess_data(freq)
  self$data <- self$data %>%
    mutate(weekday = wday(Date, label = TRUE, abbr = TRUE)) 

  # Compute average return on weekdays
  avg_wkd_rets <- self$data %>%
    group_by(weekday) %>%
      summarize(avg_return = mean(value, na.rm = TRUE)) %>%
        arrange(weekday)
  
  # Compute longest consequtive streak of positive and negative weekday returns
  positive <- self$data %>%
    group_by(weekday) %>%
      summarise(longest_series_weekdays = max(rle(value > 0)$lengths * (rle(value > 0)$values))) %>%
          ungroup()

  negative <- self$data %>%
    mutate(weekday = wday(Date, label = TRUE, abbr = TRUE)) %>%
      group_by(weekday) %>%
        summarise(longest_series_weekdays = max(rle(value < 0)$lengths * (rle(value < 0)$values))) %>%
            ungroup()

  res_longest <- merge(positive, negative, by = "weekday", all = TRUE) %>%
    rename(longest_positive = longest_series_weekdays.x, longest_negative = longest_series_weekdays.y) %>% 
      arrange(weekday)

  # Test hypothesis if any weekday return is statistically different from rets mean return

  # Overall mean return
  overall_mean <- mean(self$data$value)

  # Perform Wilcoxon signed-rank test for each weekday
  avg_wkd_rets <- avg_wkd_rets %>%
    rowwise() %>%
      mutate(
    test_statistic = wilcox.test(self$data$value, mu = avg_return, alternative = "two.sided")$statistic,
    p_value = format(wilcox.test(self$data$value, mu = avg_return, alternative = "two.sided")$p.value, nsmall = 5)
  )

  return(list(avg_wkd_rets = avg_wkd_rets, res_longest = res_longest))
},

compute_summary_statistics = function(freq = "daily") {
  self$data <- private$preprocess_data(freq)
  summary_stats <- self$data %>%
    summarise(
      mean_return = mean(value),
      median_return = median(value),
      sd_return = sd(value),
      skewness = skewness(value),
      kurtosis = kurtosis(value)
    )
  
  return(summary_stats)
}

  ), # end of public list arguments
  
private = list(

preprocess_data = function(freq) {
      # Convert xts data into tibble 
      data <- data.frame(self$original_data)  
      data <- data %>%
        rename_with(~ sub(".*\\.", "", .), everything()) %>%
        mutate(Date = as.Date(rownames(data))) %>%
        select(Date, everything()) %>%
        na.omit() %>%
        as.tibble()
      
      # Choose values frequency: daily overlapping values or bi-weekly non-overlapping 
      switch(
        freq,
        "daily" = {
          data <- data
        },
        "biweekly" = {
          bdates <- seq.Date(
            from = (from_date + 0:6)[weekdays(from_date + 0:6) %in% "Wednesday"], 
            to = (to_date - 0:6)[weekdays(to_date - 0:6) %in% "Wednesday"], by = 14)
            data <- data %>% filter(Date %in% bdates)
        },
        stop("Invalid value for 'freq' argument. Choose 'daily' or 'biweekly'.")
      )
      return(data)
    }
  )
)

# Define parent class
Strategy <- R6Class(
  "Strategy",
  public = list(
    data = NULL,

initialize = function(data) {
      self$data <- data
},

# Signal generation, specific to each sub-class (generic method)
generate_signals = function() {
},

convert_to_tibble = function(ts) {
    ts_df <- data.frame(ts)
    ts_df <- ts_df %>%
        rename_with(~ sub(".*\\.", "", .), everything()) %>%
          mutate(Date = as.Date(as.character(rownames(.)))) %>%
            select(Date, everything()) %>%
                na.omit() %>% 
                    as_tibble()
    return(ts_df)
},

# Macrospopic level (overall performance) - understand the trading profile of a Strategy (inlcuding 0.1% transaction fee)
estimate_performance = function(data_type, split_data, cut_date, window, apply_rm, max_risk, reward_ratio, capital, leverage, symbol) {
  
  # Slice self$data using the private slicer method
  self$data <- private$slicer(self$data, cut_date, data_type)

  # Generate signals
  self$generate_signals()

  if(apply_rm) {
    self$data <- private$apply_risk_management(self$data, max_risk, reward_ratio, leverage, capital)
  } else {

    # Initialize columns
    self$data <- self$data %>% 
    mutate(
        nopActive = 0,
        nopPassive = 0,  # Initial number of passive positions (constant)
        #nopPassive <- capital * leverage / self$data$Close[1],
        pnlActive = 0,
        pnlPassive = c(0, diff(Close)),  # Difference in close price to calculate passive pnl
        pnlActiveCumulative = 0,
        pnlPassiveCumulative = 0,
        eqlActive = capital,
        eqlPassive = capital,
        From = as.Date(NA),
        To = as.Date(NA),
        pnlActiveType = NA
    )

    self$data$position[1] <- 0

    eqlActive <- eqlActive2 <- capital
    eqlPassive <- eqlPassive2 <- capital

    # Iterate over each row in self$data
    for (i in 2:nrow(self$data)) {
    
    prev_nop_Active <- self$data$nopActive[i - 1]
    
    # Handle position change (for active portfolio)
    if (self$data$position[i] != self$data$position[i - 1]) {
        self$data$nopActive[i] <- eqlActive * leverage / self$data$Close[i]
    } else {
        # Keep previous nopActive if position hasn't changed
        self$data$nopActive[i] <- self$data$nopActive[i - 1]
    }
    
    # Compute pnlActive (profit/loss for active positions)
    #self$data$pnlActive[i] <- (self$data$Close[i] - self$data$Close[i - 1]) * self$data$position[i-1] * self$data$nopActive[i]
    self$data$pnlActive[i] <- if (self$data$position[i] == 0) 0 else (self$data$Close[i] - self$data$Close[i - 1]) * self$data$position[i - 1] * self$data$nopActive[i - 1]


    # Update active equity
    eqlActive <- eqlActive + self$data$pnlActive[i]
    eqlActive2 <- if (eqlActive < 0) 0 else eqlActive
    self$data$eqlActive[i] <- eqlActive2

    if (self$data$position[i] == 0) {
      self$data$pnlActiveType[i] <- NA  # No position, so pnlActiveType is NA
    } else if ((self$data$position[i - 1] == 1 && self$data$position[i] == -1) || (self$data$position[i - 1] == -1 && self$data$position[i] == 1)) {
      self$data$pnlActiveType[i] <- "R"  # Realized PnL
    } else {
      self$data$pnlActiveType[i] <- "U"  # Unrealized PnL
    }
    
    # Passive strategy
    self$data$nopPassive[i] <- eqlPassive * leverage / self$data$Close[i]
    self$data$pnlPassive[i] <- (self$data$Close[i] - self$data$Close[i - 1]) * self$data$nopPassive[i - 1]
    eqlPassive <- eqlPassive + self$data$pnlPassive[i]
    eqlPassive2 <- if (eqlPassive < 0) 0 else eqlPassive
    self$data$eqlPassive[i] <- eqlPassive2

    }

  }

  # Add additional metrics
  self$data <- self$data %>%
    mutate(
      annual_vol = rollapply(value, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(365),
      pnlActiveCumulative = cumsum(replace_na(pnlActive, 0)),
      pnlPassiveCumulative = cumsum(replace_na(pnlPassive, 0)),
      r_eqlActive = (eqlActive - lag(eqlActive)) / lag(eqlActive),
      r_eqlPassive = (eqlPassive - lag(eqlPassive)) / lag(eqlPassive)
    )

  if (split_data) {
    start_date <- min(self$data$Date)
    end_date <- max(self$data$Date)
    period_start <- start_date
    period_end <- period_start %m+% months(window * 12) - days(1)

    performance_list <- list()

    while (period_start <= end_date) {
      current_end <- min(period_end, end_date)
      data_period <- self$data %>% filter(Date >= period_start & Date <= current_end)
      self$data <- self$data %>%
        mutate(
          From = as.Date(ifelse(Date >= period_start & Date <= current_end, period_start, From)),
          To = as.Date(ifelse(Date >= period_start & Date <= current_end, current_end, To))
        )
      if (nrow(data_period) > 0) {
        metrics <- private$compute_metrics(data_period, symbol)
        metrics$from <- period_start
        metrics$to <- current_end
        metrics$data_type <- data_type
        metrics$leverage <- leverage
        metrics$max_risk <- max_risk
        metrics$reward_ratio <- reward_ratio
        metrics$capital <- capital
        performance_list[[length(performance_list) + 1]] <- metrics
      }
      period_start <- period_start %m+% months(window * 12)
      period_end <- period_start %m+% months(window * 12) - days(1)
    }

    performance_df <- bind_rows(performance_list) %>%
      select(ticker, from, to, data_type, leverage, max_risk, reward_ratio, capital, Strategy, everything())
    return(performance_df)

  } else {
    metrics <- private$compute_metrics(self$data, symbol)
    metrics$from <- min(self$data$Date)
    metrics$to <- max(self$data$Date)
    metrics$data_type <- data_type
    metrics$leverage <- leverage
    metrics$max_risk <- max_risk
    metrics$reward_ratio <- reward_ratio
    metrics$capital <- capital
    performance_df <- as.data.frame(metrics) %>%
      select(ticker, from, to, data_type, leverage, max_risk, reward_ratio, capital, Strategy, everything())
    return(performance_df)
  }
},

# Microscopic level (tabular list of all trades)
get_trades = function() {
  # Prepare trade summary with tradePnL
  trades <- self$data %>%
    mutate(
      Date = as.Date(Date),
      trade_direction = ifelse(position == -1, "Sell", "Buy"),
      entry = as.Date(ifelse(position != lag(position), Date, NA)), # Entry point
      entry_price = ifelse(position != lag(position), Close, NA), # Entry price
      entry_size = ifelse(position != lag(position), nopActive, NA), # Entry size
      exit = as.Date(ifelse(position != lead(position), Date, NA)), # Exit point
      exit_price = ifelse(position != lead(position), Close, NA), # Exit price
      exit_size = ifelse(position != lead(position), nopActive, NA) # Exit size
    ) %>%
    tidyr::fill(entry, entry_price, entry_size, .direction = "down") %>%
    tidyr::fill(exit, exit_price, exit_size, .direction = "up") %>%
    filter(!is.na(entry) & !is.na(exit)) %>%
    group_by(entry, exit) %>%
    summarise(
      Trade = first(trade_direction), # Trade type (Buy/Sell)
      EntryDate = as.Date(first(entry)), # Entry date
      Size = round(first(entry_size), 5), # Correct entry size
      EntryPrice = round(first(entry_price), 2), # Price at entry
      ExitDate = as.Date(first(exit)), # Exit date
      ExitPrice = round(first(exit_price), 2), # Price at exit
      Trade_PnL = round(ifelse(Trade == "Buy", ExitPrice - EntryPrice, EntryPrice - ExitPrice) * Size, 0) # Trade profit/loss with size
    ) %>%
    ungroup() %>% # Remove grouping for calculating Running_PnL
    mutate(
      Running_PnL = round(cumsum(Trade_PnL), 2), # Running cumulative PnL across all trades
      Efficiency = round((Trade_PnL / abs(Running_PnL)) * 100, 2) # Efficiency as % of Running_PnL
    ) %>% select(
      Trade, EntryDate, ExitDate, Size, EntryPrice, ExitPrice, Trade_PnL, Running_PnL, Efficiency
    )
  
  # Generate the plot
  pnl_hist <- ggplot(data = data.frame(Trade_PnL = trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0]), 
                    aes(x = Trade_PnL, fill = Trade_PnL < 0)) +
    geom_histogram(binwidth = diff(range(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])) / 100, 
                  color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("green", "red")) +  # Green for positive, red for negative
    labs(title = "Trade Profit and Loss (PnL) Distribution", 
        x = "Trade PnL", 
        y = "Frequency") +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(min(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0]), 
                max(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])),
      breaks = unique(c(seq(floor(min(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])), 
                            ceiling(max(trades$Trade_PnL[is.finite(trades$Trade_PnL) & trades$Trade_PnL != 0])), 
                            by = 200), 0))
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal()
  
  # Return both the trades and the plot as a list
  return(list(
    trades = trades,
    plot = pnl_hist
  ))
},

# Visualize equity lines for active strategy and passive (buy and hold)
plot_equity_lines = function(strategy_name, signal_flag = FALSE, symbol, capital) {
  # Line size
  active_line_size <- ifelse(signal_flag, 1, 0.8)
  passive_line_size <- ifelse(signal_flag, 1, 0.8)
  
  p <- ggplot(self$data, aes(x = Date)) +
    labs(
      title = paste0(
        "Asset: ", symbol, ", capital trajectory for Active (", 
        as.character(strategy_name), ") and Passive (buy-and-hold)\n",
        "strategies with original investment of ", capital, " USDC ",
        "over the period from ", self$data$Date %>% head(1), " to ", self$data$Date %>% tail(1)
      ),
      x = "Date",
      y = "Equity line",
      color = "Strategy",  # Change label to 'Strategy' for the equity lines
      linetype = "Position"  # Change label to 'Position' for dashed lines
    ) +
    theme_minimal()
  
  # Add vertical dashed lines for positions (short and long)
  if (signal_flag) {
    p <- p +
      geom_vline(data = self$data[self$data$position == -1, ], 
                 aes(xintercept = as.numeric(Date), linetype = "Short Position"), 
                 color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$position == 1, ], 
                 aes(xintercept = as.numeric(Date), linetype = "Long Position"), 
                 color = "green", alpha = 0.5)
  }
  
  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_linetype_manual(values = c("Short Position" = "dashed", "Long Position" = "dashed"))  # Define line types
  
  # Add vertical lines for the From and To columns
  period_lines <- data.frame(From = unique(self$data$From), To = unique(self$data$To))
  p <- p + 
    geom_vline(data = period_lines, aes(xintercept = as.numeric(From)), 
               linetype = "solid", color = "black", alpha = 1, size = 1) +
    geom_vline(data = period_lines, aes(xintercept = as.numeric(To)), 
               linetype = "solid", color = "black", alpha = 1, size = 1)

  # Return the plot
  return(p)
},

# Estimate Average True Range (ATR)
estimate_range_potential = function(n = 14) {
  self$data <- self$data %>% data.table
  
  # Calculate TR1, TR2, TR3
  self$data[, `:=`(
    TR1 = High - Low,
    TR2 = abs(High - shift(Close, type = "lag")),
    TR3 = abs(Low - shift(Close, type = "lag"))
  )]
  
  # Calculate TR as the maximum of TR1, TR2, TR3
  self$data[, TR := pmax(TR1, TR2, TR3, na.rm = TRUE)]
  
  # Calculate ATR as a rolling average of TR over n periods
  self$data[, ATR := frollmean(TR, n, fill = NA)]
  
  # Calculate N as TR / ATR
  self$data[, N := TR / ATR]
  
  # Return updated self$data
  return(self$data)
},

# Plot Close price and volatility (range potential)
plot_close_vs_vol = function(ndays) {

  # Filter self$data for the last ndays
  filtered_data <- tail(self$data, ndays)
  
  # First plot: Line plot of Close price
  close <- ggplot(filtered_data, aes(x = Date, y = Close)) +
    geom_line(color = "black") +
    labs(title = paste0("Close Price (Last ", ndays, " days) for ", symbol), x = "Date", y = "Close") +
    theme_minimal()
  
  # Second plot: Bar plot of N with horizontal dashed lines at 0.5 and 1
  n <- ggplot(filtered_data, aes(x = Date, y = N)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = "N = TR / ATR with thresholds", x = "Date", y = "N") +
    theme_minimal()
  
  # Return the individual plots
  return(list(
    close = close,
    n = n
  ))

}

  ),
    
private = list(

# Apply stop loss and profit take
apply_risk_management = function(data, max_risk, reward_ratio, leverage, capital) {
  
  data$position[1] <- 0
  eqlActive <- eqlActive2 <- capital
  eqlPassive <- eqlPassive2 <- capital
  previous_position <- 0
  stopLoss <- profitTake <- NA
  flat <- FALSE
  reversed_position <- NA
  
  # Initialize columns
  data <- data %>%
    mutate(
      position1 = position,
      group = cumsum(signal != shift(signal, type = "lag", fill = 0)),
      stopLoss = NA,
      profitTake = NA,
      eventSL = NA,
      eventPT = NA,
      nopActive = 0,
      nopPassive = capital / Close[1] * leverage,  # Initial passive number of positions
      pnlActive = 0,
      pnlPassive = 0,
      eqlActive = capital,
      eqlPassive = capital,
      From = as.Date(NA),
      To = as.Date(NA),
      pnlActiveType = NA
    )
  
  # Iterate over each row in the data
  for (i in 2:nrow(data)) {
    
    if (flat) data$position[i] <- 0  # Stay flat after reversal
    
    if (!is.na(reversed_position)) {
      data$position[i] <- reversed_position
      reversed_position <- NA
    }
    
    if (data$position[i] != previous_position) {
      data$nopActive[i] <- eqlActive * leverage / data$Close[i]
      
      if (data$position[i] == 1) {
        stopLoss <- data$Close[i] - (max_risk * eqlActive / data$nopActive[i])
        profitTake <- max(0, data$Close[i] + (reward_ratio * max_risk * eqlActive / data$nopActive[i]))
      } else if (data$position[i] == -1) {
        stopLoss <- data$Close[i] + (max_risk * eqlActive / data$nopActive[i])
        profitTake <- max(0, data$Close[i] - (reward_ratio * max_risk * eqlActive / data$nopActive[i]))
      } else {
        stopLoss <- profitTake <- NA
      }
      previous_position <- data$position[i]
    } else {
      data$nopActive[i] <- data$nopActive[i - 1]
    }
    
    data$stopLoss[i] <- stopLoss
    data$profitTake[i] <- profitTake
    
    if (data$position[i] == 1) {
      data$eventSL[i] <- if (!is.na(stopLoss) && data$Close[i] <= stopLoss) TRUE else NA
      data$eventPT[i] <- if (!is.na(profitTake) && data$Close[i] >= profitTake) TRUE else NA
    } else if (data$position[i] == -1) {
      data$eventSL[i] <- if (!is.na(stopLoss) && data$Close[i] >= stopLoss) TRUE else NA
      data$eventPT[i] <- if (!is.na(profitTake) && data$Close[i] <= profitTake) TRUE else NA
    } else {
      data$eventSL[i] <- data$eventPT[i] <- NA
    }
    
    if (!flat) {
      if (!is.na(data$eventSL[i]) || !is.na(data$eventPT[i])) {
        flat <- TRUE
        reversed_position <- -data$position[i]
      } else {
        data$position[i] <- data$signal[i - 1]
      }
    }
    
    if (i > 2 && data$group[i] != data$group[i - 1]) {
      flat <- FALSE
    }
    
    #data$pnlActive[i] <- (data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1]
    data$pnlActive[i] <- if (data$position[i] == 0) 0 else (data$Close[i] - data$Close[i - 1]) * data$position[i - 1] * data$nopActive[i - 1]
    eqlActive <- eqlActive + data$pnlActive[i]
    eqlActive2 <- if (eqlActive < 0) 0 else eqlActive
    data$eqlActive[i] <- eqlActive2
    
    if (data$position[i] == 0) {
      data$pnlActiveType[i] <- NA  # No position, so pnlActiveType is NA
    } else if ((data$position[i - 1] == 1 && data$position[i] == -1) || (data$position[i - 1] == -1 && data$position[i] == 1)) {
      data$pnlActiveType[i] <- "R"  # Realized PnL
    } else {
      data$pnlActiveType[i] <- "U"  # Unrealized PnL
    }
    
    # Passive strategy
    data$nopPassive[i] <- eqlPassive * leverage / data$Close[i]
    data$pnlPassive[i] <- (data$Close[i] - data$Close[i - 1]) * data$nopPassive[i - 1]
    eqlPassive <- eqlPassive + data$pnlPassive[i]
    eqlPassive2 <- if (eqlPassive < 0) 0 else eqlPassive
    data$eqlPassive[i] <- eqlPassive2
    
  }
  
  data <- data %>%
    mutate(
      pnlActiveCumulative = cumsum(replace_na(pnlActive, 0)),
      pnlPassiveCumulative = cumsum(replace_na(pnlPassive, 0))
    )
  
  return(data)
},

# Function to compute metrics for the trading profile of a Strategy
compute_metrics = function(data_subset, symbol) {
    
  estimate_trading_profile <- function(data_subset, strategy_type) {

      data_subset$Date <- as.Date(data_subset$Date)

      # Select appropriate columns based on strategy type
      pnl_col <- ifelse(strategy_type == "Active", "pnlActive", "pnlPassive")
      eql_col <- ifelse(strategy_type == "Active", "eqlActive", "eqlPassive")
      r_col <- ifelse(strategy_type == "Active", "r_eqlActive", "r_eqlPassive")
      
      # Generate a trade_id based on changes in position
      data_subset <- data_subset %>% mutate(trade_id = cumsum(position != lag(position, default = 1)))

      GrossProfit <- round(GrossProfit <- sum(na.omit(tail(data_subset[[eql_col]], 1)) - na.omit(data_subset[[eql_col]][1])), 0)

      # 1. Annualized Profit
      AnnualizedProfit <- round(as.numeric(Return.annualized(as.numeric(na.omit(data_subset[[r_col]])), scale = 252, geometric = TRUE) * 100), 2)
      #AnnualizedProfit2 <- round((prod(1 + na.omit(data_subset[[r_col]])) / 1)^(1 / (length(na.omit(data_subset[[r_col]])) / 252)) - 1, 2) * 100 

      # 2. Number of Trades per Year
      NumberOfTradesPerYear <- round((if (strategy_type == "Active") sum(diff(data_subset$position) != 0) + 1 else 1) / 
                                    length(unique(format(data_subset$Date, "%Y"))), 0)

      # 3. Percentage of Winning Trades
      PercentageOfWinningTrades <- round(
        sum(aggregate(data_subset[[pnl_col]], by = list(cumsum(c(1, diff(data_subset$position) != 0))), sum, na.rm = TRUE)$x > 0) / 
        nrow(aggregate(data_subset[[pnl_col]], by = list(cumsum(c(1, diff(data_subset$position) != 0))), sum, na.rm = TRUE)) * 100, 2)

      # 4. Largest Win
      LargestWin <- round(max(data_subset[[pnl_col]], na.rm = TRUE), 0)

      # 5. Length of Largest Win
      LengthOfLargestWin <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.max(data_subset[[pnl_col]])], ], 
                                  as.numeric(max(Date) - min(Date) + 1))

      # 6. Average Win
      AverageWin <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] > 0], na.rm = TRUE), 0)

      # 7. Length of Average Win
      AverageWinLength <- data_subset %>%
        transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
        aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
        subset(cum_pnl > 0) %>%
        merge(data_subset, by = "trade_id") %>%
        aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
        with(round(mean(Date, na.rm = TRUE)))
      
      # 8. Largest Loss
      LargestLoss <- round(min(data_subset[[pnl_col]], na.rm = TRUE),0)

      # 9. Length of Largest Loss
      LengthOfLargestLoss <- with(data_subset[data_subset$trade_id == data_subset$trade_id[which.min(data_subset[[pnl_col]])], ], 
                                  as.numeric(max(Date) - min(Date) + 1))

      # 10. Average Loss
      AverageLoss <- round(mean(data_subset[[pnl_col]][data_subset[[pnl_col]] < 0], na.rm = TRUE),0)

      # 11. Length of Average Loss
      AverageLossLength <- data_subset %>%
        transform(cum_pnl = ave(get(pnl_col), trade_id, FUN = cumsum)) %>%
        aggregate(cum_pnl ~ trade_id, data = ., FUN = tail, n = 1) %>%
        subset(cum_pnl < 0) %>%
        {if (nrow(.) == 0) return(NA) else .} %>%
        merge(data_subset, by = "trade_id") %>%
        aggregate(Date ~ trade_id, data = ., FUN = function(x) as.numeric(max(x) - min(x) + 1)) %>%
        with(round(mean(Date, na.rm = TRUE)))

      # 12-15: Winning Runs
      is_winning <- data_subset[[pnl_col]] > 0
      winning_runs <- rle(is_winning)$lengths[rle(is_winning)$values]
      winning_runs <- winning_runs[!is.na(winning_runs)]

      # 12. Average Winning Run
      AverageWinningRun <- round(mean(winning_runs), 2)

      # 13. Largest Winning Run
      LargestWinningRun <- max(winning_runs)

      # 14. Length of Time in Largest Winning Run
      largest_run_start <- sum(head(rle(is_winning)$lengths, which.max(winning_runs) - 1)) + 1
      largest_run_end <- largest_run_start + LargestWinningRun - 1
      LengthOfTimeInLargestWinningRun <- as.numeric(diff(range(data_subset$Date[largest_run_start:largest_run_end]))) + 1

      # 15. Length of Time in Average Winning Run
      average_run_lengths <- sapply(winning_runs, function(len) {
        start <- sum(head(rle(is_winning)$lengths, which(winning_runs == len)[1] - 1)) + 1
        end <- start + len - 1
        as.numeric(diff(range(data_subset$Date[start:end]))) + 1
      })
      LengthOfTimeInAverageWinningRun <- round(mean(average_run_lengths), 0)

      # 16-19: Losing Runs
      is_losing <- data_subset[[pnl_col]] < 0
      losing_runs <- rle(is_losing)$lengths[!rle(is_losing)$values]  # Identify losing runs
      losing_runs <- losing_runs[!is.na(losing_runs)]

      # 16. Average Losing Run
      AverageLosingRun <- round(mean(losing_runs, 2))

      # 17. Length of Time in Average Losing Run
      average_run_lengths_losing <- sapply(losing_runs, function(len) {
        start <- sum(head(rle(is_losing)$lengths, which(losing_runs == len)[1] - 1)) + 1
        end <- start + len - 1
        as.numeric(diff(range(data_subset$Date[start:end]))) + 1
      })
      LengthOfTimeInAverageLosingRun <- round(mean(average_run_lengths_losing), 0)

      # 18. Largest Losing Run
      LargestLosingRun <- max(losing_runs, na.rm = TRUE)

      # 19. Length of Time in Largest Losing Run
      largest_run_start <- sum(head(rle(is_losing)$lengths, which(losing_runs == LargestLosingRun)[1] - 1)) + 1
      largest_run_end <- largest_run_start + LargestLosingRun - 1
      LengthOfLargestLosingRun <- as.numeric(diff(range(data_subset$Date[largest_run_start:largest_run_end]))) + 1

      # 20. Maximum equity drawdown (as a percentage)
      MaxDrawdown <- round(min(data_subset %>%
                          mutate(cum_max_eql = cummax(get(eql_col)),
                                  drawdown = (get(eql_col) - cum_max_eql) / cum_max_eql) %>%
                          pull(drawdown), na.rm = TRUE) * 100, 2)

      # 21. Start and end dates of maximum drawdown
      drawdown_data <- data_subset %>%
        mutate(cum_max_eql = cummax(get(eql_col)), 
              drawdown = (get(eql_col) - cum_max_eql) / cum_max_eql)

      peak_idx <- which.max(drawdown_data[[eql_col]][1:which.min(drawdown_data$drawdown)])  # Peak before max drawdown
      trough_idx <- which.min(drawdown_data$drawdown)  # Trough for max drawdown

      StartDateMaxDrawdown <- as.Date(drawdown_data$Date[peak_idx])
      EndDateMaxDrawdown <- as.Date(drawdown_data$Date[trough_idx])

      # 22. Length of maximum drawdown period (in days)
      LengthOfMaxDrawdown <- as.numeric(EndDateMaxDrawdown - StartDateMaxDrawdown)

      # 23. Maximum equity run-up (as a percentage)
      MaxRunUp <- round(max(data_subset %>%
                        mutate(cum_min_eql = cummin(get(eql_col)),
                              run_up = (get(eql_col) - cum_min_eql) / cum_min_eql) %>%
                        pull(run_up), na.rm = TRUE) * 100,2)

      # 24. Start and end dates of maximum run-up
      run_up_data <- data_subset %>%
        mutate(cum_min_eql = cummin(get(eql_col)),
              run_up = (get(eql_col) - cum_min_eql) / cum_min_eql)

      # Identify the peak (maximum run-up) and trough (start of run-up)
      trough_idx_run_up <- which.min(run_up_data[[eql_col]])  # Trough before the run-up
      peak_idx_run_up <- which.max(run_up_data$run_up)  # Peak during the run-up

      # Ensure that the peak happens after the trough
      if (peak_idx_run_up < trough_idx_run_up) {
        peak_idx_run_up <- which.max(run_up_data$run_up[trough_idx_run_up:length(run_up_data$run_up)]) + trough_idx_run_up - 1
      }

      StartDateMaxRunUp <- as.Date(run_up_data$Date[trough_idx_run_up])
      EndDateMaxRunUp <- as.Date(run_up_data$Date[peak_idx_run_up])

      # 25. Length of maximum run-up period (in days)
      LengthOfMaxRunUp <- as.numeric(EndDateMaxRunUp - StartDateMaxRunUp)

      # Return the metrics as a list
      return(
        list(
          GrossProfit = GrossProfit,
          AnnualizedProfit = AnnualizedProfit,
          NumberOfTradesPerYear = NumberOfTradesPerYear,
          PercentageOfWinningTrades = PercentageOfWinningTrades,
          AverageWin = AverageWin,
          LengthOfAverageWin = AverageWinLength,
          AverageLoss = AverageLoss,
          LengthOfAverageLoss = AverageLossLength,
          LargestWin = LargestWin,
          LengthOfLargestWin = LengthOfLargestWin,
          LargestLoss = LargestLoss,
          LengthOfLargestLoss = LengthOfLargestLoss,
          AverageWinningRun = AverageWinningRun,
          LengthOfTimeInAverageWinningRun = LengthOfTimeInAverageWinningRun,
          AverageLosingRun = AverageLosingRun,
          LengthOfTimeInAverageLosingRun = LengthOfTimeInAverageLosingRun,
          LargestWinningRun = LargestWinningRun,
          LengthOfTimeInLargestWinningRun = LengthOfTimeInLargestWinningRun,
          LargestLosingRun = LargestLosingRun,
          LengthOfTimeInLargestLosingRun = LengthOfLargestLosingRun,
          MaxDrawdown = MaxDrawdown,
          StartDateMaxDrawdown = as.Date(StartDateMaxDrawdown),
          EndDateMaxDrawdown = as.Date(EndDateMaxDrawdown),
          LengthOfMaxDrawdown = LengthOfMaxDrawdown,
          MaxRunUp = MaxRunUp,
          StartDateMaxRunUp = as.Date(StartDateMaxRunUp),
          EndDateMaxRunUp = as.Date(EndDateMaxRunUp),
          LengthOfMaxRunUp = LengthOfMaxRunUp
        )
      )
    }

    # Metrics for Active strategy
    active <- estimate_trading_profile(data_subset, "Active")

    # Metrics for Passive strategy
    passive <- estimate_trading_profile(data_subset, "Passive")

    # Combine metrics into a dataframe

    metrics_df <- data.frame(
      Strategy = c("Active", "Passive"),
      ticker = symbol,
      GrossProfit = c(active$GrossProfit, passive$GrossProfit),
      AnnualizedProfit = c(active$AnnualizedProfit, passive$AnnualizedProfit),
      NumberOfTradesPerYear = c(active$NumberOfTradesPerYear, passive$NumberOfTradesPerYear),
      PercentageOfWinningTrades = c(active$PercentageOfWinningTrades, "NotApplicable"),
      AverageWin = c(active$AverageWin, passive$AverageWin),
      LengthOfAverageWin = c(active$LengthOfAverageWin, passive$LengthOfAverageWin),
      LargestWin = c(active$LargestWin, passive$LargestWin),
      LengthOfLargestWin = c(active$LengthOfLargestWin, passive$LengthOfLargestWin),
      AverageLoss = c(active$AverageLoss, passive$AverageLoss),
      LengthOfAverageLoss = c(active$LengthOfAverageLoss, passive$LengthOfAverageLoss),
      LargestLoss = c(active$LargestLoss, passive$LargestLoss),
      LengthOfLargestLoss = c(active$LengthOfLargestLoss, passive$LengthOfLargestLoss),
      AverageWinningRun = c(active$AverageWinningRun, passive$AverageWinningRun),
      LengthOfTimeInAverageWinningRun = c(active$LengthOfTimeInAverageWinningRun, passive$LengthOfTimeInAverageWinningRun),
      LargestWinningRun = c(active$LargestWinningRun, passive$LargestWinningRun),
      LengthOfTimeInLargestWinningRun = c(active$LengthOfTimeInLargestWinningRun, passive$LengthOfTimeInLargestWinningRun),
      AverageLosingRun = c(active$AverageLosingRun, passive$AverageLosingRun),
      LengthOfTimeInAverageLosingRun = c(active$LengthOfTimeInAverageLosingRun, passive$LengthOfTimeInAverageLosingRun),
      LargestLosingRun = c(active$LargestLosingRun, passive$LargestLosingRun),
      LengthOfTimeInLargestLosingRun = c(active$LengthOfTimeInLargestLosingRun, passive$LengthOfTimeInLargestLosingRun),
      MaxDrawdown = c(active$MaxDrawdown, passive$MaxDrawdown),
      LengthOfMaxDrawdown = c(active$LengthOfMaxDrawdown, passive$LengthOfMaxDrawdown),
      StartDateMaxDrawdown = c(as.Date(active$StartDateMaxDrawdown), as.Date(passive$StartDateMaxDrawdown)),
      EndDateMaxDrawdown = c(as.Date(active$EndDateMaxDrawdown), as.Date(passive$EndDateMaxDrawdown)),
      MaxRunUp = c(active$MaxRunUp, passive$MaxRunUp),
      LengthOfMaxRunUp = c(active$LengthOfMaxRunUp, passive$LengthOfMaxRunUp),
      StartDateMaxRunUp = c(as.Date(active$StartDateMaxRunUp), as.Date(passive$StartDateMaxRunUp)),
      EndDateMaxRunUp = c(as.Date(active$EndDateMaxRunUp), as.Date(passive$EndDateMaxRunUp))
  )
  
  return(metrics_df)

},

# Cut the Strategy time horizon, used for the data split
slicer = function(data, cut_date, data_type) {
  if (inherits(data, c("xts", "zoo"))) {
    # For xts/zoo, filter by the date index
    data <- switch(data_type,
                   "in_sample" = data[index(data) <= as.Date(cut_date), ],
                   "out_of_sample" = data[index(data) > as.Date(cut_date), ],
                   stop("Invalid data_type. Use 'in_sample' or 'out_of_sample'.")
    )
  } else {
    # For non-xts data (data.frame/tibble), filter by 'Date' column
    data <- switch(data_type,
                   "in_sample" = data %>% filter(Date <= as.Date(cut_date)),
                   "out_of_sample" = data %>% filter(Date > as.Date(cut_date)),
                   stop("Invalid data_type. Use 'in_sample' or 'out_of_sample'.")
    )
  }
  
  return(data)
}

  )
)

# Define SMA1 (trend following strategy) class
SMA1 <- R6Class(
  "SMA1",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = NULL,

initialize = function(data, window_size, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$ma_type <- ma_type
},

generate_signals = function() {
      ma_func <- get(self$ma_type)
      self$data <- mutate(self$data, 
                          ma = ma_func(Close, self$window_size, align = "right", fill = NA),
                          signal = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
},

# Testing different strategy parameters given multiperiod and multimarket
run_backtest = function(symbols, window_sizes, ma_types, data_type, split, cut_date, from_date, to_date, slicing_years, apply_rm, max_risks, reward_ratios, leverages, output_df = FALSE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (ma_type in ma_types) {
        for(max_risk in max_risks) {
          for(reward_ratio in reward_ratios) {
            for (leverage in leverages) {

        # Fetch data using DataFetcher for the current symbol and date range
        data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
        data <- data_fetcher$download_xts_data()
        
        # Ensure data is not empty
        if (nrow(data) == 0) {
          warning(paste("No data available for symbol:", symbol))
          next
        }

        # Create an instance of SMA1 strategy
        sma_instance <- SMA1$new(data, window_size = window_size, ma_type = ma_type)
        
      # Estimate performance based on the split argument
      if (split) {
        performance <- sma_instance$estimate_performance(
          data_type = data_type,
          split_data = TRUE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      } else {
        performance <- sma_instance$estimate_performance(
          data_type = data_type,
          split_data = FALSE,
          cut_date = cut_date,
          window = slicing_years,
          apply_rm = apply_rm,
          max_risk = max_risk,
          reward_ratio = reward_ratio,
          capital = capital,
          leverage = leverage,
          symbol = symbol
        )
      }
        # Skip if performance is NULL
        if (is.null(performance) || nrow(performance) == 0) {
          warning(paste("No performance data for symbol:", symbol, 
                        "window_size:", window_size, 
                        "ma_type:", ma_type))
          next
        }

        # Store the results
        results[[paste(symbol, window_size, ma_type, max_risk, reward_ratio, leverage, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("SMA1:", window_size, ma_type),
          Window_Size = window_size,
          MA_Type = ma_type,
          Max_Risk = max_risk,
          Reward_Ratio = reward_ratio,
          #Leverage = leverage,
          Performance = performance
        )

        print(paste0(
          "Processed SMA1 (symbol: ", symbol, 
          ", class: ", meta$assets[[symbol]]$class, 
          ", window_size: ", window_size, 
          ", ma_type: ", ma_type, 
          ", split: ", split, 
          ", max_risk: ", max_risk, 
          ", reward_ratio: ", reward_ratio, 
          ", leverage: ", leverage,
          ")"
          )
        )
            }
          }
        }
      }
    }
  }

  # Check if results list is empty
  if (length(results) == 0) {
    stop("No valid results were generated. Check the input parameters or data availability.")
  }

  # Create the final data frame if output_df is TRUE
  if (output_df) {
    res_df <- do.call(rbind, lapply(results, function(x) {
      performance_data <- x$Performance

      # Combine 'from' and 'to' into 'Period'
      if ("from" %in% names(performance_data) && "to" %in% names(performance_data)) {
        performance_data$Period <- paste(performance_data$from, "to", performance_data$to)
      } else {
        performance_data$Period <- "Full Period"
      }

      # Remove 'from', 'to', and 'ticker' columns
      performance_data <- performance_data[, !names(performance_data) %in% c("from", "to", "ticker")]

      # Add metadata columns
      cbind(
        Symbol = x$Symbol,
        Class = x$Class,
        Methodology = x$Methodology,
        Window_Size = x$Window_Size,
        MA_Type = x$MA_Type,
        Max_Risk = x$Max_Risk,
        Reward_Ratio = x$Reward_Ratio,
        #Leverage = x$Leverage,
        performance_data
      )
    }))

    # Reset row names
    rownames(res_df) <- 1:nrow(res_df)

    return(res_df)
  } else {
    return(results)
  }
}

  )
)

######################################################
# Specify trading strategy parameters
from_date <- as.Date("2018-01-01") 
to_date <- Sys.Date()
symbol <- "BTC-USD"
#capital <- 100000 # USD
capital <- 1000 # USDC
leverage <- 1 # financial leverage used to calculate number of positions in estimate_performance in Strategy class
# Also, it is assumed 100% portfolio investment (number of positions =  capital / price per unit).
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

# Run instance of SMA1

# IN-SAMPLE (WITHOUT SPLIT)
sma1 <- SMA1$new(ts, window_size = 20, ma_type = 'EMA')
sma1$estimate_range_potential(n=14)
plots <- sma1$plot_close_vs_vol(30)
grid.arrange(plots$close, plots$n, ncol = 1)

# in-sample:
sma1_res_in_sample <- t(sma1$estimate_performance(data_type = "in_sample", split = FALSE, cut_date = as.Date("2024-01-01"), window = 0.5, 
  apply_rm = TRUE, max_risk = 0.3, reward_ratio = 6, capital, leverage = 1, symbol))

sma1_res_in_sample_dt <- cbind(Metric = rownames(sma1_res_in_sample), as.data.table(as.data.frame(sma1_res_in_sample, stringsAsFactors = FALSE)))

sma1_res_in_sample_dt[, units := ifelse(
  .I <= 5 | Metric %in% c("max_risk", "reward_ratio", "NumberOfTradesPerYear"), "",
  ifelse(
    Metric %in% c("AnnualizedProfit", "PercentageOfWinningTrades", "MaxDrawdown", "MaxRunUp"), "%",
    ifelse(
      Metric %in% c("LengthOfLargestWin", "LengthOfLargestLoss", "LengthOfAverageWin", "LengthOfAverageLoss", 
                    "LengthOfMaxDrawdown", "LengthOfMaxRunUp", "LengthOfTimeInLargestWinningRun", "LengthOfTimeInLargestLosingRun", 
                    "LengthOfTimeInAverageWinningRun", "LengthOfTimeInAverageLosingRun", "LargestWinningRun", "LargestLosingRun"), "days",
      ifelse(
        grepl("Date", Metric), "Date", 
        "USD"  # Default case for other rows
      )
    )
  )
)]

sma1$plot_equity_lines("SMA1", signal_flag = FALSE, capital, symbol)
trades <- sma1$get_trades()$trades

data <- sma1$data

# Trades
ggplot(data = data.frame(Efficiency = trades$Efficiency[is.finite(trades$Efficiency)]), aes(x = Efficiency)) +
  geom_histogram(binwidth = diff(range(trades$Efficiency[is.finite(trades$Efficiency)])) / 20, 
                 fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Trade Efficiency = Trade PnL / Abs(Running PnL)", 
       x = "Efficiency", 
       y = "Frequency") +
  scale_x_continuous(
    expand = c(0, 0), 
    limits = c(min(trades$Efficiency[is.finite(trades$Efficiency)]), NA),
    breaks = seq(floor(min(trades$Efficiency[is.finite(trades$Efficiency)])), 
                 ceiling(max(trades$Efficiency[is.finite(trades$Efficiency)])), 
                 by = 100)  # Adjust the 'by' value to control the spacing of ticks
  ) + 
  theme_minimal()


trades$Efficiency %>% summary # % distribution
trades$Trade_Cum_PnL %>% summary # expected PnL given 1000 USD

dataset <- sma1$data
table(dataset$position)

sum(dataset$value > 0.01) / nrow(dataset) * 100
sum(dataset$value > 0.025) / nrow(dataset) * 100
sum(dataset$value > 0.05) / nrow(dataset) * 100

# IN-SAMPLE (WITHOUT SPLIT)

window_sizes = round(10 * (1.25 ^ (0:13)))

# Overall trading profile (NO SPLIT with stop-loss)
res_sma1_overall_btc_bnb_eth <- sma1$run_backtest(
  symbols = c("BTC-USD"),
  window_sizes = window_sizes,
  ma_types = c("SMA", "EMA"), 
  data_type = "in_sample",
  split = FALSE,
  cut_date = as.Date("2024-01-01"),
  from_date = as.Date("2018-01-01"),
  to_date = as.Date("2024-01-01"),
  slicing_years = 4,
  apply_rm = TRUE,
  max_risks = seq(0.1, 0.3, by = 0.1),
  reward_ratios = seq(2,3, by = 1),
  leverages = seq(1, 2, by = 1),
  output_df = TRUE
)

#fread(res_sma1_overall_btc_bnb_eth, "/Users/olegb/Documents/ATS/ATS/bin/res_sma1_btc.csv")

# Backtest visualization
ggplot(res_sma1_overall_btc_bnb_eth %>% filter(leverage == 2), aes(x = Window_Size, y = AnnualizedProfit)) +
  geom_point(aes(color = MA_Type, shape = MA_Type), size = 3, alpha = 0.6) +  # Points with different shapes and colors for each MA_Type
  geom_smooth(method = "loess", se = FALSE, color = "red") +  # Single smooth line
  scale_color_manual(values = c("SMA" = "blue", "EMA" = "green", "WMA" = "purple")) +  # Custom colors for each MA_Type
  scale_shape_manual(values = c("SMA" = 16, "EMA" = 15, "WMA" = 17)) +  # Circle for SMA, Square for EMA, Triangle for WMA
  labs(
    title = paste("Annualized Profit vs Window Size for", symbol),
    x = "Window Size (Days)",
    y = "Annualized Profit (%)",
    color = "MA Type",
    shape = "MA Type"
  ) +
  scale_x_continuous(breaks = seq(min(res_sma1_overall_btc_bnb_eth$Window_Size), 
                                  max(res_sma1_overall_btc_bnb_eth$Window_Size), by = 10)) +  # Ticks every 10 days
  scale_y_continuous(breaks = seq(floor(min(res_sma1_overall_btc_bnb_eth$AnnualizedProfit, na.rm = TRUE) / 5) * 5, 
                                  ceiling(max(res_sma1_overall_btc_bnb_eth$AnnualizedProfit, na.rm = TRUE) / 5) * 5, by = 5)) +  # Ticks every 5%
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

#res_sma1_overall_btc_bnb_eth <- fread("/Users/olegb/Documents/ATS/ATS/res_sma1_overall_btc_bnb_eth.csv")

res_sma1_overall_btc_bnb_eth <- res_sma1_overall_btc_bnb_eth %>%
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs based on rows
  group_by(PairID) %>%
  mutate(
    Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(
        Strategy == "Active" & AnnualizedProfit > AnnualizedProfit[Strategy == "Passive"][1], 
        "Yes", 
        "No"
      )
    } else {
      NA  # Assign NA if the group is incomplete
    }
  ) %>%
  ungroup()

# Filter for Active strategies and group by strategy parameters
res_sma1_overall_btc_bnb_eth %>%
  filter(Strategy == "Active") %>%
  group_by(Methodology, Window_Size, MA_Type, Max_Risk, Reward_Ratio) %>%
  summarise(
    Avg_AnnualizedProfit = mean(AnnualizedProfit, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_AnnualizedProfit)) %>%
  slice(1)

# Best in-sample combinations:
# BTC-USD, SMA 116-day 0.3 6
# ETH-USD
# BNB-USD 
# CANDIDATE: 

# IN-SAMPLE: HOW STRATEGY BEHAVES UNDER DIFFERENT PERIODS
# More granular (split) - only for potential good candidates to check robustness
res_sma1_granular <- sma1$run_backtest(
  symbols = c("BTC-USD"),
  window_sizes = 116, 
  ma_type = c("SMA"),
  data_type = "in_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  from_date = as.Date("2018-01-01"),
  to_date = Sys.Date(),
  slicing_years = 2,
  apply_rm = TRUE,
  max_risks = seq(0.1, 0.3, by = 0.1),
  reward_ratios = seq(2, 3, by = 1),
  leverages = seq(2, 3, by = 1),
  output_df = TRUE
)

# Check if a Methodology is superior based on the criteria
superior_methodologies <- res_sma1_granular %>% 
#filter(Symbol == "BNB-USD") %>%
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
  group_by(PairID) %>%
  mutate(
    Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(
        Strategy == "Active" & 
          AnnualizedProfit > AnnualizedProfit[Strategy == "Passive"], 
        "Yes", 
        "No"
      )
    } else {
      NA  # Set NA if either Active or Passive is missing
    }
  ) %>%
  group_by(Methodology) %>% # Now group by Methodology to evaluate all periods
  mutate(
    Superior = if (all(Period_Superior == "Yes", na.rm = TRUE)) {
      "Yes"
    } else {
      "No"
    }
  ) %>%
  ungroup()

paste0("The robustness of in-sample superiority (%) is ", superior_methodologies %>% filter(Period_Superior == "Yes") %>% nrow / (nrow(superior_methodologies) / 2) * 100)

# Evaluate the robustness of each strategy across multiple periods
ranked_strategies <- res_sma1_granular %>% 
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
  group_by(PairID) %>%
  mutate(
    Period_Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(
        Strategy == "Active" & 
          AnnualizedProfit > AnnualizedProfit[Strategy == "Passive"], 
        "Yes", 
        "No"
      )
    } else {
      NA  # Set NA if either Active or Passive is missing
    }
  ) %>%
  ungroup() %>%
  filter(Symbol == "BTC-USD") %>%
  group_by(Strategy, Methodology, Max_Risk, Reward_Ratio) %>%
  summarise(
    Total_Periods = n(),  # Total number of periods
    Superior_Periods = sum(Period_Superior == "Yes", na.rm = TRUE),  # Count superior periods
    Robustness = (Superior_Periods / Total_Periods) * 100  # Calculate robustness as a percentage
  ) %>%
  arrange(desc(Robustness))  # Rank strategies by robustness in descending order

# Output the ranking
ranked_strategies

# ACHIEVE ~60-70% 

# OUT-OF-SAMPLE PERFORMANCE
sma1_os <- SMA1$new(ts, window_size = 100, ma_type = 'HMA')
sma1_res_out_sample <- t(sma1_os$estimate_performance(data_type = "out_of_sample", split = FALSE, cut_date = as.Date("2024-01-01"), window = 1,
 apply_stop_loss = TRUE, stop_loss_threshold = 0.015, reward_ratio = 25, capital, leverage, symbol))

sma1_res_out_sample_dt <- cbind(Metric = rownames(sma1_res_out_sample), as.data.table(as.data.frame(sma1_res_out_sample, stringsAsFactors = FALSE)))

# Apply the same logic for the units column
sma1_res_out_sample_dt[, units := ifelse(
  .I <= 5 | Metric == "NumberOfTradesPerYear", "",  # First five rows and 'NumberOfTradesPerYear' are empty
  ifelse(
    Metric %in% c("AnnualizedProfit", "PercentageOfWinningTrades", "MaxDrawdown", "MaxRunUp"), "%",
    ifelse(
      Metric %in% c("LengthOfLargestWin", "LengthOfLargestLoss", "LengthOfAverageWin", "LengthOfAverageLoss", 
                    "LengthOfMaxDrawdown", "LengthOfMaxRunUp", "LengthOfTimeInLargestWinningRun", "LengthOfTimeInLargestLosingRun", 
                    "LengthOfTimeInAverageWinningRun", "LengthOfTimeInAverageLosingRun", "LargestWinningRun", "LargestLosingRun"), "days",
      ifelse(
        grepl("Date", Metric), "Date", 
        "USD"  # Default case for other rows
      )
    )
  )
)]

sma1_os$plot_equity_lines("SMA1", signal_flag = FALSE)

#trades <- sma1_os$get_trades()

# Overall trading profile
res_sma1_overall_os <- sma1_os$run_backtest(
  symbols = c("BTC-USD", "BNB-USD", "ETH-USD"),
  window_sizes = 20,
  ma_type = "EMA",  # Add more MA types here
  data_type = "out_of_sample",
  split = TRUE,
  cut_date = as.Date("2024-01-01"),
  from_date = as.Date("2020-01-01"),
  to_date = Sys.Date(),
  slicing_years = 1,
  apply_stop_loss = FALSE,
  stop_loss_threshold = 0.025,
  reward_ratio = 15,
  output_df = TRUE
  )

# All results in one df
library(stringr)
res_all <- fread("Run_backtest_results/res_all.csv")

res_all <- res_all %>% 
  mutate(
    Meth = str_extract(Methodology, "^[^:]+")
) %>%
  select(Symbol, Class, Meth, Methodology, everything(.)) %>%
  mutate(PairID = ceiling(row_number() / 2)) %>%  # Assign pair IDs
  group_by(PairID) %>%
  mutate(
    Superior = if (all(c("Active", "Passive") %in% Strategy)) {
      ifelse(Strategy == "Active" & aR > aR[Strategy == "Passive"], "Yes", "No")
    } else {
      NA  # Assign NA if the group is incomplete
    }
  ) %>%
  ungroup()

# View the updated data
res_all %>% filter(Symbol == "BTC-USD" & Strategy == "Active" & Superior == "Yes") %>% arrange(desc(aR)) %>% select(Methodology) %>% unique

best_strategies <- res_all %>% group_by(Symbol) %>%
filter(Strategy == "Active") %>%
  filter(aR == max(aR)) %>%
  ungroup() %>% arrange(Class)

res_all %>%
filter(Symbol == "BTC-USD" & Strategy == "Active") %>%
group_by(Methodology) %>%
 #filter(Methodology == "SMA1: 40 SMA") %>%
  select(aR) %>% 
    summary

# Compute percentage of superior rows for each Methodology for a Symbol
res_all %>%
filter(Symbol == "ETH-USD") %>%
  group_by(Meth) %>%
  summarise(
    TotalRows = n(),
    SuperiorRows = sum(Superior == "Yes"),
    PercentageSuperior = (SuperiorRows / TotalRows) * 100
  ) %>% arrange(desc(PercentageSuperior))

res_sma1_overall_btc_bnb_eth %>%
filter(Symbol == "BTC-USD") %>%
 filter(Methodology == "SMA1: 100 HMA" & Strategy == "Active") %>%
  select(c(AnnualizedProfit, MaxDrawdown, NumberOfTradesPerYear)) %>% 
    summary
