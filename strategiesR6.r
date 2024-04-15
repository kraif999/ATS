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
    tsRaw <- getSymbols(self$symbol, from = self$from_date, to = self$to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(tsRaw)
    # UseMethod("mutate") : no applicable method for 'mutate' applied to an object of class "c('xts', 'zoo')"
    ts$value <-  log(ts[,paste0(symbol, ".Close")]) - log(lag(ts[,paste0(symbol, ".Close")]))
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

######################################################
# Specify trading strategy parameters
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2024-03-10", format = "%Y-%m-%d")
symbol <- "BZ=F" # oil
capital <- 50000 # units of initial capital invested
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher_garch <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher_garch$download_xts_data()
data_fetcher_garch$plot_close_or_rets(type = "close")
data_fetcher_garch$plot_close_or_rets(type = "rets")
data_fetcher_garch$plot_close_or_rets(type = "rets_hist")
data_fetcher_garch$compute_NA_close_price_ratio()

# Define TSA class
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

# Instances of TSA
# daily overlapping returns
tsa <- TSA$new(ts)
tsa$estimate_stationarity(freq = "daily")
tsa$estimate_heteroscedasticity(freq = "daily")
tsa$estimate_autocorr_rets_vol(test = "rets", freq = "daily", plot_flag = TRUE)
tsa$estimate_seasonality(freq = "daily")
tsa$estimate_arch_effects(freq = "daily", p = 1, q = 1)
tsa$compute_wkd_rets(freq = "daily")
tsa$compute_summary_statistics(freq = "daily")

# biweekly non-overlapping returns
tsa$estimate_stationarity(freq = "biweekly")
tsa$estimate_heteroscedasticity(freq = "biweekly")
tsa$estimate_autocorr_rets_vol(test = "rets", freq = "biweekly", plot_flag = TRUE)
tsa$estimate_seasonality(freq = "biweekly")
tsa$estimate_outliers(test = "zscore", freq = "daily", plot_flag = TRUE)
tsa$estimate_outliers(test = "fences", freq = "biweekly", plot_flag = TRUE, q1 = 0.25, q3 = 0.75, threshold = 1.5)
tsa$estimate_outliers(test = "fences", freq = "biweekly", plot_flag = TRUE, q1 = 0.25, q3 = 0.75, threshold = 2)
tsa$estimate_arch_effects(freq = "daily", p = 1, q = 1)


# Define parent Strategy class
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
          mutate(Date = as.Date(rownames(.))) %>%
            select(Date, everything()) %>%
                na.omit() %>% 
                    as.tibble()
    return(ts_df)
},

# Performance estimation for Active and Passive strategies (annualized return, standard deviation, Information ratio, maximum drawdown, trades count, success prediction rate)
estimate_performance = function() {

  self$generate_signals()  # Call generate_signals dynamically

  self$data <- self$data %>% 
    mutate(
    pnlActive = c(0, diff(Close) * signal[-length(Close)]),
    pnlPassive = c(0, diff(Close)),
    nopActive = 0,
    nopPassive = 0,
    eqlActive = 0,
    eqlPassive = 0
  )

  # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
  self$data$eqlActive[1] <- capital
  self$data$nopActive[1] <- floor(capital / self$data$Close[1])
  self$data$eqlPassive[1] <- capital
  self$data$nopPassive[1] <- floor(capital / self$data$Close[1])   

  for (i in 2:nrow(self$data)) {

    # Active
    pnlActive <- self$data$pnlActive[i]
    prev_nop_Active <- floor(self$data$nopActive[i - 1])
    current_nop_Active <- floor((self$data$eqlActive[i - 1]) / self$data$Close[i])
    self$data$eqlActive[i] <- self$data$eqlActive[i - 1] + prev_nop_Active * pnlActive
    self$data$nopActive[i] <- current_nop_Active
    
    # Passive
    pnlPassive <- self$data$pnlPassive[i]
    prev_nop_Passive <- floor(self$data$nopPassive[i - 1])
    current_nop_Passive <- floor((self$data$eqlPassive[i - 1]) / self$data$Close[i])
    self$data$eqlPassive[i] <- self$data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
    self$data$nopPassive[i] <- current_nop_Passive
  }

  self$data <- self$data %>%
    mutate(r_eqlActive = quantmod::Delt(eqlActive),
            r_eqlPassive = quantmod::Delt(eqlPassive)) %>%
              na.omit

  # Performance metrics for active strategy
  aR_active <- round(as.numeric(Return.annualized(as.numeric(self$data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(self$data$r_eqlActive), scale = 252) * 100), 3)
  IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
  MD_active <- round(as.numeric(maxDrawdown(as.numeric(self$data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
  trades_active <- sum(c(1, ifelse(self$data$signal[-1] * self$data$signal[-length(self$data$signal)] < 0, 1, 0)))

  # Performance metrics for passive strategy
  aR_passive <- round(as.numeric(Return.annualized(as.numeric(self$data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(self$data$r_eqlPassive), scale = 252) * 100), 3)
  IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
  MD_passive <- round(as.numeric(maxDrawdown(as.numeric(self$data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
  trades_passive <- 1

  # Create dataframe
  df <- data.frame(
    Strategy = c("Active", "Passive"),
    aR = c(aR_active, aR_passive),
    aSD = c(aSD_active, aSD_passive),
    IR = c(IR_active, IR_passive),
    MD = c(MD_active, MD_passive),
    trades = c(trades_active, trades_passive)
  )

  print(df)

  # Calculate success rates
  buy_success_rate_active <- sum(self$data$position > 0 & self$data$value > 0) / nrow(self$data)
  buy_success_rate_passive <- sum(self$data$value > 0) / nrow(self$data)

  short_success_rate_active <- sum(self$data$position < 0 & self$data$value < 0) / nrow(self$data)
  short_success_rate_passive <- 0

  combined_rate_active <- (sum(self$data$position > 0 & self$data$value > 0) + sum(self$data$position < 0 & self$data$value < 0)) / nrow(self$data)
  combined_rate_passive <- (sum(self$data$value > 0)) / nrow(self$data)

  # Create data frame for success rates
  success_rate <- data.frame(
    Strategy = c("Active", "Passive"),
    Buy_Success_Rate = c(buy_success_rate_active, buy_success_rate_passive),
    Short_Success_Rate = c(short_success_rate_active, short_success_rate_passive),
    Combined_Success_Rate = c(combined_rate_active, combined_rate_passive)
  )

  # Print the performance data frame and success rate data frame
  print(success_rate)

  return(self$data)
},

# Calculate cumulative return (method to be removed)
calculate_cumulative_return = function() {
      #self$generate_signals()  # Call generate_signals dynamically
      cret = data.frame(
        Active = prod(1+self$data$value * self$data$position) - 1,
        Buy_and_hold = prod(1+self$data$value) - 1
      )
      return(cret)
},

# Visualize equity lines for active strategy and passive (buy and hold)
plot_equity_lines = function(strategy_name) {
      # Plot equity lines
      ggplot(self$data, aes(x = Date)) +
        #geom_line(aes(y = equity_line, color = "Active Strategy")) +
        geom_line(aes(y = eqlActive, color = "Active Strategy")) +
        #geom_line(aes(y = equity_line_bh, color = "Buy and Hold Strategy")) +
        geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy")) +
        labs(title = paste0("Equity Lines for Active ", "(", as.character(strategy_name), ")", " and Passive (buy-and-hold) strategies"),
             x = "Date",
             y = "Equity line") +
        scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "green")) +
        theme_minimal()
}

  )
)

# Define class for Strategy based on GARCH model
GARCHStrategy <- R6Class(
  "GARCHStrategy",
    inherit = Strategy,
    public = list(
    specification = NULL, 
    n_start = NULL, 
    refit_every = NULL, 
    refit_window = NULL, 
    distribution_model = NULL, 
    realized_vol = NULL, 
    #cluster = makePSOCKcluster(parallel::detectCores(logical = FALSE)),

initialize = function(
    data,
    specification, 
    n_start, 
    refit_every, 
    refit_window, 
    distribution_model, 
    realized_vol, 
    cluster) {
      super$initialize(data)
      self$specification <- specification
      self$n_start <- n_start
      self$refit_every <- refit_every
      self$refit_window <- refit_window
      self$distribution_model <- distribution_model
      self$realized_vol <- realized_vol
      #self$cluster <- cluster
    },

# Method to estimate realized volatility by different approaches
estimate_realized_volatility = function(data) {
    ohlc <- data %>% 
        data.frame %>%
            rename_with(~ sub(".*\\.", "", .), everything()) %>%
                select(-Volume, -Adjusted) %>%
                    na.omit  %>%
                        as.matrix

    # Different realized volatility estimators for returns (TTR)
    histVolest <- merge(
    garman <- as.xts(na.omit(volatility(ohlc, calc = "garman"))) / sqrt(252),
    close <- as.xts(na.omit(volatility(ohlc[,4], calc = "close"))) / sqrt(252),
    parkinson <- as.xts(na.omit(volatility(ohlc, calc = "parkinson"))) / sqrt(252),
    rogers.satchell <- as.xts(na.omit(volatility(ohlc, calc = "rogers.satchell"))) / sqrt(252),
    garman_modified <- as.xts(na.omit(volatility(ohlc, calc = "gk.yz"))) / sqrt(252),
    yang.zhang <- as.xts(na.omit(volatility(ohlc, calc = "yang.zhang"))) / sqrt(252)
    ) %>% 
    as.data.frame %>% 
        rename_with(~ c("garman", "close", "parkinson", "rogers_satchell", "garman_modified", "yang_zhang")) %>%
        mutate(TradeDate = as.Date(rownames(.))) %>%
            select(TradeDate, everything(.)) %>%
            na.omit

    return(histVolest)
},

# Method to specify signal criteria (based on GARCH model volatility forecasts)
signal_criteria = function(volData) {
    modified_volData <- volData %>%
    mutate(
        q75 = rollapply(Forecast, width = self$n_start, FUN = function(x) quantile(x, probs = 0.75), align = "right", fill = NA),
        signal = case_when(
        Forecast < q75 ~ 1,
        Forecast > q75 ~ -1,
        TRUE ~ 0    
        ),
        position = lag(signal)
    ) %>% na.omit  # Remove the first row since it will have NA for signal
    return(modified_volData)
},

# Method to generate column of signals and positions
generate_signals = function() {
    histVolest <- self$estimate_realized_volatility(self$data)
    instr <- self$data %>% 
        as.data.frame() %>%
            rename_with(~ sub(".*\\.", "", .), everything()) %>%
                mutate(TradeDate = as.Date(rownames(.))) %>%
                    select(TradeDate, Open, High, Low, Close) %>%
                        mutate(value = as.numeric(log(Close/lag(Close)))) %>%
                            # EquityLine = cumprod(ifelse(is.na(rets), 1, 1 + rets))) %>%
                                na.omit

    listgarch <- expand.grid(
    specification = self$specification,
    window.size = self$n_start,
    refit.frequency = self$refit_every,
    refit.window.type = self$refit_window,
    distribution.model = self$distribution_model,
    realized.vol.method = self$realized_vol,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
    )
    colnames(listgarch)[1:6] <- c("specification", "window.size", "refit.frequency", "refit.window.type", "distribution.model", "realized.vol.method")
      
    if(listgarch$specification == "fGARCH") {
        spec <- ugarchspec(
        variance.model = list(
        model = listgarch$specification,
        garchOrder = c(1, 1), 
        submodel = "TGARCH", 
        external.regressors = NULL, 
        variance.targeting = FALSE), 
        
        mean.model = list(
        armaOrder = c(1, 1), 
        include.mean = TRUE, 
        archm = FALSE, 
        archpow = 1, 
        arfima = FALSE, 
        external.regressors = NULL, 
        archex = FALSE),
        distribution.model = listgarch$distribution.model) 
    # , start.pars = list(), fixed.pars = list(), ...)
  } else {
    spec <- ugarchspec(
        variance.model = list(
        model = listgarch$specification, 
        garchOrder = c(1, 1), 
        submodel = NULL, 
        external.regressors = NULL, 
        variance.targeting = FALSE), 

        mean.model = list(
        armaOrder = c(1, 1), 
        include.mean = TRUE, 
        archm = FALSE, 
        archpow = 1, 
        arfima = FALSE, 
        external.regressors = NULL, 
        archex = FALSE),
        distribution.model = listgarch$distribution.model) 
  }

    if(listgarch$refit.window.type == "moving") {
        roll = ugarchroll(
        spec, 
        #instr_ret[,1], 
        self$data[,"value"],
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch$window.size,  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch$refit.frequency, # determines every how many periods the model is re-estimated.
        refit.window = listgarch$refit.window.type, # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead).
        window.size = listgarch$window.size,
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = makePSOCKcluster(parallel::detectCores(logical = FALSE)),
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
    } else {
    roll = ugarchroll(
        spec, 
        #instr_ret[,1], 
        self$data[,"value"],
        # n.ahead = 1 - window size - the number of periods to forecast, supported only n.ahead = 1 by default
        n.start = listgarch$window.size,  # starting point in the dataset from which to initialize the rolling forecast
        refit.every = listgarch$refit.frequency, # determines every how many periods the model is re-estimated.
        refit.window = listgarch$refit.window.type, # Whether the refit is done on an expanding window including all the previous data or a moving window,
        # where all previous data is used for the first estimation and then moved by a length equal to refit.every (unless the window.size option is used instead)
        # window.size = listgarch[i,2],
        solver = "hybrid", # the solver to use 
        calculate.VaR = TRUE, # 
        VaR.alpha = c(0.01, 0.05), 
        cluster = makePSOCKcluster(parallel::detectCores(logical = FALSE)),
        # realizedVol = sp500ret2[,2], solver.control=list(tol=1e-6, trace=1), fit.control=list(scale=1),
        keep.coef = TRUE) 
  }
    # roll <- resume(roll, solver= "gosolnp") # if object contains non-converged windows
    # show(roll) # 20.02 secs

    forecastVolRoll <- data.frame(
        #Date = roll@model$index[(listgarch$window.size[i]+1):length(roll@model$index)],
        Date = roll@model$index[(listgarch$window.size+1):length(roll@model$index)],
        Forecast = roll@forecast$density$Sigma
            )

    # Join realized volatility estimation and instr log returns given TradeDate
    volForHistRoll <- forecastVolRoll %>%
        mutate(TradeDate = Date) %>% 
            left_join(histVolest, by = 'TradeDate') %>%
            na.omit %>% select(-Date) %>%
                select(TradeDate, everything()) %>%
                left_join(select(instr, TradeDate, Close, value), by = "TradeDate")
    
    volForHistRoll <- self$signal_criteria(volForHistRoll) %>%
        mutate(Date = TradeDate) %>%
            as.tibble()
    
    #self$data <- copy(volForHistRoll)

    self$data <- as.data.frame(self$data)
    self$data <- self$data %>% rename_with(~ sub(".*\\.", "", .), everything()) %>%
                mutate(Date = as.Date(rownames(.))) %>%
                    select(Date, value) %>%
                        left_join(select(volForHistRoll, Date, Close, signal, position)) %>%
                            na.omit %>%
                                as.tibble
}
  )
)

# Instances of GARCH strategy
garch_strategy <- GARCHStrategy$new(
  data = ts,
  specification = "sGARCH",
  n_start = 126,
  refit_every = 21,
  refit_window = "moving",
  distribution_model = "snorm",
  realized_vol = "close"
)

garch_strategy$estimate_performance()
garch_strategy$plot_equity_lines("sGARCH-126-21-moving-snorm-close")

# Define SMA1 class
SMA1 <- R6Class(
  "SMA1",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = "simple",
    initialize = function(data, window_size, ma_type = "simple") {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$ma_type <- ma_type
    },
    generate_signals = function(ma_type = self$ma_type) {
      ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
      self$data <- mutate(self$data, 
                          #ma = ma_func(value, k = self$window_size, align = "right", fill = NA),
                          ma = ma_func(Close, k = self$window_size, align = "right", fill = NA),
                          #signal = ifelse(value > ma, 1, ifelse(value < ma, -1, 0)),
                          signal = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
    }
  )
)

# Instances of SMA1 strategy
sma1 <- SMA1$new(ts, window_size = 10, ma_type = 'exp')
sma1$estimate_performance()
sma1$plot_equity_lines("SMA1")

# Define SMA2 class
SMA2 <- R6Class(
  "SMA2",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = "simple",

    initialize = function(data, window_size1, window_size2, ma_type = "simple") {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$ma_type <- ma_type
    },

    generate_signals = function(ma_type = self$ma_type) {
      ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
      # Calculate first and second moving averages
      self$data <- mutate(self$data, 
                          #ma1 = ma_func(value, k = self$window_size1, align = "right", fill = NA),
                          ma1 = ma_func(Close, k = self$window_size1, align = "right", fill = NA),
                          #ma2 = ma_func(value, k = self$window_size2, align = "right", fill = NA),
                          ma2 = ma_func(Close, k = self$window_size2, align = "right", fill = NA),
                          signal = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit

    }
  )
)

# Create instances SMA2
sma2 <- SMA2$new(ts, window_size1 = 10, window_size2 = 100,  ma_type = "simple")
sma2$estimate_performance()
sma2$plot_equity_lines("SMA2")

# Define SMA1 class with modified signals
SMA1M <- R6Class(
  "SMA1M",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    ma_type = "simple",

initialize = function(data, window_size, ma_type = "simple") {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$ma_type
    },

# Generate modified signals
generate_signals = function(ma_type = self$ma_type) {

    ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
    self$data <- mutate(
    self$data, 
    #ma = ma_func(value, k = self$window_size, align = "right", fill = NA),
    ma = ma_func(Close, k = self$window_size, align = "right", fill = NA),
    #signal1 = ifelse(value > ma, 1, ifelse(value < ma, -1, 0)),
    signal1 = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0))) %>%
    #position = lag(signal1, default = 0)) %>% # position will be defined at the end given additional condition
    na.omit
    
    # Initialize last_long_value and last_short_value (dynamic trailing threshold)
    last_long_value <- NA
    last_short_value <- NA

    # Create empty columns in the data frame to store last long and short values
    self$data$last_long_value <- NA
    self$data$last_short_value <- NA

    # Loop through each row of the data frame
    for (i in 1:nrow(self$data)) {
        # Check if current row has signal == 1
        if (self$data$signal1[i] == 1) {
        # Find the index of the first previous occurrence of 1
        first_previous_index <- NA
        if (any(self$data$signal1[1:(i-1)] == 1)) {
            first_previous_index <- max(which(self$data$signal1[1:(i-1)] == 1))
        }
        # If a previous occurrence of 1 is found, update last_long_value
        if (!is.na(first_previous_index)) {
            #last_long_value <- self$data$value[first_previous_index]
            last_long_value <- self$data$Close[first_previous_index]
        }
        # Assign last long value to the corresponding row in the data frame
        self$data$last_long_value[i] <- last_long_value
        }
        # Check if current row has signal1 == -1
        if (self$data$signal1[i] == -1) {
        # Find the index of the first previous occurrence of -1
        first_previous_index <- NA
        if (any(self$data$signal1[1:(i-1)] == -1)) {
            first_previous_index <- max(which(self$data$signal1[1:(i-1)] == -1))
        }
        # If a previous occurrence of -1 is found, update last_short_value
        if (!is.na(first_previous_index)) {
            #last_short_value <- self$data$value[first_previous_index]
            last_short_value <- self$data$Close[first_previous_index]
        }
        # Assign last short value to the corresponding row in the data frame
        self$data$last_short_value[i] <- last_short_value
        }

        # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
        self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
        self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)
    }

    # Compare data$value[i] with the first previous value and update data$s2
    self$data$signal <- NA
    # self$data$signal <- ifelse((self$data$value > self$data$last_long_value) & (self$data$value > self$data$ma), 1,
    #                     ifelse((self$data$value < self$data$last_short_value) & (self$data$value < self$data$ma), -1, 0))

    self$data$signal <- ifelse((self$data$Close > self$data$last_long_value) & (self$data$Close > self$data$ma), 1,
                        ifelse((self$data$Close < self$data$last_short_value) & (self$data$Close < self$data$ma), -1, 0))

    # Replacing 0s by previous signal value:
    self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
    self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)

    self$data$position <- lag(self$data$signal, default = 0)

    #self$data <- self$data 
    # %>% select(-c(signal1, last_long_value, last_short_value))
        }
    )
)

# Instances of SMA1M strategy
sma1m <- SMA1M$new(ts, window_size = 50, ma_type = 'exp')
sma1m$estimate_performance()
sma1m$plot_equity_lines("SMA1M")

# SMA2 (modified by dynamic trailing stop)
SMA2M <- R6Class(
  "SMA2M",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = "simple",

initialize = function(data, window_size1, window_size2, ma_type = "simple") {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
    },

generate_signals = function(ma_type = self$ma_type) {
      
      ma_func <- ifelse(ma_type == "simple", rollmean, EMA)
      self$data <- mutate(self$data, 
                          #ma1 = rollmean(value, k = self$window_size1, align = "right", fill = NA),
                          ma1 = rollmean(Close, k = self$window_size1, align = "right", fill = NA),
                          #ma2 = rollmean(value, k = self$window_size2, align = "right", fill = NA),
                          ma2 = rollmean(Close, k = self$window_size2, align = "right", fill = NA),
                          signal1 = ifelse(ma1 > ma2, 1, ifelse(ma1 < ma2, -1, 0))) %>%
                          # position = lag(signal1, default = 0)) %>% # position is defined at the end after additional condition implementation 
                            na.omit
      
      # Initialize last_long_value and last_short_value (dynamic trailing threshold)
      last_long_value <- NA
      last_short_value <- NA

      # Create empty columns in the data frame to store last long and short values
      self$data$last_long_value <- NA
      self$data$last_short_value <- NA

      # Loop through each row of the data frame
      for (i in 1:nrow(self$data)) {
          # Check if current row has signal1 == 1
          if (self$data$signal1[i] == 1) {
            # Find the index of the first previous occurrence of 1
            first_previous_index <- NA
            if (any(self$data$signal1[1:(i-1)] == 1)) {
              first_previous_index <- max(which(self$data$signal1[1:(i-1)] == 1))
            }
            # If a previous occurrence of 1 is found, update last_long_value
            if (!is.na(first_previous_index)) {
              last_long_value <- self$data$ma1[first_previous_index]
            }
            # Assign last long value to the corresponding row in the data frame
            self$data$last_long_value[i] <- last_long_value
          }
          # Check if current row has signal1 == -1
          if (self$data$signal1[i] == -1) {
            # Find the index of the first previous occurrence of -1
            first_previous_index <- NA
            if (any(self$data$signal1[1:(i-1)] == -1)) {
              first_previous_index <- max(which(self$data$signal1[1:(i-1)] == -1))
            }
            # If a previous occurrence of -1 is found, update last_short_value
            if (!is.na(first_previous_index)) {
              #last_short_value <- self$data$ma2[first_previous_index]
              last_short_value <- self$data$ma1[first_previous_index]
            }
            # Assign last short value to the corresponding row in the data frame
            self$data$last_short_value[i] <- last_short_value
          }

          # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
          self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
          self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)
      }

      # Signals:
      self$data$signal <- NA
      self$data$signal <- ifelse((self$data$ma1 > self$data$last_long_value) & (self$data$ma1 > self$data$ma2), 1,
                                 ifelse((self$data$ma1 < self$data$last_short_value) & (self$data$ma1 < self$data$ma2), -1, 0))
      #self$data <- self$data 
      # %>% select(-c(last_long_value, last_short_value))
    # Replacing 0s by previous signal value:
      self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
      self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)
      self$data$position <- lag(self$data$signal, default = 0)
    }
  )
)

sma2m <- SMA2M$new(ts, window_size1 = 10, window_size2 = 200, ma_type = "exp")
sma2m$estimate_performance()
sma2m$plot_equity_lines("SMA2M")

# Define Relative Strength Index class
RSI <- R6Class(
  "RSI",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    threshold_oversold = 30,
    threshold_overbought = 70,

    initialize = function(data, window_size, threshold_oversold = 30, threshold_overbought = 70) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$threshold_oversold <- threshold_oversold
      self$threshold_overbought <- threshold_overbought
    },

  generate_signals = function() {
        self$data <- mutate(self$data,
                            #avg_gain = rollmean(ifelse(value > 0, value, 0), k = self$window_size, align = "right", fill = NA),
                            avg_gain = rollmean(ifelse(Close > 0, Close, 0), k = self$window_size, align = "right", fill = NA),
                            #avg_loss = rollmean(ifelse(value < 0, abs(value), 0), k = self$window_size, align = "right", fill = NA),
                            avg_loss = rollmean(ifelse(Close < 0, abs(Close), 0), k = self$window_size, align = "right", fill = NA),
                            rs = avg_gain / avg_loss,
                            rsi = 100 - (100 / (1 + rs)),
                            signal = ifelse(rsi < self$threshold_oversold, 1, ifelse(rsi > self$threshold_overbought, -1, 0)),
                            position = lag(signal, default = 0)) %>%
                    na.omit
        },

    plot_avg_gain_loss_with_equity_lines = function() {
        ggplot(self$data, aes(x = Date)) + # include the equity lines plot from the parent Strategy class
            geom_line(aes(y = avg_gain, color = "Average Gain")) +
            geom_line(aes(y = avg_loss, color = "Average Loss")) +
            #geom_line(aes(y = equity_line, color = "Equity Line")) +  # Include equity line
            geom_line(aes(y = equity_line, color = "Equity Line")) +  # Include equity line
            labs(title = "Average Gain, Average Loss, and Equity Line",
                x = "Date",
                y = "Value") +
            scale_color_manual(values = c("Average Gain" = "blue", "Average Loss" = "red", "Equity Line" = "green")) +
            theme_minimal()
        }
  )
)

# Create an instance of RSI class
rsi <- RSI$new(ts, window_size = 14, threshold_oversold = 40, threshold_overbought = 60)
rsi$estimate_performance()
rsi$plot_equity_lines("RSI")

# Define Bollinger Bands Breakout class
BollingerBreakout <- R6Class(
  "BollingerBreakout",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    sd_multiplier = 2,  # Multiplier for standard deviation

    initialize = function(data, window_size, sd_multiplier = 2) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$sd_multiplier <- sd_multiplier
    },
generate_signals = function() {
      self$data <- mutate(self$data, 
                          #ma = rollmean(value, k = self$window_size, align = "right", fill = NA),
                          ma = rollmean(Close, k = self$window_size, align = "right", fill = NA),
                          #sd = rollapply(value, width = self$window_size, sd, align = "right", fill = NA),
                          sd = rollapply(Close, width = self$window_size, sd, align = "right", fill = NA),
                          upper_band = ma + self$sd_multiplier * sd,
                          lower_band = ma - self$sd_multiplier * sd,
                          signal = case_when(
                            value > upper_band ~ 1,
                            value < lower_band ~ -1,
                            TRUE ~ 0
                          ),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
    }
  )
)

# Create an instance of BollingerBreakout class
bol_br <- BollingerBreakout$new(ts, window_size = 20, sd_multiplier = 2)
bol_br$estimate_performance()
bol_br$plot_equity_lines("BB")

# Define Volatility Mean Reversion class
VolatilityMeanReversion <- R6Class(
  "VolatilityMeanReversion",
  inherit = Strategy,
  public = list(
    window_size = NULL,

    initialize = function(data, window_size) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
    },

generate_signals = function() {
      # Estimate historical volatility
      #hist_vol <- rollapply(self$data$value, width = self$window_size, sd, align = "right", fill = NA)
      hist_vol <- rollapply(self$data$Close, width = self$window_size, sd, align = "right", fill = NA)
      
      # Calculate rolling mean of historical volatility
      mean_vol <- rollmean(hist_vol, k = self$window_size, align = "right", fill = NA)
      
      # Generate signals
      self$data <- mutate(self$data,
                          historical_volatility = hist_vol,
                          mean_volatility = mean_vol,
                          signal = ifelse(hist_vol > mean_vol, -1, 1),
                          position = lag(signal, default = 0)) %>%
                        na.omit
    }
  )
)

# Create an instance of VolatilityMeanReversion class
vol_mean_rev <- VolatilityMeanReversion$new(ts, window_size = 20)
vol_mean_rev$estimate_performance()
vol_mean_rev$plot_equity_lines("VolatilityMeanReversion")

# Define Random strategy class (generate random signals)
Random <- R6Class(
  "Random",
  inherit = Strategy,
  public = list(
    prob = NULL,
    initialize = function(data, prob = 0.5) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$prob = prob
    },
    generate_signals = function(prob = self$prob) {
      self$data <- mutate(self$data, 
                          signal = sample(c(-1,1), size = nrow(self$data), prob),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
    }
  )
)

# Instances of Random strategy
rand <- Random$new(ts, prob = 0.5)
rand$estimate_performance()
rand$plot_equity_lines("Random")




