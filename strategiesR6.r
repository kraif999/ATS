# Copyright (c) June 2024 Oleh Bilyk
# Price-time based strategies

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
    # UseMethod("mutate") : no applicable method for 'mutate' applied to an object of class "c('xts', 'zoo')"
    #ts$value <-  log(ts[,paste0(self$symbol, ".Close")]) - log(lag(ts[,paste0(self$symbol, ".Close")]))
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

######################################################
# Specify trading strategy parameters
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()
symbol <- "BZ=F" # oil
capital <- 50000 # units of initial capital invested
leverage <- 1 # financial leverage used to calculate number of positions in estimate_performance in Strategy class
# Also, it is assumed 100% portfolio investment (number of positions =  capital / price per unit).
######################################################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()
data_fetcher$plot_close_or_rets(type = "close")
data_fetcher$plot_close_or_rets(type = "rets")
data_fetcher$plot_close_or_rets(type = "rets_hist")
data_fetcher$compute_NA_close_price_ratio()

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
          mutate(Date = as.Date(as.character(rownames(.)))) %>%
            select(Date, everything()) %>%
                na.omit() %>% 
                    as_tibble()
    return(ts_df)

    # ts_df <- data.frame(Date = index(ts), coredata(ts)) %>%
    #   rename_with(~ sub(".*\\.", "", .), everything()) %>%
    #     na.omit() %>%
    #       as_tibble()
    # return(ts_df)
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
  self$data$nopActive[1] <- floor(capital / (self$data$Close[1] / leverage))
  self$data$eqlPassive[1] <- capital
  self$data$nopPassive[1] <- floor(capital / (self$data$Close[1] / leverage))   

  # Check if "L" column (probability indicator) exists
  has_L <- "nop_sizing" %in% names(self$data)
  has_Vol <- "vol_nop_sizing" %in% names(self$data)

  for (i in 2:nrow(self$data)) {
    # Active
    pnlActive <- self$data$pnlActive[i]
    prev_nop_Active <- floor(self$data$nopActive[i - 1])
    current_nop_Active <- floor((self$data$eqlActive[i - 1]) / (self$data$Close[i] / leverage))
    self$data$eqlActive[i] <- self$data$eqlActive[i - 1] + prev_nop_Active * pnlActive
    
    # Calculate nopActive based on the presence of the "L" column
    if (has_L) {
      self$data$nopActive[i] <- ifelse(
        self$data$L[i], 
        current_nop_Active * self$data$nop_sizing[i], 
        current_nop_Active
      )
    } else if (has_Vol) {
      self$data$nopActive[i] <- ifelse(
        #self$data$L[i], 
        self$data$vol_nop_sizing[i], 
        current_nop_Active * self$data$vol_nop_sizing[i], 
        current_nop_Active
      )
    } else {
      self$data$nopActive[i] <- current_nop_Active
    }
    
    # Passive
    pnlPassive <- self$data$pnlPassive[i]
    prev_nop_Passive <- floor(self$data$nopPassive[i - 1])
    current_nop_Passive <- floor((self$data$eqlPassive[i - 1]) / (self$data$Close[i] / leverage))
    self$data$eqlPassive[i] <- self$data$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
    self$data$nopPassive[i] <- current_nop_Passive
  }

  self$data <- self$data %>%
    mutate(r_eqlActive = quantmod::Delt(eqlActive),
            r_eqlPassive = quantmod::Delt(eqlPassive))
            #%>% na.omit()

  # Performance metrics for active strategy
  aR_active <- round(as.numeric(Return.annualized(as.numeric(self$data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(self$data$r_eqlActive), scale = 252) * 100), 3)
  IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
  MD_active <- round(as.numeric(maxDrawdown(as.numeric(self$data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
  #trades_active <- sum(c(1, ifelse(self$data$signal[-1] * self$data$signal[-length(self$data$signal)] < 0, 1, 0)))
  trades_active <- sum(diff(self$data$signal) != 0)
  buy_active <- sum(self$data$signal == 1)
  short_active <- sum(self$data$signal == -1)

  # Performance metrics for passive strategy
  aR_passive <- round(as.numeric(Return.annualized(as.numeric(self$data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(self$data$r_eqlPassive), scale = 252) * 100), 3)
  IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
  MD_passive <- round(as.numeric(maxDrawdown(as.numeric(self$data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100),3)
  trades_passive <- 1
  buy_passive <- 1
  short_passive <- 0

  # Calculate success rates
  buy_success_rate_active <- sum(self$data$position > 0 & self$data$value > 0) / nrow(self$data)
  buy_success_rate_passive <- sum(self$data$value > 0) / nrow(self$data)

  short_success_rate_active <- sum(self$data$position < 0 & self$data$value < 0) / nrow(self$data)
  short_success_rate_passive <- 0

  combined_rate_active <- (sum(self$data$position > 0 & self$data$value > 0) + sum(self$data$position < 0 & self$data$value < 0)) / nrow(self$data)
  combined_rate_passive <- (sum(self$data$value > 0)) / nrow(self$data)

  # Unique months
  unique_months <- length(unique(format(self$data$Date, "%Y-%m")))

  # Create performance dataframe
  df <- data.frame(
    Strategy = c("Active", "Passive"),
    aR = c(aR_active, aR_passive),
    aSD = c(aSD_active, aSD_passive),
    IR = c(IR_active, IR_passive),
    MD = c(MD_active, MD_passive),
    trades = c(trades_active, trades_passive),
    avg_no_monthly_trades = round(c(trades_active / unique_months, 0), 2),
    buys = c(buy_active, buy_passive),
    sells = c(short_active, short_passive),
    Buy_Success_Rate = round(c(buy_success_rate_active, buy_success_rate_passive), 4),
    Short_Success_Rate = round(c(short_success_rate_active, short_success_rate_passive), 4),
    Combined_Success_Rate = round(c(combined_rate_active, combined_rate_passive), 4),
    PortfolioEndValue = round(c(self$data[nrow(self$data),]$eqlActive, self$data[nrow(self$data),]$eqlPassive), 0)
  )

  # Print the performance data frame and success rate data frame
  # print(df)
  # return(self$data)

  # print(self$data)
  return(df)
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
plot_equity_lines = function(strategy_name, signal_flag = FALSE) {
  # Line size
  active_line_size <- ifelse(signal_flag, 1, 0.8)
  passive_line_size <- ifelse(signal_flag, 1, 0.8)
  
  # Plot equity lines
  p <- ggplot(self$data, aes(x = Date)) +
    labs(title = paste0("Equity Lines for Active ", "(", as.character(strategy_name), ")", " and Passive (buy-and-hold) strategies"),
         x = "Date",
         y = "Equity line") +
    theme_minimal()

  # Add signals if signal_flag is TRUE
  if (signal_flag) {
    p <- p +
      geom_vline(data = self$data[self$data$signal == -1, ], aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red", alpha = 0.5) +
      geom_vline(data = self$data[self$data$signal == 1, ], aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "green", alpha = 0.5) +
      scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "green"))
  }

  # Add equity lines
  p <- p +
    geom_line(aes(y = eqlActive, color = "Active Strategy"), size = active_line_size) +
    geom_line(aes(y = eqlPassive, color = "Buy and Hold Strategy"), size = passive_line_size) +
    scale_color_manual(values = c("Active Strategy" = "red", "Buy and Hold Strategy" = "darkgreen")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")

  # Print or return the plot
  if (signal_flag) {
    print(p)
  } else {
    return(p)
  }
},

# Plot Japanese candles for the period of latest ndays
plot_candles = function(ndays) {
  self$data <- tail(self$data, ndays)
  fig <- self$data %>% plot_ly(x = ~Date, type= "candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) 
  fig <- fig %>% layout(title = paste0("Candlestick Chart for last ", ndays, " days"))
  fig
},

# Estimate Average True Range (ATR)
estimate_average_true_range = function(ts, n = 14) { # 14 days by default
  atr <- ATR(HLC(ts), n)
  atr_data <- data.frame(Date = index(atr), coredata(atr))
  ts_data <- data.frame(Date = index(ts), coredata(ts))
  combined_data <- merge(ts_data, atr_data, by = "Date") %>%
    #mutate(tr_reserve = 1 - tr/atr * 100)
    mutate(tr_reserve = tr/atr * 100)
  return(combined_data)
}

# Money management rules (risk per trade, position sizing, diversification, profit taking, risk tolerance level) are to be added

  )
)

# Define class for Strategy based on GARCH model
GARCHbasedStrategy <- R6Class(
  "GARCHbasedStrategy",
    inherit = Strategy,
    public = list(
    specification = NULL, 
    n_start = NULL, 
    refit_every = NULL, 
    refit_window = NULL, 
    distribution_model = NULL, 
    realized_vol = NULL, 
    cluster = NULL,

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
      self$cluster <- cluster
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
    garman <- as.xts(na.omit(TTR::volatility(ohlc, calc = "garman"))) / sqrt(252),
    close <- as.xts(na.omit(TTR::volatility(ohlc[,4], calc = "close"))) / sqrt(252),
    parkinson <- as.xts(na.omit(TTR::volatility(ohlc, calc = "parkinson"))) / sqrt(252),
    rogers.satchell <- as.xts(na.omit(TTR::volatility(ohlc, calc = "rogers.satchell"))) / sqrt(252),
    garman_modified <- as.xts(na.omit(TTR::volatility(ohlc, calc = "gk.yz"))) / sqrt(252),
    yang.zhang <- as.xts(na.omit(TTR::volatility(ohlc, calc = "yang.zhang"))) / sqrt(252)
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
        cluster = self$cluster,
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
        cluster = self$cluster,
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
},

run_backtest = function(symbols, specifications = NULL, n_starts = NULL, refits_every  = NULL, refit_windows = NULL, distribution_models = NULL, realized_vols = NULL, output_df = FALSE) {
  
  # Create an empty list to store results
  results <- list()
  
  # Loop through the combinations of specifications and parameters
  for (symbol in symbols) {
    for (spec in specifications) {
      for (n_start in n_starts) {
        for (refit in refits_every) {
          for (window in refit_windows) {
            for (dist_model in distribution_models) {
              for (realized_vol in realized_vols) {

            tryCatch({

            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
              
              # Create an instance of GARCHbasedStrategy
              garch_instance <- GARCHbasedStrategy$new(
                data = data,
                specification = spec,
                n_start = n_start,
                refit_every = refit,
                refit_window = window,
                distribution_model = dist_model,
                realized_vol = realized_vol,
                cluster = self$cluster
              )
              
        
              # Store the results
              results[[paste(symbol, spec, n_start, refit, window, dist_model, realized_vol, sep = "_")]] <- list(
                Symbol = symbol,
                Class = meta$assets[[symbol]]$class,
                Methodology = paste(spec, ":", n_start, refit, window, dist_model, realized_vol),
                Specification = spec,
                N_Start = n_start,
                Refit_Every = refit,
                Refit_Window = window,
                Distribution_Model = dist_model,
                Realized_Vol = realized_vol,
                Performance = garch_instance$estimate_performance() # Assuming you have an estimate_performance method
              )
            print(paste0("Results for ", "GARCHbased ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
            "spec:", spec, ",", "window_size:", n_start, ",", "refit_freq:", refit, ",", "refit_window:", window, ",",
              "dist_err:", dist_model, ",", "RV:", realized_vol, ")"))
              
              }, error = function(e) {
                cat("Error:", e$message, "occurred for", paste(symbol, n_start, refit, window, dist_model, realized_vol, sep = "_"), "\n")
                }
                )
              }
            }
          }
        }
      }
    }
  }
  
      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class =  item$Class,
          Methodology = item$Methodology,
          Specification = item$Specification,
          N_Start = item$N_Start,
          Refit_Every = item$Refit_Every,
          Refit_Window = item$Refit_Window,
          Distribution_Model = item$Distribution_Model,
          Realized_Vol = item$Realized_Vol,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
    }   else {
          return(results)
      }
}
  )
)

# Instances of GARCH strategy
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
garch_strategy <- GARCHbasedStrategy$new(
  data = ts,
  specification = "sGARCH",
  n_start = 126,
  refit_every = 21,
  refit_window = "moving",
  distribution_model = "snorm",
  realized_vol = "close",
  cluster = cl)

garch_strategy$estimate_performance()
garch_strategy$plot_equity_lines(paste(symbol, ":", "sGARCH-126-21-moving-snorm-close"), signal_flag = TRUE)
# ggsave(filename = "garch.png", plot = last_plot(), dpi = 300, type = "cairo", bg = "white")

# Instances of GARCH based strategy (run backtesting)
leverage <- 1
res_garch <- garch_strategy$run_backtest(
  symbols = assets,
  specifications = c("sGARCH", "eGARCH"),
  n_starts = c(126, 252),
  refits_every = 21,
  refit_windows = c("moving", "expanding"),
  distribution_models = "snorm",
  realized_vols = "close",
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

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
                          #signal = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0)),
                          signal = ifelse(Close > lag(ma), 1, ifelse(Close < lag(ma), -1, 0)),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
},

run_backtest = function(symbols, window_sizes, ma_types, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      # Loop through symbols, window sizes, and MA types to create instances and estimate performance
      for (symbol in symbols) {
        for (window_size in window_sizes) {
          for (ma_type in ma_types) {
            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
            
            # Create an instance of SMA1 strategy
            sma_instance <- SMA1$new(data, window_size = window_size, ma_type = ma_type)
                        
            # Store the results
            results[[paste(symbol, window_size, ma_type, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("SMA1:", window_size, ma_type),
              Window_Size = window_size,
              MA_Type = ma_type,
              Performance = sma_instance$estimate_performance()
            )

            print(paste0("Results are for ", "SMA1 ", "(", "symbol: ", symbol, ",", "class: ", meta$assets[[symbol]]$class, ",", "window_size: ", window_size, ",", "ma_type: ", ma_type, ")"))
            
          }
        }
      }

      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class = item$Class,
          Methodology = item$Methodology,
          Window_Size = item$Window_Size,
          MA_Type = item$MA_Type,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
    }   else {
          return(results)
      }
}

  )
)

# Instances of SMA1 strategy
sma1 <- SMA1$new(ts, window_size = 10, ma_type = 'EMA')
sma1$estimate_performance()
sma1$plot_equity_lines("SMA1", signal_flag = TRUE) # strategy name, not to be confused with moving average type
#a <- sma1$data %>% select(Date, Close, signal, position, pnlActive, value, nopActive, eqlActive) # too frequent signal generation (add dynamic threshold in signal generation)

# Instances of SMA1 strategy (run backtesting)
leverage <- 1
res_sma1 <- sma1$run_backtest(
  symbols = assets,
  window_sizes = seq(10, 100, by = 10), 
  ma_type = c("EMA", "SMA"),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define SMA2 (trend following strategy) class
SMA2 <- R6Class(
  "SMA2",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = NULL,

initialize = function(data, window_size1, window_size2, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$ma_type <- ma_type
},

generate_signals = function() {
      ma_func <- get(self$ma_type)
      # Calculate first and second moving averages
      self$data <- mutate(self$data, 
                          ma1 = ma_func(Close, self$window_size1, align = "right", fill = NA),
                          ma2 = ma_func(Close, self$window_size2, align = "right", fill = NA),
                          signal = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit

},

run_backtest = function(symbols, window_sizes1, window_sizes2, ma_types, from_date, to_date, output_df = TRUE) {
    # Create an empty list to store results
    results <- list()

    # Loop through symbols, window sizes, and MA types to create instances and estimate performance
    for (symbol in symbols) {
      for (window_size1 in window_sizes1) {
        for (window_size2 in window_sizes2) {
          for (ma_type in ma_types) {
            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
            
            # Create an instance of SMA2 strategy
            sma_instance <- SMA2$new(data, window_size1 = window_size1, window_size2 = window_size2, ma_type = ma_type)
                        
            # Store the results
            results[[paste(symbol, window_size1, window_size2, ma_type, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("SMA2:", window_size1, window_size2, ma_type),
              Window_Size1 = window_size1,
              Window_Size2 = window_size2,
              MA_Type = ma_type,
              Performance = sma_instance$estimate_performance()
            )

            print(paste0("Results for ", "SMA2 ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
            "window_size1:", window_size1, ",", "window_size2:", window_size2, ",", "ma_type:", ma_type, ")"))

          }
        }
      }
    }

    # Convert results to a data frame
    results_df <- map_dfr(names(results), ~{
      item <- results[[.x]]
      data_frame(
        Symbol = item$Symbol,
        Class = item$Class,
        Methodology = item$Methodology,
        Window_Size1 = item$Window_Size1,
        Window_Size2 = item$Window_Size2,
        MA_Type = item$MA_Type,
        Strategy = item$Performance$Strategy,
        aR = item$Performance$aR,
        aSD = item$Performance$aSD,
        IR = item$Performance$IR,
        MD = item$Performance$MD,
        trades = item$Performance$trades,
        avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
        buys = item$Performance$buys,
        sells = item$Performance$sells,
        Buy_Success_Rate = item$Performance$Buy_Success_Rate,
        Short_Success_Rate = item$Performance$Short_Success_Rate,
        Combined_Success_Rate = item$Performance$Combined_Success_Rate,
        PortfolioValue = item$Performance$PortfolioEndValue
      )
    })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}

  )
)

# Instances of SMA2 class
sma2 <- SMA2$new(ts, window_size1 = 10, window_size2 = 100,  ma_type = "EMA")
sma2$estimate_performance()
sma2$plot_equity_lines("SMA2", signal_flag = TRUE)

# Instances of SMA2 strategy (run backtesting)
leverage <- 1
res_sma2 <- sma2$run_backtest(
  symbols = assets, 
  window_sizes1 = seq(10, 50, by = 10), 
  window_sizes2 = seq(55, 115, by = 10),
  ma_type = c("EMA", "SMA"),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define SMA1 class with modified signals
SMA1M <- R6Class(
  "SMA1M",
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

# Generate modified signals
generate_signals = function() {
    ma_func <- get(self$ma_type)
    self$data <- mutate(
    self$data, 
    ma = ma_func(Close, self$window_size, align = "right", fill = NA),
    #signal1 = ifelse(Close > ma, 1, ifelse(Close < ma, -1, 0))) %>%
    signal1 = ifelse(Close > lag(ma), 1, ifelse(Close < lag(ma), -1, 0))) %>%
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

    # self$data$signal <- ifelse((self$data$Close > self$data$last_long_value) & (self$data$Close > self$data$ma), 1,
    #                     ifelse((self$data$Close < self$data$last_short_value) & (self$data$Close < self$data$ma), -1, 0))

    self$data$signal <- ifelse((self$data$Close > self$data$last_long_value) & (self$data$Close > lag(self$data$ma)), 1,
                    ifelse((self$data$Close < self$data$last_short_value) & (self$data$Close < lag(self$data$ma)), -1, 0))

    # Replacing 0s by previous signal value:
    self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
    self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)

    self$data$position <- lag(self$data$signal, default = 0)

    #self$data <- self$data 
    # %>% select(-c(signal1, last_long_value, last_short_value))
},

run_backtest = function(symbols, window_sizes, ma_types, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      # Loop through symbols, window sizes, and MA types to create instances and estimate performance
      for (symbol in symbols) {
        for (window_size in window_sizes) {
          for (ma_type in ma_types) {
            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
            
            # Create an instance of SMA1 strategy
            sma_instance <- SMA1M$new(data, window_size = window_size, ma_type = ma_type)
                        
            # Store the results
            results[[paste(symbol, window_size, ma_type, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("SMA1M:", window_size, ma_type),
              Window_Size = window_size,
              MA_Type = ma_type,
              Performance = sma_instance$estimate_performance()
            )
            print(paste0("Results for ", "SMA1M ", "(", "symbol: ", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
              "window_size:", window_size, ",", "ma_type:", ma_type, ")"))
          }
        }
      }

      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class = item$Class,
          Methodology = item$Methodology,
          Window_Size = item$Window_Size,
          MA_Type = item$MA_Type,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
    }   else {
          return(results)
      }
}

    )
)

# Instances of SMA1M class
sma1m <- SMA1M$new(ts, window_size = 50, ma_type = 'EMA')
sma1m$estimate_performance()
sma1m$plot_equity_lines(paste(symbol, ":", "SMA1M, window_size = 50, ma_type = EMA"))
View(sma1m$data %>% select(Date, Close, signal, pnlActive, pnlPassive, eqlActive, eqlPassive))

# Instances of SMA1M strategy (run backtesting)
leverage <- 1
res_sma1m <- sma1m$run_backtest(
  symbols = assets, 
  window_sizes = seq(10, 100, by = 10), 
  ma_type = c("EMA", "SMA"),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# SMA2 (modified by dynamic trailing stop)
SMA2M <- R6Class(
  "SMA2M",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    ma_type = NULL,

initialize = function(data, window_size1, window_size2, ma_type) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$ma_type <- ma_type
},

generate_signals = function() {
      ma_func <- get(self$ma_type)
      self$data <- mutate(self$data, 
                          ma1 = ma_func(Close, self$window_size1, align = "right", fill = NA),
                          ma2 = ma_func(Close, self$window_size2, align = "right", fill = NA),
                          signal1 = ifelse(ma1 > lag(ma2), 1, ifelse(ma1 < lag(ma2), -1, 0))) %>%
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
      
      # self$data$signal <- NA
      # self$data$signal <- ifelse((self$data$ma1 > self$data$last_long_value) & (self$data$ma1 > self$data$ma2), 1,
      #                            ifelse((self$data$ma1 < self$data$last_short_value) & (self$data$ma1 < self$data$ma2), -1, 0))

      self$data$signal <- NA
      self$data$signal <- ifelse((lag(self$data$ma1) > self$data$last_long_value) & (lag(self$data$ma1) > lag(self$data$ma2)), 1,
                                 ifelse((lag(self$data$ma1) < self$data$last_short_value) & (lag(self$data$ma1) < lag(self$data$ma2)), -1, 0))
      #self$data <- self$data 
      # %>% select(-c(last_long_value, last_short_value))
    # Replacing 0s by previous signal value:
      self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
      self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)
      self$data$position <- lag(self$data$signal, default = 0)
},

run_backtest = function(symbols, window_sizes1, window_sizes2, ma_types, from_date, to_date, output_df = TRUE) {
    # Create an empty list to store results
    results <- list()

    # Loop through symbols, window sizes, and MA types to create instances and estimate performance
    for (symbol in symbols) {
      for (window_size1 in window_sizes1) {
        for (window_size2 in window_sizes2) {
          for (ma_type in ma_types) {
            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
            
            # Create an instance of SMA2 strategy
            sma_instance <- SMA2M$new(data, window_size1 = window_size1, window_size2 = window_size2, ma_type = ma_type)
                        
            # Store the results
            results[[paste(symbol, window_size1, window_size2, ma_type, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("SMA2M:", window_size1, window_size2, ma_type),
              Window_Size1 = window_size1,
              Window_Size2 = window_size2,
              MA_Type = ma_type,
              Performance = sma_instance$estimate_performance()
            )
              print(paste0("Results for ", "SMA2M ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
            "window_size1:", window_size1, ",", "window_size2:", window_size2, ",", "ma_type:", ma_type, ")"))

          }
        }
      }
    }

    # Convert results to a data frame
    results_df <- map_dfr(names(results), ~{
      item <- results[[.x]]
      data_frame(
        Symbol = item$Symbol,
        Class = item$Class,
        Methodology = item$Methodology,
        Window_Size1 = item$Window_Size1,
        Window_Size2 = item$Window_Size2,
        MA_Type = item$MA_Type,
        Strategy = item$Performance$Strategy,
        aR = item$Performance$aR,
        aSD = item$Performance$aSD,
        IR = item$Performance$IR,
        MD = item$Performance$MD,
        trades = item$Performance$trades,
        avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
        buys = item$Performance$buys,
        sells = item$Performance$sells,
        Buy_Success_Rate = item$Performance$Buy_Success_Rate,
        Short_Success_Rate = item$Performance$Short_Success_Rate,
        Combined_Success_Rate = item$Performance$Combined_Success_Rate,
        PortfolioValue = item$Performance$PortfolioEndValue
      )
    })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}
  )
)

# Instances of SMA2M class
sma2m <- SMA2M$new(ts, window_size1 = 10, window_size2 = 200, ma_type = "SMA")
sma2m$estimate_performance()
sma2m$plot_equity_lines("SMA2M")

# Instances of SMA2M strategy (run backtesting)
leverage <- 1
res_sma2m <- sma2m$run_backtest(
  symbols = assets, 
  window_sizes1 = seq(10, 50, by = 10), 
  window_sizes2 = seq(55, 115, by = 10),
  ma_type = c("SMA", "EMA"),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define MACD class
MACD <- R6Class(
"MACD",
inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,
    sline = NULL,

initialize = function(data, window_size1 = 12, window_size2 = 26, sline = 9) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size1 <- window_size1
      self$window_size2 <- window_size2
      self$sline <- sline
},

generate_signals = function() {
      self$data <- mutate(self$data,
                          ma1 = EMA(Close, self$window_size1, align = "right", fill = NA),
                          ma2 = EMA(Close, self$window_size2, align = "right", fill = NA),
                          macd_line = ma1 - ma2,
                          signal_line = EMA(macd_line, self$sline),
                          signal = ifelse(macd_line > signal_line, 1, ifelse(macd_line < signal_line, -1, 0)),
                          position = lag(signal, default = 0)) %>%
                            na.omit()
},

run_backtest = function(symbols, window_sizes1, window_sizes2, slines, from_date, to_date, output_df = TRUE) {
    # Create an empty list to store results
    results <- list()

    # Loop through symbols, window sizes to create instances and estimate performance
    for (symbol in symbols) {
      for (window_size1 in window_sizes1) {
        for (window_size2 in window_sizes2) {
          for (sline in slines) {

            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
            
            # Create an instance of MACD strategy
            macd_instance <- MACD$new(data, window_size1 = window_size1, window_size2 = window_size2, sline = sline)
            
            # Generate signals
            # macd_instance$generate_signals()
                        
            # Store the results
            results[[paste(symbol, window_size1, window_size2, sline, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("MACD:", window_size1, window_size2, sline),
              Window_Size1 = window_size1,
              Window_Size2 = window_size2,
              Sline = sline,
              Performance = macd_instance$estimate_performance()
            )

              print(paste0("Results for ", "MACD ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
            "window_size1:", window_size1, ",", "window_size2:", window_size2, ",", "ma_type:", sline, ")"))

          }
        }
      }
    }
    # Convert results to a data frame
    results_df <- map_dfr(names(results), ~{
      item <- results[[.x]]
      data_frame(
        Symbol = item$Symbol,
        Class = item$Class,
        Methodology = item$Methodology,
        Window_Size1 = item$Window_Size1,
        Window_Size2 = item$Window_Size2,
        Sline = item$Sline,
        Strategy = item$Performance$Strategy,
        aR = item$Performance$aR,
        aSD = item$Performance$aSD,
        IR = item$Performance$IR,
        MD = item$Performance$MD,
        trades = item$Performance$trades,
        avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
        buys = item$Performance$buys,
        sells = item$Performance$sells,
        Buy_Success_Rate = item$Performance$Buy_Success_Rate,
        Short_Success_Rate = item$Performance$Short_Success_Rate,
        Combined_Success_Rate = item$Performance$Combined_Success_Rate,
        PortfolioValue = item$Performance$PortfolioEndValue
      )
    })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}

  )
)

# Instance of MACD class
macd <- MACD$new(ts, window_size1 = 12, window_size2 = 26, sline = 9)
macd$estimate_performance()
macd$plot_equity_lines("MACD", signal_flag = FALSE)

# Instances of MACD strategy (run backtesting)
leverage <- 1
res_macd <- macd$run_backtest(
  symbols = assets, 
  window_sizes1 = seq(9, 13, by = 1), 
  window_sizes2 = seq(25, 30, by = 1),
  sline = seq(7, 11, by = 1),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Donchian Channel (DC) class (also, it could be used with RSI and MACD)
DonchianChannel <- R6Class(
  "DonchianChannel",
  inherit = Strategy,
  public = list(
    window_size = NULL,

initialize = function(data, window_size) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$window_size = window_size
},

generate_signals = function() {
  self$data <- mutate(self$data,
                      upper_channel = rollapply(self$data$High, self$window_size, max, align = "right", fill = NA),
                      lower_channel = rollapply(self$data$Low, self$window_size, min, align = "right", fill = NA),
                      mid = (upper_channel + lower_channel) / 2, # optional
                      signal1 = ifelse(Close > lag(upper_channel), 1, ifelse(Close < lag(lower_channel), -1, 0)),
                      signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
                          position = lag(signal, default = 0)) %>% 
                        na.omit()
},

plot_channels = function(name) {
  ggplot(self$data, aes(x = Date)) +
    geom_line(aes(y = Close, color = "black"), size = 1) +
    geom_line(aes(y = upper_channel, color = "green"), linetype = "longdash", size = 1) +
    geom_line(aes(y = lower_channel, color = "red"), linetype = "longdash", size = 1) +
    geom_line(aes(y = mid, color = "yellow"), linetype = "longdash", size = 1) +
    labs(
      title = paste0("Donchian Channel for ", name),
      x = "Date",
      y = "Price"
    ) +
    scale_y_continuous(breaks = seq(min(self$data$lower_channel, na.rm = TRUE), 
                                  max(self$data$upper_channel, na.rm = TRUE), 
                                  by = 20)) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    
    theme_minimal()
},

run_backtest = function(symbols, window_sizes, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      # Loop through symbols, window sizes, and MA types to create instances and estimate performance
      for (symbol in symbols) {
        for (window_size in window_sizes) {

            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
            
            # Create an instance of SMA1 strategy
            dc <- DonchianChannel$new(data, window_size = window_size)
                        
            # Store the results
            results[[paste(symbol, window_size, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("DonchianChannel:", window_size),
              Window_Size = window_size,
              Performance = dc$estimate_performance()
            )

            print(paste0("Results for ", "DonchianChannel ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
            "window_size:", window_size, ")"))
        }
      }

      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class = item$Class,
          Methodology = item$Methodology,
          Window_Size = item$Window_Size,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
    }   else {
          return(results)
      }
}
  )
)

# Instances of DonchianChannel class
dc <- DonchianChannel$new(ts, window_size = 20)
dc$estimate_performance()
dc$plot_channels("window_size = 20")
dc$plot_equity_lines("DC", signal_flag = TRUE)

# Instances of DonchianChannel strategy (run_backtest)
leverage <- 1
res_dc <- dc$run_backtest(
  symbols = assets, 
  window_sizes = seq(5, 25, by = 1), 
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Relative Strength Index class
RSI <- R6Class(
  "RSI",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    threshold_oversold = NULL,
    threshold_overbought = NULL,

initialize = function(data, window_size, threshold_oversold = 30, threshold_overbought = 70) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$threshold_oversold <- threshold_oversold
      self$threshold_overbought <- threshold_overbought
},

generate_signals = function() {
  self$data <- mutate(self$data,
                      avg_gain = rollmean(ifelse(Close > 0, Close, 0), k = self$window_size, align = "right", fill = NA),
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
},

run_backtest = function(symbols, window_sizes, thresholds_overbought, thresholds_oversold, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()
  
  # Loop through symbols, window sizes to create instances and estimate performance
  for (symbol in symbols) {
    for(window_size in window_sizes) {
      for(threshold_oversold in thresholds_oversold) {
        for (threshold_overbought in thresholds_overbought) {

          # Fetch data using DataFetcher for the current symbol and date range
          data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
          data <- data_fetcher$download_xts_data()

          # Create an instance of RSI class
          rsi_instance <- RSI$new(data,  window_size = window_size, threshold_oversold = threshold_oversold, threshold_overbought = threshold_overbought)

          # Generate signals
          # rsi_instance$generate_signals()
          
          # Store the results
          results[[paste(symbol, window_size, threshold_oversold, threshold_overbought, sep = "_")]] <- list(
            Symbol = symbol,
            Class = meta$assets[[symbol]]$class,
            Methodology = paste("RSI:", window_size, threshold_oversold, threshold_overbought),
            Window_Size = window_size,
            Threshold_oversold = threshold_oversold,
            Threshold_overbought = threshold_overbought,
            Performance = rsi_instance$estimate_performance()
            )

            print(paste0("Results for ", "RSI ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
            "window_size:", window_size, ",", "threshold_oversold:", threshold_oversold, ",", "threshold_overbought:", threshold_overbought, ")"))
          }
        }
    }
  }

  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Window_Size = item$Window_Size,
      Threshold_oversold = item$Threshold_oversold,
      Threshold_overbought = item$Threshold_overbought,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

  if (output_df) {
    return(results_df)
  } else {
    return(results)
  }
}
  )
)

# Create an instance of RSI class
rsi <- RSI$new(ts, window_size = 14, threshold_oversold = 40, threshold_overbought = 60)
rsi$estimate_performance()
rsi$plot_equity_lines("RSI")

# Instances of MACD strategy (run backtesting)
res_rsi <- rsi$run_backtest(
  symbols = assets, 
  window_sizes = seq(10, 30, by = 5), 
  thresholds_oversold = seq(20, 40, by = 5),
  thresholds_overbought = seq(50, 70, by = 5),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define TurtleTrading class (Richard)
TurtleTrading <- R6Class(
  "TurtleTrading",
  inherit = Strategy,
  public = list(
    window_size1 = NULL,
    window_size2 = NULL,

initialize = function(data, window_size1, window_size2) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$window_size1 <- window_size1
  self$window_size2 <- window_size2
},

generate_signals = function() {
  self$data <- mutate(self$data,
                      upper_channel1 = rollapply(self$data$High, self$window_size1, max, align = "right", fill = NA),
                      upper_channel2 = rollapply(self$data$High, self$window_size2, max, align = "right", fill = NA),
                      lower_channel1 = rollapply(self$data$Low, self$window_size1, min, align = "right", fill = NA),
                      lower_channel2 = rollapply(self$data$Low, self$window_size2, min, align = "right", fill = NA),
                      mid1 = (upper_channel1 / lower_channel1) / 2,
                      mid2 = (upper_channel2 / lower_channel2) / 2,
                      signal1 = ifelse(Close > lag(upper_channel1) & Close > lag(upper_channel2), 1,
                        ifelse(Close < lag(lower_channel1) & Close < lag(lower_channel2), -1, 0)),
                      signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
                      position = lag(signal, default = 0)) %>% 
                        na.omit()
},

run_backtest = function(symbols, window_sizes1, window_sizes2, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window sizes, and MA types to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size1 in window_sizes1) {
      for (window_size2 in window_sizes2) {

        # Fetch data using DataFetcher for the current symbol and date range
        data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
        data <- data_fetcher$download_xts_data()
        
        # Create an instance of SMA1 strategy
        tt <- TurtleTrading$new(data, window_size1 = window_size1, window_size2 = window_size2)
                    
        # Store the results
        results[[paste(symbol, window_size1, window_size2, sep = "_")]] <- list(
          Symbol = symbol,
          Class = meta$assets[[symbol]]$class,
          Methodology = paste("TurtleTrading:", window_size1, window_size2),
          Window_Size1 = window_size1,
          Window_Size2 = window_size2,
          Performance = tt$estimate_performance()
        )
          print(paste0("Results for ", "TurtleTrading ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "window_size1:", window_size1, ",", "window_size2:", window_size2, ")"))
      }
    }
  }

  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Window_Size1 = item$Window_Size1,
      Window_Size2 = item$Window_Size2,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

  if (output_df) {
    return(results_df)
  }   else {
      return(results)
  }
}

  )
)

# Create instance of TurtleTrading class
tt <- TurtleTrading$new(ts, window_size1 = 20, window_size2 = 40)
tt$estimate_performance()
tt$plot_equity_lines(paste(symbol, ":", "TurtleTrading, w1 = 20, w2 = 40"), signal_flag = TRUE)
#ggsave(filename = "tt.png", plot = last_plot(), dpi = 300, type = "cairo", bg = "white")

# Instances of Turtle Trading strategy (run backtesting)
res_tt <- tt$run_backtest(
  symbols = assets, 
  window_sizes1 = seq(10, 40, by = 5), 
  window_sizes2 = seq(45, 75, by = 5),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Stop and Reversal (SAR) class
StopAndReversal <- R6Class(
  "StopAndReversal",
  inherit = Strategy,
  public = list(
    accel = NULL,
    accel_max = NULL,
    accel_vector = NULL,

initialize = function(data, accel, accel_max) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$accel <- accel
      self$accel_max <- accel_max
      self$accel_vector <- c(accel, accel_max)
    },

generate_signals = function() {
      self$data <- self$data %>% 
        mutate(
          SAR = TTR::SAR(select(., High, Low), accel = self$accel_vector),
          signal = case_when(
            Close > lag(SAR) ~ 1,
            Close < lag(SAR) ~ -1,
            TRUE ~ 0
          ),
          position = lag(signal, default = 0)
        ) %>%
        na.omit()
},

plot_sar = function(name){
  ggplot(self$data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Price"), size = 0.5) +
  geom_point(aes(y = SAR, color = "SAR"), size = 1) +
  labs(
    title = paste0("Parabolic SAR for ", name),
    x = "Date",
    y = "Price"
  ) +
  scale_color_manual(values = c("Price" = "red", "SAR" = "blue")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal()
},

run_backtest = function(symbols, accels, accels_max, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window size  and bands to create instances and estimate performance
  for (symbol in symbols) {
    for (accel in accels) {
      for (accel_max in accels_max) {

          # Fetch data using DataFetcher for the current symbol and date range
          data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
          data <- data_fetcher$download_xts_data()
        
          # Create an instance of MACD strategy
          sra <- StopAndReversal$new(data, accel, accel_max)

          # Store the results
            results[[paste(symbol, accel, accel_max, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("Stop_and_reversal:", accel, accel_max),
              Accel = accel,
              Accel_max = accel_max,
              Performance = sra$estimate_performance()
            )

          print(paste0("Results for ", "StopAndReversal ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "accel:", accel, ",", "accel_max:", accel_max, ")"))

        }
      }
    }
  
  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Accel = item$Accel,
      Accel_max = item$Accel_max,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}
  )
)

# Create instance of Stop and Reversal strategy
sar <- StopAndReversal$new(ts, 0.02, 0.2)
sar$estimate_performance()
sar$plot_sar("acceleration: 0.02, 0,2")
sar$plot_equity_lines("acceleration: 0.02, 0.2")

#  Instances of Stop and Reversal strategy (run backtest)
leverage <- 1
res_sar <- sar$run_backtest(
  symbols = assets,
  accels = seq(0.01, 0.1, by = 0.01),
  accels_max = seq(0.15, 0.35, by = 0.05), # max acceleration ranges from 15% to 35%
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Average Directional Index (ADX) class
ADX <- R6Class(
  "AverageDirectionalIndex",
  inherit = Strategy,
  public = list(
    ndx = NULL,
    trend_strength = NULL,

initialize = function(data, ndx, trend_strength) {
  super$initialize(data)
  self$data <- super$convert_to_tibble(self$data)
  self$ndx = ndx
  self$trend_strength = trend_strength
},

generate_signals = function() {
  self$data <- self$data %>%
                  mutate(
                  self$data,
                  as.data.frame(TTR::ADX(select(., High, Low, Close), n = self$ndx)),
                  signal1 = case_when(
                    #DIp > DIn & ADX > self$trend_strength ~ 1, # lag ?
                    #DIp > lag(DIn) & ADX > self$trend_strength ~ 1, # lag
                    DIp > lag(DIn) & lag(ADX) > self$trend_strength ~ 1, # lag
                    lag(DIp) > lag(DIn) & lag(ADX) > self$trend_strength ~ 1, # lag
                    #DIp < DIn & ADX > self$trend_strength ~ -1,
                    #DIp < lag(DIn) & ADX > self$trend_strength ~ -1,
                    DIp < lag(DIn) & lag(ADX) > self$trend_strength ~ -1,
                    lag(DIp) < lag(DIn) & lag(ADX) > self$trend_strength ~ -1,
                    TRUE ~ 0
                  ),
                  signal = na.locf(ifelse(signal1 == 0, NA, signal1), fromLast = FALSE, na.rm = FALSE),
                  position = lag(signal, default = 0)
                  ) %>%
                  na.omit()
                    
},

run_backtest = function(symbols, ndxs, trend_strengths, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window size  and bands to create instances and estimate performance
  for (symbol in symbols) {
    for (ndx in ndxs) {
      for (trend_strength in trend_strengths) {

          # Fetch data using DataFetcher for the current symbol and date range
          data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
          data <- data_fetcher$download_xts_data()
        
          # Create an instance of MACD strategy
          adx <- ADX$new(data, ndx, trend_strength)

          # Store the results
            results[[paste(symbol, ndx, trend_strength, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("Average_Directional_Index:", ndx, trend_strength),
              Ndx = ndx,
              Trend_strength = trend_strength,
              Performance = adx$estimate_performance()
            )
          print(paste0("Results for ", "AverageDirectionalIndex ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "ndx:", ndx, ",", "trend_strength:", trend_strength, ")"))
        }
      }
    }
  
  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Ndx = item$Ndx,
      Trend_strength = item$Trend_strength,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}
  )
)

# Create  an instance of the ADX class
#adx <- ADX$new(ts, ndx = 14, trend_strength = 25)
adx <- ADX$new(ts, ndx = 5, trend_strength = 25)
adx$estimate_performance()
adx$plot_equity_lines("Average Directional Index, ndx = 14, trend_strength = 25", signal_flag = TRUE)
adx_df <- adx$data
adx$plot_candles(ndays = 10)
atr_df <- adx$estimate_average_true_range(ts, 14)

# Create  an instance of the ADX class (run backtesting)
leverage <- 1
res_adx <- adx$run_backtest(
  symbols = assets,
  ndxs = seq(5, 15, by = 1),
  trend_strengths = seq(20, 40, by = 5),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Bollinger Bands Breakout class
BollingerBreakout <- R6Class(
  "BollingerBreakout",
  inherit = Strategy,
  public = list(
    window_size = NULL,
    sd_mult = NULL,  # Multiplier for standard deviation

initialize = function(data, window_size, sd_mult = 2) {
    super$initialize(data)
    self$data <- super$convert_to_tibble(self$data)
    self$window_size <- window_size
    self$sd_mult <- sd_mult
},

generate_signals = function() {
    self$data <- mutate(self$data, 
                        ma = rollmean(Close, k = self$window_size, align = "right", fill = NA),
                        sd = rollapply(Close, width = self$window_size, sd, align = "right", fill = NA),
                        upper_band = ma + self$sd_mult * sd,
                        lower_band = ma - self$sd_mult * sd,
                        signal = case_when(
                          Close > lag(upper_band) ~ 1,
                          Close < lag(lower_band) ~ -1,
                          TRUE ~ 0
                        ),
                        position = lag(signal, default = 0)) %>% 
                          na.omit
},

run_backtest = function(symbols, window_sizes, sd_mults, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window size  and bands to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (sd_mult in sd_mults) {

           # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
          
          # Create an instance of BB strategy
          bb <- BollingerBreakout$new(data, window_size, sd_mult)
          # bb$generate_signals()

          # Store the results
            results[[paste(symbol, window_size, sd_mult, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("BollingerBreakout:", window_size, sd_mult),
              Window_Size = window_size,
              SD_Multiplier = sd_mult,
              Performance = bb$estimate_performance()
            )

          print(paste0("Results for ", "BollingerBreakout: ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "window_size:", window_size, ",", "sd_mult:", sd_mult, ")"))
        }
      }
    }
  
  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Window_Size = item$Window_Size,
      SD_Multiplier = item$SD_Multiplier,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}
  )
)

# Create an instance of BollingerBreakout class
bb <- BollingerBreakout$new(ts, window_size = 20, sd_mult = 0.5)
bb$estimate_performance()
bb$plot_equity_lines("BB", signal_flag = TRUE)

# Instances of Bollinger Breakout strategy (run backtesting)
leverage <- 1
res_bb <- bb$run_backtest(
  symbols = assets, 
  window_sizes = seq(5, 40, by = 5), 
  sd_mults = seq(0.5, 2, by = 0.5),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Volatility Mean Reversion class
VolatilityMeanReversion <- R6Class(
  "VolatilityMeanReversion",
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
      # Estimate historical volatility
      ma_func <- get(self$ma_type)

      # Generate signals
      self$data <- mutate(self$data,
                          hist_vol = rollapply(self$data$Close, width = self$window_size, sd, align = "right", fill = NA),
                          mean_vol = ma_func(hist_vol, self$window_size, align = "right", fill = NA),
                          signal = ifelse(hist_vol > lag(mean_vol), -1, 1),
                          position = lag(signal, default = 0)) %>%
                            na.omit
},

run_backtest = function(symbols, window_sizes, ma_types, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window size  and bands to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (ma_type in ma_types) {

           # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
          
          # Create an instance of MACD strategy
          vmr <- VolatilityMeanReversion$new(data, window_size, ma_type)
          # vmr$generate_signals()

          # Store the results
            results[[paste(symbol, window_size, ma_type, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("Volatility_mean_reversion:", window_size, ma_type),
              Window_Size = window_size,
              MA_type = ma_type,
              Performance = vmr$estimate_performance()
            )

          print(paste0("Results for ", "VolatilityMeanReversion ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "window_size:", window_size, ",", "ma_type:", ma_type, ")"))
        }
      }
    }
  
  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Window_Size = item$Window_Size,
      MA_Type = item$MA_type,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}
  )
)

# Create an instance of VolatilityMeanReversion class
vmr <- VolatilityMeanReversion$new(ts, window_size = 20, "SMA")
vmr$estimate_performance()
vmr$plot_equity_lines("VolatilityMeanReversion")

# Instances of Volatility Mean Reversion strategy (run backtesting)
res_vmr <- vmr$run_backtest(
  symbols = assets, 
  window_sizes = seq(10, 50, by = 5), 
  ma_types = c("SMA", "EMA"),
  from_date,
  to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define Random strategy class (generate random signals)
Random <- R6Class(
"Random",
  inherit = Strategy,
  public = list(
    prob = NULL,
    seed = NULL,
initialize = function(data, prob = 0.5, seed) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$prob <- prob
      self$seed <- seed
},

generate_signals = function(prob = self$prob, seed = self$seed) {
    if (self$seed) {
    set.seed(123)
  }
      self$data <- mutate(self$data, 
                          signal = sample(c(-1,1), size = nrow(self$data), prob),
                          position = lag(signal, default = 0)) %>% 
                            na.omit
},

run_backtest = function(symbols, probs, seed, from_date, to_date, output_df = TRUE) {
  # Create an empty list to store results
  results <- list()

  # Loop through symbols, window size  and bands to create instances and estimate performance
  for (symbol in symbols) {
      for (prob in probs) {

           # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
          
          # Create an instance of Random strategy
          random <- Random$new(data, prob, seed)

          # Store the results
            results[[paste(symbol, prob, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("Random:", prob),
              Probability = prob,
              Performance = random$estimate_performance()
            )

          print(paste0("Results for ", "Random ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "prob:", prob, ")"))
        }
    }
  
  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Probability = item$Probability,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}
  )
)

# Instances of Random strategy
rand <- Random$new(ts, prob = 0.5, seed = TRUE)
rand$estimate_performance()
rand$plot_equity_lines("Random", signal_flag = TRUE)

# Instances of Random strategy (run backtesting)
res_random <- rand$run_backtest(
  symbols = assets,
  probs = seq(0.1, 0.9, by = 0.1),
  seed = FALSE,
  from_date = from_date,
  to_date = to_date,
  #output_df = FALSE
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
  trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Define ARIMA class
ARIMAbased <- R6Class(
  "ARIMAbased",
  inherit = Strategy,
  public = list(
    window_size = NULL, # for "moving" window_type it is window_size, for "expanding" window_type it is starting window_size only (as then it expands given iterations)
    window_type = NULL,
    best_arima = NULL,
    p1 = NULL,
    d1 = NULL,
    q1 = NULL,
    
initialize = function(data, window_size, window_type, best_arima = TRUE, p1 = NULL, d1 = NULL, q1 = NULL) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$window_size <- window_size
      self$window_type <- window_type
      self$best_arima <- best_arima
      self$p1 <- p1
      self$d1 <- d1
      self$q1 <- q1
},
    
generate_signals = function() {
  # Register parallel backend
  # num_cores <- parallel::detectCores() - 1 
  # registerDoParallel(cores = num_cores)

  n <- nrow(self$data)
  # Preallocate vectors
  forecast_values <- numeric(n - self$window_size + 1)
  actual_values <- numeric(n - self$window_size + 1)
  dates <- as.Date(character(n - self$window_size + 1))
  
  # Rolling window type
  switch(
    self$window_type,
    "moving" = {
      # Perform rolling forecasts
      for (i in 1:(n - self$window_size + 1)) {
      # foreach(i = 1:(n - self$window_size + 1)) %dopar% {

        # Extract current window of data
        window_data <- self$data$Close[i:(i + self$window_size - 1)]
        
        tryCatch({
          if (self$best_arima) {    
            # Fit ARIMA model
            best_arima <- auto.arima(window_data, stepwise = TRUE, approximation = TRUE)
            p <- best_arima$arma[1]
            d <- best_arima$arma[2]
            q <- best_arima$arma[3]
            P <- best_arima$arma[4]
            D <- best_arima$arma[5]
            Q <- best_arima$arma[6]
            
            fit <- arima(window_data, order = c(p, d, q), seasonal = list(order = c(P, D, Q)), method = "CSS-ML")
          } else {
            # Use provided p1, d1, q1 parameters
            fit <- arima(window_data, order = c(self$p1, self$d1, self$q1), method = "CSS-ML")
          }
          
          # Forecast (1 day ahead)
          forecast_result <- forecast(fit, lead = 1, output = FALSE) %>%
            data.frame %>% 
            select(Forecast) %>% as.numeric
          
          # Store dates, forecast, and actual value
          forecast_values[i] <- forecast_result
          actual_values[i] <- self$data$Close[i + self$window_size - 1]
          dates[i] <- self$data$Date[i + self$window_size - 1]
        }, error = function(e) {
          # Handle errors
          print(paste("Error in iteration", i, ":", e$message))
        })
      }
    },

    "expanding" = {
      # Perform expanding window forecasts
      for (i in self$window_size:n) {
      # foreach(i = self$window_size:n) %dopar% {

        # Extract current window of data
        window_data <- self$data$Close[1:i]
        
        tryCatch({
          if (self$best_arima) {    
            # Fit ARIMA model
            best_arima <- auto.arima(window_data, stepwise = TRUE, approximation = TRUE)
            p <- best_arima$arma[1]
            d <- best_arima$arma[2]
            q <- best_arima$arma[3]
            P <- best_arima$arma[4]
            D <- best_arima$arma[5]
            Q <- best_arima$arma[6]
            
            fit <- arima(window_data, order = c(p, d, q), seasonal = list(order = c(P, D, Q)), method = "CSS-ML")
          } else {
            # Use provided p1, d1, q1 parameters
            fit <- arima(window_data, order = c(self$p1, self$d1, self$q1), method = "CSS-ML")
          }
          
          # Forecast (1 day ahead)
          forecast_result <- forecast(fit, lead = 1, output = FALSE) %>%
            data.frame %>% 
            select(Forecast) %>% as.numeric
          
          # Store dates, forecast, and actual value
          forecast_values[i] <- forecast_result
          actual_values[i] <- self$data$Close[i]
          dates[i] <- self$data$Date[i]
        }, error = function(e) {
          # Handle errors
          print(paste("Error in iteration", i, ":", e$message))
        })
      }
    }
  )
  
  res <- data.frame(
    Date = dates,
    Forecast = forecast_values, 
    Actual = actual_values
  )
  
  # Compare Forecast with Actual value
  self$data <- self$data %>%
    left_join(res %>% select(Date, Forecast), by = "Date") %>%
    mutate(
      signal1 = case_when(
        Forecast > Close ~ 1,
        Forecast < Close ~ -1,
        TRUE ~ 0 
      )
      # position = lag(signal, default = 0)
    ) %>%
      na.omit()

  # Initialize last_long_value and last_short_value (dynamic trailing threshold)
  last_long_value <- rep(NA, nrow(self$data))
  last_short_value <- rep(NA, nrow(self$data))

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
              last_long_value[i] <- self$data$Forecast[first_previous_index]
          }
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
              last_short_value[i] <- self$data$Forecast[first_previous_index]
          }
      }
  }

  # Assign last_long_value and last_short_value to the data frame
  self$data$last_long_value <- last_long_value
  self$data$last_short_value <- last_short_value

  # Replace NA or invalid values with 0 in the last_long_value and last_short_value columns
  self$data$last_long_value <- replace(self$data$last_long_value, !is.finite(self$data$last_long_value), 0)
  self$data$last_short_value <- replace(self$data$last_short_value, !is.finite(self$data$last_short_value), 0)

  # Compare data$Forecast[i] with the first previous value and update data$s2
  self$data$signal <- ifelse((self$data$Forecast > self$data$last_long_value) & (self$data$Forecast > self$data$Close), 1,
                      ifelse((self$data$Forecast < self$data$last_short_value) & (self$data$Forecast < self$data$Close), -1, 0))

  # Replacing 0s by previous signal value:
  self$data$signal <- na.locf(ifelse(self$data$signal == 0, NA, self$data$signal), fromLast = FALSE, na.rm = FALSE)
  self$data$signal <- replace(self$data$signal, is.na(self$data$signal), 0)

  self$data$position <- lag(self$data$signal, default = 0)
},

# Choose ARIMA model by directly looping through different AR, I, MA parameters
run_backtest1 = function(symbols, window_sizes, window_types, best_arima = FALSE, p1s, d1s, q1s, from_date, to_date, output_df = TRUE) {
  
  # Initialize an empty list to store results
  results <- list()

  # Loop through symbols and parameters to create instances and estimate performance
    for (symbol in symbols) {
      for (window_size in window_sizes) {
        for (window_type in window_types) {
          for (p1 in p1s) {
            for (d1 in d1s) {
              for (q1 in q1s) {
    
  # Fetch data using DataFetcher for the current symbol and date range
  data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
  data <- data_fetcher$download_xts_data()
  
  # Create an instance of ARIMA strategy
  arima <- ARIMAbased$new(data, window_size, window_type, best_arima = FALSE, p1, d1, q1)
  
  # Store the results
    results[[paste(symbol, window_size, window_type, p1, d1, q1, sep = "_")]] <- list(
      Symbol = symbol,
      Class = meta$assets[[symbol]]$class,
      Methodology = paste("ARIMAbased:", window_size, window_type, p1, d1, q1),
      Window_Size = window_size,
      Window_Type = window_type,
      P1 = p1,
      D1 = d1,
      Q1 = q1,
      Performance = arima$estimate_performance()
    )

  print(paste0("Results for ", "ARIMAbased ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
    "window_size:", window_size, ",", "window_type:", window_type, ",", "AR:", p1, ",", "I:", d1, ",", "q1:", q1, ")"))

              }
            }
          }
        }
      }
    }

  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Window_Size = item$Window_Size,
      Window_Type = item$Window_Type,
      P1 = item$P1,
      D1 = item$D1,
      Q1 = item$Q1,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }

},

# Choose best ARIMA model based on AIC/BIC criteria
run_backtest2 = function(symbols, window_sizes, window_types, best_arima = TRUE, from_date, to_date, output_df = TRUE) {
  
  # Create an empty list to store results
  results <- list()

  # Loop through symbols and parameters to create instances and estimate performance
  for (symbol in symbols) {
    for (window_size in window_sizes) {
      for (window_type in window_types) {

          # Fetch data using DataFetcher for the current symbol and date range
          data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
          data <- data_fetcher$download_xts_data()
          
          arima <- ARIMAbased$new(data, window_size, window_type, best_arima = TRUE)

          # Store the results
            results[[paste(symbol, window_size, window_type, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("AutoARIMAbased:", window_size, window_type),
              Window_Size = window_size,
              Window_Type = window_type,
              Performance = arima$estimate_performance()
            )

          print(paste0("Results for ", "AutoARIMA: ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
          "window_size:", window_size, ",", "window_type:", window_type, ")"))
        }
      }
    }
  
  # Convert results to a data frame
  results_df <- map_dfr(names(results), ~{
    item <- results[[.x]]
    data_frame(
      Symbol = item$Symbol,
      Class = item$Class,
      Methodology = item$Methodology,
      Window_Size = item$Window_Size,
      Window_Type = item$Window_Type,
      Strategy = item$Performance$Strategy,
      aR = item$Performance$aR,
      aSD = item$Performance$aSD,
      IR = item$Performance$IR,
      MD = item$Performance$MD,
      trades = item$Performance$trades,
      avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
      buys = item$Performance$buys,
      sells = item$Performance$sells,
      Buy_Success_Rate = item$Performance$Buy_Success_Rate,
      Short_Success_Rate = item$Performance$Short_Success_Rate,
      Combined_Success_Rate = item$Performance$Combined_Success_Rate,
      PortfolioValue = item$Performance$PortfolioEndValue
    )
  })

    if (output_df) {
      return(results_df)
    } else {
      return(results)
    }
}

  )
)

# Create single instance of ARIMA 
arima <- ARIMAbased$new(ts, window_size = 21, window_type = "expanding", best_arima = FALSE, p1 = 1, d1 = 1, q1 = 1)
arima$estimate_performance()
arima$plot_equity_lines(paste(symbol, ":", "ARIMAbased, window_size = 60, window_type = expanding"), signal_flag = TRUE)
check_signals <- arima$data %>% select(Date, value)

# Create many instances of ARIMA (run backtest1) 2023-01-01 - 2024-04-24
res_arima <- arima$run_backtest1(
  symbols = assets,
  #symbols = "GC=F",
  window_sizes = c(21, 126),
  window_types = c("expanding", "moving"),
  best_arima = FALSE,
  p1s = c(0, 1, 2, 3),
  d1s = c(1,2),
  q1s = c(1, 2, 3),
  from_date = as.Date("2022-01-01", format = "%Y-%m-%d"),
  to_date = to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Create many instances of ARIMA (run backtest1) 2023-01-01 - 2024-04-24
res_arima_auto <- arima$run_backtest2(
  symbols = assets,
  window_sizes = c(21, 63, 126),
  window_types = c("expanding", "moving"),
  best_arima = TRUE,
  from_date = as.Date("2022-01-01", format = "%Y-%m-%d"),
  to_date = to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)
 
# Define EventBased (intrinsic time approach: currently for mean reverted ts: FX, etc.)
EventBased <- R6Class(
  "EventBased",
  inherit = Strategy,
  public = list(
    threshold = NULL,
    extremes = NULL,
    window_size = NULL,

initialize = function(data, threshold, extremes, window_size) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$threshold <- threshold
      self$extremes <- extremes
      self$window_size <- window_size # optional (for extremes = "local")
},

# Signal generation, algorithm that:
# Opens the position when markets are overshoot (done)
# Manages the position by cascading or de-cascading during the evolution of the long coastline of prices until it closes in a profit (to be done)
generate_signals = function() {

    self$data <- self$data %>%
    mutate(
      # Initialize columns with NA
      p = (High + Low) / 2, # outlier is present, disturbing entire lowest series
      lowest_close = Close,
      lowest_low = Low,
      highest_close = Close,
      highest_high = High,
      Event = NA,
      Event = replace(Event, 1, 1),
      Run = NA,
      Run = replace(Run, 1, "Upward"),
      UE_start = NA,
      UE_end = NA,
      DE_start = NA,
      DE_end = NA,
      OS = NA,
      signal = NA
    )

  # Loop through each row of the self$data
  for (t in 2:nrow(self$data)) {
    # Update the lowest and highest prices up to time t (depends on frequency chosen)
    if (self$extremes == "local") {
      window_start <- max(1, t - self$window_size)  # Start of the 20-day window
      self$data$lowest_close[t] <- min(self$data$Close[window_start:t])
      self$data$lowest_low[t] <- min(self$data$Low[window_start:t])
      self$data$highest_close[t] <- max(self$data$Close[window_start:t])
      self$data$highest_high[t] <- max(self$data$High[window_start:t])
    } else if (self$extremes == "global") {
      self$data$lowest_close[t] <- min(self$data$Close[1:t])
      self$data$lowest_low[t] <- min(self$data$Low[1:t])
      self$data$highest_close[t] <- max(self$data$Close[1:t])
      self$data$highest_high[t] <- max(self$data$High[1:t])
    } else {
      stop("Invalid extremes specified.")
    }

    # Check if directional change: DE or UE
    if (self$data$Close[t] <= self$data$highest_high[t] * (1 - self$threshold) & self$data$Close[t] > self$data$highest_high[t] * (1 - self$threshold - 0.005)) {
      self$data$Event[t] <- -1
      if (self$data$Event[t] == -1) {
        self$data$DE_start[t] <- self$data$highest_high[t]
        self$data$DE_end[t] <- self$data$Close[t]
      }
    } else if (self$data$Close[t] >= self$data$lowest_close[t] * (1 + self$threshold) & self$data$Close[t] < self$data$lowest_close[t] * (1 + self$threshold + 0.005)) {
      self$data$Event[t] <- 1
      if (self$data$Event[t] == 1) {
        self$data$UE_start[t] <- self$data$lowest_close[t]
        self$data$UE_end[t] <- self$data$Close[t]
      }
    } else {
      self$data$Event[t] <- NA
    }
  }

  self$data <- self$data %>%
    mutate(
      Run = ifelse(Event == 1, "Upward", ifelse(Event == -1, "Downward", NA)),
      Directional_Change = c(TRUE, diff(Event) != 0),
      Directional_Change = replace_na(Directional_Change, FALSE)
    )

    # Find indices of Upturn and Downturn events
    upturn_indices <- which(self$data$Event == 1) # Indices of Upturn events
    downturn_indices <- which(self$data$Event == -1) # Indices of Downturn events

  # Iterate through each pair of Upturn and Downturn events
  for (i in 1:min(length(upturn_indices), length(downturn_indices))) {
    # Starting index of the current Upturn
    upturn_start <- upturn_indices[i]
    # Ending index of the next Downturn
    downturn_end <- downturn_indices[which(downturn_indices > upturn_start)][1]
    
    # Mark the period between as UOS
    if (!is.na(downturn_end)) {
      self$data$OS[(upturn_start + 1):(downturn_end - 1)] <- "UOS"
    } else {
      self$data$OS[(upturn_start + 1):nrow(self$data)] <- "UOS"  # Mark until the end if no Downturn found
      break  # Exit loop if no more Downturn events
    }
    
    # Starting index of the current Downturn
    downturn_start <- downturn_indices[i]
    # Ending index of the next Upturn
    upturn_end <- upturn_indices[which(upturn_indices > downturn_start)][1]
    
    # Mark the period between as DOS
    if (!is.na(upturn_end)) {
      self$data$OS[(downturn_start + 1):(upturn_end - 1)] <- "DOS"
    } else {
      self$data$OS[(downturn_start + 1):nrow(self$data)] <- "DOS"  # Mark until the end if no Upturn found
      break  # Exit loop if no more Upturn events
    }
  }

  # Signal generation
  self$data <- self$data %>% 
    mutate(
      signal = if_else(OS == "UOS", -1, if_else(OS == "DOS", 1, signal)),
      signal = replace_na(signal, 0),
      position = lag(signal, default = 0),
      OS = replace_na(OS, "0"),
      Event = replace_na(Event, 0)
    ) %>%
    select(Date, Open, High, Low, Close, lowest_low, highest_high, Volume, Adjusted, value, OS, Event, signal, position)
},
    
plot_overshoots = function(symbol, extremes) {

  p <- ggplot(self$data, aes(x = Date, y = Close)) +
    geom_vline(data = self$data[self$data$OS == "UOS", ], aes(xintercept = as.numeric(Date), color = "UOS"), linetype = "dashed") +  # Add vertical lines for UOS
    geom_vline(data = self$data[self$data$OS == "DOS", ], aes(xintercept = as.numeric(Date), color = "DOS"), linetype = "dashed") +  # Add vertical lines for DOS
    geom_line() +  # Plot close price
    labs(title = paste("Close Price with Overshoot for", symbol, "(", extremes, ")"), x = "Date", y = "Close Price") +
    theme_minimal() +
    scale_color_manual(name = "Overshoot", values = c("UOS" = "blue", "DOS" = "red"), labels = c("UOS" = "Upward OS", "DOS" = "Downward OS"))

    print(p)
},

# global:
run_backtest1 = function(extremes, symbols, thresholds, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      tryCatch({
        # Loop through symbols, window size  and bands to create instances and estimate performance
        for (symbol in symbols) {
          for (threshold in thresholds) {
            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
          
            # Create an instance of BB strategy
            event <- EventBased$new(data, threshold, extremes, NA)

            # Store the results
            results[[paste(symbol, threshold, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("EventBased:", threshold),
              Threshold = threshold,
              Performance = event$estimate_performance()
            )

            print(paste0("Results for ", "EventBased: ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
              "threshold:", threshold, ")"))
          }
        }
      }, error = function(e) {
        # Handle errors
        print(paste("Error in iteration", i, ":", e$message))
      })

      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class = item$Class,
          Methodology = item$Methodology,
          Threshold = item$Threshold,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
      } else {
        return(results)
      }
},

# local:
run_backtest2 = function(symbols, thresholds, window_sizes, from_date, to_date, output_df = TRUE) {
    # Create an empty list to store results
    results <- list()

    tryCatch({
        # Loop through symbols, thresholds, and window sizes to create instances and estimate performance
        for (symbol in symbols) {
            for (threshold in thresholds) {
                for (window_size in window_sizes) {
                    # Fetch data using DataFetcher for the current symbol and date range
                    data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
                    data <- data_fetcher$download_xts_data()
                
                    # Create an instance of the EventBased strategy
                    event <- EventBased$new(data, threshold, extremes = "local", window_size)
                    
                    # Store the results
                    results[[paste(symbol, threshold, window_size, sep = "_")]] <- list(
                        Symbol = symbol,
                        Class = meta$assets[[symbol]]$class,
                        Methodology = paste("EventBased:", threshold, window_size),
                        Threshold = threshold,
                        Window_Size = window_size,
                        Performance = event$estimate_performance()
                    )
                    
                    print(paste0("Results for EventBased (daily): (symbol:", symbol, ", class:", meta$assets[[symbol]]$class, ", threshold:", threshold, ", window_size:", window_size, ")"))
                }
            }
        }
    }, error = function(e) {
        # Handle errors
        print(paste("Error:", e$message))
    })

    # Convert results to a data frame
    results_df <- purrr::map_dfr(names(results), ~{
        item <- results[[.x]]
        tibble::tibble(
            Symbol = item$Symbol,
            Class = item$Class,
            Methodology = item$Methodology,
            Threshold = item$Threshold,
            Window_Size = item$Window_Size,
            Strategy = item$Performance$Strategy,
            aR = item$Performance$aR,
            aSD = item$Performance$aSD,
            IR = item$Performance$IR,
            MD = item$Performance$MD,
            trades = item$Performance$trades,
            avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
            buys = item$Performance$buys,
            sells = item$Performance$sells,
            Buy_Success_Rate = item$Performance$Buy_Success_Rate,
            Short_Success_Rate = item$Performance$Short_Success_Rate,
            Combined_Success_Rate = item$Performance$Combined_Success_Rate,
            PortfolioValue = item$Performance$PortfolioEndValue
        )
    })

    if (output_df) {
        return(results_df)
    } else {
        return(results)
    }
}
  
  )
)

# Instances of EventBased strategy
leverage <- 1
event <- EventBased$new(ts, threshold = 0.01, extremes = "local", window_size = 126)
event <- EventBased$new(ts, threshold = 0.01, extremes = "global", window_size = 126)
event$estimate_performance()
event$plot_equity_lines(paste0("EventBased for ", symbol), signal_flag = TRUE)
#####
event$plot_overshoots(symbol, "local")
event$generate_signals()
fx <- event$data

fxs <- names(Filter(function(x) x$class == "FX", meta$assets))

# Instances of EventBased strategy (run backtesting)
res_event1 <- event$run_backtest1(
  extremes = "global",
  symbols = fxs, 
  thresholds = seq(0.01, 0.03, by = 0.005), # from 1% to 3% threshold range considered
  from_date,
  to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# Instances of EventBased strategy (run backtesting) (saved in List)
res_event2 <- event$run_backtest2(
  #symbols = fxs, 
  symbols = "EURUSD=X", 
  thresholds = seq(0.01, 0.03, by = 0.005), # from 1% to 3% threshold range considered
  window_sizes = c(21, 126, 252),
  from_date,
  to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

######################################################
# Define AlphaEngine (based on intrinsic time approach)
######################################################

AlphaEngine <- R6Class(
  "AlphaEngine",
  inherit = Strategy,
  public = list(

    threshold = NULL,
    profit_taking = NULL,
    signal_generation = NULL,
    position_sizing = NULL,

initialize = function(data, threshold, profit_taking, signal_generation = "TH", position_sizing = FALSE) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$threshold <- threshold
      self$profit_taking <- profit_taking
      self$signal_generation <- signal_generation
      self$position_sizing <- position_sizing
},

generate_signals = function() {

    cat("Threshold value:", self$threshold, "\n")  # Print the threshold value

    # Mid data
    self$data <- self$data %>%
        mutate(
            mid = (High + Low) / 2
        )

    # STEP1 : DETECT DIRECTIONAL CHANGES
    self$data <- private$identify_events(data = self$data, threshold = self$threshold)

    # STEP2 : INTRODUCE PROBABILITY INDICATOR FOR POSITION SIZING

    # Function to compute transition probabilities and surprises (only for position_sizing)

    if(self$position_sizing) {

    self$data$surprise <- NA_real_
    self$data$H1 <- NA_real_
    self$data$H2 <- NA_real_

    # Loop over rows to compute probability and surprise for each row
    for (i in 1:nrow(self$data)) {

        data <- slice(self$data, 1:i)  # Subset data up to current row
        probs_surprises <- private$estimate_prob_surprise(data)  # Compute probabilities and surprises
        entropies <- private$estimate_entropies(data)

        self$data$surprise[i] <- sum(probs_surprises$surprise) # surprise of the transition
        self$data$H1[i] <- entropies$H1
        self$data$H2[i] <- entropies$H2
    }

    # Initialize K with 1
    self$data$K <- 1

    # Initialize a vector to store unique combinations
    unique_combinations <- c(paste(self$data$events[1], self$data$j[1]))
    self$data$j <- lag(self$data$events, default = 0)

    # Iterate over rows and update K when a new combination is identified
    for (i in 2:nrow(self$data)) {
    current_combination <- paste(self$data$dc[i], self$data$j[i])
    if (!(current_combination %in% unique_combinations)) {
        unique_combinations <- c(unique_combinations, current_combination)
    }
    self$data$K[i] <- min(length(unique_combinations), 4)
    if (length(unique_combinations) > 4) {
        self$data$K[i:nrow(self$data)] <- 4
        break
        }
    }

    # estimate L
    self$data <- self$data %>%
    mutate(
        surprise = replace_na(surprise, 0),
        d = (surprise - K * H1) / sqrt(K * H2),
        L = 1 - pnorm(d)
    )

    }

    # STEP3 : GENERATE ENTRY SIGNALS (BASED ON THRESHOLD)

    # Identify threshold value (which is used to compare current mid price)

    # Initialize an empty vector to store mid prices for the change_value
    self$data$change_value <- NA

    # Initialize variables
    prev_dc_index <- NULL
    prev_mid <- NULL

    # Loop through each row
    for (i in 1:nrow(self$data)) {
    # Check if dc is TRUE
    if (self$data$dc[i]) {
        # If this is the first dc or if it's not consecutive with the previous one
        if (is.null(prev_dc_index) || i != prev_dc_index + 1) {
        prev_dc_index <- i  # Update prev_dc_index
        prev_mid <- self$data$mid[i]  # Record the mid price
        }
    }
    # Assign the previous mid price to the change_value column
    self$data$change_value[i] <- prev_mid
    
    # Check if the price further changes by the threshold and update prev_mid accordingly
    if (!is.na(prev_mid) && !is.na(self$data$OS[i])) {
      if (self$data$OS[i] == "UOS" && self$data$mid[i] > prev_mid * (1 + self$profit_taking)) {
        prev_mid <- self$data$mid[i]  # Update prev_mid for UOS
      } else if (self$data$OS[i] == "DOS" && self$data$mid[i] < prev_mid * (1 - self$profit_taking)) {
        prev_mid <- self$data$mid[i]  # Update prev_mid for DOS
      }
    }
  }

    #self$profit_taking <- 0.005 # assymetric
    self$data <- self$data %>%
    mutate(
        row_number = row_number(),
        signal = case_when(
        OS == "UOS" & mid >= change_value * (1 + self$profit_taking) ~ -1,
        OS == "DOS" & mid <= change_value * (1 - self$profit_taking) ~ 1,
        TRUE ~ 0
        ),
        position = lag(signal, default = 0),
        OSS = mid >= change_value * (1 + self$profit_taking) | mid <= change_value * (1 - self$profit_taking) 
    ) #%>% 
        #select(Date, High, Low, Close, mid, change_value, OS, dc, events, L, signal, OSS, position, row_number)

    entries <- self$data %>% 
            filter(signal == 1 | signal == -1) %>%
                mutate(
                    change = c(2, abs(diff(signal))), default = 0,
                    Exit = case_when(
                        signal == -1 ~ mid * (1 - self$profit_taking),
                        signal == 1 ~ mid * (1 + self$profit_taking),
                        TRUE ~ 0
                        )
                    )

    self$data <- self$data %>%
    mutate(in_entries = row_number() %in% entries$row_number,
    signal = ifelse(in_entries, signal, 0),
    position = lag(signal, default = 0)
    ) %>% 
        left_join(entries %>% select(row_number, Exit), by = c("row_number" = "row_number"))


    # STEP3 : GENERATE ENTRY SIGNALS (BASED ON AVERAGE LENGTH OF OVERSHOOTS : overshoot duration)

    # Calculate sequence lengths for UOS and DOS
    sequence_lengths <- private$calculate_sequence_lengths(self$data)

    avgOS <- data.frame(avgUOS = floor(mean(sequence_lengths$uos_lengths)), avgDOS = floor(mean(sequence_lengths$dos_lengths)))

    # Usage:
    self$data <- private$calculate_OS_length(self$data)
    self$data <- self$data %>%
        mutate(
            ExitOS = case_when(
            signalOS == -1 ~ mid * (1 - self$profit_taking),
            signalOS == 1 ~ mid * (1 + self$profit_taking),
            TRUE ~ 0
            )
        )

    # Updating signal and Exits value given signal based method

    self$data <- self$data %>%
      mutate(
        signal = case_when(
          self$signal_generation == "OS" ~ signalOS,
          TRUE ~ signal  # No change for other cases
        ),
        Exit = case_when(
          self$signal_generation == "OS" ~ ExitOS,
          TRUE ~ Exit  # No change for other cases
        )
      )

    # STEP4 : EXIT SIGNALS GENERATION (CASCADE AND DE-CASCADE POSITIONS)

    self$data <- private$generateExitSignals(self$data, self$signal_generation)

    exits <- self$data %>% 
            filter(signalE == 1 | signalE == -1) %>%
                mutate(
                    change = c(2, abs(diff(signalE))),
                        ) %>% 
                            filter(change == 2)

    if(self$position_sizing) {

    self$data <- self$data %>%
      mutate(
        out_exits = row_number() %in% exits$row_number,
        signal = if_else(in_entries, signal, 0),
        signalE = ifelse(signalE != 0, signalE, 0),
        signalE = if_else(out_exits, signalE, 0),
        signal = if_else(signal != signalE & signalE != 0, signalE, signal),
        position = lag(signal, default = 0),
        nop_sizing = case_when(
          L < 0.1 ~ 0.1,
          L < 0.4 ~ 0.5,
          TRUE ~ 1
      )
  ) %>% 
    mutate(
      L = replace_na(L, 1)
  ) %>% 
      select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, events,
         signal, signalOS, signalE, signalE_closes_row, L, nop_sizing, position, in_entries, out_exits, Exit, OS_length)
    } else {

    self$data <- self$data %>%
    mutate(
        out_exits = row_number() %in% exits$row_number,
        signal = if_else(in_entries, signal, 0),
        signalE = ifelse(signalE != 0, signalE, 0),
        signalE = if_else(out_exits, signalE, 0),
        signal = if_else(signal != signalE & signalE != 0, signalE, signal), # shift to have more exits rather than entries
         #signal = if_else(signal != signalE & signalE != 0, signalE, signal), # signals cancel entry and exit
        position = lag(signal, default = 0)
    ) %>% select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, events,
        signal, signalOS, signalE, signalE_closes_row, position, in_entries, out_exits, Exit, OS_length)
    }


  #   if (!self$position_sizing) {
  #     self$data <- subset(self$data, select = -nop_sizing)
  #   }

},
    
# Plot directional changes and market overshoots: 
plot_events = function(symbol) {

    #data <- na.omit(self$data)

    p <- ggplot(self$data, aes(x = Date, y = Close, color = OS)) +
    geom_point(data = self$data[self$data$dc == TRUE,], aes(shape = "dc"), color = "black", size = 2) +  # Black triangles for dc
    geom_point(data = self$data, aes(shape = ifelse(dc, NA, "Close")), size = 1, alpha = 0.6) +  # Regular points with decreased size and some transparency
    scale_color_manual(values = c("black", "red", "green"),
                        labels = c("Regular", "Downward Overshoot", "Upward Overshoot")) +
    scale_shape_manual(values = c("dc" = 17, "Close" = 16), 
                        labels = c("Close", "Directional Change")) +
    labs(title = paste("Market overshoots and directional changes for", symbol),
        x = "Date", y = "Close Price") +  # Adding axis labels
    
    scale_x_date(date_labels = "%b-%Y", date_breaks = "2 years") +

    theme_minimal() +
    theme(legend.position = "bottom",  # Moving legend to bottom
            axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels for better readability
            plot.title = element_text(size = 14, face = "bold"),  # Increasing title font size and making it bold
            axis.title = element_text(size = 12),  # Increasing axis label font size
            legend.text = element_text(size = 10),  # Adjusting legend text size
            legend.title = element_text(size = 12, face = "bold"))  # Adjusting legend title size and making it bold

    print(p)

},

# Plot Close price given intrinsic time (event based price)
plot_dc = function(symbol) {

    self$data <- self$data %>% 
    filter(events == 1 | events == -1)

    ggplot(self$data, aes(x = Date, y = Close)) +
    geom_line() +  # Plot close price
    geom_vline(data = subset(self$data, events == 1), aes(xintercept = as.numeric(Date)), color = "blue", linetype = "dashed") +  # Add vertical lines for UE
    geom_vline(data = subset(self$data, events == -1), aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed") +  # Add vertical lines for DE
    labs(title = paste("Close Price filtered by Directional Changes for", symbol), x = "Date", y = "Close Price") +
    theme_minimal()

},

plotSignals = function(data) {
  ggplot(self$data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_point(data = subset(self$data, signal == 1), aes(color = "Buy", shape = "Buy"), size = 1.5) +
    geom_point(data = subset(self$data, signal == -1), aes(color = "Sell", shape = "Sell"), size = 1.5) +
    geom_point(data = subset(self$data, signalE == 1), aes(color = "Exit Buy", shape = "Exit Buy"), size = 1.5) +
    geom_point(data = subset(self$data, signalE == -1), aes(color = "Exit Sell", shape = "Exit Sell"), size = 1.5) +
    scale_shape_manual(values = c("Buy" = 1, "Sell" = 2, "Exit Buy" = 1, "Exit Sell" = 2)) +
    scale_color_manual(values = c("Buy" = "green", "Sell" = "red", "Exit Buy" = "darkgreen", "Exit Sell" = "darkred")) +
    labs(title = "Close Price") +
    theme_minimal()
},

run_backtest = function(symbols, thresholds, profit_takings, signal_generations, position_sizings, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      tryCatch({
        # Loop through symbols, window size  and bands to create instances and estimate performance
        for (symbol in symbols) {
          for (threshold in thresholds) {
            for (profit_taking in profit_takings) {
              for (signal_generation in signal_generations) {
                for (position_sizing in position_sizings) {

            # Fetch data using DataFetcher for the current symbol and date range
            data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
            data <- data_fetcher$download_xts_data()
          
            # Create an instance of Alpha strategy 
            alpha <- AlphaEngine$new(data, threshold, profit_taking, signal_generation, position_sizing)

            # Store the results
            results[[paste(symbol, threshold, profit_taking, signal_generation, position_sizing, sep = "_")]] <- list(
              Symbol = symbol,
              Class = meta$assets[[symbol]]$class,
              Methodology = paste("AlphaEngine:", threshold, profit_taking, signal_generation, position_sizing),
              Threshold = threshold,
              ProfitTaking = profit_taking,
              Signal_generation  = signal_generation,
              Position_sizing = position_sizing,
              Performance = alpha$estimate_performance()
            )

            print(paste0("Results for ", "AlphaEngine: ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
              "threshold:", threshold, ",", "profit_taking:", profit_taking, ",", "signal_generation:", signal_generation, ",", "position sizing:", position_sizing, ")"))
                
                }
              }
            }
          }
        }
      }, error = function(e) {
        # Handle errors
        print(paste("Error in iteration", i, ":", e$message))
      })

      # Convert results to a data frame
      results_df <- map_dfr(names(results), ~{
        item <- results[[.x]]
        data_frame(
          Symbol = item$Symbol,
          Class = item$Class,
          Methodology = item$Methodology,
          Threshold = item$Threshold,
          ProfitTaking = item$ProfitTaking,
          Signal_generation = item$Signal_generation,
          Position_sizing = item$Position_sizing,
          Strategy = item$Performance$Strategy,
          aR = item$Performance$aR,
          aSD = item$Performance$aSD,
          IR = item$Performance$IR,
          MD = item$Performance$MD,
          trades = item$Performance$trades,
          avg_no_monthly_trades = item$Performance$avg_no_monthly_trades,
          buys = item$Performance$buys,
          sells = item$Performance$sells,
          Buy_Success_Rate = item$Performance$Buy_Success_Rate,
          Short_Success_Rate = item$Performance$Short_Success_Rate,
          Combined_Success_Rate = item$Performance$Combined_Success_Rate,
          PortfolioValue = item$Performance$PortfolioEndValue
        )
      })

      if (output_df) {
        return(results_df)
      } else {
        return(results)
      }
}
  
  ),

  private = list(

identify_events = function(data, threshold) {
  # Initialize event vector
  data <- data %>%
    mutate(
        mid = (High + Open) / 2
    )

  events <- numeric(nrow(data))
  
  # Initialize highest high and lowest low
  highest_high <- 0
  lowest_low <- Inf
  
  # Initialize event flag
  event_flag <- 1  # 1: Event start, -1: Event end
  events[1] <- 1  # Set the first value to 1 as the starting point
  
  # Initialize column for combined overshoot events
  data$OS <- ""
  
  # Initialize columns for highest_high and lowest_low
  data$highest_high <- NA
  data$lowest_low <- NA
  
  # Loop through each row of the data
  for (i in 1:nrow(data)) {
    # Check if event flag is 1
    if (event_flag == 1) {
      # Check condition for Event = 1 (Upturn)
      if (data$High[i] > highest_high) {
        highest_high <- data$High[i]
      }
      if (data$mid[i] <= highest_high * (1 - threshold)) {
        events[i] <- -1
        event_flag <- -1
        #lowest_low <- data$Low[i]
        highest_high <- data$High[i]  # Reset highest_high
        lowest_low <- Inf  # Reset lowest_low to infinity
      }
    } else {
      # Check condition for Event = -1 (Downturn)
      if (data$Low[i] < lowest_low) {
        lowest_low <- data$Low[i]
      }
      if (data$mid[i] >= lowest_low * (1 + threshold)) {
        events[i] <- 1
        event_flag <- 1
        #highest_high <- data$High[i]
        lowest_low <- data$Low[i]  # Reset lowest_low
        highest_high <- Inf
      }
    }
    
    # Update highest_high and lowest_low in the dataframe
    data$highest_high[i] <- highest_high
    data$lowest_low[i] <- lowest_low
  }
  
  # Initialize current state
  current_state <- NA
  
  # Assign OS values
  for (i in seq_along(events)) {
    if (events[i] == 1) {
      current_state <- "UOS"
    } else if (events[i] == -1) {
      current_state <- "DOS"
    }
    if (!is.na(current_state)) {
      data$OS[i] <- current_state
    }
    if (is.na(data$OS[i]) && !is.na(current_state) && i > 1) {
      data$OS[i] <- data$OS[i - 1]
    }
  }
  
  # Lag OS column
  data$OS <- lag(data$OS)
  
  # Return dataframe with events column
  result <- data.frame(data, events = events)
  
  # Calculate dc column
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  
  # Set default values
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)
},

estimate_prob_surprise = function(data) {
  # Step 1: Identify transitions in the price trajectory
  transitions <- data %>%
    mutate(state_i = ifelse(dc, "change", "overshoot"),
           state_j = lead(state_i)) %>%
    filter(!is.na(state_j)) %>%
    select(state_i, state_j)
  
  if (nrow(transitions) == 0) {
    return(tibble(probability = NA_real_, surprise = NA_real_))
  }
  
  # Step 2: Calculate transition probabilities
  transition_probs <- transitions %>%
    group_by(state_i, state_j) %>%
    summarise(count = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(probability = count / sum(count))
  
  # Step 3: Compute surprise for each transition
  transition_probs <- transition_probs %>%
    mutate(surprise = -log(probability))
  
  return(transition_probs)
},

estimate_entropies = function(data) {
  # Identify transitions
  entropy_transitions <- data %>%
    mutate(state_i = lag(dc),
           state_j = dc) %>%
    filter(!is.na(state_i)) %>%
    select(state_i, state_j)
  
  # Calculate transition probabilities for H(1)
  entropy_transition_probs <- entropy_transitions %>%
    group_by(state_i, state_j) %>%
    summarise(count = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(probability = count / sum(count))
  
  # Compute entropy rate H(1)
  H1 <- -sum(entropy_transition_probs$probability * log(entropy_transition_probs$probability))
  
  # Identify pairs of consecutive transitions for H(2)
  pairs <- entropy_transitions %>%
    mutate(state_i_2 = lag(state_i)) %>%
    filter(!is.na(state_i_2)) %>%
    select(state_i_2, state_i, state_j)
  
  # Calculate joint probabilities for each pair of consecutive transitions
  joint_probs <- pairs %>%
    group_by(state_i_2, state_i, state_j) %>%
    summarise(count = n(), .groups = "keep") %>%
    ungroup() %>%
    mutate(probability = count / sum(count))
  
  # Compute the entropy rate H(2)
  H2 <- -sum(joint_probs$probability * log(joint_probs$probability))
  
  # Create dataframe with entropies
  entropy_df <- data.frame(H1 = H1,
                           H2 = H2)
  
  return(entropy_df)
},

calculate_sequence_lengths = function(h) {
  uos_lengths <- numeric()  # Initialize vector to store UOS sequence lengths
  dos_lengths <- numeric()  # Initialize vector to store DOS sequence lengths
  #current_sequence <- NULL  # Initialize variable to store current sequence
  current_sequence <- ""
  
  for (event in h$OS) {
    if (is.null(current_sequence)) {
      current_sequence <- event  # Initialize current sequence if it's NULL
    } else if (current_sequence != event) {
      if (current_sequence == "UOS") {
        #uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
        uos_lengths <- c(uos_lengths, nchar(current_sequence))
      } else if (current_sequence == "DOS") {
        #dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
        dos_lengths <- c(dos_lengths, nchar(current_sequence))  # Store length of DOS sequence
      }
      current_sequence <- event  # Reset current sequence to new event
    } else {
      #current_sequence <- c(current_sequence, event)  # Add event to current sequence
      current_sequence <- paste(current_sequence, event, sep = "")
    }
  }
  
  # Add length of the last sequence
  if (!is.null(current_sequence)) {
    if (current_sequence == "UOS") {
      #uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
      uos_lengths <- c(uos_lengths, nchar(current_sequence))  # Store length of UOS sequence
    } else if (current_sequence == "DOS") {
      #dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
      dos_lengths <- c(dos_lengths, nchar(current_sequence))  # Store length of DOS sequence
    }
  }
  
  return(list(uos_lengths = uos_lengths, dos_lengths = dos_lengths))
},

calculate_OS_length = function(data) {
  # Initialize an empty vector to store OS lengths
  data$OS_length <- 0
  
  # Initialize variables to keep track of previous signal
  prev_signal <- NULL
  prev_os_length <- 0
  
  # Loop through each row
  for (i in 1:nrow(data)) {
    # Check if the signal changes from the previous row
    if (!is.null(prev_signal) && data$OS[i] != prev_signal) {
      # Reset OS length when the signal changes
      prev_os_length <- 0
    }
    
    # Increment OS length if the signal is UOS or DOS
    if (data$OS[i] == "UOS" || data$OS[i] == "DOS") {
      prev_os_length <- prev_os_length + 1
    } else {
      prev_os_length <- 0
    }
    
    # Update OS_length column
    data$OS_length[i] <- prev_os_length
    
    # Update previous signal for the next iteration
    prev_signal <- data$OS[i]
  }
  
  # Calculate signalOS based on OS_length and average UOS/DOS lengths
  data <- data %>% mutate(
    signalOS = ifelse(OS == "UOS" & OS_length >= avgOS$avgUOS, -1, 
                      ifelse(OS == "DOS" & OS_length >= avgOS$avgDOS, 1, 0))
  )
  
  return(data)
},

generateExitSignals = function(df, signal_generation = "TH") {
    
  df <- df %>%
    mutate(Exit = case_when(
      signal_generation == 'OS' ~ ExitOS,
      TRUE ~ Exit
    ))
  
  sell_exit_price <- c()
  buy_exit_price <- c()
  
  # Initialize signalE column with NA
  df$signalE <- NA
  
  # Initialize metrics to count removals
  sell_exit_removals <- 0
  buy_exit_removals <- 0
  
  # Initialize columns to store which row signalE closes and where exit prices are saved
  df$signalE_closes_row <- NA
  sell_exit_rows <- list()
  buy_exit_rows <- list()
  
  # Iterate over each row of the dataframe
  for (i in 1:nrow(df)) {
    if (!is.na(df$Exit[i]) && df$signal[i] == 1) {
      sell_exit_price <- c(sell_exit_price, df$Exit[i])
      sell_exit_rows <- c(sell_exit_rows, i)
    } else if (!is.na(df$Exit[i]) && df$signal[i] == -1) {
      buy_exit_price <- c(buy_exit_price, df$Exit[i])
      buy_exit_rows <- c(buy_exit_rows, i)
    }
    
    if (df$events[i] == 1) {
      if (any(df$Close[i] > sell_exit_price)) {
        first_index <- which.max(df$Close[i] > sell_exit_price)  # Get the index of the first TRUE value
        df$signalE[i] <- -1  # Set signalE = -1
        df$signalE_closes_row[i] <- sell_exit_rows[first_index]  # Store the index of the row where exit condition is met
        sell_exit_price <- sell_exit_price[-first_index]  # Remove the exit price at the first_index
        sell_exit_rows <- sell_exit_rows[-first_index]  # Remove the corresponding row index
        sell_exit_removals <- sell_exit_removals + 1  # Increment removal count
      }
    } else if (df$events[i] == -1) {
      if (any(df$Close[i] < buy_exit_price)) {
        first_index <- which.max(df$Close[i] < buy_exit_price)  # Get the index of the first TRUE value
        df$signalE[i] <- 1  # Set signalE = 1
        df$signalE_closes_row[i] <- buy_exit_rows[first_index]  # Store the index of the row where exit condition is met
        buy_exit_price <- buy_exit_price[-first_index]  # Remove the exit price at the first_index
        buy_exit_rows <- buy_exit_rows[-first_index]  # Remove the corresponding row index
        buy_exit_removals <- buy_exit_removals + 1  # Increment removal count
      }
    }
  }
  
  # Replace NA values in signalE with 0
  df$signalE[is.na(df$signalE)] <- 0
  
  # Calculate the ratio of removed entry signals to the total number of entry signals for sell exits
  total_sell_entry_signals <- sum(df$signal == 1)
  ratio_removed_to_total_sell <- sell_exit_removals / total_sell_entry_signals
  
  # Print the ratio for sell exits
  cat("Ratio of removed sell entry signals to total:", ratio_removed_to_total_sell, "\n")
  
  # Calculate the ratio of removed entry signals to the total number of entry signals for buy exits
  total_buy_entry_signals <- sum(df$signal == -1)
  ratio_removed_to_total_buy <- buy_exit_removals / total_buy_entry_signals
  
  # Print the ratio for buy exits
  cat("Ratio of removed buy entry signals to total:", ratio_removed_to_total_buy, "\n")
  
  return(df)
}

    )
)

################################################################################
# IMPLEMENTATION (class instances)
################################################################################

# Instances of AlphaEngine strategy
leverage <- 1
symbol <- "MXN=X"
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()

# Asset meta data (asset, symbol, class, description)
meta <- jsonlite::fromJSON("instr_config.json")
fxs <- names(Filter(function(x) x$class == "FX", meta$assets))

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

# Exchange rate evolution  over time
data_fetcher$plot_close_or_rets(type = "close")
data_fetcher$plot_close_or_rets(type = "rets")
data_fetcher$plot_close_or_rets(type = "rets_hist")
data_fetcher$compute_NA_close_price_ratio()

# Instance of AlphaEngine class given threshold
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005) # signal_generation by default is based on threshold
#alpha1$generate_signals()
alpha1$estimate_performance()
alpha1$plot_equity_lines(paste0("EventBased for ", symbol), signal_flag = TRUE)
alpha1$plot_events(symbol)
alpha1$plot_dc(symbol)
#a <- alpha1$data # check how the final data looks like

################################################################################
# PICK UP ONLY LEAST VOLATILE CURRENCIES
################################################################################

# Take least volatile currencies

# Downlaod all FX data
data_fetcher_mult <- DataFetcher$new(fxs, from_date, to_date, type = "Close")
df_fx <- data_fetcher_mult$convert_xts_to_wide_df()

# Compute standard deviations (volatility proxy) for each column except the first one (Date)
lapply(df_fx[-1, ], sd)
vol_df <- data.frame(FX = names(df_fx)[-1],
                        SD = unlist(lapply(df_fx[-1, -1], sd)))

# Rank the columns based on their standard deviation
vol_df <- vol_df[order(vol_df$SD), , drop = FALSE]

print(vol_df) 

# TOP5 most mean reverting ones (in descending order) are:
# EUR=X (USD/EUR)
# AUDUSD=X (AUD/USD))
# EURUSD=X (EUR/USD)
# CAD=X (USD/CAD)
# GBPUSD=X (GBP/USD)

##############################
# Test strategy
##############################
symbol <- "GBPUSD=X"
symbol <- "NZDJPY=X"
#from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
#from_date <- as.Date("2020-01-01", format = "%Y-%m-%d")
#to_date <- Sys.Date()
from_date <- as.Date("2006-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2015-01-01", format = "%Y-%m-%d")
##############################

# Download data from Yahoo (instances of DataFetcher class)
data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher$download_xts_data()

# Exchange rate evolution  over time
data_fetcher$plot_close_or_rets(type = "close")

# Instance of AlphaEngine class given threshold
# alpha1 <-  AlphaEngine$new(ts, threshold = 2.525729 * 0.01, profit_taking = 0.005, signal_generation = "TH")
# alpha1 <-  AlphaEngine$new(ts, threshold = 0.015, profit_taking = 0.005, signal_generation = "OS")
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH")
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE) # AR: 15%
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = TRUE) # AR: 15%

#alpha1$generate_signals()
alpha1$estimate_performance()
alpha1$plot_equity_lines(paste0("AlphaEngine for ", symbol), signal_flag = TRUE)
alpha1$plot_events(symbol)
alpha1$plot_dc(symbol)
alpha1$plotSignals()
a <- alpha1$data

##############################
# Run backtest:
 ##############################
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH", position_sizing = FALSE) # signal_generation by default is based on threshold
res_alpha <- alpha1$run_backtest(
  symbols = fxs,  
  thresholds = c(0.005, 0.01, 0.015, 0.02, 0.01 * 2.525729),
  profit_takings = c(0.0001, 0.001, 0.01),
  signal_generations = c("TH", "OS"),
  position_sizings = FALSE,
  from_date,
  to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)