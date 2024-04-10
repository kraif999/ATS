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

    return(ts)
    }
  )
)

# Download data from Yahoo (instances of DataFetcher class)
from_date <- as.Date("2007-01-01", format = "%Y-%m-%d")
to_date <- as.Date("2024-10-03", format = "%Y-%m-%d")
symbol <- "BZ=F"

data_fetcher_garch <- DataFetcher$new(symbol, from_date, to_date)
ts <- data_fetcher_garch$download_xts_data()

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

# Calculate portfolio value for each trading day
calculate_positions_and_equity_lines = function() {
      self$generate_signals()  # Call generate_signals dynamically

      # Active strategy
      self$data <- mutate(self$data, equity_line = cumsum(position * value))
      
      # Buy and hold
      self$data <- mutate(self$data, 
                          position_bh = 1,
                          equity_line_bh = cumsum(position_bh * value))
      # Filter out OHLC (used for GARCH based strategies only)
      return(self$data)
},

# Calculate cumulative return
calculate_cumulative_return = function() {
      #self$generate_signals()  # Call generate_signals dynamically
      cret = data.frame(
        Active = prod(1+self$data$value * self$data$position) - 1,
        Buy_and_hold = prod(1+self$data$value) - 1
      )
      return(cret)
},

# Performance measurement (Information Ratio) for active and buy and hold strategies
calculate_information_ratio = function() {

      self$data <- na.omit(self$data)
      # Calculate annualized returns
      aR <- (self$data$equity_line[length(self$data$equity_line)] / self$data$equity_line[1]) ^ (252 / length(self$data$equity_line)) - 1
      aR_bh <- (self$data$equity_line_bh[length(self$data$equity_line_bh)] / self$data$equity_line_bh[1]) ^ (252 / length(self$data$equity_line_bh)) - 1

      # Calculate annualized standard deviation
      aSD <- sd(self$data$equity_line) * sqrt(252)
      aSD_bh <- sd(self$data$equity_line_bh) * sqrt(252)

      # Create a data frame with information ratios for both strategies
      ir_df <- data.frame(
        Active_strategy = aR / aSD,
        Buy_and_Hold_Strategy_IR = aR_bh / aSD_bh)
    
      return(ir_df)

},

# Visualize equity lines for active strategy and passive (buy and hold)
plot_equity_lines = function() {
      # Plot equity lines
      ggplot(self$data, aes(x = Date)) +
        geom_line(aes(y = equity_line, color = "Active Strategy")) +
        geom_line(aes(y = equity_line_bh, color = "Buy and Hold Strategy")) +
        labs(title = "Equity Lines for Active (based on quantitative modeling) and Passive (buy-and-hold) Strategies",
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
                        left_join(select(volForHistRoll, Date, signal, position)) %>%
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

garch_strategy$calculate_positions_and_equity_lines()
garch_strategy$plot_equity_lines()
garch_strategy$calculate_cumulative_return()
garch_strategy$calculate_information_ratio()

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
sma1$calculate_positions_and_equity_lines()
sma1$plot_equity_lines()
sma1$calculate_cumulative_return()

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
sma2$calculate_positions_and_equity_lines()
sma2$plot_equity_lines()
sma2$calculate_cumulative_return()

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
sma1m$calculate_positions_and_equity_lines()
sma1m$plot_equity_lines()
sma1m$calculate_cumulative_return()

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
sma2m$calculate_positions_and_equity_lines()
sma2m$plot_equity_lines()
sma2m$calculate_cumulative_return()

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
rsi$generate_signals()  # Generate signals
rsi$calculate_positions_and_equity_lines()  # Calculate positions and equity lines
rsi$plot_avg_gain_loss_with_equity_lines()  # Plot average gain, average loss, and equity line
rsi$plot_equity_lines()

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
bol_br$generate_signals()  # Generate signals
bol_br$calculate_positions_and_equity_lines()  # Calculate positions and equity lines

# Plot equity lines for Bollinger Breakout
bol_br$plot_equity_lines()
bol_br$calculate_cumulative_return()

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
# Generate signals
vol_mean_rev$generate_signals()
# Calculate positions and equity lines
vol_mean_rev$calculate_positions_and_equity_lines()
# Plot equity lines for Volatility Mean Reversion
vol_mean_rev$plot_equity_lines()
# Calculate cumulative return
vol_mean_rev$calculate_cumulative_return()

