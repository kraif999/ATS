convert_xts_to_wide_df <- function(symbols, from_date, to_date, type = "rets") {

  dfs <- list()

  # Iterate through symbols to retrieve data and create data frames
  for (symbol in symbols) {
    # Fetch data for symbol
    ts <- getSymbols(symbol, from = from_date, to = to_date, period = "day", auto.assign = FALSE)
    ts <- na.omit(ts)
    
    data <- coredata(ts)

    dates <- index(ts)
    # close_price <- ts[, paste0(symbol, ".Close")]
    close_price <- as.numeric(data[,4])

    # Create data frame for the symbol
    if(type == "rets") {

    # Combine Date and log returns into a data.frame
    df <- data.frame(Date = as.Date(dates), 
                     rets = as.numeric(log(close_price/lag(close_price)))) %>%
                        na.omit %>%
                            unnest(rets)

    } else if (type == "Close") {
    df <- data.frame(Date = as.Date(dates), Close = close_price) %>% 
        na.omit %>%
            unnest

    } else {
      stop("Invalid value for 'type' argument. Choose 'rets' or 'Close'.")
    }
    
    # Store the data frame in the list
    dfs[[symbol]] <- df
  }

  # Combine all data frames into a single wide data frame
  wide_df <- bind_rows(dfs, .id = "symbol") %>%
    pivot_wider(names_from = "symbol", values_from = ifelse(type == "rets", "rets", "Close"), names_prefix = ifelse(type == "rets", "rets_", "Close_")) %>%
    na.omit()
  
  return(wide_df)
}

# Define a function to calculate Information Ratio for a given window_size
calculate_ir <- function(data, window_size) {
  # Create an instance of SMA1 with the current window_size
  strategy <- SMA1$new(data, window_size)
  
  # Calculate positions and equity lines
  strategy$calculate_positions_and_equity_lines()
  
  # Calculate Information Ratio
  ir_df <- strategy$calculate_information_ratio()
  
  # Add window_size column to the result dataframe
  ir_df$window_size <- window_size
  
  # Return Information Ratio for the current window_size
  return(ir_df)
}
