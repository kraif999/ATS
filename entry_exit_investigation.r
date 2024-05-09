data_fetcher <- DataFetcher$new("MXN=X", from_date, to_date)
ts <- data_fetcher$download_xts_data()

ts_df <- data.frame(ts)
ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything()) %>%
            na.omit() %>% 
                as_tibble()


# STEP1 : DETECT DIRECTIONAL CHANGES

identify_events <- function(data, threshold) {
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
        lowest_low <- data$Low[i]
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
        highest_high <- data$High[i]
        lowest_low <- data$Low[i]  # Reset lowest_low
      }
    }
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
  result$dc <- ifelse(c(FALSE, diff(na.locf(ifelse(result$events == 0, NA, result$events)))) != 0, TRUE, FALSE)
  result$OS[1] <- "" # no overshoots since it is the first value
  result$dc[1] <- TRUE # default
  
  return(result)

}

# adding extremes to df
identify_events <- function(data, threshold) {
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
}

# prototype
prot <- identify_events(ts_df, 0.01)

table(prot$dc)

# STEP2 : INTRODUCE PROBABILITY INDICATOR FOR POSITION SIZING

# Function to compute transition probabilities and surprises
estimate_prob_surprise <- function(data) {
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
}

# Function to compute entropies
estimate_entropies <- function(data) {
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
}

# prototype
prot$surprise <- NA_real_
prot$H1 <- NA_real_
prot$H2 <- NA_real_

# Loop over rows to compute probability and surprise for each row
for (i in 1:nrow(prot)) {

    data <- slice(prot, 1:i)  # Subset data up to current row
    probs_surprises <- estimate_prob_surprise(data)  # Compute probabilities and surprises
    entropies <- estimate_entropies(data)

    prot$surprise[i] <- sum(probs_surprises$surprise) # surprise of the transition
    prot$H1[i] <- entropies$H1
    prot$H2[i] <- entropies$H2
}

# Initialize K with 1
prot$K <- 1

# Initialize a vector to store unique combinations
unique_combinations <- c(paste(prot$events[1], prot$j[1]))
prot$j <- lag(prot$events, default = 0)

# Iterate over rows and update K when a new combination is identified
for (i in 2:nrow(prot)) {
  current_combination <- paste(prot$dc[i], prot$j[i])
  if (!(current_combination %in% unique_combinations)) {
    unique_combinations <- c(unique_combinations, current_combination)
  }
  prot$K[i] <- min(length(unique_combinations), 4)
  if (length(unique_combinations) > 4) {
    prot$K[i:nrow(prot)] <- 4
    break
  }
}

#View(prot)

# estimate L
prot <- prot %>%
  mutate(
    surprise = replace_na(surprise, 0),
    d = (surprise - K * H1) / sqrt(K * H2),
    L = 1 - pnorm(d)
  )

##############################
# ENTRY SIGNAL GENERATION BASED ON THRESHOLD (if overshoot moves further the original threshold)
##############################

colnames(prot)

# Identify threshold value (which is used to compare current mid price)

th <- 0.005 # assymetric
# Initialize an empty vector to store mid prices for the change_value
prot$change_value <- NA

# Initialize variables
prev_dc_index <- NULL
prev_mid <- NULL

# Loop through each row
for (i in 1:nrow(prot)) {
  # Check if dc is TRUE
  if (prot$dc[i]) {
    # If this is the first dc or if it's not consecutive with the previous one
    if (is.null(prev_dc_index) || i != prev_dc_index + 1) {
      prev_dc_index <- i  # Update prev_dc_index
      prev_mid <- prot$mid[i]  # Record the mid price
    }
  }
  
  # Assign the previous mid price to the change_value column
  prot$change_value[i] <- prev_mid
  
  # Check if the price further changes by the threshold and update prev_mid accordingly
  if (!is.na(prev_mid) && !is.na(prot$OS[i])) {
    if (prot$OS[i] == "UOS" && prot$mid[i] > prev_mid * (1 + th)) {
      prev_mid <- prot$mid[i]  # Update prev_mid for UOS
    } else if (prot$OS[i] == "DOS" && prot$mid[i] < prev_mid * (1 - th)) {
      prev_mid <- prot$mid[i]  # Update prev_mid for DOS
    }
  }
}

prot <- prot %>%
  mutate(
    row_number = row_number(),
    signal = case_when(
      OS == "UOS" & mid >= change_value * (1 + th) ~ -1,
      OS == "DOS" & mid <= change_value * (1 - th) ~ 1,
      TRUE ~ 0
    ),
    position = lag(signal, default = 0),
    OSS = mid >= change_value * (1 + th) | mid <= change_value * (1 - th) 
  ) %>% 
    select(Date, High, Low, Close, mid, change_value, OS, dc, events, L, signal, OSS, position, row_number)

View(prot)

entries <- prot %>% 
        filter(signal == 1 | signal == -1) %>%
            mutate(
                change = c(2, abs(diff(signal))), default = 0,
                Exit = case_when(
                    signal == -1 ~ mid * (1 - th),
                    signal == 1 ~ mid * (1 + th),
                    TRUE ~ 0
                    )
                )

prot <- prot %>%
  mutate(in_entries = row_number() %in% entries$row_number,
  signal = ifelse(in_entries, signal, 0),
  position = lag(signal, default = 0)
  ) %>% 
    left_join(entries %>% select(row_number, Exit), by = c("row_number" = "row_number"))

# Plot Close price
p <- ggplot(prot, aes(x = Date, y = Close)) +
  geom_line() +  # Plot the Close price as a line
  labs(title = "Close Price") +
  theme_minimal()

# Highlight buy and sell signals
p + geom_point(data = subset(prot, signal == 1), aes(x = Date, y = Close), color = "blue", size = 1.5) +  # Highlight buy signals in blue
  geom_point(data = subset(prot, signal == -1), aes(x = Date, y = Close), color = "red", size = 1.5) +  # Highlight sell signals in red
  theme_minimal()

#View(prot) # here Exits are in the prot2

##############################
# ENTRY SIGNAL GENERATION BASED ON OVERSHOOT DURATION (enter when the number of observed OS in a sequence exceeds or equal to the average OS based on the whole dataset)
##############################

# overshoot duration
calculate_sequence_lengths <- function(h) {
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
}

# Calculate sequence lengths for UOS and DOS
sequence_lengths <- calculate_sequence_lengths(prot)

avgOS <- data.frame(avgUOS = floor(mean(sequence_lengths$uos_lengths)), avgDOS = floor(mean(sequence_lengths$dos_lengths)))

calculate_OS_length <- function(data) {
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
}

# Usage:
prot <- calculate_OS_length(prot)
prot <- prot %>%
    mutate(
        ExitOS = case_when(
        signalOS == -1 ~ mid * (1 - th),
        signalOS == 1 ~ mid * (1 + th),
        TRUE ~ 0
        )
    )

subset <- prot %>% head(100)
fwrite(subset, "subset.csv")

############### EXIT SIGNAL GENERATION ###############
############### COASTLINE TRADING $$$$$$$$$$$$$$$$$$$

generateExitSignals <- function(data, signal_generation) {
  # Initialize exit signals column
  data$signalE <- 0
  
  # Initialize exit_value_downward and exit_value_upward
  exit_value_downward <- NA
  exit_value_upward <- NA
  
  # Initialize flags for current run direction
  current_run <- "upward"
  
  # Modifications for further calculations
  data$signal[data$signal == 0] <- NA
  data <- data %>% mutate(signal = na.locf(signal, na.rm = FALSE))
  data$signal <- na.locf(data$signal, na.rm = FALSE, fromLast = TRUE)
  
  # Choose Exit column based on signal_generation
  if (signal_generation == 'OS') {
    data$Exit <- na.locf(data$ExitOS, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  } else {
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  }
  
  # Loop through each row
  for (i in 1:nrow(data)) {
    
    # Check if Exit is not NA
    if (!is.na(data$Exit[i])) {
      
      # Set exit value for the respective directional run
      if (data$signal[i] == -1) {
        exit_value_downward <- data$Exit[i]  # Set exit value for downward run
        current_run <- "upward"  # Update current run direction
      } else if (data$signal[i] == 1) {
        exit_value_upward <- data$Exit[i]  # Set exit value for upward run
        current_run <- "downward"  # Update current run direction
      }
      
      # Check for exit condition during downward run
      if (current_run == "downward" && !is.na(exit_value_downward) && data$mid[i] < exit_value_downward) {
        data$signalE[i] <- 1  # Close sell signal with buy signal during downward run
        cat("Exit condition met for downward run at row:", i, "\n")
      }
      
      # Check for exit condition during upward run
      if (current_run == "upward" && !is.na(exit_value_upward) && data$mid[i] > exit_value_upward) {
        data$signalE[i] <- -1  # Close buy signal with sell signal during upward run
        cat("Exit condition met for upward run at row:", i, "\n")
      }
    } 
  }
  
  # Reset exit values and current run direction after loop
  exit_value_downward <- NA
  exit_value_upward <- NA
  current_run <- "upward"
  
  return(data)
}

generateExitSignals2 <- function(data, signal_generation) {
  # Initialize exit signals column
  data$signalE <- 0
  
  # Initialize exit_value_downward and exit_value_upward
  exit_value_downward <- NA
  exit_value_upward <- NA
  
  # Initialize flags for current run direction
  current_run <- "upward"
  
  # Modifications for further calculations
  data$signal[data$signal == 0] <- NA
  data <- data %>% mutate(signal = na.locf(signal, na.rm = FALSE))
  data$signal <- na.locf(data$signal, na.rm = FALSE, fromLast = TRUE)
  if (signal_generation == 'OS') {
    data$Exit <- na.locf(data$ExitOS, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  } else {
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
    data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  }
  
  # Loop through each row
  for (i in 1:nrow(data)) {
    
    # Check if Exit is not NA
    if (!is.na(data$Exit[i])) {
      
      # Set exit value for the respective directional run
      if (data$signal[i] == -1) {
        exit_value_downward <- data$Exit[i]  # Set exit value for downward run
        current_run <- "upward"  # Update current run direction
      } else if (data$signal[i] == 1) {
        exit_value_upward <- data$Exit[i]  # Set exit value for upward run
        current_run <- "downward"  # Update current run direction
      }
      
      # Check for exit condition during downward run
      if (current_run == "downward" && !is.na(exit_value_downward) && data$mid[i] < exit_value_downward) {
        data$signalE[i] <- 1  # Close sell signal with buy signal during downward run
        cat("Exit condition met for downward run at row:", i, "\n")
        # Find the original line corresponding to this exit
        original_line <- which(data$events == -1 & data$OS == "DOS" & data$Exit == exit_value_downward)
        if (length(original_line) > 0) {
          cat("Exit condition corresponds to the original line:", original_line, "\n")
        }
      }
      
      # Check for exit condition during upward run
      if (current_run == "upward" && !is.na(exit_value_upward) && data$mid[i] > exit_value_upward) {
        data$signalE[i] <- -1  # Close buy signal with sell signal during upward run
        cat("Exit condition met for upward run at row:", i, "\n")
        # Find the original line corresponding to this exit
        original_line <- which(data$events == 1 & data$OS == "UOS" & data$Exit == exit_value_upward)
        if (length(original_line) > 0) {
          cat("Exit condition corresponds to the original line:", original_line, "\n")
        }
      }
    } 
  }
  
  # Reset exit values and current run direction after loop
  exit_value_downward <- NA
  exit_value_upward <- NA
  current_run <- "upward"
  
  return(data)
}

generateExitSignals <- function(df, signal_generation = "TH") {
    
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

#prot <- generateExitSignals(prot, signal_generation = "TH")
prot2 <- generateExitSignals(prot, signal_generation = "TH")
subset <- generateExitSignals(subset, signal_generation = "TH")

exits <- prot2 %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    )

exits <- prot2 %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    ) %>% 
                        filter(change == 2)

#View(exits)


# PLOT ENTRIES (GREEN) AND EXITS (RED)

prot2 <- prot2 %>%
  mutate(
    out_exits = row_number() %in% exits$row_number,
    signal = if_else(in_entries, signal, 0),
    signalE = ifelse(signalE != 0, signalE, 0),
    signalE = if_else(out_exits, signalE, 0),
    signal = if_else(signal != signalE & signalE != 0, signalE, signal),
    #signal = if_else(signal != signalE & signalE != 0, signalE, signal), # signals cancel entry and exit
    position = lag(signal, default = 0)
  ) %>% 
    select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, 
        signal, signalOS, signalE, signalE_closes_row, L, position, in_entries, out_exits, Exit, OS_length)


prot3 <- prot2 %>%
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
  select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, 
         signal, signalOS, signalE, signalE_closes_row, L, nop_sizing, position, in_entries, out_exits, Exit, OS_length)


View(prot3) # final table with signals

plotSignals <- function(data) {
  ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_point(data = subset(data, signal == 1), aes(color = "Buy", shape = "Buy"), size = 1.5) +
    geom_point(data = subset(data, signal == -1), aes(color = "Sell", shape = "Sell"), size = 1.5) +
    geom_point(data = subset(data, signalE == 1), aes(color = "Exit Buy", shape = "Exit Buy"), size = 1.5) +
    geom_point(data = subset(data, signalE == -1), aes(color = "Exit Sell", shape = "Exit Sell"), size = 1.5) +
    scale_shape_manual(values = c("Buy" = 1, "Sell" = 2, "Exit Buy" = 3, "Exit Sell" = 4)) +
    scale_color_manual(values = c("Buy" = "green", "Sell" = "red", "Exit Buy" = "darkgreen", "Exit Sell" = "darkred")) +
    labs(title = "Close Price") +
    theme_minimal()
}

# Example usage:
plotSignals(prot2)

##################### position sizing given volatility (standard deviation as proxy) ########################
colnames(prot3)
prot3 %>% head

sd(prot3$Close)

prot4 <- prot3 %>%
  mutate(
    rollSD = rollapply(Close, width = 21 * 3, FUN = sd, fill = NA, align = "right"),
    rollSD75  = rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE), align = "right", fill = NA),
    rollSD95  = rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.95, na.rm = TRUE), align = "right", fill = NA),
    rollSD999  = rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.999, na.rm = TRUE), align = "right", fill = NA),
    vol_nop_sizing = case_when(
        rollSD >= rollSD75 ~ 0.5,
        rollSD >= rollSD95 ~ 0.2,
        TRUE ~ 1
    )
  )

table(prot4$vol_nop_sizing) / (240+688)

View(prot4)

###################################################
# FX BUCKET (POSITION SIZING GIVEN CORRELATION STRUCTURE)
# method to construct a portfolio (FX components only, new method in Alpha class)
# plot rolling correlations (new method in Alpha class)
# estimate performance (new method in Strategy class)
###################################################

# Take 3 currencies pairs:
symbol1 <- "GBPUSD=X"
symbol2 <- "NZDJPY=X"
symbol3 <- "CAD=X"
from_date <- as.Date("2020-01-01", format = "%Y-%m-%d")
to_date <- Sys.Date()

# Download data
data_fetcher1 <- DataFetcher$new(symbol1, from_date, to_date)
ts1 <- data_fetcher1$download_xts_data()

data_fetcher2 <- DataFetcher$new(symbol2, from_date, to_date)
ts2 <- data_fetcher2$download_xts_data()

data_fetcher3 <- DataFetcher$new(symbol3, from_date, to_date)
ts3 <- data_fetcher3$download_xts_data()

# check performance
a1$estimate_performance()
a2$estimate_performance()
a3$estimate_performance()

a1 <-  AlphaEngine$new(ts1, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE)
a1 <- a1$generate_signals()
a2 <- AlphaEngine$new(ts2, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE)
a2 <- a2$generate_signals()
a3 <- AlphaEngine$new(ts3, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE)
a3 <- a3$generate_signals()

colnames(a1)

# List of data frames
data_list <- list(a1, a2, a3)

# Add capital, number of positions

# Estimate volatility: rollling standard deviation
estimate_vol <- function(data) {
  data <- data %>%
    mutate(
      rollSD = rollapply(mid, width = 21 * 3, FUN = sd, fill = NA, align = "right"),
      rollSD_q = as.vector(rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE), align = "right", fill = NA)),
      vol_nop_sizing = case_when(
        rollSD >= rollSD_q & is.na(Exit) ~ 0.5,
        TRUE ~ 1
      )
    )
  
  return(data)
}

# Loop through each data frame and apply the estimate_vol function
for (i in seq_along(data_list)) {
  data_list[[i]] <- estimate_vol(data_list[[i]])
}

select_cols <- function(data) {
  data <- data %>%
    # mutate(
    #   capital = 0,
    #   capital = ifelse(row_number() == 1, 50000 / length(data_list), capital),
    #   nop = 0,
    #   nop = ifelse(row_number() == 1, capital[1] / Close[1], nop),
    #   sizing = 0,
    #   pnl = 0,
    #   eql = 0
    # ) %>%
    select(Date, Close, value, signal, position, rollSD, rollSD_q)
    return(data)
}

data_list <- lapply(data_list, select_cols)

# Unpack the modified data frames back to individual objects if needed
a <- data_list[[1]]
colnames(a) 
# a2 <- data_list[[2]]
# a3 <- data_list[[3]]

# colnames(a1)[-1] <- map(colnames(a1)[-1], ~ paste(symbol1, .x, sep = "_"))
# colnames(a2)[-1] <- map(colnames(a2)[-1], ~ paste(symbol2, .x, sep = "_"))
# colnames(a3)[-1] <- map(colnames(a3)[-1], ~ paste(symbol3, .x, sep = "_"))

# Define symbols
symbol_list <- list(symbol1, symbol2, symbol3)
# Apply the column renaming operation to each data frame in the list

data_list <- lapply(seq_along(data_list), function(i) {
  symbol <- symbol_list[[i]]
  colnames(data_list[[i]])[-1] <- paste(symbol, colnames(data_list[[i]])[-1], sep = "_")
  return(data_list[[i]])
})

a1 <- data_list[[1]]
View(a1)

#merged_df <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), list(a1, a2, a3))
merged_df <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)
colnames(merged_df)
# Example close columns
close_columns <- c("GBPUSD=X_Close", "NZDJPY=X_Close", "CAD=X_Close")

# Create all possible unique combinations of close columns
column_combinations <- combn(close_columns, 2)

column_combinations_df <- as.data.frame(t(column_combinations))

# Add a column indicating the pair number
column_combinations_df$Pair <- paste0("Pair", seq_len(ncol(column_combinations)))

# Print the combinations
print(column_combinations_df)
column_combinations_df$V1

# Assuming merged_df is your merged data frame containing all necessary data

# Define a function to compute rolling correlations for a pair of columns
compute_rolling_correlation <- function(data, pair, cor_window) {
  # Extract column names from the pair
  col1 <- as.character(pair[1])
  col2 <- as.character(pair[2])
  
  # Compute rolling correlation for the pair
  data <- data %>%
    mutate(!!paste0("Cor_", col1, ";", col2) := rollapply(data[, c(col1, col2)], 
                                                      width = cor_window, 
                                                      FUN = function(x) cor(x, use = "pairwise.complete.obs")[col1, col2], 
                                                      by.column = FALSE, 
                                                      align = "right", 
                                                      fill = NA)
    )
  
  # Return the updated data frame
  return(data[ncol(data)])
}

# Compute rolling correlations for each pair and bind them to the original data frame
res_cor <- bind_cols(merged_df, lapply(1:nrow(column_combinations_df), function(i) {
  # Get the pair
  pair <- column_combinations_df[i, 1:2]
  
  # Compute rolling correlations for the pair
  compute_rolling_correlation(merged_df, pair, 365/2)
}))

# Plot
cor_columns <- c("Cor_GBPUSD=X_Close;NZDJPY=X_Close", 
                 "Cor_GBPUSD=X_Close;CAD=X_Close", 
                 "Cor_NZDJPY=X_Close;CAD=X_Close")

# Subset the data frame to include only the date and rolling correlation columns
cor_data <- res_cor[, c("Date", cor_columns)]

# Melt the data frame to long format for easier plotting
cor_data_long <- tidyr::gather(cor_data, Pair, Rolling_Correlation, -Date)

# Plot rolling correlations using ggplot
ggplot(cor_data_long, aes(x = Date, y = Rolling_Correlation, color = Pair)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +  # Add dashed horizontal line at y = 0.5
  geom_hline(yintercept = -0.5, linetype = "dashed") +  # Add dashed horizontal line at y = 0.5
  labs(title = "Rolling Correlations",
       x = "Date",
       y = "Rolling Correlation") +
  theme_minimal()

# Now, `rolling_correlations` contains a list of rolling correlations for each pair
# Each element of the list corresponds to one pair, and it contains the rolling correlations over time

b <- compute_rolling_correlation(merged_df, pair)

b[ncol(b)] %>% head

plot(b$`Cor_GBPUSD=X_Close;NZDJPY=X_Close`, type = 'l')
plot(a1$`GBPUSD=X_rollSD`, type = 'l')
plot(res_cor$`NZDJPY=X_rollSD`, type = 'l')
plot(res_cor$`Cor_NZDJPY=X_Close;CAD=X_Close`)

pair <- column_combinations_df[1, 1:2]

c <- merged_df[, c(col1$V1, col2$V2)]

d <- cor(c, use = "pairwise.complete.obs")[col1$V1, col2$V2]

d[col1$V1, col2$V2]

merged_df[, "GBPUSD=X_Close"]
merged_df[, col1$V1]

######################################## 
# Add CAPITAL, number of positions, sizing, pnl, eql
########################################

View(res_cor)

p <- plot(res_cor$`Cor_NZDJPY=X_Close;CAD=X_Close`)

###############################################
# Capital allocation given rolling correlations
###############################################

# Extract correlation values and symbols from column names
correlation_cols <- c("Date", grep("^Cor_", colnames(res_cor), value = TRUE))
symbols <- gsub("^Cor_", "", correlation_cols[-1])

rm(DecisionCor)
# Initialize an empty data frame to store correlation decisions
DecisionCor <- data.frame(
  Date = as.Date(character()),
  Symbol1 = character(),
  Symbol2 = character(),
  cor_value = numeric(),
  cor_type = character(),
  signalCor = character(),
  iteration = integer(),
  stringsAsFactors = FALSE
)

# Initialize iteration counter
iteration_i <- 0

res_cor <- res_cor %>%
    na.omit()

# Run the loop over rows of res_cor
for (i in 2:nrow(res_cor)) {  # Start from the second row since we need to compare with the previous row
  iteration_i <- iteration_i + 1
  
  # Check correlations between different pairs of symbols
  correlations <- sapply(correlation_cols, function(col) res_cor[i, col])

  # Identify the pair with the highest and lowest correlation
  highest_cor <- max(abs(correlations[-1]))
  lowest_cor <- min(abs(correlations[-1]))
  
  # Record pairs and correlations in the DecisionCor data frame
  highest_cor_pair <- symbols[which.max(correlations[-1])]
  lowest_cor_pair <- symbols[which.min(correlations[-1])]
  
  # Add cor_value, cor_type, and iteration columns to DecisionCor
  DecisionCor <- rbind(DecisionCor, data.frame(
    Date = as.Date(correlations[1]),
    Symbol1 = strsplit(highest_cor_pair, ";")[[1]][1],
    Symbol2 = strsplit(highest_cor_pair, ";")[[1]][2],
    cor_value = highest_cor,
    #cor_type = ifelse(abs(highest_cor) >= 0.8, "highest", ifelse(abs(highest_cor) < 0.5, "lowest", 0)),
    cor_type = "highest",
    signalCor = ifelse(abs(highest_cor) >= 0.8, -1, ifelse(abs(highest_cor) < 0.5, 1, 0)),
    iteration = iteration_i
  ))
  DecisionCor <- rbind(DecisionCor, data.frame(
    Date = as.Date(correlations[1]),
    Symbol1 = strsplit(lowest_cor_pair, ";")[[1]][1],
    Symbol2 = strsplit(lowest_cor_pair, ";")[[1]][2],
    cor_value = lowest_cor,
    #cor_type = ifelse(abs(lowest_cor) >= 0.8, "highest", ifelse(abs(lowest_cor) < 0.5, "lowest", 0)),
    cor_type = "lowest",
    signalCor = ifelse(abs(lowest_cor) >= 0.8, -1, ifelse(abs(lowest_cor) < 0.5, 1, 0)),
    iteration = iteration_i
  ))
}

# Generate signals based on recorded pairs and correlations
for (i in 1:nrow(DecisionCor)) {
  symbol1 <- DecisionCor[i, "Symbol1"]
  symbol2 <- DecisionCor[i, "Symbol2"]
  signal <- as.numeric(DecisionCor[i, "signalCor"])
  
  # Generate signals for Symbol1
  res_cor[[paste0(symbol1, "_signal")]] <- ifelse(res_cor[[paste0("Cor_", symbol1)]] == DecisionCor$cor_value[i], signal, 0)
  # Generate signals for Symbol2
  res_cor[[paste0(symbol2, "_signal")]] <- ifelse(res_cor[[paste0("Cor_", symbol2)]] == DecisionCor$cor_value[i], signal, 0)
}

strsplit("GBPUSD=X_Close;CAD=X_Close" , ";")[[1]][1]

#########################
# split res_cor into 
#########################
# Strategy class
#########################

# Split res_cor into data frames based on symbol_list
symbol_dataframes <- lapply(symbol_list, function(symbol) {
  # Filter columns related to the current symbol
  symbol_columns <- grep(paste0("^", symbol), names(res_cor), value = TRUE)
  symbol_df <- res_cor %>%
    select(Date, all_of(symbol_columns)) %>%
    rename_with(~ gsub(paste0("^", symbol), "", .x), contains(symbol)) %>%
    rename_all(~ gsub("^_", "", .))  # Remove underscore from the beginning of column names
  
  # Rename the Date column to avoid conflicts
  colnames(symbol_df)[1] <- "Date"
  
  return(symbol_df)
})

# Rename list elements with symbol names
names(symbol_dataframes) <- symbol_list

# Print the first few rows of each data frame in the list
lapply(symbol_dataframes, head) # ok

######### process the data and plot equity lines  #########

# Function to perform the required calculations
process_data <- function(df) {
  # Calculate pnlActive and pnlPassive
  df <- df %>% 
    mutate(
      pnlActive = c(0, diff(Close) * signal[-length(Close)]),
      pnlPassive = c(0, diff(Close)),
      nopActive = 0,
      nopPassive = 0,
      eqlActive = 0,
      eqlPassive = 0
    )

  # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
  df$eqlActive[1] <- capital
  df$nopActive[1] <- floor(capital / (df$Close[1] / leverage))
  df$eqlPassive[1] <- capital
  df$nopPassive[1] <- floor(capital / (df$Close[1] / leverage))   

  # Check if "L" column (probability indicator) exists
  has_L <- "nop_sizing" %in% names(df)
  has_Vol <- "vol_nop_sizing" %in% names(df)

  for (i in 2:nrow(df)) {
    # Active
    pnlActive <- df$pnlActive[i]
    prev_nop_Active <- floor(df$nopActive[i - 1])
    current_nop_Active <- floor((df$eqlActive[i - 1]) / (df$Close[i] / leverage))
    df$eqlActive[i] <- df$eqlActive[i - 1] + prev_nop_Active * pnlActive

    # Calculate nopActive based on the presence of the "L" column
    if (has_L) {
      df$nopActive[i] <- ifelse(
        df$L[i], 
        current_nop_Active * df$nop_sizing[i], 
        current_nop_Active
      )
    } else if (has_Vol) {
      df$nopActive[i] <- ifelse(
        df$L[i], 
        current_nop_Active * df$vol_nop_sizing[i], 
        current_nop_Active
      )
    } else {
      df$nopActive[i] <- current_nop_Active
    }

    # Passive
    pnlPassive <- df$pnlPassive[i]
    prev_nop_Passive <- floor(df$nopPassive[i - 1])
    current_nop_Passive <- floor((df$eqlPassive[i - 1]) / (df$Close[i] / leverage))
    df$eqlPassive[i] <- df$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
    df$nopPassive[i] <- current_nop_Passive
  }

  df <- df %>%
    mutate(r_eqlActive = quantmod::Delt(eqlActive),
           r_eqlPassive = quantmod::Delt(eqlPassive))
  return(df)
}

# Apply the function to each data frame in the list
processed_data <- lapply(symbol_dataframes, process_data)

# Print the first few rows of each processed data frame
lapply(processed_data, head)

# Estimate performance
# Initialize an empty list to store the results
results_list <- list()

# Define a function to compute performance metrics for each currency pair
estimate_performance <- function(data) {

 # Performance metrics for active strategy
  aR_active <- round(as.numeric(Return.annualized(as.numeric(data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data$r_eqlActive), scale = 252) * 100), 3)
  IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
  MD_active <- round(as.numeric(maxDrawdown(as.numeric(data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  trades_active <- sum(diff(data$signal) != 0)
  buy_active <- sum(data$signal == 1)
  short_active <- sum(data$signal == -1)

  # Performance metrics for passive strategy
  aR_passive <- round(as.numeric(Return.annualized(as.numeric(data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data$r_eqlPassive), scale = 252) * 100), 3)
  IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
  MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  trades_passive <- 1
  buy_passive <- 1
  short_passive <- 0
  
  # Calculate success rates
  buy_success_rate_active <- sum(data$position > 0 & data$value > 0) / nrow(data)
  buy_success_rate_passive <- sum(data$value > 0) / nrow(data)
  short_success_rate_active <- sum(data$position < 0 & data$value < 0) / nrow(data)
  short_success_rate_passive <- 0
  combined_rate_active <- (sum(data$position > 0 & data$value > 0) + sum(data$position < 0 & data$value < 0)) / nrow(data)
  combined_rate_passive <- (sum(data$value > 0)) / nrow(data)
  
  # Unique months
  unique_months <- length(unique(format(data$Date, "%Y-%m")))
  
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
    PortfolioEndValue = round(c(data[nrow(data),]$eqlActive, data[nrow(data),]$eqlPassive), 0)
  )
  
  return(df)
}

for (pair in symbol_list) {
  data <- processed_data[[pair]]  # Assuming data for each currency pair is available in the environment
  results_list[[pair]] <- estimate_performance(data)
}

# View the results
results_list

# Define symbols
symbol_list <- names(processed_data)

# Apply the column renaming operation to each data frame in the list
data_list <- lapply(seq_along(processed_data), function(i) {
  symbol <- symbol_list[i]
  colnames(processed_data[[i]])[-1] <- paste(symbol, colnames(processed_data[[i]])[-1], sep = "_")
  return(processed_data[[i]])
})

# Merge the modified data frames
p <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)

# Print the merged data frame
p %>% head

# Mutate to add a new column summing all columns containing "eqlActive" but not "r_eqlActive"
p <- p %>%
  mutate(
    AlleqlActive = rowSums(select(., contains("eqlActive"), -contains("r_eqlActive")), na.rm = TRUE),
    AlleqlPassive = rowSums(select(., contains("eqlPassive"), -contains("r_eqlPassive")), na.rm = TRUE)
  )

colnames(p)


################ Plot all individual equity lines ################
# Filter columns containing "eqlActive"
eqlActive_columns <- p %>% select(grep("eqlActive", colnames(p), value = TRUE)) %>%
                      select(-contains("r_eqlActive")) %>%
                        select(-contains("AlleqlActive"))

# Add Date column
eqlActive_columns$Date <- p$Date

# Melt the data frame to long format for easier plotting
eqlActive_melted <- melt(eqlActive_columns, id.vars = "Date")

# Plot using ggplot
ggplot(data = eqlActive_melted, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(

    title = "AlphaEngine performance of FX portfolio",
    x = "Date",
    y = "eqlActive Values", 
    color = "eqlActive Columns") +
  
  theme_minimal()

# Plot using ggplot
ggplot(p, aes(x = Date)) +
  geom_line(aes(y = AlleqlActive), color = "red") +
  geom_line(aes(y = AlleqlPassive), color = "green") +
  labs(x = "Date", y = "Value") +
  scale_color_identity(name = "Legend", labels = c("Active", "Passive"), guide = "legend") +
  theme_minimal()

# compute performance of the overall portfolio

# individual:

for (pair in symbol_list) {
  data <- processed_data[[pair]]  # Assuming data for each currency pair is available in the environment
  results_list[[pair]] <- estimate_performance(data)
}

results_list

# bucket:

estimate_performance_bucket <- function(data) {

  data <- data %>%
    mutate(
        r_AlleqlActive = as.numeric(quantmod::Delt(AlleqlActive)),
        r_AlleqlPassive = as.numeric(quantmod::Delt(AlleqlPassive))
        )
  # Performance metrics for active strategy
  aR_active <- round(as.numeric(Return.annualized(as.numeric(data$r_AlleqlActive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data$r_AlleqlActive), scale = 252) * 100), 3)
  IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
  MD_active <- round(as.numeric(maxDrawdown(as.numeric(data$r_AlleqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  
  # Performance metrics for passive strategy
  aR_passive <- round(as.numeric(Return.annualized(as.numeric(data$r_AlleqlPassive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data$r_AlleqlPassive), scale = 252) * 100), 3)
  IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
  MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data$r_AlleqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  
  # Create performance dataframe
  df <- data.frame(
    Strategy = c("Active", "Passive"),
    aR = c(aR_active, aR_passive),
    aSD = c(aSD_active, aSD_passive),
    IR = c(IR_active, IR_passive),
    MD = c(MD_active, MD_passive),
    PortfolioEndValue = c(round(data[nrow(data), ]$AlleqlActive, 0), round(data[nrow(data), ]$AlleqlPassive, 0))
  )
  
  return(df)
}

estimate_performance_bucket(p)

# clean up the prototype, create methods in Strategy class and AlphaEngine (public and private)
# Update README file, add description of classes structure and update README description
# Pull to main branch Alpha (TBD)

################################## clean and add to AlphaEngine class ##################################

# Parameters: sd_quantile, sd_window, cor_window
estimate_rolling_correlations <- function(symbol_list, from_date, to_date) {

  data_list <- list()
  
  # Fetch data and create AlphaEngine instances for each symbol
  for (i in seq_along(symbol_list)) {
    symbol <- symbol_list[i]
    # Download data
    data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
    ts <- data_fetcher$download_xts_data()
    
    # Create AlphaEngine instance
    alpha_engine <- AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, 
                                     signal_generation = "TH", position_sizing = FALSE, 
                                     vol_position_sizing = FALSE)
    
    # Generate signals
    alpha <- alpha_engine$generate_signals()

    # Add to data list with automatic numbering
    data_list[[i]] <- alpha
  }
    
    # Estimate rolling standard deviation as volatility proxy
    estimate_vol <- function(data) {
    data <- data %>%
        mutate(
        rollSD = rollapply(mid, width = 21 * 3, FUN = sd, fill = NA, align = "right"),
        rollSD_q = as.vector(rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE), align = "right", fill = NA)),
        vol_nop_sizing = case_when(
            rollSD >= rollSD_q & is.na(Exit) ~ 0.5,
            TRUE ~ 1
        )
        )
    
    return(data)
    }

    # Loop through each data frame and apply the estimate_vol function
    for (i in seq_along(data_list)) {
    data_list[[i]] <- estimate_vol(data_list[[i]])
    }

    select_cols <- function(data) {
    data <- data %>%
        select(Date, Close, value, signal, position, vol_nop_sizing, rollSD, rollSD_q)
        return(data)
    }

    data_list <- lapply(data_list, select_cols)

    data_list <- lapply(
        seq_along(data_list), function(i) {
        symbol <- symbol_list[[i]]
        colnames(data_list[[i]])[-1] <- paste(symbol, colnames(data_list[[i]])[-1], sep = "_")
        return(data_list[[i]])
    })

    merged_df <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)

    close_columns <- c(paste0(symbol_list, "_Close"))

    # Create all possible unique combinations of close columns
    column_combinations <- combn(close_columns, 2)

    column_combinations_df <- as.data.frame(t(column_combinations))

    # Add a column indicating the pair number
    column_combinations_df$Pair <- paste0("Pair", seq_len(ncol(column_combinations)))

    # Define a function to compute rolling correlations for a pair of columns
    compute_rolling_correlation <- function(data, pair, cor_window) {
    # Extract column names from the pair
    col1 <- as.character(pair[1])
    col2 <- as.character(pair[2])
    
    # Compute rolling correlation for the pair
    data <- data %>%
        mutate(!!paste0("Cor_", col1, ";", col2) := rollapply(data[, c(col1, col2)], 
                                                        width = cor_window, 
                                                        FUN = function(x) cor(x, use = "pairwise.complete.obs")[col1, col2], 
                                                        by.column = FALSE, 
                                                        align = "right", 
                                                        fill = NA)
        )
    
    # Return the updated data frame
    return(data[ncol(data)])
    }

    # Compute rolling correlations for each pair and bind them to the original data frame
    res_cor <- bind_cols(
        merged_df, 
        lapply(1:nrow(column_combinations_df), function(i) {
        # Get the pair
        pair <- column_combinations_df[i, 1:2]
        
        # Compute rolling correlations for the pair
        compute_rolling_correlation(merged_df, pair, 365/2)

            }
        )
    )

    # Remove Invalid numbers, NaN, NAs
    res_cor <- res_cor[complete.cases(res_cor), ]
    return(res_cor)
}

# Function definition with plot_flag parameter
plot_rolling_correlations <- function(data, plot_flag = TRUE) {

  cor_columns <- colnames(data)[grep("Cor_", colnames(data))]
  
  cor_data <- data[, c("Date", cor_columns)]
  
  cor_data_long <- tidyr::gather(cor_data, Pair, Rolling_Correlation, -Date)
  
  if (plot_flag) {
    # Plot rolling correlations using ggplot
    ggplot(cor_data_long, aes(x = Date, y = Rolling_Correlation, color = Pair)) +
      geom_line() +
      geom_hline(yintercept = 0.5, linetype = "dashed") +  
      geom_hline(yintercept = -0.5, linetype = "dashed") +  
      labs(title = "Rolling Correlations",
           x = "Date",
           y = "Rolling Correlation") +
      theme_minimal()
  }
}

cp <- estimate_rolling_correlations(symbol_list, from_date, to_date) # res_cor
plot_rolling_correlations(cp)

# Estimate performance

split_data <- function(cp, symbol_list) {

  symbol_dataframes <- lapply(symbol_list, function(symbol) {
    # Filter columns related to the current symbol
    symbol_columns <- grep(paste0("^", symbol), names(cp), value = TRUE)
    symbol_df <- cp %>%
      select(Date, all_of(symbol_columns)) %>%
      rename_with(~ gsub(paste0("^", symbol), "", .x), contains(symbol)) %>%
      rename_all(~ gsub("^_", "", .))  # Remove underscore from the beginning of column names
    
    # Rename the Date column to avoid conflicts
    colnames(symbol_df)[1] <- "Date"
    
    return(symbol_df)
  })
  
  # Rename list elements with symbol names
  names(symbol_dataframes) <- symbol_list
  
  return(symbol_dataframes)
}

# Usage
symbol_dataframes <- split_data(cp, symbol_list)

# Function to perform the required calculations
process_data <- function(df) {
  # Calculate pnlActive and pnlPassive
  df <- df %>% 
    mutate(
      pnlActive = c(0, diff(Close) * signal[-length(Close)]),
      pnlPassive = c(0, diff(Close)),
      nopActive = 0,
      nopPassive = 0,
      eqlActive = 0,
      eqlPassive = 0
    )

  # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
  df$eqlActive[1] <- capital
  df$nopActive[1] <- floor(capital / (df$Close[1] / leverage))
  df$eqlPassive[1] <- capital
  df$nopPassive[1] <- floor(capital / (df$Close[1] / leverage))   

  # Check if "L" column (probability indicator) exists
  has_L <- "nop_sizing" %in% names(df)
  has_Vol <- "vol_nop_sizing" %in% names(df)

  for (i in 2:nrow(df)) {
    # Active
    pnlActive <- df$pnlActive[i]
    prev_nop_Active <- floor(df$nopActive[i - 1])
    current_nop_Active <- floor((df$eqlActive[i - 1]) / (df$Close[i] / leverage))
    df$eqlActive[i] <- df$eqlActive[i - 1] + prev_nop_Active * pnlActive

    # Calculate nopActive based on the presence of the "L" column
    if (has_L) {
      df$nopActive[i] <- ifelse(
        df$L[i], 
        current_nop_Active * df$nop_sizing[i], 
        current_nop_Active
      )
    } else if (has_Vol) {
      df$nopActive[i] <- ifelse(
        df$vol_nop_sizing[i], 
        current_nop_Active * df$vol_nop_sizing[i], 
        current_nop_Active
      )
    } else {
      df$nopActive[i] <- current_nop_Active
    }

    # Passive
    pnlPassive <- df$pnlPassive[i]
    prev_nop_Passive <- floor(df$nopPassive[i - 1])
    current_nop_Passive <- floor((df$eqlPassive[i - 1]) / (df$Close[i] / leverage))
    df$eqlPassive[i] <- df$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
    df$nopPassive[i] <- current_nop_Passive
  }

  df <- df %>%
    mutate(r_eqlActive = quantmod::Delt(eqlActive),
           r_eqlPassive = quantmod::Delt(eqlPassive))
  return(df)
}

# Apply the function to each data frame in the list
processed_data <- lapply(symbol_dataframes, process_data)

################################
# INDIVIDUAL PERFORMANCE
################################

# Initialize an empty list to store the results
results_list <- list()

# Define a function to compute performance metrics for each currency pair
compute_performance_metrics <- function(data) {

 # Performance metrics for active strategy
  aR_active <- round(as.numeric(Return.annualized(as.numeric(data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data$r_eqlActive), scale = 252) * 100), 3)
  IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
  MD_active <- round(as.numeric(maxDrawdown(as.numeric(data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  trades_active <- sum(diff(data$signal) != 0)
  buy_active <- sum(data$signal == 1)
  short_active <- sum(data$signal == -1)

  # Performance metrics for passive strategy
  aR_passive <- round(as.numeric(Return.annualized(as.numeric(data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data$r_eqlPassive), scale = 252) * 100), 3)
  IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
  MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  trades_passive <- 1
  buy_passive <- 1
  short_passive <- 0
  
  # Calculate success rates
  buy_success_rate_active <- sum(data$position > 0 & data$value > 0) / nrow(data)
  buy_success_rate_passive <- sum(data$value > 0) / nrow(data)
  short_success_rate_active <- sum(data$position < 0 & data$value < 0) / nrow(data)
  short_success_rate_passive <- 0
  combined_rate_active <- (sum(data$position > 0 & data$value > 0) + sum(data$position < 0 & data$value < 0)) / nrow(data)
  combined_rate_passive <- (sum(data$value > 0)) / nrow(data)
  
  # Unique months
  unique_months <- length(unique(format(data$Date, "%Y-%m")))
  
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
    PortfolioEndValue = round(c(data[nrow(data),]$eqlActive, data[nrow(data),]$eqlPassive), 0)
  )
  
  return(df)
}

for (pair in symbol_list) {
  data <- processed_data[[pair]]  # Assuming data for each currency pair is available in the environment
  results_list[[pair]] <- compute_performance_metrics(data)
}

# View the results
results_list

################################
# PORTFOLIO PERFORMANCE
################################

# Apply the column renaming operation to each data frame in the list
data_list <- lapply(seq_along(processed_data), function(i) {
  symbol <- symbol_list[i]
  colnames(processed_data[[i]])[-1] <- paste(symbol, colnames(processed_data[[i]])[-1], sep = "_")
  return(processed_data[[i]])
})

# Merge the modified data frames
p <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)

# Mutate to add a new column summing all columns containing "eqlActive" but not "r_eqlActive"
p <- p %>%
  mutate(
    AlleqlActive = rowSums(select(., contains("eqlActive"), -contains("r_eqlActive")), na.rm = TRUE),
    AlleqlPassive = rowSums(select(., contains("eqlPassive"), -contains("r_eqlPassive")), na.rm = TRUE)
  )

estimate_performance_bucket <- function(data) {

  data <- data %>%
    mutate(
        r_AlleqlActive = as.numeric(quantmod::Delt(AlleqlActive)),
        r_AlleqlPassive = as.numeric(quantmod::Delt(AlleqlPassive))
        )
  # Performance metrics for active strategy
  aR_active <- round(as.numeric(Return.annualized(as.numeric(data$r_AlleqlActive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data$r_AlleqlActive), scale = 252) * 100), 3)
  IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
  MD_active <- round(as.numeric(maxDrawdown(as.numeric(data$r_AlleqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  
  # Performance metrics for passive strategy
  aR_passive <- round(as.numeric(Return.annualized(as.numeric(data$r_AlleqlPassive), scale = 252, geometric = TRUE) * 100), 3)
  aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data$r_AlleqlPassive), scale = 252) * 100), 3)
  IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
  MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data$r_AlleqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
  
  # Create performance dataframe
  df <- data.frame(
    Strategy = c("Active", "Passive"),
    aR = c(aR_active, aR_passive),
    aSD = c(aSD_active, aSD_passive),
    IR = c(IR_active, IR_passive),
    MD = c(MD_active, MD_passive),
    PortfolioEndValue = c(round(data[nrow(data), ]$AlleqlActive, 0), round(data[nrow(data), ]$AlleqlPassive, 0))
  )
  
  return(df)
}

estimate_performance_bucket(p)

################################
# VISUALIZATION
################################

plot_portfolio_components <- function(df, type) {
  columns <- NULL
  title <- NULL
  ylabel <- NULL
  color_label <- NULL
  
  switch(type,
         "IND" = {
           columns <- df %>% 
             select(contains("eqlActive")) %>% 
             select(-contains("r_eqlActive")) %>%
             select(-contains("AlleqlActive"))
           title <- "AlphaEngine performance of FX components"
           ylabel <- "eqlActive Values"
           color_label <- "eqlActive Columns"
         },
         "PORT" = {
           columns <- df %>% select(contains("AlleqlActive"), contains("AlleqlPassive"))
           title <- "AlphaEngine performance of FX portfolio"
           ylabel <- "Portfolio Value"
           color_label <- "AlleqlActive Columns"
         },
         stop("Invalid type. Choose either 'IND' for individual currencies or 'PORT' for portfolio")
  )
  
  # Add Date column
  columns$Date <- df$Date
  
  # Melt the data frame to long format for easier plotting
  melted <- melt(columns, id.vars = "Date")
  
  # Plot
  ggplot(data = melted, aes(x = Date, y = value, color = variable)) +
    geom_line() +
    labs(
      title = title,
      x = "Date",
      y = ylabel,
      color = color_label
    ) +
    theme_minimal()
}

plot_portfolio_components(p, "IND")  # Plot individual currencies
plot_portfolio_components(p, "PORT")  # Plot portfolio

# Filter columns containing "eqlActive"

# eqlActive_columns <- p %>% select(grep("eqlActive", colnames(p), value = TRUE)) %>%
#                       select(-contains("r_eqlActive")) %>%
#                         select(-contains("AlleqlActive"))

# AlleqlActive_columns <- p %>% 
#     select(contains("AlleqlActive"), contains("AlleqlPassive"))


# # Add Date column
# eqlActive_columns$Date <- p$Date
# AlleqlActive_columns$Date <- p$Date

# # Melt the data frame to long format for easier plotting
# eqlActive_melted <- melt(eqlActive_columns, id.vars = "Date")
# AlleqlActive_melted <- melt(AlleqlActive_columns, id.vars = "Date")

# # Plot individual currencies 
# ggplot(data = eqlActive_melted, aes(x = Date, y = value, color = variable)) +
#   geom_line() +
#   labs(

#     title = "AlphaEngine performance of FX components)",
#     x = "Date",
#     y = "eqlActive Values", 
#     color = "eqlActive Columns") +
  
#   theme_minimal()

# # Plot portfolio
# ggplot(data = AlleqlActive_melted, aes(x = Date, y = value, color = variable)) +
#  geom_line() +
#   labs(

#     title = "AlphaEngine performance of FX portfolio",
#     x = "Date",
#     y = "Portfolo Value", 
#     color = "AlleqlActive Columns") +
  
#   theme_minimal()


############### all in one ######################

estimate_portfolio_performance <- function(cp, symbol_list, capital, leverage) {

# Estimate rolling correlations
cp <- private$estimate_rolling_correlations(symbol_list, from_date, to_date)

# Split data into individual dataframes
split_data <- function(cp, symbol_list) {
    # Function to filter and rename columns for each symbol
    symbol_dataframes <- lapply(symbol_list, function(symbol) {
      symbol_columns <- grep(paste0("^", symbol), names(cp), value = TRUE)
      symbol_df <- cp %>%
        select(Date, all_of(symbol_columns)) %>%
        rename_with(~ gsub(paste0("^", symbol), "", .x), contains(symbol)) %>%
        rename_all(~ gsub("^_", "", .))  # Remove underscore from the beginning of column names
      colnames(symbol_df)[1] <- "Date"  # Rename Date column to avoid conflicts
      return(symbol_df)
    })
    names(symbol_dataframes) <- symbol_list  # Rename list elements with symbol names
    return(symbol_dataframes)
}
  
process_data <- function(df) {
    # Calculate pnlActive and pnlPassive
    df <- df %>% 
        mutate(
        pnlActive = c(0, diff(Close) * signal[-length(Close)]),
        pnlPassive = c(0, diff(Close)),
        nopActive = 0,
        nopPassive = 0,
        eqlActive = 0,
        eqlPassive = 0
        )

    # Entry is set to the initial amount of money invested and number of positions (no leverage) given Close price at entry point
    df$eqlActive[1] <- capital
    df$nopActive[1] <- floor(capital / (df$Close[1] / leverage))
    df$eqlPassive[1] <- capital
    df$nopPassive[1] <- floor(capital / (df$Close[1] / leverage))   

    # Check if "L" column (probability indicator) exists
    has_L <- "nop_sizing" %in% names(df)
    has_Vol <- "vol_nop_sizing" %in% names(df)

    for (i in 2:nrow(df)) {
        # Active
        pnlActive <- df$pnlActive[i]
        prev_nop_Active <- floor(df$nopActive[i - 1])
        current_nop_Active <- floor((df$eqlActive[i - 1]) / (df$Close[i] / leverage))
        df$eqlActive[i] <- df$eqlActive[i - 1] + prev_nop_Active * pnlActive

        # Calculate nopActive based on the presence of the "L" column
        if (has_L) {
        df$nopActive[i] <- ifelse(
            df$L[i], 
            current_nop_Active * df$nop_sizing[i], 
            current_nop_Active
        )
        } else if (has_Vol) {
        df$nopActive[i] <- ifelse(
            df$vol_nop_sizing[i], 
            current_nop_Active * df$vol_nop_sizing[i], 
            current_nop_Active
        )
        } else {
        df$nopActive[i] <- current_nop_Active
        }

        # Passive
        pnlPassive <- df$pnlPassive[i]
        prev_nop_Passive <- floor(df$nopPassive[i - 1])
        current_nop_Passive <- floor((df$eqlPassive[i - 1]) / (df$Close[i] / leverage))
        df$eqlPassive[i] <- df$eqlPassive[i - 1] + prev_nop_Passive * pnlPassive
        df$nopPassive[i] <- current_nop_Passive
    }

    df <- df %>%
        mutate(r_eqlActive = quantmod::Delt(eqlActive),
            r_eqlPassive = quantmod::Delt(eqlPassive))
    return(df)
}
    
# Define a function to compute performance metrics for each currency pair
compute_performance_metrics <- function(data) {

    # Performance metrics for active strategy
    aR_active <- round(as.numeric(Return.annualized(as.numeric(data$r_eqlActive), scale = 252, geometric = TRUE) * 100), 3)
    aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data$r_eqlActive), scale = 252) * 100), 3)
    IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
    MD_active <- round(as.numeric(maxDrawdown(as.numeric(data$r_eqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
    trades_active <- sum(diff(data$signal) != 0)
    buy_active <- sum(data$signal == 1)
    short_active <- sum(data$signal == -1)

    # Performance metrics for passive strategy
    aR_passive <- round(as.numeric(Return.annualized(as.numeric(data$r_eqlPassive), scale = 252, geometric = TRUE) * 100), 3)
    aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data$r_eqlPassive), scale = 252) * 100), 3)
    IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
    MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data$r_eqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
    trades_passive <- 1
    buy_passive <- 1
    short_passive <- 0
    
    # Calculate success rates
    buy_success_rate_active <- sum(data$position > 0 & data$value > 0) / nrow(data)
    buy_success_rate_passive <- sum(data$value > 0) / nrow(data)
    short_success_rate_active <- sum(data$position < 0 & data$value < 0) / nrow(data)
    short_success_rate_passive <- 0
    combined_rate_active <- (sum(data$position > 0 & data$value > 0) + sum(data$position < 0 & data$value < 0)) / nrow(data)
    combined_rate_passive <- (sum(data$value > 0)) / nrow(data)
    
    # Unique months
    unique_months <- length(unique(format(data$Date, "%Y-%m")))
    
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
        PortfolioEndValue = round(c(data[nrow(data),]$eqlActive, data[nrow(data),]$eqlPassive), 0)
    )
    
    return(df)
}
    
estimate_performance_bucket <- function(data) {

    data <- data %>%
        mutate(
            r_AlleqlActive = as.numeric(quantmod::Delt(AlleqlActive)),
            r_AlleqlPassive = as.numeric(quantmod::Delt(AlleqlPassive))
            )
    # Performance metrics for active strategy
    aR_active <- round(as.numeric(Return.annualized(as.numeric(data$r_AlleqlActive), scale = 252, geometric = TRUE) * 100), 3)
    aSD_active <- round(as.numeric(StdDev.annualized(as.numeric(data$r_AlleqlActive), scale = 252) * 100), 3)
    IR_active <- round(as.numeric(aR_active / aSD_active), 3) 
    MD_active <- round(as.numeric(maxDrawdown(as.numeric(data$r_AlleqlActive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
    
    # Performance metrics for passive strategy
    aR_passive <- round(as.numeric(Return.annualized(as.numeric(data$r_AlleqlPassive), scale = 252, geometric = TRUE) * 100), 3)
    aSD_passive <- round(as.numeric(StdDev.annualized(as.numeric(data$r_AlleqlPassive), scale = 252) * 100), 3)
    IR_passive <- round(as.numeric(aR_passive / aSD_passive), 3) 
    MD_passive <- round(as.numeric(maxDrawdown(as.numeric(data$r_AlleqlPassive), weights = NULL, geometric = TRUE, invert = TRUE) * 100), 3)
    
    # Create performance dataframe
    df <- data.frame(
        Strategy = c("Active", "Passive"),
        aR = c(aR_active, aR_passive),
        aSD = c(aSD_active, aSD_passive),
        IR = c(IR_active, IR_passive),
        MD = c(MD_active, MD_passive),
        PortfolioEndValue = c(round(data[nrow(data), ]$AlleqlActive, 0), round(data[nrow(data), ]$AlleqlPassive, 0))
    )
    
    return(df)
}
    
# Apply all functions to the provided data

# Split data
symbol_dataframes <- split_data(cp, symbol_list)

# Process data
processed_data <- lapply(symbol_dataframes, process_data)

# Compute performance metrics for each currency pair
results_list <- list()
for (pair in symbol_list) {
    data <- processed_data[[pair]]
    results_list[[pair]] <- compute_performance_metrics(data)
}

# Merge individual dataframes for portfolio performance estimation
data_list <- lapply(seq_along(processed_data), function(i) {
    symbol <- symbol_list[i]
    colnames(processed_data[[i]])[-1] <- paste(symbol, colnames(processed_data[[i]])[-1], sep = "_")
    return(processed_data[[i]])
})
p <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)

p <- p %>% 
    mutate(
AlleqlActive = rowSums(select(., contains("eqlActive"), -contains("r_eqlActive")), na.rm = TRUE),
AlleqlPassive = rowSums(select(., contains("eqlPassive"), -contains("r_eqlPassive")), na.rm = TRUE)
)

# Estimate portfolio performance bucket
portfolio_performance <- estimate_performance_bucket(p)

print(results_list)
print(portfolio_performance)
    
    return(p)

}

# Usage:
result <- estimate_portfolio_performance(cp, symbol_list, capital, leverage)
# results_list <- result$results_list
# portfolio_performance <- result$portfolio_performance

# Visualization:
plot_portfolio_components(result, "IND")  # Plot individual currencies
plot_portfolio_components(result, "PORT")  # Plot portfolio

############### all in one ######################
###### end
#################################################

