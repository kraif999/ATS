data_fetcher <- DataFetcher$new("MXN=X", from_date, to_date)
ts <- data_fetcher$download_xts_data()

ts_df <- data.frame(ts)
ts_df <- ts_df %>%
    rename_with(~ sub(".*\\.", "", .), everything()) %>%
      mutate(Date = as.Date(rownames(.))) %>%
        select(Date, everything()) %>%
            na.omit() %>% 
                as_tibble()

ts_df <- ts_df %>%
  mutate(mid = (High + Low) / 2)

from_date <- as.Date("2020-01-01", format = "%Y-%m-%d")
# STEP1 : DETECT DIRECTIONAL CHANGES

identify_events <- function(data, threshold) {
  # Initialize event vector
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

threshold <- 0.01
# prototype
prot <- identify_events(ts_df, 0.01)



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

# Initialize an empty vector to store mid prices for the change_value
prot$change_value <- NA

# Initialize variables
prev_dc_index <- NULL
prev_mid <- NULL


th <- 0.005 # assymetric

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
  if (!is.na(prev_mid) && abs(prot$mid[i] - prev_mid) > th * prev_mid) {
    prev_mid <- prot$mid[i]  # Update prev_mid
  }
}


# add OS type in initializer: signalTh or signalOS

prot2 <- prot %>%
  mutate(
    row_number = row_number(),
    signal = case_when(
      OS == "UOS" & mid >= change_value * (1 + th) ~ -1,
      OS == "DOS" & mid <= change_value * (1 - th) ~ 1,
      TRUE ~ 0
    ),
    position = lag(signal, default = 0),
    OSS = mid >= change_value * (1 + th) | mid <= change_value * (1 - th) 
  ) %>% select(Date, High, Low, Close, mid, change_value, OS, dc, L, signal, OSS, position, row_number)

entries <- prot2 %>% 
        filter(signal == 1 | signal == -1) %>%
            mutate(
                change = c(2, abs(diff(signal))), default = 0,
                Exit = case_when(
                    signal == -1 ~ mid * (1 - th),
                    signal == 1 ~ mid * (1 + th),
                    TRUE ~ 0
                    )
                )

prot2 <- prot2 %>%
  mutate(in_entries = row_number() %in% entries$row_number,
  signal = ifelse(in_entries, signal, 0),
  position = lag(signal, default = 0)
  ) %>% 
    left_join(entries %>% select(row_number, Exit), by = c("row_number" = "row_number"))


#View(prot2) # here Exits are in the prot2

##############################
# ENTRY SIGNAL GENERATION BASED ON OVERSHOOT DURATION (enter when the number of observed OS in a sequence exceeds or equal to the average OS based on the whole dataset)
##############################

# overshoot duration
calculate_sequence_lengths <- function(h) {
  uos_lengths <- numeric()  # Initialize vector to store UOS sequence lengths
  dos_lengths <- numeric()  # Initialize vector to store DOS sequence lengths
  current_sequence <- NULL  # Initialize variable to store current sequence
  
  for (event in h$OS) {
    if (is.null(current_sequence)) {
      current_sequence <- event  # Initialize current sequence if it's NULL
    } else if (current_sequence != event) {
      if (current_sequence == "UOS") {
        uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
      } else if (current_sequence == "DOS") {
        dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
      }
      current_sequence <- event  # Reset current sequence to new event
    } else {
      current_sequence <- c(current_sequence, event)  # Add event to current sequence
    }
  }
  
  # Add length of the last sequence
  if (!is.null(current_sequence)) {
    if (current_sequence == "UOS") {
      uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
    } else if (current_sequence == "DOS") {
      dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
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
prot2 <- calculate_OS_length(prot2)
prot2 <- prot2 %>%
    mutate(
        ExitOS = case_when(
        signal == -1 ~ mid * (1 - th),
        signal == 1 ~ mid * (1 + th),
        TRUE ~ 0
        )
    )

############### EXIT SIGNAL GENERATION ###############
############### COASTLINE TRADING $$$$$$$$$$$$$$$$$$$

generateExitSignals <- function(data) {
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
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  
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

prot3 <- generateExitSignals(prot2)

exits <- prot3 %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    ) %>% 
                    filter(change == 2)

View(exits)

prot4 <- prot3 %>%
  mutate(
    out_exits = row_number() %in% exits$row_number,
    signal = if_else(in_entries, signal, 0),
    signalE = ifelse(signalE != 0, signalE, 0),
    signalE = if_else(out_exits, signalE, 0),
    signal = if_else(signal != signalE & signalE != 0, signalE, signal),
    position = lag(signal, default =) # final signal
  ) %>% 
    select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, signal, signalOS, signalE, L, position, in_entries, out_exits, Exit, ExitOS, OS_length)

View(prot4) # final table with signals
table(prot4$signal) # final signals


# Create a ggplot object
p <- ggplot(prot4, aes(x = Date, y = Close)) +
  geom_line() +  # Add a line plot for Close price
  labs(x = "Date", y = "Close Price", title = "Close Price and Signals") +  # Set axis labels and plot title
  theme_minimal()  # Use minimal theme (optional)

# Add vertical dashed lines for buy signals (green)
p + geom_vline(data = subset(prot4, signal == 1), aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "green") +
  # Add vertical dashed lines for sell signals (red)
  geom_vline(data = subset(prot4, signal == -1), aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red")


################################## do it for one data (self$data) ##################################
################################## ################################## ################################## 
################################## ################################## ################################## 
################################## ################################## ################################## 
################################## ################################## ################################## 


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
  if (!is.na(prev_mid) && abs(prot$mid[i] - prev_mid) > th * prev_mid) {
    prev_mid <- prot$mid[i]  # Update prev_mid
  }
}

th <- 0.005 # assymetric
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
    select(Date, High, Low, Close, mid, change_value, OS, dc, L, signal, OSS, position, row_number)

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


#View(prot) # here Exits are in the prot2

##############################
# ENTRY SIGNAL GENERATION BASED ON OVERSHOOT DURATION (enter when the number of observed OS in a sequence exceeds or equal to the average OS based on the whole dataset)
##############################

# overshoot duration
calculate_sequence_lengths <- function(h) {
  uos_lengths <- numeric()  # Initialize vector to store UOS sequence lengths
  dos_lengths <- numeric()  # Initialize vector to store DOS sequence lengths
  current_sequence <- NULL  # Initialize variable to store current sequence
  
  for (event in h$OS) {
    if (is.null(current_sequence)) {
      current_sequence <- event  # Initialize current sequence if it's NULL
    } else if (current_sequence != event) {
      if (current_sequence == "UOS") {
        uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
      } else if (current_sequence == "DOS") {
        dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
      }
      current_sequence <- event  # Reset current sequence to new event
    } else {
      current_sequence <- c(current_sequence, event)  # Add event to current sequence
    }
  }
  
  # Add length of the last sequence
  if (!is.null(current_sequence)) {
    if (current_sequence == "UOS") {
      uos_lengths <- c(uos_lengths, length(current_sequence))  # Store length of UOS sequence
    } else if (current_sequence == "DOS") {
      dos_lengths <- c(dos_lengths, length(current_sequence))  # Store length of DOS sequence
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

############### EXIT SIGNAL GENERATION ###############
############### COASTLINE TRADING $$$$$$$$$$$$$$$$$$$

generateExitSignals <- function(data) {
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
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = FALSE)
  data$Exit <- na.locf(data$Exit, na.rm = FALSE, fromLast = TRUE)
  
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

prot <- generateExitSignals(prot)

exits <- prot %>% 
        filter(signalE == 1 | signalE == -1) %>%
            mutate(
                change = c(2, abs(diff(signalE))),
                    ) %>% 
                        filter(change == 2)

#View(exits)

prot <- prot %>%
  mutate(
    out_exits = row_number() %in% exits$row_number,
    signal = if_else(in_entries, signal, 0),
    signalE = ifelse(signalE != 0, signalE, 0),
    signalE = if_else(out_exits, signalE, 0),
    signal = if_else(signal != signalE & signalE != 0, signalE, signal),
    position = lag(signal, default = 0)
  ) %>% 
    select(Date, row_number, High, Low, Close, mid, change_value, OS, OSS, dc, signal, signalOS, signalE, L, position, in_entries, out_exits, Exit, OS_length)

View(prot) # final table with signals


################################################################################
# Define AlphaEngine (based on intrinsic time approach)
################################################################################

AlphaEngine <- R6Class(
  "AlphaEngine",
  inherit = Strategy,
  public = list(

  threshold = NULL,
  profit_taking = NULL,
  signal_generation = NULL,
  position_sizing = NULL,
  vol_position_sizing = NULL,

initialize = function(data, threshold, profit_taking, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE) {
      super$initialize(data)
      self$data <- super$convert_to_tibble(self$data)
      self$threshold <- threshold
      self$profit_taking <- profit_taking
      self$signal_generation <- signal_generation
      self$position_sizing <- position_sizing
      self$vol_position_sizing <- vol_position_sizing
},

generate_signals = function() {

    cat("Threshold value:", self$threshold, "\n")  # Print the threshold value

    # Mid data
    # self$data <- self$data %>%
    #     mutate(
    #         mid = (High + Low) / 2
    #     )

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

    if(self$vol_position_sizing) {
        self$data <- self$data %>%
        mutate(
            rollSD = rollapply(mid, width = 21 * 3, FUN = sd, fill = NA, align = "right"),
            rollSD75  = rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE), align = "right", fill = NA),
            rollSD95  = rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.95, na.rm = TRUE), align = "right", fill = NA),
            rollSD999  = rollapply(rollSD, width = 21 * 3, FUN = function(x) quantile(x, probs = 0.999, na.rm = TRUE), align = "right", fill = NA),
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
      #if (self$data$OS[i] == "UOS" && self$data$mid[i] > prev_mid * (1 + self$profit_taking)) {
      if (self$data$OS[i] == "UOS" && self$data$mid[i] > prev_mid * (1 + self$threshold)) {
        prev_mid <- self$data$mid[i]  # Update prev_mid for UOS
      #} else if (self$data$OS[i] == "DOS" && self$data$mid[i] < prev_mid * (1 - self$profit_taking)) {
      } else if (self$data$OS[i] == "DOS" && self$data$mid[i] < prev_mid * (1 - self$threshold)) {
        prev_mid <- self$data$mid[i]  # Update prev_mid for DOS
      }
    } 
  }

    #self$profit_taking <- 0.005 # assymetric
    self$data <- self$data %>%
    mutate(
        row_number = row_number(),
        signal = case_when(
        #OS == "UOS" & mid >= change_value * (1 + self$profit_taking) ~ -1,
        OS == "UOS" & Open >= lag(change_value, default = 0) * (1 + self$threshold) ~ -1,
        #OS == "UOS" & Open >= change_value * (1 + self$threshold) ~ -1,
        #OS == "DOS" & mid <= change_value * (1 - self$profit_taking) ~ 1,
        OS == "DOS" & Open <= lag(change_value, default = 0) * (1 - self$threshold) ~ 1,
        #OS == "DOS" & Open <= change_value * (1 - self$threshold) ~ 1,
        TRUE ~ 0
        ),
        position = lag(signal, default = 0),
        #OSS = mid >= change_value * (1 + self$profit_taking) | mid <= change_value * (1 - self$profit_taking) 
        #OSS = mid >= lag(change_value, default = 0) * (1 + self$threshold) | mid <= change_value * (1 - self$threshold)
        OSS = mid >= lag(change_value, default = 0) * (1 + self$threshold) | mid <= lag(change_value, default = 0) * (1 - self$threshold)  
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

    avgOS <- data.frame(
      avgUOS = floor(mean(sequence_lengths$uos_lengths)), 
      avgDOS = floor(mean(sequence_lengths$dos_lengths))
    )

    # Modify calculate_OS_length to accept avgOS
    self$data <- private$calculate_OS_length(self$data, avgOS)

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

    self$data <- self$data %>%
    mutate(
        out_exits = row_number() %in% exits$row_number,
        signal = if_else(in_entries, signal, 0),
        signalE = ifelse(signalE != 0, signalE, 0),
        signalE = if_else(out_exits, signalE, 0),
        signal = if_else(signal != signalE & signalE != 0, signalE, signal),
        position = lag(signal, default = 0)
    )

    if (self$position_sizing) {
    self$data <- self$data %>%
        mutate(
        nop_sizing = case_when(
            L < 0.1 ~ 0.1,
            L < 0.4 ~ 0.5,
            TRUE ~ 1
        )
        ) %>% 
        mutate(
        L = replace_na(L, 1)
        ) %>% 
      select(Date, row_number, High, Low, Close, value, mid, change_value, OS, OSS, dc, events,
         signal, signalOS, signalE, signalE_closes_row, L, nop_sizing, position, in_entries, out_exits, Exit, OS_length)

    } else if (self$vol_position_sizing) {
    self$data <- self$data %>%
      mutate(
        vol_nop_sizing = case_when(
        # rollSD >= rollSD75 & is.na(Exit) ~ 0.5, # vol position sizing is applied for Entry signals only
        # rollSD >= rollSD95 & is.na(Exit) ~ 0.2, # vol position sizing is applied for Entry signals only
        rollSD >= lag(rollSD75, default = 0) & lag(signalE, default = 0) == 0 ~ 0.5, # vol position sizing is applied for Entry signals only
        rollSD >= lag(rollSD95, default = 0) & lag(signalE == 0, default = 0) ~ 0.2, # vol position sizing is applied for Entry signals only
        TRUE ~ 1
    )
      ) %>%
        select(Date, row_number, High, Low, Close, value, mid, change_value, OS, OSS, dc, events,
            signal, signalOS, signalE, signalE_closes_row, position, vol_nop_sizing, in_entries, out_exits, Exit, OS_length)
    } else {
    self$data <- self$data %>%
        select(Date, row_number, High, Low, Close, value, mid, change_value, OS, OSS, dc, events,
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
            #axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels for better readability
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
    geom_vline(data = subset(self$data, events == 1), aes(xintercept = as.numeric(Date)), color = "blue", linetype = "dashed") +  # Add vertical lines for UE
    geom_vline(data = subset(self$data, events == -1), aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed") +  # Add vertical lines for DE
    geom_line() +  # Plot close price
    labs(title = paste("Close Price filtered by Directional Changes for", symbol), x = "Date", y = "Close Price") +
    theme_minimal()

},

# Plot Entry and Exit signals
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

# Create multiple instances of  the class object given different parameters value
run_backtest = function(symbols, thresholds, profit_takings, signal_generations, position_sizings, vol_position_sizings, from_date, to_date, output_df = TRUE) {
      # Create an empty list to store results
      results <- list()

      tryCatch({
        # Loop through symbols, window size  and bands to create instances and estimate performance
        for (symbol in symbols) {
          for (threshold in thresholds) {
            for (profit_taking in profit_takings) {
              for (signal_generation in signal_generations) {
                for (position_sizing in position_sizings) {
                  for (vol_position_sizing in vol_position_sizings) {

                    # Fetch data using DataFetcher for the current symbol and date range
                    data_fetcher <- DataFetcher$new(symbol, from_date, to_date)
                    data <- data_fetcher$download_xts_data()
                  
                    # Create an instance of Alpha strategy 
                    alpha <- AlphaEngine$new(data, threshold, profit_taking, signal_generation, position_sizing, vol_position_sizing)

                    # Store the results
                    results[[paste(symbol, threshold, profit_taking, signal_generation, position_sizing, vol_position_sizing, sep = "_")]] <- list(
                      Symbol = symbol,
                      Class = meta$assets[[symbol]]$class,
                      Methodology = paste("AlphaEngine:", threshold, profit_taking, signal_generation, position_sizing, vol_position_sizing),
                      Threshold = threshold,
                      ProfitTaking = profit_taking,
                      Signal_generation  = signal_generation,
                      Position_sizing = position_sizing,
                      Vol_position_sizing = vol_position_sizing,
                      Performance = alpha$estimate_performance()
                    )

                    print(paste0("Results for ", "AlphaEngine: ", "(", "symbol:", symbol, ",", "class:", meta$assets[[symbol]]$class, ",", 
                      "threshold:", threshold, ",", "profit_taking:", profit_taking, ",", "signal_generation:", 
                      signal_generation, ",", "position sizing:", position_sizing, ",", "vol_position_sizing:", vol_position_sizing, ")"))
                    
                  }
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
          Vol_position_sizing = item$Vol_position_sizing,
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

        # Filter and combine "Active" and unique "Passive" strategies in one step
        results_df <- results_df %>%
          filter(Strategy == "Active" | Strategy == "Passive") %>%
            group_by(Symbol, Strategy) %>%
              filter(row_number() == 1 | Strategy == "Active") %>%
                ungroup() %>% 
                  arrange(desc(Symbol), desc(aR))
            
      if (output_df) {
        return(results_df)
      } else {
        return(results)
      }
}

  ),

  private = list(

# Identify directional changes given threshold (1%, etc.)
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

# Estimate L (information theoretical value): measures the unlikeliness of the occurence of price trajecotries (helper function)
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

# Estimate L (information theoretical value): measures the unlikeliness of the occurence of price trajecotries (helper function)
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

# Average duration of UOS or DOS overshoots 
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

# Map average duration of UOS or DOS overshoots 
calculate_OS_length = function(data, avgOS) {
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
    signalOS = case_when(
      OS == "UOS" & OS_length >= avgOS$avgUOS ~ -1, 
      OS == "DOS" & OS_length >= avgOS$avgDOS ~ 1, 
      TRUE ~ 0
    )
  )
  
  return(data)
},

# Generate Exit signals
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

#################################################
# Multivariate approach: FX portfolio composition
#################################################

AlphaEngineMult <- R6Class(
  "AlphaEngineMult",
  inherit = AlphaEngine,
  public = list(

# Methods for portfolio composition (multivariate)
estimate_rolling_correlations = function(symbol_list, from_date, to_date) {

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
        compute_rolling_correlation(merged_df, pair, 365/4)

            }
        )
    )

    # Remove Invalid numbers, NaN, NAs
    res_cor <- res_cor[complete.cases(res_cor), ]
    return(res_cor)
},

# Function definition with plot_flag parameter
plot_rolling_correlations = function(data, plot_flag = TRUE) {

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
},

estimate_portfolio_performance = function(cp, symbol_list, capital, leverage) {

  # Estimate rolling correlations
  cp <- self$estimate_rolling_correlations(symbol_list, from_date, to_date)
      
  # Split data
  symbol_dataframes <- private$split_data(cp, symbol_list)

  # Process data
  processed_data <- lapply(symbol_dataframes, private$process_data)

  # Compute performance metrics for each currency pair
  results_list <- list()
  for (pair in symbol_list) {
      data <- processed_data[[pair]]
      results_list[[pair]] <- private$compute_performance_metrics(data)
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
  portfolio_performance <- private$estimate_performance_bucket(p)

  print(results_list)
  print(portfolio_performance)
      
  return(p)
    
},

plot_portfolio_components = function(df, type) {
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
  
  ),
  private = list(

##################################################
# Helper functions for estimate_performance_bucket

# Split data into individual dataframes
split_data = function(cp, symbol_list) {
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
},

# Prepare data for performance estimation
process_data = function(df) {
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
},

# Define a function to compute performance metrics for each currency pair
compute_performance_metrics = function(data) {

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
},
    
estimate_performance_bucket = function(data) {
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
alpha1 <- AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = TRUE) # signal_generation by default is based on threshold
#alpha1$generate_signals()
alpha1$estimate_performance()
alpha1$plot_equity_lines(paste0("AlphaEngine for ", symbol), signal_flag = TRUE)
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
symbol <- "MXN=X"
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
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH") # Warsaw Rondo ONZ, May 6th, 2024
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = TRUE) # AR: 15%
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE) # AR: 15%
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.5, signal_generation = "TH", position_sizing = FALSE)
alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.001, signal_generation = "TH", position_sizing = TRUE) # AR: 15%

#alpha1$generate_signals()
alpha1$estimate_performance()
alpha1$plot_equity_lines(paste0("AlphaEngine for ", symbol), signal_flag = TRUE)
alpha1$plot_events(symbol)
alpha1$plot_dc(symbol)
alpha1$plotSignals()
#a <- alpha1$data

##############################
# Run backtest:
 ##############################

symbol_list <- c("EUR=X", "AUDUSD=X", "EURUSD=X", "CAD=X", "GBPUSD=X")
symbol_list <- c("EUR=X", "NZDJPY=X", "GBPUSD=X")
symbol_list <- c("USDPLN=X", "MXN=X", "NZDCAD=X")
symbol_list <- c("NZDJPY=X", "JPY=X", "EUR=X")

alpha1 <-  AlphaEngine$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE)
res_alpha <- alpha1$run_backtest(
  #symbols = fxs,  
  #symbols = "EUR=X",
  symbols = symbol_list,
  thresholds = c(0.005, 0.01),
  profit_takings = c(0.0001),
  signal_generations = c("TH", "OS"),
  position_sizings = FALSE,
  vol_position_sizing = c(TRUE, FALSE),
  from_date,
  to_date,
  output_df = TRUE
) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
    trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

# res_alpha <- alpha1$run_backtest(
#   symbols = fxs,  
#   thresholds = c(0.005, 0.01, 0.015, 0.02, 0.01 * 2.525729),
#   profit_takings = c(0.0001, 0.001, 0.01),
#   signal_generations = c("TH", "OS"),
#   position_sizings = FALSE,
#   vol_position_sizing = c(TRUE, FALSE),
#   from_date,
#   to_date,
#   output_df = TRUE
# ) %>% select(Symbol, Class, Methodology, Strategy, aR, aSD, IR, MD, 
#     trades, avg_no_monthly_trades, buys, sells, Buy_Success_Rate, Short_Success_Rate, Combined_Success_Rate, PortfolioValue)

##############################
# Portfolio composition
##############################

alpha1 <-  AlphaEngineMult$new(ts, threshold = 0.01, profit_taking = 0.005, signal_generation = "TH", position_sizing = FALSE, vol_position_sizing = FALSE)
alpha1$estimate_performance()

# Rolling correlations:
cp1 <- alpha1$estimate_rolling_correlations(symbol_list, from_date, to_date)
cp1 <- alpha1$estimate_rolling_correlations(fxs, from_date, to_date)
alpha1$plot_rolling_correlations(cp1)

fx_portfolio <- alpha1$estimate_portfolio_performance(cp1, fxs, capital, leverage)
alpha1$plot_portfolio_components(fx_portfolio, "IND")
alpha1$plot_portfolio_components(fx_portfolio, "PORT")

# Download data from Yahoo (instances of DataFetcher class)
aud_usd <- DataFetcher$new("AUD=X", from_date, to_date) # 2006-12-25 outlier
ts <- aud_usd$download_xts_data()

# Exchange rate evolution  over time
aud_usd$plot_close_or_rets(type = "close")

a <- alpha1$data
