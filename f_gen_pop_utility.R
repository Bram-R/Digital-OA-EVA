f_gen_pop_utility <- function(n_age_baseline, n_t, n_p_female) {
  #' @title Prepare Utility Data for State Transition Model
  #' @description Processes general population utility data to match the monthly cycles
  #' of a state transition model.
  #' @param n_age_baseline Integer. The starting age of the cohort.
  #' @param n_t Integer. The total number of months in the model.
  #' @param n_p_female Numeric. The proportion of females in the cohort (0 to 1).
  #' @return A data frame containing utility values replicated for monthly cycles, with
  #'   columns for age, utility, and cycle number.
  #' @examples
  #' # Example usage:
  #' f_gen_pop_utility(n_age_baseline = 60, n_t = 240)
  
  # Load the general population utility data
  # The data source is based on HSE 2014 (https://www.sheffield.ac.uk/nice-dsu/methods-development/estimating-eq-5d)
  utility_data <- read.csv(
    "Gen_pop_utility_2014.csv",
    fileEncoding = "UTF-8-BOM"
  )
  
  # Calculate the gender-weighted average utility
  utility_data$un <- utility_data$un_males * (1 - n_p_female) + 
    utility_data$un_females * n_p_female
  
  # Calculate the age range
  age_min <- n_age_baseline                      # Minimum age to include
  age_max <- n_age_baseline + n_t / 12           # Maximum age to include
  
  # Filter utility data for ages within the specified range
  filtered_data <- utility_data[
    utility_data$age >= age_min & utility_data$age <= age_max,  # Filter by age range
    c("age", "un")                                      # Select relevant columns
  ]
  
  # Expand filtered data to match monthly cycles
  expanded_data <- filtered_data[
    rep(seq_len(nrow(filtered_data)), each = 12),  # Replicate each row 12 times
  ]
  
  # Add a column for the cycle number
  expanded_data$cycle <- seq_len(nrow(expanded_data))
  
  # Return the processed data
  return(expanded_data[1:(n_t + 1),]) # remove last 11 rows
}
