f_model <- function(params, intermediate = FALSE) {
  #' State-Transition Model
  #'
  #' This function simulates a state-transition model using a Markov framework. It calculates 
  #' the expected costs, quality-adjusted life years (QALYs), and life-years (LYs) for two 
  #' treatment strategies over a specified time horizon. 
  #'
  #' @param params A named list containing model parameters, including:
  #' \itemize{
  #'   \item **Transition Probabilities:**
  #'     \itemize{
  #'     }
  #'   \item **Health State Utilities:**
  #'     \itemize{
  #'       \item \code{utility_alive}, \code{utility_alive_intervention}, \code{utility_death}: QALY weights per state.
  #'     }
  #'   \item **Costs:**
  #'     \itemize{
  #'       \item \code{cost_t1}, \code{cost_t2}: Strategy-specific costs.
  #'       \item \code{cost_alive}, \code{cost_alive_intervention}, \code{cost_death}: Costs per health state.
  #'     }
  #'   \item **Duration of intervention impact:**
  #'     \itemize{
  #'       \item \code{duration_utility_impact}, \code{duration_cost_impact}: Duration of intervention impact on utility and costs.
  #'     }
  #'   \item **Discount Rates:**
  #'     \itemize{
  #'       \item \code{discount_costs}, \code{discount_qalys}, \code{discount_lys}: Discount rates for costs, QALYs, and life-years.
  #'     }
  #' }
  #' 
  #' @param intermediate Logical. If \code{FALSE} (default), the function returns a named vector of total 
  #' costs, QALYs, and LYs for each treatment strategy. If \code{TRUE}, it returns detailed 
  #' intermediate cycle-level outputs in a matrix format for further analysis.
  #'
  #' @details
  #' This function:
  #' \itemize{
  #'   \item Defines transition matrices for each treatment.
  #'   \item Ensures transition probabilities are consistent (e.g., adjusting for background mortality).
  #'   \item Estimates state transitions.
  #'   \item Computes **discounted costs, QALYs, and LYs** for each treatment strategy.
  #'   \item Returns a named vector with results for cost-effectiveness analysis.
  #' }
  #'
  #' @return A named vector containing total costs, QALYs, and LYs for the treatment strategies:
  #' \itemize{
  #'   \item \code{Cost_Current_practice}, \code{Cost_New_treatment}
  #'   \item \code{QALY_Current_practice}, \code{QALY_New_treatment}
  #'   \item \code{LY_Current_practice}, \code{LY_New_treatment}
  #' }
  #'
  #' If \code{intermediate = TRUE}, returns a **matrix** containing:
  #' \itemize{
  #'   \item **Cycle-level health state costs** for each treatment.
  #'   \item **Cycle-level QALYs and LYs** for each treatment.
  #'   \item **Markov trace** (state occupancy probabilities) for each treatment.
  #' }
  #' 
  #' @examples
  #' # Run model
  #' results <- f_model(f_input(n_sim = 1))
  #' print(results)
  #'
  #' @export
  
  #### Health state costs calculations ----
  cost_primary_care <- params$cost_gp + params$cost_prescriptions
  cost_primary_care_digital <- params$cost_gp * (1 - params$p_red_gp_digital) + 
    params$cost_prescriptions * (1 - params$p_red_prescription_digital)
  
  cost_community_care <- params$cost_physio
  cost_community_care_digital <- params$cost_physio * (1 - params$p_red_physio_digital)
  cost_secondary_care <- params$cost_inpatient + params$cost_ae
  cost_secondary_care_digital <- params$cost_inpatient * (1 - params$p_red_inpatient_digital) +
    params$cost_ae * (1 - params$p_red_ae_digital)
  
  cost_secondary_care <- cost_secondary_care_digital <- 0
  
  cost_alive <- params$cost_medication + cost_primary_care + cost_community_care + cost_secondary_care 
  cost_alive_intervention <- params$cost_medication * (1 - params$p_red_medication_digital) + cost_primary_care_digital + cost_community_care_digital + cost_secondary_care_digital 
  
  #### Transition matrices ----
  # Initialize transition probability and transition dynamics arrays
  a_transition <- a_transition_dynamics <- array(
    data = 0,
    dim = c(n_treatments, n_t, n_states, n_states),
    dimnames = list(v_treatments, 1:n_t, v_states, v_states)
  )
  
  # Transition probabilities for treatment 1
  # From health state 1 "Alive"
  a_transition[1, , v_states[1], v_states[1]] <- 1 - m_gen_pop_mortality[, 2]   # Stay in health state 1 "Alive"
  a_transition[1, , v_states[1], v_states[2]] <- m_gen_pop_mortality[, 2]       # Transition to health state 2 "Death"
  
  # From health state 2 "Death"
  a_transition[1, , v_states[2], v_states[2]] <- 1                              # Heath state 4 "Death" is absorbing
  
  # Transition probabilities for treatment 2
  a_transition[2, , , ] <- a_transition[1, , , ]                                # Copy from treatment 1
  
  # checking whether transitions sum to 1 using testthat package
  # for (i_treatment in 1:n_treatments){ # loop through the treatment options
  #   for (t in 1:n_t){ # loop through the number of cycles
  #     testthat::expect_equal(as.vector(rowSums(a_transition[i_treatment, t, 1:n_states,])), rep(1, n_states))
  #   } # close for loop for cycles
  # } # close for loop for treatments
  
  # check min and max transition probabilities
  # min(a_transition)
  # max(a_transition)
  
  #### Markov trace ----
  # Initialize Markov trace array
  a_state_trace <- array(
    data = NA,
    dim = c(n_treatments, n_t + 1, n_states),
    dimnames = list(v_treatments, 0:n_t, v_states)
  )
  a_state_trace[, 1, ] <- matrix(c(1, 0), nrow = n_treatments, ncol = n_states, byrow = TRUE) # Starting health state: 1 "Event free"
  
  # State transitions using nested loops 
  for (i_treatment in 1:n_treatments) {
    for (t in 1:n_t) {
      a_state_trace[i_treatment, t + 1, ] <- a_state_trace[i_treatment, t, ] %*% a_transition[i_treatment, t, , ] # estimate Markov trace for next cycle
      a_transition_dynamics[i_treatment, t, , ] <- diag(a_state_trace[i_treatment, t, ]) %*% a_transition[i_treatment, t, , ] # estimate transition dynamics for next cycle
    }
  }
  
  # checking whether cohort sums to 1 (each cycle) using testthat package
  # for (i_treatment in 1:n_treatments){ # loop through the treatment options
  #     testthat::expect_equal(as.vector(rowSums(a_state_trace[i_treatment, , ])), rep(1, n_t + 1))
  # } # close for loop for treatments
  
  #### Outcomes ----
  # calculate discount weight for each cycle
  m_discount <- matrix(c(1 / (1 + params$discount_costs) ^ (0:n_t / 12), 
                         1 / (1 + params$discount_qalys) ^ (0:n_t / 12),
                         1 / (1 + params$discount_lys) ^ (0:n_t / 12)),
                       nrow = n_t + 1, ncol = 3)
  
  # Cost and (dis)utility matrices
  m_cost <- matrix(c(cost_alive,                     # Cost for health state 1 "Alive"
                     params$cost_death,                     # Cost for health state 2 "Death"
                     cost_alive,                     # Cost for health state 1 "Alive" with intervention
                     params$cost_death),                    # Cost for health state 2 "Death" with intervention
                   nrow = n_t + 1, ncol = n_states * n_treatments, byrow = TRUE)
  
  # SAFELY CAP duration TO MAX time horizon
  duration_cost <- min(params$duration_cost_impact, n_t)
  if (duration_cost > 0) {
    m_cost[2:(duration_cost + 1), 3] <- params$uptake * cost_alive_intervention + (1-params$uptake) * cost_alive # Add technology specific costs
  }
  
  m_utility <- matrix(c(params$utility_alive_SoC,           # Utility for health state 1 "Alive"
                        params$utility_death,               # Utility for health state 2 "Death"
                        params$utility_alive_SoC,           # Utility for health state 1 "Alive" with intervention
                        params$utility_death),              # Utility for health state 2 "Death" with intervention
                      nrow = n_t + 1, ncol = n_states * n_treatments, byrow = TRUE)
  
  duration_util <- min(params$duration_utility_impact, n_t)
  if (duration_util > 0) {
    m_utility[2:(duration_util + 1), 3] <- params$uptake * params$utility_alive_intervention + (1-params$uptake) * params$utility_alive_SoC # Add technology specific utility values
  } 
  
  m_utility[1, 1] <- params$utility_alive_base_SoC # Baseline utility SoC
  m_utility[1, 3] <- params$utility_alive_base_intervention # Baseline utility intervention
  
  m_utility <- apply(m_utility, 2, function(col) pmin(m_gen_pop_utility[,2], col)) # Ensure utility  are =< age and gender matched general population utility
  
  # Initialize costs, QALYs and LYs arrays
  a_costs <- a_qalys <- array(
    data = NA,
    dim = c(n_treatments, n_t + 1, n_states),
    dimnames = list(v_treatments, 0:n_t, v_states)
  )
  
  a_lys <- array(
    data = NA,
    dim = c(n_treatments, n_t + 1, n_states - 1),
    dimnames = list(v_treatments, 0:n_t, v_states[-2])
  )
  
  # Calculate costs, QALYs and LYs per cycle
  a_costs[1, ,] <- a_state_trace[1, , ] * m_cost[, 1:2] * m_discount[, 1]           # Multiply by cost matrix and discount factor
  a_costs[2, ,] <- a_state_trace[2, , ] * m_cost[, 3:4] * m_discount[, 1]           # Multiply by cost matrix and discount factor
  a_costs[1, 1, 1] <- a_costs[1, 1, 1] + params$cost_t1                             # Add strategy costs
  a_costs[2, 1, 1] <- a_costs[2, 1, 1] + params$cost_t2                             # Add strategy costs

  a_qalys[1, ,] <- a_state_trace[1, , ] * m_utility[, 1:2] * m_discount[, 2] * 1/12 # Multiply by cost matrix, discount factor and time correction (monthly cycles)
  a_qalys[2, ,] <- a_state_trace[2, , ] * m_utility[, 3:4] * m_discount[, 2] * 1/12 # Multiply by cost matrix, discount factor and time correction (monthly cycles)

  a_lys <- a_state_trace[, , -2] * rep(m_discount[, 3], each = n_treatments) * 1/12 # Remove health state 2 "Death", multiply by discount factor and time correction  (monthly cycles) 
  
  # Event related costs (calculated using a_transition_dynamics)
  ## Not applicable in this model
  
  # Return results
  return(
    if(intermediate == FALSE) { 
      setNames(
        c(rowSums(a_costs),  
          rowSums(a_qalys), 
          rowSums(a_lys)
        ), # end c    
        c(paste0("Cost_", v_treatments),                          
          paste0("QALY_", v_treatments), 
          paste0("LY_", v_treatments)
        ) # end c
      ) # end setNames
    } else {
      data.matrix( # convert dataframe to matrix (for storing in array)
        data.frame( # create dataframe
          "t1.Health.state.costs" = a_costs[1,,],
          "t2.Health.state.costs" = a_costs[2,,],
          "t1.Health.state.qalys" = a_qalys[1,,],
          "t2.Health.state.qalys" = a_qalys[2,,],
          "t1.Trace" = a_state_trace[1,,],
          "t2.Trace" = a_state_trace[2,,]
        ) # close dataframe
      ) # close data.matrix
    } # close if statement
  ) # close return
}

