#### Description: Intermediate results of a State-Transition Model ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# General settings
options(scipen = 999, max.print = 10000, digits = 4)

# Load and install necessary libraries
required_packages <- c(
  "dampack", "matrixStats", "summarytools"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages(lapply(required_packages, require, character.only = TRUE))

# Clear workspace
rm(list = ls())

# Load custom functions
source("f_input.R")                # check function using: docstring(f_input)
source("f_model.R")                # check function using: docstring(f_model)
source("f_wrapper.R")              # check function using: docstring(f_wrapper_sa)
source("f_gen_pop_utility.R")      # check function using: docstring(f_gen_pop_utility)
source("f_gen_pop_mortality.R")    # check function using: docstring(f_gen_pop_mortality)
source("f_healthstate_costs.R")    # check function using: docstring(f_healthstate_costs)

#### General Setup ----
v_states <- c("Alive", "Dead")                         # Vector of model health states
n_states <- length(v_states)                            # Number of health states
v_treatments <- c("Current_practice",
                  "Current_practice_with_Tech")         # Vector of strategy names
n_treatments <- length(v_treatments)                    # Number of treatments
n_t <- 1 * 11                                          # Model time horizon (monthly cycle)
n_sim <- 5000                                           # Number of Monte Carlo simulations
n_age_baseline <- 62                                    # Baseline age
n_p_female <- 0.683                                     # Proportion females
n_currency <- "Pound"
n_wtp <- 20000 # willingness to pay value
v_wtp <- seq(from = 0, to = 50000, by = 2000)           # willingness to pay vector

#### Model Inputs ----
# Create a dataframe for probabilistic sensitivity analysis (PSA) inputs
m_gen_pop_utility <- f_gen_pop_utility(n_age_baseline = n_age_baseline, n_t = n_t, n_p_female = n_p_female)
m_gen_pop_mortality <- f_gen_pop_mortality(n_age_baseline = n_age_baseline, n_t = n_t + 1, n_p_female = n_p_female)[-1,]
df_input <- f_input(n_sim = n_sim)

##### Obtain intermediate results using wrapper intermediate function ----
# Probabilistic results (intermediate)
a_out_interm <- f_wrapper_intermediate(df_input)

# str(a_out_interm[,,]) # inspect
# names(a_out_interm[1, , 1]) # inspect

##### Costs and QALYs per cycle ----
a_out_cycle_res <- array(
  c(apply(a_out_interm[, 1:2, ], c(1, 3), sum),
    apply(a_out_interm[, 3:4, ], c(1, 3), sum),
    apply(a_out_interm[, 5:6, ], c(1, 3), sum),
    apply(a_out_interm[, 7:8, ], c(1, 3), sum)
  ),
  dim = c(481, 5000, 4)
)

a_out_cycle_res <- aperm(a_out_cycle_res, perm = c(1, 3, 2))

m_out_cycle_res <- rowMeans(a_out_cycle_res[, 1:4, ], dims = 2)
m_out_cycle_res_percentiles <- cbind(
  rowQuantiles(a_out_cycle_res[, 1, ], probs = c(0.025, 0.975)),
  rowQuantiles(a_out_cycle_res[, 2, ], probs = c(0.025, 0.975)),
  rowQuantiles(a_out_cycle_res[, 3, ], probs = c(0.025, 0.975)),
  rowQuantiles(a_out_cycle_res[, 4, ], probs = c(0.025, 0.975))
)

# Probabilistic costs
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 1]),
    cumsum(m_out_cycle_res_percentiles[, 1]), 
    cumsum(m_out_cycle_res_percentiles[, 2])
  ),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Cumulative costs (probabilistic) ", v_treatments[1]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)

matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 2]),
    cumsum(m_out_cycle_res_percentiles[, 3]), 
    cumsum(m_out_cycle_res_percentiles[, 4])
  ),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Cumulative costs (probabilistic) ", v_treatments[2]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)

matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 1]),
    cumsum(m_out_cycle_res[, 2])
  ),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Cumulative costs (probabilistic) ", v_treatments[1], " and ", v_treatments[2]),
  col = c(2, 2),
  lty = c(1, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  v_treatments,
  cex = 0.5,
  col = c(2, 2),
  lty = c(1, 2),
  bty = "n"
)

# Probabilistic QALYs 
matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 3]),
    cumsum(m_out_cycle_res_percentiles[, 5]), 
    cumsum(m_out_cycle_res_percentiles[, 6])
  ),
  type = "l",
  ylab = "QALYs",
  xlab = "Cycle",
  main = paste0("Cumulative QALYs (probabilistic) ", v_treatments[1]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "bottomright", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)

matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 4]),
    cumsum(m_out_cycle_res_percentiles[, 7]), 
    cumsum(m_out_cycle_res_percentiles[, 8])
  ),
  type = "l",
  ylab = "QALYs",
  xlab = "Cycle",
  main = paste0("Cumulative QALYs (probabilistic) ", v_treatments[2]),
  col = c(1, 1, 1),
  lty = c(1, 2, 2)
)
legend(
  "bottomright", 
  inset = c(0.05, 0),
  c("Probabilistic mean", dimnames(m_out_cycle_res_percentiles[, 1:2])[[2]]),
  cex = 0.5,
  col = c(1, 1, 1),
  lty = c(1, 2, 2),
  bty = "n"
)

matplot(
  x = 0:n_t,
  y = cbind(
    cumsum(m_out_cycle_res[, 3]),
    cumsum(m_out_cycle_res[, 4])
  ),
  type = "l",
  ylab = "QALYs",
  xlab = "Cycle",
  main = paste0("Cumulative QALYs (probabilistic) ", v_treatments[1], " and ", v_treatments[2]),
  col = c(2, 2),
  lty = c(1, 2)
)
legend(
  "topleft", 
  inset = c(0.05, 0),
  v_treatments,
  cex = 0.5,
  col = c(2, 2),
  lty = c(1, 2),
  bty = "n"
)

##### Traces per cycle ----
m_trace <- rowMeans(a_out_interm[, 9:12, ], dims = 2)

# Probabilistic
matplot(
  x = 0:n_t,
  y = m_trace[, 1:4], 
  ylim = c(0, 1),
  type = "l",
  ylab = "Probability of state occupancy",
  xlab = "Cycle",
  main = paste0("Average traces for ", v_treatments[1], " and ", v_treatments[2]),
  col = rep(2:3, 2),
  lty = c(rep(1, 2), rep(2, 2))
) # matplot end
legend(
  "topleft", 
  inset = c(0.05, 0),
  dimnames(m_trace[, 1:4])[[2]], 
  cex = 0.75,
  col = rep(2:3, 2),
  lty = c(rep(1, 2), rep(2, 2)),
  bty = "n"
) # legend end

##### Health state costs and QALYs per cycle ----
m_out_dis_cycle_res <- rowMeans(a_out_interm[, 1:8, ], dims = 2)

# Probabilistic
matplot(
  x = 0:n_t,
  y = m_out_dis_cycle_res[, 1:4], 
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Average health state costs for ", v_treatments[1], " and ", v_treatments[2]),
  col = rep(2:3, 2),
  lty = c(rep(1, 2), rep(2, 2))
) # matplot end
legend(
  "topright", 
  inset = c(0.05, 0),
  dimnames(m_out_dis_cycle_res[, 1:4])[[2]], 
  cex = 0.75,
  col = rep(2:3, 2),
  lty = c(rep(1, 2), rep(2, 2)),
  bty = "n"
) # legend end

# Probabilistic
matplot(
  x = 0:n_t,
  y = m_out_dis_cycle_res[, 5:8], 
  ylim = c(0, 1),
  type = "l",
  ylab = "Costs",
  xlab = "Cycle",
  main = paste0("Average health state QALYs ", v_treatments[1], " and ", v_treatments[2]),
  col = rep(2:3, 2),
  lty = c(rep(1, 2), rep(2, 2))
) # matplot end
legend(
  "topright", 
  inset = c(0.05, 0),
  dimnames(m_out_dis_cycle_res[, 5:8])[[2]], 
  cex = 0.75,
  col = rep(2:3, 2),
  lty = c(rep(1, 2), rep(2, 2)),
  bty = "n"
) # legend end

##### Summary of intermediate outcomes ----
view(
  dfSummary(
    t(colSums(a_out_interm, dims = 1)), # Convert to data frame automatically
    round.digits = 3,
    style = "grid",
    plain.ascii = FALSE,
    graph.magnif = 1.2,
    headings = FALSE,
    display.labels = FALSE,
    escape.pipe = TRUE,
    varnumbers = FALSE,
    labels.col = FALSE
  )
)

