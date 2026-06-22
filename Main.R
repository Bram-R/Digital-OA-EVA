#### Description: Probabilistic State-Transition Model ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# General settings
options(scipen = 999, max.print = 10000, digits = 4)

# Load and install necessary libraries
required_packages <- c(
  "docstring", "DiagrammeR", "dampack", "BCEA"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages(lapply(required_packages, require, character.only = TRUE))

# Clear workspace
rm(list = ls())

# Load custom functions
source("f_stm_diagram.R")          # check function using: docstring(f_stm_diagram)
source("f_input.R")                # check function using: docstring(f_input)
source("f_model.R")                # check function using: docstring(f_model)
source("f_gen_pop_utility.R")      # check function using: docstring(f_gen_pop_utility)
source("f_gen_pop_mortality.R")    # check function using: docstring(f_gen_pop_mortality)

# Define  color-blind-friendly palette (Okabe–Ito subset)
cb_palette <- c("#D55E00", "#0072B2", "#009E73", "#F0E442", "#CC79A7")

# Set as global default discrete color and fill scales
options(
  ggplot2.discrete.fill = cb_palette,
  ggplot2.discrete.colour = cb_palette
)

# options( # optional: restore defaults
#   ggplot2.discrete.fill = NULL,
#   ggplot2.discrete.colour = NULL
# )

#### General Setup ----
v_states <- c("Alive", "Dead")                         # Vector of model health states
n_states <- length(v_states)                           # Number of health states
v_treatments <- c("Standard care",
                  "Digital technologies")        # Vector of strategy names
n_treatments <- length(v_treatments)                   # Number of treatments
n_t <- 1 * 11                                          # Model time horizon (monthly cycle)
n_sim <- 5000                                          # Number of Monte Carlo simulations
n_age_baseline <- 62                                   # Baseline age
n_p_female <- 0.683                                    # Proportion females
n_currency <- "Pound"
n_wtp <- 20000 # willingness to pay value
v_wtp <- seq(from = 0, to = 50000, by = 2000)          # willingness to pay vector

#### Model structure ----
state_labels <- list(
  state2 = c(v_states[1], "0,2"),
  state3 = c(v_states[2], "0,0")
)

transitions <- data.frame(
  from = c("state2", "state2",
           "state3"),
  to = c("state2", "state3", 
         "state3"),
  label = rep("", 3) 
) 

f_stm_diagram(state_labels, transitions)
rm(state_labels, transitions)
rm(validate_state_labels, validate_transitions, generate_nodes, generate_edges, get_self_loop_style)

#### Model Inputs ----
# Create a dataframe for probabilistic sensitivity analysis (PSA) inputs
m_gen_pop_utility <- f_gen_pop_utility(n_age_baseline = n_age_baseline, n_t = n_t, n_p_female = n_p_female)
m_gen_pop_mortality <- f_gen_pop_mortality(n_age_baseline = n_age_baseline, n_t = n_t + 1, n_p_female = n_p_female)[-1,]
df_input <- f_input(n_sim = n_sim)

inputs_overview <- data.frame(
  pars = names(df_input), # parameter names
  deterministic = as.numeric(f_input(n_sim = 1)), # deterministic values
  mean = as.numeric(lapply(df_input, mean)), # mean values
  sd = as.numeric(lapply(df_input, sd)), # sd values
  min = as.numeric(lapply(df_input, min)), # min values
  perc025 = stack(sapply(df_input, quantile, prob = 0.025, names = FALSE))[,1], # 2.5th percentile values
  perc250 = stack(sapply(df_input, quantile, prob = 0.250, names = FALSE))[,1], # 25th percentile values
  median = as.numeric(lapply(df_input, median)), # median values
  perc750 = stack(sapply(df_input, quantile, prob = 0.750, names = FALSE))[,1], # 75th percentile values
  perc975 = stack(sapply(df_input, quantile, prob = 0.975, names = FALSE))[,1], # 97.5th percentile values
  max = as.numeric(lapply(df_input, max)) # max values
) # close params_range dataframe 

write.table(inputs_overview, "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)

#### Model Results ----
# f_model(df_input[1, ]) # test model

# Use a list to store result matrices
m_results <- matrix(
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

# Run PSA
for (x in 1:n_sim) m_results[x, ] <- f_model(df_input[x, ])

# obtain dampack object
v_out_mean <- as.vector(colMeans(m_results[, 1:4])) # calculate average results

obj_icers <- calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:2], # mean costs per strategy
  effect = v_out_mean[3:4], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print(v_out_mean[3] * n_wtp - v_out_mean[1], digits = 10) # NMB standard care
print(v_out_mean[3] - (v_out_mean[1] / n_wtp), digits = 10) # NHB standard care

print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB digital technologies
print(v_out_mean[4] - v_out_mean[2] / n_wtp, digits = 10) # NHB digital technologies

obj_psa_dam <- make_psa_obj( # generate dampack PSA object
  cost = as.data.frame(m_results[,1:2]), # dataframe of costs
  effectiveness = as.data.frame(m_results[,3:4]), # dataframe of effects
  parameters = df_input, # dataframe of input parameters
  strategies = v_treatments, # vector of strategy names
) # end make_psa_obj

# obtain bcea object
obj_bcea <- bcea( # create bcea object
  e = m_results[,3:4], # matrix of effects
  c = m_results[,1:2], # matrix of costs
  ref = 2, # selects the 2nd row of (e,c) as reference intervention
  interventions = v_treatments, # vector of strategy names
  Kmax = 100000 # maximum value possible for the wtp
) # bcea end

##### View results ----
summary(m_results[,1:4]) # alternatively use summarytools::dfSummary(m_results[,1:4])
plot(density(m_results[,1]))
plot(density(m_results[,2]))
plot(density(m_results[,3]))
plot(density(m_results[,4]))

print(obj_icers)
print((v_out_mean[4] - v_out_mean[3]) * n_wtp - (v_out_mean[2] - v_out_mean[1]), digits = 10) #NMB
print((v_out_mean[4] - v_out_mean[3]) - (v_out_mean[2] - v_out_mean[1]) / n_wtp, digits = 10) #NHB

# Cost effectiveness frontier
plot(
  x = obj_icers, # icers object
  currency = n_currency, # costs units
  effect_units = "QALYs", # effects units
  label = "all" # add label to all strategies
) # plot end

# Incremental cost effectiveness plane
contour2(
  he = obj_bcea, # bcea object
  title = "",
  ylab = "Incremental costs (GBP)",
  xlab = "Incremental QALYs",
  wtp = n_wtp, # selects the wtp
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  #xlim = c(-0.0005, 0.0005),
  nlevels = 4, # selects the number of levels to be plotted (default=4)
  levels = NULL, # specifies the actual levels to be plotted (default=NULL, so that R will decide)
  scale = 0.5, # scales the bandwiths for both x- and y-axis (default=0.5)
  graph = "base" # uses base graphics to produce the plot,
) # contour2 end

# Incremental Benefit plots
eib.plot(
  he = obj_bcea, # bcea object
  ylab = "Expected incremental benefit",
  xlab = "Willingness to pay (Costs (GBP) per QALY)",
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  plot.cri = TRUE,
  graph = "base" # use base graphics (instead of ggplot)
) # eib.plot end

ib.plot(
  he = obj_bcea, # bcea object
  comparison = 1, # if more than 2 interventions, selects the pairwise comparison
  wtp = n_wtp, # selects the wtp
  graph = "base" # uses base graphics (instead of ggplot)
) # ib.plot end

# CEAC
plot(
  ceac(
    psa = obj_psa_dam, 
    wtp = v_wtp
  ), # ceac end
  ylim = c(0, 1),
  currency = n_currency,
  frontier = TRUE,
  points = TRUE
) # plot end

# ELC
plot(
  calc_exp_loss(
    psa = obj_psa_dam, 
    wtp = v_wtp
  ), # calc_exp_loss end
  currency = n_currency,
  n_x_ticks = 8, 
  n_y_ticks = 6
) # plot end

# EVPI
plot(
  calc_evpi(
    psa = obj_psa_dam, 
    wtp = v_wtp,
    pop = 1
  ), # calc_evpi end
  currency = n_currency,
  n_x_ticks = 8, 
  n_y_ticks = 6
) # plot end

# info rank
info.rank(
  parameter = 1:ncol(df_input),
  inp = createInputs(inputs = df_input, print_is_linear_comb = FALSE),
  he = obj_bcea,
  wtp = n_wtp,
  howManyPars = 10, 
  graph = "base"
) # plot end

