#### Description: Sensitivity and scenario analyses of a State-Transition Model ####
#### Conventions: n = single number, v = vector, df = dataframe, m = matrix, a = array, l = list ####

# General settings
options(scipen = 999, max.print = 10000, digits = 4)

# Load and install necessary libraries
required_packages <- c(
  "dampack"
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

# Define  color-blind-friendly palette (Okabe–Ito subset)
cb_palette <- c("#D55E00", "#0072B2", "#009E73", "#F0E442", "#CC79A7")

# Set as global default discrete color and fill scales
options(
  ggplot2.discrete.fill = cb_palette,
  ggplot2.discrete.colour = cb_palette
)

# options( # optional: restore deffaults
#   ggplot2.discrete.fill = NULL,
#   ggplot2.discrete.colour = NULL
# )

#### General Setup ----
v_states <- c("Alive", "Dead")                         # Vector of model health states
n_states <- length(v_states)                            # Number of health states
v_treatments <- c("Standard care",
                  "Digital technologies")         # Vector of strategy names
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

##### Deterministic base-case #####
# store deterministic base-case results
m_results <- f_model(f_input(n_sim = 1))

calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results[1:2]), # mean costs per strategy
  effect = as.numeric(m_results[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print((m_results[4] - m_results[3]) * n_wtp - (m_results[2] - m_results[1]), digits = 10) #incremental NMB
print((m_results[4] - m_results[3]) - (m_results[2] - m_results[1]) / n_wtp, digits = 10) #incremental NHB

print(m_results[3] * n_wtp - m_results[1], digits = 10) # NMB standard care
print(m_results[3] - (m_results[1] / n_wtp), digits = 10) # NHB standard care

print(m_results[4] * n_wtp - m_results[2], digits = 10) # NMB digital technologies
print(m_results[4] - m_results[2] / n_wtp, digits = 10) # NHB digital technologies


##### Obtain deterministic sensitivity analyses #####
df_input_owsa <- f_input() # generate input parameters (min and max values for OWSA) with default n_sim
df_input_owsa <- df_input_owsa[, apply(df_input_owsa, 2, var, na.rm = TRUE) != 0] # only select parameters with variance > 0

obj_owsa_dam <- run_owsa_det( # generate dampack OWSA object
  params_range = data.frame( # dataframe to be used for OWSA
    pars = names(df_input_owsa), # parameter names
    min = stack(sapply(df_input_owsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for owsa
    max = stack(sapply(df_input_owsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for owsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  #lapply(df_input_owsa, mean), # alternative to f_input(n_sim = 1) = mean of psa inputs 
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB", "iQALY", "iCost", "iCER", "iNMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_owsa_det

df_input_twsa <- df_input_owsa[, c("utility_alive_intervention", "duration_utility_impact")]  
obj_twsa_dam1 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("uptake", "utility_alive_intervention")] 
obj_twsa_dam2 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("utility_alive_intervention", "cost_t2")] 
obj_twsa_dam3 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("uptake", "cost_t2")] 
obj_twsa_dam4 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("utility_alive_SoC", "utility_alive_intervention")] 
obj_twsa_dam5 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("duration_cost_impact", "cost_t2")] 
obj_twsa_dam6 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("p_red_physio_digital", "uptake")] 
obj_twsa_dam7 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("p_red_physio_digital", "duration_cost_impact")] 
obj_twsa_dam8 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("p_red_gp_digital", "duration_cost_impact")] 
obj_twsa_dam9 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = stack(sapply(df_input_twsa, quantile, prob = 0.025, names = FALSE))[,1], # use 95% percentiles for twsa
    max = stack(sapply(df_input_twsa, quantile, prob = 0.975, names = FALSE))[,1] # use 95% percentiles for twsa 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

df_input_twsa <- df_input_owsa[, c("uptake", "cost_t2")] 
obj_twsa_dam10 <- run_twsa_det( # generate dampack TWSA object
  params_range = data.frame( # dataframe to be used for TWSA
    pars = names(df_input_twsa), # parameter names
    min = 0.50, 
    max = 375 
  ), # close params_range dataframe 
  params_basecase = f_input(n_sim = 1), # base-case parameters (n_sim = 1 provides deterministic parameters)
  nsamp = 100, # number of sets of parameter values to be generated (between min and max)
  FUN = f_wrapper_sa, wtp = n_wtp, # function that produces outcomes
  outcomes = c("QALY", "Cost", "NMB"), # outcomes of interest produced by FUN 
  progress = TRUE # show progression in console
) # end run_twsa_det

##### View deterministic sensitivity analyses results #####
owsa_opt_strat( # gives error as no parameter leads to changes in optimal strategy as they vary
  obj_owsa_dam$owsa_QALY,
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end

owsa_opt_strat( # gives error as no parameter leads to changes in optimal strategy as they vary
  obj_owsa_dam$owsa_Cost,
  maximize = FALSE, # need to minimize costs
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end

owsa_opt_strat(
  obj_owsa_dam$owsa_NMB,
  plot_const = FALSE, # TRUE = do also plot parameters that don't lead to changes in optimal strategy as they vary
  n_x_ticks = 5
) # owsa_opt_strat end

plot(
  obj_owsa_dam$owsa_QALY,
  n_y_ticks = 3, 
  n_x_ticks = 2
) # plot end

plot(
  obj_owsa_dam$owsa_Cost,
  n_y_ticks = 3, 
  n_x_ticks = 2
) # plot end

plot(
  obj_owsa_dam$owsa_NMB,
  n_y_ticks = 3, 
  n_x_ticks = 2
) # plot end

owsa_tornado(
  obj_owsa_dam$owsa_iQALY,
  #min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end

owsa_tornado(
  obj_owsa_dam$owsa_iCost,
  #min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end

owsa_tornado(
  obj_owsa_dam$owsa_iNMB,
  #min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end

owsa_tornado(
  obj_owsa_dam$owsa_iCER,
  #min_rel_diff = 0.05 # only plot parameters that lead to a relative change in the outcome greater than or equal to this value
) # owsa_tornado end

plot(obj_twsa_dam1$twsa_NMB)
plot(obj_twsa_dam2$twsa_NMB)
plot(obj_twsa_dam3$twsa_NMB)
plot(obj_twsa_dam4$twsa_NMB)
plot(obj_twsa_dam5$twsa_NMB)
plot(obj_twsa_dam6$twsa_NMB)
plot(obj_twsa_dam7$twsa_NMB)
plot(obj_twsa_dam8$twsa_NMB)
plot(obj_twsa_dam9$twsa_NMB)
plot(obj_twsa_dam10$twsa_NMB)

##### Scenario analyses #####

# 1 self-management with higher technology costs and additional features
df_input_scen1 <- f_input(n_sim = 1)
df_input_scen1$cost_t2 <- mean(c(229.50, 46.15, 296.25, 375, 112.50))

m_results_scen <- f_model(df_input_scen1)

calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_scen[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_scen[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print((m_results_scen[4] - m_results_scen[3]) * n_wtp - (m_results_scen[2] - m_results_scen[1]), digits = 10) #incremental NMB
print((m_results_scen[4] - m_results_scen[3]) - (m_results_scen[2] - m_results_scen[1]) / n_wtp, digits = 10) #incremental NHB
print(m_results_scen[3] * n_wtp - m_results_scen[1], digits = 10) # NMB standard care
print(m_results_scen[3] - (m_results_scen[1] / n_wtp), digits = 10) # NHB standard care
print(m_results_scen[4] * n_wtp - m_results_scen[2], digits = 10) # NMB digital technologies
print(m_results_scen[4] - m_results_scen[2] / n_wtp, digits = 10) # NHB digital technologies

# 1 self-management with higher technology costs and additional features (probabilistic)
df_input_scen1 <- f_input(n_sim = 5000, 
                          seed = 12345)
df_input_scen1$cost_t2 = generate_gamma(mean(c(229.50, 46.15, 296.25, 375, 112.50)), 
                                        mean(c(229.50, 46.15, 296.25, 375, 112.50)) * 0.30, 
                                        n_sim, T)

m_results_scen <- matrix(
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

for (x in 1:n_sim) m_results_scen[x, ] <- f_model(df_input_scen1[x, ])

v_out_mean <- as.vector(colMeans(m_results_scen[, 1:4])) # calculate average results

calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:2], # mean costs per strategy
  effect = v_out_mean[3:4], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print(v_out_mean[3] * n_wtp - v_out_mean[1], digits = 10) # NMB standard care
print(v_out_mean[3] - (v_out_mean[1] / n_wtp), digits = 10) # NHB standard care
print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB digital technologies
print(v_out_mean[4] - v_out_mean[2] / n_wtp, digits = 10) # NHB digital technologies


# 2 self-management with lower technology costs and limited additional features
df_input_scen2 <- f_input(n_sim = 1)
df_input_scen2$cost_t2 <- mean(c(18.84, 45.28))
df_input_scen2$cost_prescriptions <- NA     # marked confidential
df_input_scen2$cost_gp <- NA                # marked confidential
df_input_scen2$cost_physio <- NA           # marked confidential
df_input_scen2$p_red_medication_digital <- NA     # marked confidential
df_input_scen2$p_red_prescription_digital <- NA   # marked confidential
df_input_scen2$p_red_gp_digital <- NA           # marked confidential
df_input_scen2$p_red_physio_digital <- NA        # marked confidential

m_results_scen <- f_model(df_input_scen2)

calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_scen[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_scen[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print((m_results_scen[4] - m_results_scen[3]) * n_wtp - (m_results_scen[2] - m_results_scen[1]), digits = 10) #incremental NMB
print((m_results_scen[4] - m_results_scen[3]) - (m_results_scen[2] - m_results_scen[1]) / n_wtp, digits = 10) #incremental NHB
print(m_results_scen[3] * n_wtp - m_results_scen[1], digits = 10) # NMB standard care
print(m_results_scen[3] - (m_results_scen[1] / n_wtp), digits = 10) # NHB standard care
print(m_results_scen[4] * n_wtp - m_results_scen[2], digits = 10) # NMB digital technologies
print(m_results_scen[4] - m_results_scen[2] / n_wtp, digits = 10) # NHB digital technologies

# 2 self-management with lower technology costs and limited additional features (probabilistic)
df_input_scen2 <- f_input(n_sim = 5000, 
                          seed = 12345)
df_input_scen2$cost_t2 <- generate_gamma(mean(c(18.84, 45.28)), 
                                         mean(c(18.84, 45.28)) * 0.30, 
                                         n_sim, T)
df_input_scen2$cost_prescriptions <- generate_gamma(NA, NA * 0.30, n_sim, T) * 27     # marked confidential
df_input_scen2$cost_gp <- generate_gamma(NA, NA * 0.30, n_sim, T) * 45                # marked confidential
df_input_scen2$cost_physio <- generate_gamma(NA, NA * 0.30, n_sim, T) * 41             # marked confidential
df_input_scen2$p_red_medication_digital <- generate_static(NA, n_sim, T)             # marked confidential
df_input_scen2$p_red_prescription_digital <- generate_static(NA, n_sim, T)           # marked confidential
df_input_scen2$p_red_gp_digital <- generate_beta(NA, NA * 0.30, n_sim, T)             # marked confidential
df_input_scen2$p_red_physio_digital <- generate_beta(NA, NA * 0.30, n_sim, T) # marked confidential                                     

m_results_scen <- matrix(
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

for (x in 1:n_sim) m_results_scen[x, ] <- f_model(df_input_scen2[x, ])

v_out_mean <- as.vector(colMeans(m_results_scen[, 1:4])) # calculate average results

calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:2], # mean costs per strategy
  effect = v_out_mean[3:4], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print(v_out_mean[3] * n_wtp - v_out_mean[1], digits = 10) # NMB standard care
print(v_out_mean[3] - (v_out_mean[1] / n_wtp), digits = 10) # NHB standard care
print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB digital technologies
print(v_out_mean[4] - v_out_mean[2] / n_wtp, digits = 10) # NHB digital technologies


# 3 technologies with shorter duration 
df_input_scen3 <- f_input(n_sim = 1)
df_input_scen3$cost_t2 <- mean(c(112.50, 229.50))
df_input_scen3$duration_utility_impact <- 4
df_input_scen3$duration_cost_impact <- 4

m_results_scen <- f_model(df_input_scen3)

calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_scen[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_scen[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print((m_results_scen[4] - m_results_scen[3]) * n_wtp - (m_results_scen[2] - m_results_scen[1]), digits = 10) #incremental NMB
print((m_results_scen[4] - m_results_scen[3]) - (m_results_scen[2] - m_results_scen[1]) / n_wtp, digits = 10) #incremental NHB
print(m_results_scen[3] * n_wtp - m_results_scen[1], digits = 10) # NMB standard care
print(m_results_scen[3] - (m_results_scen[1] / n_wtp), digits = 10) # NHB standard care
print(m_results_scen[4] * n_wtp - m_results_scen[2], digits = 10) # NMB digital technologies
print(m_results_scen[4] - m_results_scen[2] / n_wtp, digits = 10) # NHB digital technologies


# 3 technologies with shorter duration (probabilistic)
df_input_scen3 <- f_input(n_sim = 5000, 
                          seed = 12345)
df_input_scen3$cost_t2 <- generate_gamma(mean(c(112.50, 229.50)), 
                                         mean(c(112.50, 229.50)) * 0.30,
                                         n_sim, T)
df_input_scen3$duration_utility_impact <- round(generate_gamma(4, 4 * 0.30, n_sim, T))
df_input_scen3$duration_cost_impact <- round(generate_gamma(4, 4 * 0.30, n_sim, T))

m_results_scen <- matrix(
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

for (x in 1:n_sim) m_results_scen[x, ] <- f_model(df_input_scen3[x, ])

v_out_mean <- as.vector(colMeans(m_results_scen[, 1:4])) # calculate average results

calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:2], # mean costs per strategy
  effect = v_out_mean[3:4], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print(v_out_mean[3] * n_wtp - v_out_mean[1], digits = 10) # NMB standard care
print(v_out_mean[3] - (v_out_mean[1] / n_wtp), digits = 10) # NHB standard care
print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB digital technologies
print(v_out_mean[4] - v_out_mean[2] / n_wtp, digits = 10) # NHB digital technologies

# 4 technologies that replace standard care 
df_input_scen4 <- f_input(n_sim = 1)
df_input_scen4$p_red_gp_digital <- 1
df_input_scen4$p_red_physio_digital <- 1

m_results_scen <- f_model(df_input_scen4)

calculate_icers( # create calculate_icers object
  cost = as.numeric(m_results_scen[1:2]), # mean costs per strategy
  effect = as.numeric(m_results_scen[3:4]), # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print((m_results_scen[4] - m_results_scen[3]) * n_wtp - (m_results_scen[2] - m_results_scen[1]), digits = 10) #incremental NMB
print((m_results_scen[4] - m_results_scen[3]) - (m_results_scen[2] - m_results_scen[1]) / n_wtp, digits = 10) #incremental NHB
print(m_results_scen[3] * n_wtp - m_results_scen[1], digits = 10) # NMB standard care
print(m_results_scen[3] - (m_results_scen[1] / n_wtp), digits = 10) # NHB standard care
print(m_results_scen[4] * n_wtp - m_results_scen[2], digits = 10) # NMB digital technologies
print(m_results_scen[4] - m_results_scen[2] / n_wtp, digits = 10) # NHB digital technologies

# 4 technologies that replace standard care (probabilistic)
df_input_scen4 <- f_input(n_sim = 5000, 
                          seed = 12345)
df_input_scen4$p_red_gp_digital <- generate_static(1, n_sim, T)
df_input_scen4$p_red_physio_digital <- generate_static(1, n_sim, T) 

m_results_scen <- matrix(
  data = NA,
  nrow = n_sim,
  ncol = 3 * n_treatments,
  dimnames = list(1:n_sim, c(paste0("Cost_", v_treatments), paste0("QALY_", v_treatments), paste0("LY_", v_treatments)))
)

for (x in 1:n_sim) m_results_scen[x, ] <- f_model(df_input_scen4[x, ])

v_out_mean <- as.vector(colMeans(m_results_scen[, 1:4])) # calculate average results

calculate_icers( # create calculate_icers object
  cost = v_out_mean[1:2], # mean costs per strategy
  effect = v_out_mean[3:4], # mean effects per strategy
  strategies = v_treatments # vector of strategy names
) # calculate_icers end

print(v_out_mean[3] * n_wtp - v_out_mean[1], digits = 10) # NMB standard care
print(v_out_mean[3] - (v_out_mean[1] / n_wtp), digits = 10) # NHB standard care
print(v_out_mean[4] * n_wtp - v_out_mean[2], digits = 10) # NMB digital technologies
print(v_out_mean[4] - v_out_mean[2] / n_wtp, digits = 10) # NHB digital technologies
