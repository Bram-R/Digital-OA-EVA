#### Input function ----
f_input <- function(n_sim = 5000, 
                    seed = 12345) {
  #' Generate Inputs for Probabilistic Sensitivity Analysis (PSA)
  #'
  #' This function generates a structured data frame containing input parameters for 
  #' probabilistic sensitivity analysis (PSA). It incorporates various distributions 
  #' (beta, gamma, log-normal, and static values) to account for uncertainty in 
  #' model parameters related to transition probabilities, utilities, and costs.
  #'
  #' @param n_sim Integer. Number of simulations (use `1` for deterministic values). Default is `10000`.
  #' @param seed Integer. Random seed for reproducibility. Default is `12345`.
  #' 
  #' @details
  #' The function:
  #' \itemize{
  #'   \item Sets a random seed to ensure reproducibility.
  #'   \item Uses helper functions (`generate_static`, `generate_beta`, `generate_gamma`, `generate_lognormal`)
  #'         to sample values from different probability distributions.
  #'   \item Defines transition probabilities, health state utilities and costs.
  #' }
  #' 
  #' The generated inputs align with the structure needed for state-transition modeling in economic evaluations.
  #'
  #' @return A data frame (`df_input`) where each row corresponds to a unique set of sampled PSA parameters.
  #'
  #' @examples
  #' # Generate a small set of PSA inputs (100 simulations)
  #' psa_inputs <- f_input(n_sim = 100, seed = 12345)
  #'
  #' # Generate deterministic inputs (single set of parameters)
  #' deterministic_inputs <- f_input(n_sim = 1, seed = 12345)
  #'
  #' @export
  
  source("f_input_helper.R") # Load helper functions
  
  set.seed(seed)
  is_psa <- n_sim > 1
  
  # Parameter data frame
  df_input <- data.frame(
    
    # Model settings
    discount_costs = generate_static(0.000, n_sim, is_psa),       # Discount rate for costs (Source: Standard)
    discount_qalys = generate_static(0.000, n_sim, is_psa),       # Discount rate for QALYs (Source: Standard)
    discount_lys = generate_static(0.000, n_sim, is_psa),         # Discount rate for life-years (Source: Standard)
    
    # State-transition model probabilities
    ## Not applicable as it is an alive and death model informed by general population mortality data  
    
    
    # Health state utilities
    utility_alive_base_SoC = generate_beta(0.650, 0.140, n_sim, is_psa),                         ## Assumption based on Nero 2017 and Dieter 2023 
    utility_alive_SoC = generate_beta(0.666, 0.135, n_sim, is_psa),                              ## Assumption based on Nero 2017 and Dieter 2023  
    utility_alive_base_intervention = generate_static(NA, n_sim, is_psa),                        ## Set equal to SoC baseline utilty below   
    utility_alive_intervention = generate_beta(0.690, 0.150, n_sim, is_psa),                     ## Nero 2017 
    utility_death = generate_static(0, n_sim, is_psa),                                           ## Assumption
    
    # Strategy costs (year: XX) - one-off costs
    cost_t1 = generate_static(0, n_sim, is_psa),                                                 # costs strategy 1 (reference)
    cost_t2 = generate_gamma(mean(c(229.50, 46.15, 296.25, 375, 112.50, 18.84, 45.28)), 
                             mean(c(229.50, 46.15, 296.25, 375, 112.50, 18.84, 45.28)) * 0.30, 
                             n_sim, is_psa),                                                     # costs strategy 2 (strategy 1)   
    
    # Health state costs components (year: XX) - monthly costs
    cost_medication = (
      generate_gamma(0.060, 0.060 * 0.30, n_sim, is_psa) * 7.35 +                                # topical diclofenac
        generate_gamma(0.080, 0.080 * 0.30, n_sim, is_psa) * 2.70 +                              # topical ketoprofen
        generate_gamma(0.320, 0.320 * 0.30, n_sim, is_psa) * 1.34 +                              # topical ibuprofen
        generate_gamma(0.100, 0.100 * 0.30, n_sim, is_psa) * 1.98 +                              # topical piroxicam
        generate_gamma(0.120, 0.120 * 0.30, n_sim, is_psa) * 1.94 +                              # paracetamol
        generate_gamma(0.143, 0.143 * 0.30, n_sim, is_psa) * 1.18 +                              # codeine
        generate_gamma(0.040, 0.040 * 0.30, n_sim, is_psa) * 8.80 +                              # co-codamol
        generate_gamma(0.143, 0.143 * 0.30, n_sim, is_psa) * 3.00 +                              # dihydrocodeine
        generate_gamma(0.107, 0.107 * 0.30, n_sim, is_psa) * 2.31 +                              # ibuprofen
        generate_gamma(0.054, 0.054 * 0.30, n_sim, is_psa) * 0.86 +                              # naproxen
        generate_gamma(0.107, 0.107 * 0.30, n_sim, is_psa) * 12.52 +                             # prioxicam
        generate_gamma(0.214, 0.214 * 0.30, n_sim, is_psa) * 2.94 +                              # diclofenac
        generate_gamma(0.100, 0.100 * 0.30, n_sim, is_psa) * 2.51 +                              # nefopam hydrochloride
        generate_gamma(0.167, 0.167 * 0.30, n_sim, is_psa) * 3.63 +                              # triamcinolone acetonide 
        generate_gamma(0.167, 0.167 * 0.30, n_sim, is_psa) * 3.94                                # methylprednisolone with lidocaine 
    ),
    
    cost_gp = generate_gamma(0.208, 0.208 * 0.30, n_sim, is_psa) * 45,
    cost_prescriptions = generate_gamma(0.025, 0.025 * 0.30, n_sim, is_psa) * 27,
    cost_physio = generate_gamma(0.375, 0.375 * 0.30, n_sim, is_psa) * 41,
    cost_inpatient = generate_gamma(0.002, 0.002 * 0.30, n_sim, is_psa) * 3082.25,
    cost_ae = generate_gamma(0.001, 0.001 * 0.30, n_sim, is_psa) * 641.12,
    
    cost_death = generate_static(0, n_sim, is_psa),                                              # Assumption
    
    # Proportion reduction in health care costs for intervention
    p_red_medication_digital = generate_beta(0.10, 0.10 * 0.30, n_sim, is_psa), 
    p_red_gp_digital = generate_beta(0.30, 0.30 * 0.30, n_sim, is_psa),  
    p_red_prescription_digital = generate_beta(0.10, 0.10 * 0.30, n_sim, is_psa),  
    p_red_physio_digital = generate_beta(0.30, 0.30 * 0.30, n_sim, is_psa),   
    p_red_inpatient_digital = generate_static(0, n_sim, is_psa), 
    p_red_ae_digital = generate_static(0, n_sim, is_psa), 
    
    # Duration of intervention impact in months
    duration_utility_impact = round(generate_gamma(9, 9 * 0.30, n_sim, is_psa)),                 ## Expert consultation 
    duration_cost_impact = round(generate_gamma(9, 9 * 0.30, n_sim, is_psa)),                    ## Expert consultation
    
    # Proportion of patients that is appropriately engaged (%)                                   ## Expert consultation
    uptake = generate_beta(0.5, 0.5 * 0.3, n_sim, is_psa) 
  )
  
  df_input$utility_alive_base_intervention <- df_input$utility_alive_base_SoC # assume baseline utility is treatment independent
  
  return(df_input)
}

