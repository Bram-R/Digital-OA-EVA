calculate_oa_costs <- function() {
  # Base values
  gp_appointments <- 0.08166
  gp_price <- 45
  prescriptions <- 0.01537
  prescription_price <- 27
  physio_appointments <- 0.29166
  physio_price <- 41
  inpatient_activity <- 0.00233
  inpatient_price <- 3082.25
  ae_activity <- 0.00116
  ae_price <- 641.12
  
  # Medication (quantity * price)
  medication_cost <- sum(
    0.065 * 7.35,   # topical diclofenac
    0.047 * 2.70,   # topical ketoprofen
    0.347 * 1.34,   # topical ibuprofen
    0.108 * 1.98,   # topical piroxicam
    0.010 * 1.94,   # paracetamol
    0.036 * 1.18,   # codeine
    0.010 * 8.80,   # co-codamol
    0.036 * 3.00,   # dihydrocodeine
    0.009 * 2.31,   # ibuprofen
    0.013 * 0.86,   # naproxen
    0.028 * 12.52,  # prioxicam
    0.018 * 2.94,   # diclofenac
    0.017 * 2.59,   # celecoxib
    0.018 * 3.49   # etericoxib
    
  )
  
  # Standard Care Cost
  primary_care <- gp_appointments * gp_price + prescriptions * prescription_price
  community_care <- physio_appointments * physio_price
  secondary_care <- inpatient_activity * inpatient_price + ae_activity * ae_price
  
  total_standard <- primary_care + community_care + secondary_care + medication_cost
  
  # Reductions for Digital Technologies
  gp_appointments_digital <- gp_appointments * (1 - 0.034)
  prescriptions_digital <- prescriptions * (1 - 0.00)
  physio_appointments_digital <- physio_appointments * (1 - 0.4114)
  inpatient_activity_digital <- inpatient_activity * (1 - 0.117)
  ae_activity_digital <- ae_activity * (1 - 0.244)
  medication_cost_digital <- medication_cost * (1 - 0.00)
  
  # Digital Intervention Cost
  primary_care_digital <- gp_appointments_digital * gp_price + prescriptions * prescription_price
  community_care_digital <- physio_appointments_digital * physio_price
  secondary_care_digital <- inpatient_activity_digital * inpatient_price + ae_activity_digital * ae_price
  
  total_digital <- primary_care_digital + community_care_digital + secondary_care_digital + medication_cost_digital
  
  return(list(
    cost_standard = total_standard,
    cost_digital = total_digital,
    primary_care_digital = primary_care_digital,
    community_care_digital = community_care_digital,
    secondary_care_digital = secondary_care_digital,
    medication_cost_digital = medication_cost_digital,
    primary_care_standard = primary_care,
    community_care_standard = community_care,
    secondary_care_standard = secondary_care,
    medication_cost_standard = medication_cost
  ))
}


oa_costs <- calculate_oa_costs()