# GID-HTE10057: Digital Technologies for managing mild to moderate hip or knee osteoarthritis

This repository contains the source code and supporting scripts for the early value assessment of digital technologies to manage mild to moderate hip or knee osteoarthritis (GID-HTE10057).

---

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Model conceptualization](#model-conceptualization)
- [Model inputs](#model-inputs)
- [Model analyses](#model-analyses)
- [Model validation](#model-validation)
- [Contact](#contact)

---

## Installation

### Prerequisites

- **R** (version 4.0.0 or later, install from https://cran.r-project.org/bin/windows/base/)
- **R Studio** (install from https://www.rstudio.com/products/rstudio/download/)
- **R packages**:
  - dampack
  - docstring
  - DiagrammeR
  - BCEA
  - matrixStats
  - summarytools

### Steps

1.  **Clone the Repository (Terminal tab next to the Console tab in R Studio):**
    ```bash
    git clone https://github.com/Bram-R/EVA_10057.git
    cd EVA_10057
    ```
2.  **Install Required R Packages:**

    In R, run:
    ```r
    required_packages <- c("docstring", "DiagrammeR", "dampack", "BCEA", "matrixStats", "summarytools")
    install.packages(required_packages)
    ```
3.  **Set the Working Directory:**
    Ensure your R working directory is set to the repository folder.

## Usage

To replicate the model-based analysis:

1.  **Main results:**
    Run the script `Main.R` to generate the main results.

2.  **Sensitivity analyses:**
    Run the script `Sensitivity analyses.R` to run probabilistic and deterministic sensitivity analyses. This step provides outputs such as the incremental cost-effectiveness plane, cost-effectiveness acceptability curves, and tornado diagrams that help identify key drivers of cost-effectiveness.

3.  **Intermediate results:**
    Run the script `Intermediate results.R` to generate the intermediate results.

## Model conceptualization

A probabilistic decision-analytic state transition cohort model, with a monthly cycle length and one-year time horizon, was developed to model patients aged 16 and over with hip/knee osteoarthritis who are eligible for digital technology management in the UK. The comparator was standard care as defined by NICE guidelines. We compared standard care to digital technologies as a partial replacement for standard care. Eleven digital technologies were initially identified, of which seven were included in the economic model due to lack of (unredacted) data. Given their heterogeneity in nature and lack of comparative evidence, these were modelled as a single intervention arm. The model perspective was from the NHS and Personal Social Services. The two health states were ‘alive’ and ‘dead’ and the ‘engagement parameter’ determined if patients in the alive state used digital technologies or not. Due to a lack of data, the model focused on potential symptom improvement from digital technologies and standard care, by showing an increase in health-related quality of life. Another primary outcome was the reduction in medicine use and appointments for digital technology users. The duration of effects were assumed to be temporary. 

## Model inputs

Evidence for the model inputs was partly based on a systematic review, but primarily sourced though clinical expert opinion.
The input parameters generated through the f_input() function with deterministic values can be copied to the clipboard by: 

```r
write.table(f_input(n_sim = 1), "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)
```

| Parameter | Description |
|-----------|---------------------------------------------|
|discount_costs | discount rate for costs |
|discount_qalys | discount rate for QALYs |
|discount_lys | discount rate for lifeyears |
|utility_alive_base_SoC | HRQoL baseline value for standard care |
|utility_alive_SoC | HRQoL improvement for standard care |
|utility_alive_base_intervention | HRQoL baseline value for digital technologies |
|utility_alive_intervention | HRQoL improvement for digital technologies |
|utility_death | HRQoL value for death state |
|cost_t1 | One|off cost for strategy 1 (standard care) |
|cost_t2 | One|off cost for strategy 2 (digital technologies) |
|cost_medication | Monthly health state cost for medication/drugs |
|cost_gp | Monthly health state cost for gp activity |
|cost_prescriptions | Monthly health state cost for medication prescriptions |
|cost_physio | Monthly health state cost for physiotherapy activity |
|cost_inpatient | Monthly health state cost for inpatient care activity |
|cost_ae | Monthly health state cost for Accident & Emergency activity |
|cost_death | Monthly health state cost for death |
|p_red_medication_digital | Reduction in medication use due to digital technologies |
|p_red_gp_digital | Reduction in gp activity due to digital technologies |
|p_red_prescription_digital | Reduction in prescriptions due to digital technologies |
|p_red_physio_digital | Reduction in physio due to digital technologies |
|p_red_inpatient_digital | Reduction in inpatient care due to digital technologies |
|p_red_ae_digital | Reduction in A&E activity due to digital technologies |
|duration_utility_impact | Duration of improvement for HRQoL |
|duration_cost_impact | Duration of improvement for health state costs  |
|uptake | Engagement with digital technology |

## Model analyses

Uncertainty is examined via Monte Carlo simulations (5,000 iterations), along with deterministic sensitivity analyses, various scenario analyses and value-of-information analysis. Outputs include cost-effectiveness planes, cost-effectiveness acceptability curves, optimal strategy plots, tornado diagrams, and expected loss curves.

## Model validation

To assess the face validity of the model structure, model inputs and assumptions, clinical expert opinion was sought.
The technical implementation of the model has been reviewed by a second health economist. Stress tests (black box testing) as well as sensitivity and scenario analyses were performed to ensure the model functions logically and produces expected outcomes.

## Contact

For further information regarding this repository or the associated deliverable, please contact:

**Bram Ramaekers**
- GitHub: @Bram-R

**Mabel Wieman**
- GitHub: @mabelwieman