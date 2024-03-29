# PosViolMSM: Positivity Violations in Marginal Structural survival Models
Code for implementing a simulation study to investigate the effect of positivity violations on the estimates from IPTW-based marginal structural survival models in a survival context with longitudinal exposure and time-dependent confounding.


### Reference
Spreafico M (2024). Positivity violations in marginal structural survival models with time-dependent confounding: a simulation study on IPTW-estimator performance. https://arxiv.org/abs/2403.19606 


## Description

- Files:
  - **AI_simulations.R**: Simulation study using Algorithm I - Investigating various scenarios (see Section 4.1.1).
  - **AI_results.R**: Simulation study using Algorithm I - Results (see Sections 4.1.2 and 4.1.3).
  - **AII_simulations.R**: Simulation study using Algorithm II - Investigating various scenarios (see Section 4.2.1).
  - **AII_results.R**: Simulation study using Algorithm II - Results (see Sections 4.2.2 and 4.2.3).
    
- Sub-folder **./functions/** contains some auxiliary functions to run the main files:
  - **algorithm_I.R**: Code for Algorithm I (see Section 3.2.2).
  - **algorithm_II.R**: Code for Algorithm II (see Section 3.3.2).
  - **eval_measuresI.R**: Functions to evaluate the results for the scenarios simulated using Algorithm I. 
  - **eval_measuresII.R**: Functions to evaluate the results for the scenarios simulated using Algorithm II. 
  - **ms_simI_functions.R**: Functions to estimate the logit-MSMs using the longitudinal datasets simulated from Algorithm I.
  - **ms_simII_functions.R**: Functions to estimate the Aalen-MSMs using the longitudinal datasets simulated from Algorithm II.
    
- Sub-folder **./results/** contains the results from the various scenarios and relative performance.

## Software
- R software.
- Packages: data.table, survival, tidyr, timereg.
  
(Last update: March 29th, 2024)
