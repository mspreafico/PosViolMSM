# PosViolMSM: Positivity Violations in Marginal Structural survival Models
Code for implementing a simulation study to investigate the effect of near positivity violations on the estimates from IPTW-based marginal structural survival models in a survival context with longitudinal exposure and time-dependent confounding.


### Reference
Spreafico M. Positivity violations in marginal structural survival models with time-dependent confounding: a simulation study on IPTW-estimator performance. https://arxiv.org/abs/2403.19606 


## Description

- Files:
  - **AI_1 simulations.R**: Simulation study using Algorithm I - Investigating various scenarios (see Section 4.2.1).
  - **AI_2 results.R**: Simulation study using Algorithm I - Results (see Section 4.2.2).
  - **AI_3 figures_2_3.R**: reproducing Figures 2 and 3.
  - **AII_1 simulations.R**: Simulation study using Algorithm II - Investigating various scenarios (see Section 5.2.1).
  - **AII_results.R**: Simulation study using Algorithm II - Results (see Section 5.2.2).
  - **AII_3 figures_4_5.R**: reproducing Figures 4 and 5.
    
- Sub-folder **./functions/** contains some auxiliary functions to run the main files:
  - **algorithm_I.R**: Code for Algorithm I (see Section 4.1.2).
  - **algorithm_II.R**: Code for Algorithm II (see Section 5.1.2).
  - **eval_measuresI.R**: Functions to evaluate the results for the scenarios simulated using Algorithm I. 
  - **eval_measuresII.R**: Functions to evaluate the results for the scenarios simulated using Algorithm II. 
  - **mc_simI_functions.R**: Functions to estimate the logit-MSMs using the longitudinal datasets simulated from Algorithm I.
  - **mc_simII_functions.R**: Functions to estimate the Aalen-MSMs using the longitudinal datasets simulated from Algorithm II.
  - **plot_simI.R**: Functions to plot results for Algorithm I.
  - **plot_simII.R**: Functions to plot results for Algorithm II.
      
- Sub-folder **./results/** contains the results from the various scenarios and relative performance.

## Software
- R software, version 4.3.1
- Packages: data.table, doParallel, foreach, ggplot2, ggpubr, tidyr, timereg.
  
(Last update: January 10th, 2025)
