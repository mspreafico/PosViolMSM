# PosViolMSM: Positivity Violations in Marginal Structural survival Models
Code for implementing a simulation study to investigate the effect of near positivity violations on the estimates from IPTW-based marginal structural survival models in a survival context with longitudinal exposure and time-dependent confounding.


### Reference
Spreafico M (2025). Impact of near-positivity violations on IPTW-estimated marginal structural survival models with time-dependent confounding. *Biometrical Journal* - in press. [Pre-print version: https://arxiv.org/abs/2403.19606]

## Description

- Files:
  - **AI_1 simulations.R**: Simulation study using Algorithm I - Investigating various scenarios (see Section 4.2.1).
  - **AI_2 results.R**: Simulation study using Algorithm I - Results (see Section 4.2).
  - **AI_3 figures_2_3.R**: Results across all scenarios - reproducing Figures 2 and 3 (see Section 4.2.2).
  - **AI_4 figures_4_5_6 table_1.R**: Focused examination of weight truncation in selected scenarios - reproducing Figures 4 to 6 and Table 1 (see Section 4.2.3).
  - **AII_1 simulations.R**: Simulation study using Algorithm II - Investigating various scenarios (see Section 5.2.1).
  - **AII_results.R**: Simulation study using Algorithm II - Results (see Section 5.2).
  - **AII_3 figures 7_8.R**: Results across all scenarios - reproducing Figures 7 and 8 (see Section 5.2.2).
  - **AII_4 figures_9_10_11 tables_2_3.R**: Focused examination of weight truncation in selected scenarios - reproducing Figures 9 to 11 and Tables 2 and 3 (see Section 5.2.3).

    
- Sub-folder **./functions/** contains some auxiliary functions to run the main files:
  - **algorithm_I.R**: Code for Algorithm I (see Section 4.1.2).
  - **algorithm_II.R**: Code for Algorithm II (see Section 5.1.2).
  - **eval_measuresI.R**: Functions to evaluate the results for the scenarios simulated using Algorithm I. 
  - **eval_measuresII.R**: Functions to evaluate the results for the scenarios simulated using Algorithm II. 
  - **iptw_I.R**: Functions to perform IPTW in Simulation Study I.
  - **iptw_II.R**: Functions to perform IPTW in Simulation Study II.
  - **mc_simI_functions.R**: Functions to estimate the logit-MSMs using the longitudinal datasets simulated from Algorithm I.
  - **mc_simII_functions.R**: Functions to estimate the Aalen-MSMs using the longitudinal datasets simulated from Algorithm II.
  - **plots_simI.R**: Functions to plot results for Algorithm I.
  - **plots_simII.R**: Functions to plot results for Algorithm II.
  - **specific_plots_simI.R**: Functions to plot results for Algorithm I for the three specific scenarios examined in Section 4.2.3.
  - **specific_plots_simII.R**: Functions to plot results for Algorithm II for the three specific scenarios examined in Section 5.2.3.
      
- After running the main scripts, sub-folder **./results/** will contain the results from the various scenarios and relative performance.

## Software
- R software, version 4.3.1
- Packages: data.table, doParallel, foreach, ggplot2, ggpubr, tidyr, timereg.
  
(Last update: August 18th, 2025)
