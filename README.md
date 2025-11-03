# PosViolMSM: Positivity Violations in Marginal Structural survival Models
Code for implementing a simulation study to investigate the effect of near positivity violations on the estimates from IPTW-based marginal structural survival models in a survival context with longitudinal exposure and time-dependent confounding.


### Reference
Spreafico M (2025). **Impact of Near-Positivity Violations on IPTW-Estimated Marginal Structural Survival Models With Time-Dependent Confounding**. *Biometrical Journal*, 67(6):e70093. doi: [10.1002/bimj.70093](https://onlinelibrary.wiley.com/doi/10.1002/bimj.70093)

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

## Additional info

To reproduce the **Simulation Study I** in Section 4.2 of the manuscript:
1) first run the script **AI_1 simulations.R** to simulate the $B=1000$ datasets under the different scenarios detailed in Section 4.2.1.
   Parallelisation details/remarks:
   - This code is parallelised over the different sample sizes considered, so 5 cores are used by default.
   - If your machine has fewer than 5 cores, you should adjust the value of n.cores when calling the function *run_simulation_studyI()* (line 73). In this case, n.sizes should be a vector whose length matches the number of cores.
   - Note that if the number of cores is less than the number of sample sizes, not all sample sizes will be processed in a single run. You will need to re-run the code multiple times to obtain results for all sample sizes.
   - The running time for n.sim = 5 is 6.33 minutes. Based on this, the expected running time for n.sim = 1000 is approximately 21–22 hours.
2) Then run the scripts **AI_2 results.R** to estimate the performance measures in Section 4.2.2 and **AI_3 figures_2_3.R** for reproducing Figures 2 and 3;
3) Finally, run the script **AI_4 figures_4_5_6 table_1.R** for reproducing Figures 4, 5, 6 and Table 1 in Section 4.2.3.
 


To reproduce the **Simulation Study II** in Section 5.2 of the manuscript:
1) first run the script **AII_1 simulations.R** to perform simulations under the different scenarios detailed in Section 5.2.1.
   Parallelisation details/remarks:
   - This code is parallelised over the different sample sizes considered, so 5 cores are used by default.
   - If your machine has fewer than 5 cores, you should adjust the value of n.cores when calling the function *run_simulation_studyII()* (line 88). In this case, n.sizes should be a vector whose length matches the number of cores.
   - Note that if the number of cores is less than the number of sample sizes, not all sample sizes will be processed in a single run. You will need to re-run the code multiple times to obtain results for all sample sizes.
   - The running time for n.sim = 5 is 41.36 seconds. Based on this, the expected running time for n.sim = 1000 is approximately 2.5 hours.
2) Then run the scripts **AII_2 results.R** to estimate the performance measures in Section 5.2.2 and **AII_3 figures_4_5.R** for reproducing Figures 4 and 5;
3) finally, run the script **AII_4 figures_9_10_11 tables_2_3.R** for reproducing Figures 9, 10, 11 and Tables 2 and 3 in Section 5.2.3.
  
(Last update: November 3rd, 2025)
