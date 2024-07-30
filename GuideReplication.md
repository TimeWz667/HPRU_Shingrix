# Guideline for replicating the analyses
  
  
## Preparation
All the model inputs are preprocessed with the scripts under **scripts_premodelling/**. Some of the raw data are removed due to our information governance.
  

To test the *stan* models used for vaccine input modelling, please use the [Docker file](docker-compose.yml) to setup the environment. 


Most input parameters are compiled in **pars/** and are ready for simulation. However, as some inputs are too large after compilation, run **scripts_analyses/0_compile_pars.R** for parameter sets. The defaults runs take 1000 iterations for each scenario. Please feel to use smaller size for testing purpose or larger size for checking uncertainty. 


## Analyses
Follow the runing order by scripts labelled from **1_** to **7_**. Scripts start with the same number can be executed in any order. 

- **1_run_rzv.R**: simulations for the never-vaccinated by receiving RZV at different ages.
- **2_run_zvl2rzv** and **3_run_rzv2rzv**: simulations for those vaccinated with any anti-HZ vaccines previously.
- (optional) **4_**: sensitivity analyses for model inputs and structural assumptions.
- **5_run_burden.R**: 
- **6_group_by_5yr.R**: grouping the simulation results by 5 years using the 2023-2024 population as the weights.
- **7_visualise.R**: a quick visualisation for the simulation results.


## Output for reporting

Run **scripts_reports/3_report_jcvi_sub.R** for the key outputs about evaluating RZV among olderly-old population.


