
## Parameters: Demography

source(here::here("scripts", "2_prepare_pars_demo.R"))


## Parameters: Epidemiology
source(here::here("scripts", "2_model_epi_gpr.R"))


## Parameters: Vaccine efficacy/effectiveness
source(here::here("scripts", "3_1_prepare_ve.R"))
source(here::here("scripts", "3_2_fit_ve_rzv_zi_stan.R"))
source(here::here("scripts", "3_3_fit_ve_rzv_sens.R"))


## Parameters: Vaccine coverage
source(here::here("scripts", "4_1_vis_coverage.R"))
source(here::here("scripts", "4_2_fit_coverage.R"))
source(here::here("scripts", "4_3_sim_immunity.R"))


## Legacy simulations
source(here::here("scripts", "5_1_run_shingrix_nic.R"))
source(here::here("scripts", "5_2_run_shingrix_ic.R"))
source(here::here("scripts", "5_3_run_zostavax_nic_aj.R"))
source(here::here("scripts", "5_4_run_shingrix_nic_updated.R"))
source(here::here("scripts", "5_5_run_shingrix_ic_updated.R"))

source(here::here("scripts", "6_compare_versions.R"))
source(here::here("scripts", "6_validate_with_previous_version.R"))
