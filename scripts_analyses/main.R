

root <- "scripts_analyses"

# Prepartion
source(here::here(root, "0_compile_pars.R"))


# Main analyses


source(here::here(root, "1_run_rzv.R"))
source(here::here(root, "1_run_rzv_ic.R"))

source(here::here(root, "2_run_zvl2rzv.R"))
source(here::here(root, "3_run_rzv2rzv.R"))

# Sensitivity analyses
source(here::here(root, "4_sens_rzv_waning.R"))
source(here::here(root, "4_sens_single_dir.R"))
source(here::here(root, "4_sens_psa.R"))


# Summarise simulation scenarios
source(here::here(root, "5_run_burden.R"))
source(here::here(root, "6_group_by_5yr.R"))
source(here::here(root, "7_visualise.R"))
