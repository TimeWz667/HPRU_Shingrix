

root <- "scripts_analyses"


source(here::here(root, "0_compile_pars.R"))
source(here::here(root, "1_run_rzv.R"))
source(here::here(root, "2_run_zvl2rzv.R"))
source(here::here(root, "3_run_rzv2rzv.R"))
# source(here::here(root, "4_run_rzv_ic.R"))
source(here::here(root, "5_run_burden.R"))
source(here::here(root, "6_run_fair.R"))
source(here::here(root, "8_vis_revac.R"))
