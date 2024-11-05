library(targets)

# library(tarchetypes) # Load other packages as needed.
library(rstan)
library(tidyverse)

tar_option_set(
  packages = c("tibble", "tidyverse", "tidybayes", "rstan", "readxl", "glue")
)

tar_source()

dir.create("pars/", showWarnings = F)
dir.create("posteriors/", showWarnings = F)


options(dplyr.summarise.inform = FALSE)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
theme_set(theme_bw())


list(
  ## After all /scripts_premodelling
  ## Parameters 
  tar_target(pars_bg, load_inputs_background(year0 = 2024)),
  tar_target(pars_epi, load_inputs_epi(pars_bg)),
  
  tar_target(dis, c(0, 15, 35)),
  tar_target(pars_ce, load_inputs_ce(pars_epi, dis_e = dis / 1000, dis_c = dis / 1000), pattern = map(dis)),
  
  tar_target(vtype, c("rw", "tr")),
  tar_target(f_zvl, compile_zvl(), format = "file"),
  tar_target(f_rzv, here::here("pars", "pars_ve_rzv_rw_zlg.rdata"), format = "file"),
  
  tar_target(pars_base, load_inputs(pars_ce, vtype = vtype, f_ve_zvl = f_zvl, f_ve_rzv = f_rzv), pattern = cross(vtype, pars_ce)),
  
  tar_target(pars_proj, load_inputs_proj(pars_base), pattern = slice(pars_base, 3)),
  
  ## Cohort model
  # tar_target(yss_uv, exec_cohort_rzv(pars_base, age0s = 50:99), pattern = map(pars_base)),
  # tar_target(yss_re, exec_cohort_rerzv(pars_base, age0s = seq(60, 75, 5), age1s = 80:99), pattern = map(pars_base)), 
  
  ## Long-term projection
  
  
  
  ## Summarise results
  
  
  ## Outputing
  tar_target(f_p_base, save_pars(pars_base, f = here::here("pars", paste0("pars_base_", dis, "_", vtype, ".rdata"))), pattern = map(pars_base, cross(vtype, dis)), format = "file")
  ## 
  
  
  ## Sensitivity analyses
  


)
