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
options(mc.cores = parallel::detectCores() - 4)
rstan_options(auto_write = TRUE)
theme_set(theme_bw())


list(
  ## After all /scripts_premodelling
  ## Parameters
  tar_target(pars_bg, load_inputs_background(year0 = 2024, n_sims = 2000)),
  tar_target(pars_epi, load_inputs_epi(pars_bg)),

  tar_target(dis, c(0, 15, 35)), # discounting factor
  tar_target(pars_ce, load_inputs_ce(pars_epi, dis_e = dis / 1000, dis_c = dis / 1000), pattern = map(dis)),

  tar_target(vtype, c("rw", "tr")), # real world vs trial settings
  tar_target(f_zvl, compile_zvl(), format = "file"),
  tar_target(f_rzv, here::here("pars", "pars_ve_rzv_rw_zlg.rdata"), format = "file"),

  tar_target(pars_base, load_inputs(pars_ce, vtype = vtype, f_ve_zvl = f_zvl, f_ve_rzv = f_rzv), pattern = cross(vtype, pars_ce)),
  tar_target(pars_proj, load_inputs_proj(pars_base), pattern = slice(pars_base, c(2, 5))), # (1.5 real world + 1.5 trial)
  
  tar_target(f_p_base, save_pars(pars_base, f = here::here("pars", paste0("pars_base_", dis, "_", vtype, ".rdata"))),
             pattern = map(pars_base, cross(vtype, dis)), format = "file"),

  ## Cohort model
  tar_target(yss_uv, exec_cohort_rzv(pars_base, age0s = 50:99), pattern = map(pars_base)),
  tar_target(stats_uv, summarise_cohort(yss_uv), pattern = map(yss_uv))

  # tar_target(yss_re, exec_cohort_rerzv(pars_base, age0s = c(70, 75), age1s = 80:99), pattern = map(pars_base)),
  # tar_target(stats_re, summarise_cohort_re(yss_re), pattern = map(yss_re)),
  # 
  # tar_target(gs_thres, vis_thres(stats_uv, stats_re), pattern = map(stats_uv, stats_re)),
  # 
  # 
  # ## Long-term projection
  # tar_target(yss_proj, exec_projection(pars_proj), pattern = map(pars_proj)),
  # tar_target(stats_proj, summarise_proj(yss_proj), pattern = map(yss_proj)),
  # tar_target(gs_proj, vis_proj(stats_proj), pattern = map(stats_proj)),
  # 
  # ## Programme-based measures
  # tar_target(profile, sim_profile(pars_proj), pattern = map(pars_proj)),
  # tar_target(stats_prog, exec_programme(pars_proj, profile, stats_uv, stats_re), pattern = map(pars_proj, profile, slice(stats_uv, c(2, 5)), slice(stats_re, c(2, 5)))),
  # 
  # ## Output
  # tar_target(tabs_uv, save_tabs(stats_uv, folder = paste0(vtype, "_", dis)), pattern = map(stats_uv, cross(vtype, dis))),
  # tar_target(tabs_re, save_tabs(stats_re, folder = paste0(vtype, "_", dis)), pattern = map(stats_re, cross(vtype, dis))),
  # tar_target(tabs_proj, save_tabs(stats_proj, folder = "proj", prefix = paste0("tab_", vtype)), pattern = map(stats_proj, vtype)),
  # tar_target(tabs_prog, save_tabs(stats_prog, folder = "prog", prefix = paste0("tab_", vtype)), pattern = map(stats_prog, vtype)),
  # 
  # tar_target(figs_thres, save_fig_thres(gs_thres, folder = paste0(vtype, "_", dis), ext = ".pdf"), pattern = map(gs_thres, cross(vtype, dis))),
  # tar_target(figs_proj, save_fig_proj(gs_proj, folder = "proj", prefix = vtype, ext = ".pdf"), pattern = map(gs_proj, vtype))

  # ## Sensitivity analyses
  # tar_target(f_rzv_zle, here::here("pars", "pars_ve_rzv_rw_zle.rdata"), format = "file"),
  # tar_target(pars_waning, load_inputs_waning(pars_ce, f_ve_zvl = f_zvl, f_ve_rzv_zlg = f_rzv, f_ve_rzv_zle = f_rzv_zle), pattern = slice(pars_ce, 2)),
  # tar_target(stats_sens_waning, sens_waning(pars_waning, age0s = seq(50, 95, 5))),
  # tar_target(out_sens_waning, summarise_sens_waning(stats_sens_waning, folder = "rw_15")),
  # 
  # 
  # tar_target(stats_psa_price, sens_price(yss_uv), pattern = map(yss_uv)),
  # tar_target(out_psa_price, summarise_sens_price(stats_psa_price, folder = paste0(vtype, "_", dis)), pattern = map(stats_psa_price, cross(vtype, dis))),
  # 
  # tar_target(stats_sens_ce1w, sens_ce(yss_uv), pattern = map(yss_uv)),
  # tar_target(out_sens_ce1w, summarise_sens_ce(stats_sens_ce1w, folder = paste0(vtype, "_", dis)), pattern = map(stats_sens_ce1w, cross(vtype, dis)))

)
