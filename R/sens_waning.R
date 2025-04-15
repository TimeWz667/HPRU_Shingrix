
load_inputs_waning <- function(pars_ce, f_ve_zvl, f_ve_rzv_zlg, f_ve_rzv_zle) {
  # f_ve_zvl <- tar_read(f_zvl)
  # f_ve_rzv_zlg <- tar_read(f_rzv)
  # f_ve_rzv_zle <- here::here("pars", "pars_ve_rzv_rw_zle.rdata")
  # 

  pars <- list()
  pars$zlg <- load_inputs(pars_ce, vtype = "rw", f_ve_zvl, f_ve_rzv_zlg)
  
  pars$zlg_short <- load_inputs(pars_ce, vtype = "rw", f_ve_zvl, f_ve_rzv_zlg)
  pars$zlg_short$VE_RZV_2d <- pars$zlg_short$VE_RZV_2d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, 0, Protection))
  pars$zlg_short$VE_RZV_1d <- pars$zlg_short$VE_RZV_1d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, 0, Protection))
  pars$zlg_short$VE_ReRZV_2d <- pars$zlg_short$VE_ReRZV_2d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, 0, Protection))
  pars$zlg_short$VE_ReRZV_1d <- pars$zlg_short$VE_ReRZV_1d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, 0, Protection))
  
  pars$zlg_long <- load_inputs(pars_ce, vtype = "rw", f_ve_zvl, f_ve_rzv_zlg)
  pars$zlg_long$VE_RZV_2d <- pars$zlg_long$VE_RZV_2d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, Protection[TimeVac == 10], Protection))
  pars$zlg_long$VE_RZV_1d <- pars$zlg_long$VE_RZV_1d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, Protection[TimeVac == 10], Protection))
  pars$zlg_long$VE_ReRZV_2d <- pars$zlg_long$VE_ReRZV_2d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, Protection[TimeVac == 10], Protection))
  pars$zlg_long$VE_ReRZV_1d <- pars$zlg_long$VE_ReRZV_1d %>% group_by(Key) %>% 
    mutate(Protection = ifelse(TimeVac >= 10, Protection[TimeVac == 10], Protection))
  
  pars$zle <- load_inputs(pars_ce, vtype = "rw", f_ve_zvl, f_ve_rzv_zle)
  
 
  return(pars)
}


sens_waning <- function(pars_waning, age0s = 60:95) {
  
  stats <- lapply(pars_waning, \(pars) {
    exec_cohort_rzv(pars, age0s = age0s) %>% 
      summarise_cohort()
  })
  
  return(stats)
}


