
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
  
  fns <- c("zlg", "zlg_short", "zlg_long", "zle")

  ss <- list()
  
  ss$ves_2d <- bind_rows(lapply(1:4, \(i) {
    pars_waning[[i]]$VE_RZV_2d %>% mutate(fn = fns[i])
  })) %>% 
    filter(TimeVac <= 20) %>% 
    group_by(TimeVac, fn) %>% 
    summarise(
      M = median(Protection),
      L = quantile(Protection, 0.025),
      U = quantile(Protection, 0.975)
    )
  
  ss$ves_1d <- bind_rows(lapply(1:4, \(i) {
    pars_waning[[i]]$VE_RZV_1d %>% mutate(fn = fns[i])
  })) %>% 
    filter(TimeVac <= 20) %>% 
    group_by(TimeVac, fn) %>% 
    summarise(
      M = median(Protection),
      L = quantile(Protection, 0.025),
      U = quantile(Protection, 0.975)
    )
  
  ss$ces <- bind_rows(lapply(1:4, \(i) {
    stats[[i]]$stats_uv_ce %>% mutate(fn = fns[i])
  }))
  
  ss$yss <- bind_rows(lapply(1:4, \(i) {
    stats[[i]]$stats_uv_ys %>% mutate(fn = fns[i])
  }))
  
  return(ss)
}


summarise_sens_waning <- function(sens_waning, prefix = "", folder = NA, ext = ".pdf") {
  require(tidyverse)
  
  if (!is.na(folder)) {
    root_tab <- here::here("docs", "tabs", folder)
    root_fig <- here::here("docs", "figs", folder)
    dir.create(root_tab, showWarnings = F)
    dir.create(root_fig, showWarnings = F)
  } else {
    root_tab <- here::here("docs", "tabs")
    root_fig <- here::here("docs", "figs")
  }
  
  if (prefix != "") {
    prefix_tab <- glue::as_glue(prefix) + "_"
    prefix_fig <- glue::as_glue("g_") + prefix + "_"
  } else {
    prefix_tab <- glue::as_glue("")
    prefix_fig <- glue::as_glue("g_")
  }
  

  labs_fn <- c(
    zlg = "ZI Gamma",
    zlg_short = "ZI Gamma, drop at 10 yr",
    zlg_long = "ZI Gamma, long lasting", 
    zle = "ZI Exponential"
  )
  
  ces <- sens_waning$ces %>% 
    mutate(
      fn = factor(fn, levels = names(labs_fn))
    ) %>% 
    filter(Index %in% c("Thres20_50", "Thres30_90")) %>% 
    filter(Age0 >= 60) %>% 
    filter(Age0 <= 95) %>% 
    filter((Arm == "RZV_1d" & Age0 >= 80) | Arm == "RZV_2d") %>% 
    filter(!is.na(Arm)) %>% 
    select(Age = Age0, Arm, Index, M, fn)
    
  write_csv(ces, here::here(root_tab, prefix_tab + "sens_waning.csv"))
  
  
  gs <- list()
  
  gs$g_sens_waning_ves <- sens_waning$ves_2d%>% 
    mutate(
      fn = factor(fn, levels = names(labs_fn))
    ) %>% 
    ggplot() +
    geom_ribbon(aes(x = TimeVac, ymin = L, ymax = U, fill = fn), alpha = 0.3) +
    geom_line(aes(x = TimeVac, y = M, colour = fn)) +
    scale_y_continuous("Vaccine effectiveness, %, two doses", limits = c(0, 1), labels = scales::percent) +
    scale_x_continuous("Year since vaccination") +
    guides(fill = guide_none(), colour = guide_none()) +
    facet_wrap(.~fn, labeller = labeller(fn = labs_fn), nrow = 1)
  
  gs$g_sens_waning_thres <- ces %>% 
    mutate(Arm = factor(Arm, c("RZV_2d", "RZV_1d"))) %>% 
    ggplot() +
    geom_line(aes(x = Age, y = M, colour = Index, linetype = Arm)) +
    scale_y_continuous("Threshold price per adminstration, GBP") +
    scale_color_discrete("Threshold", labels = c(Thres20_50 = "50% CE given 20,000 WTP", Thres30_90 = "90% CE given 30,000 WTP")) +
    scale_linetype_discrete("", labels = c(RZV_1d = "single dose", RZV_2d = "two doses")) +
    expand_limits(y = c(0, 200)) +
    facet_wrap(.~fn, labeller = labeller(fn = labs_fn), nrow = 1) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  gs$g_sens_waning_bind <- ggpubr::ggarrange(
    gs$g_sens_waning_ves + labs(subtitle = "(A)"), 
    gs$g_sens_waning_thres + labs(subtitle = "(B)"), 
    nrow = 2)
  
  
  ggsave(gs$g_sens_waning_ves, filename = here::here(root_fig, prefix_fig + "sens_waning_ves" + ext), width = 10, height = 4.5)
  ggsave(gs$g_sens_waning_thres, filename = here::here(root_fig, prefix_fig + "sens_waning_thres" + ext), width = 10, height = 4.5)
  ggsave(gs$g_sens_waning_bind, filename = here::here(root_fig, prefix_fig + "sens_waning_bind" + ext), width = 10, height = 8)
  
  return(gs)
}





