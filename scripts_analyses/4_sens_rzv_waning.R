library(tidyverse)



source(here::here("scripts_analyses", "fn_arrange_inputs.R"))
source(here::here("models", "sim_hz.R"))
source(here::here("models", "misc.R"))



a_run <- function(pars, age0) {
  with(model, {
    df0 <- populate(age0, pars) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      summarise(pars) %>% 
      mutate(Arm = "SOC")
    
    df1 <- populate(age0, pars) %>% 
      vaccinate(age0, "RZV", pars) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      summarise(pars) %>% 
      mutate(Arm = "Vac")
    
    bind_rows(df0, df1) %>% 
      mutate(Scenario = glue::as_glue("Vac_") + age0, Age0 = age0)
  })
}



ds <- dir("pars")
ds <- ds[startsWith(ds, "pars_ve_rzv_rw_")]
ds <- gsub(".rdata", "", gsub("pars_ve_rzv_rw_", "", ds))


collector <- list()
ves <- list()
for (d in ds) {
  d <- glue::as_glue(d)
  
  set.seed(11667)
  
  pars_set <- load_inputs_nic(
    discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 100,
    ve_rzv = "pars_ve_rzv_rw_" + d + ".rdata"
  )
  
  ves[[d]] <- pars_set$VE_RZV %>% 
    group_by(TimeVac) %>% 
    summarise(
      M = median(Protection),
      L = quantile(Protection, 0.025),
      U = quantile(Protection, 0.975)
    ) %>% 
    mutate(VE = d)
  
  keys <- 1:pars_set$N_Sims
  
  pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 
  yss <- list()
  
  for (k in keys) {
    pars <- get_pars(pars_set, k)
    
    yss[[length(yss) + 1]] <- bind_rows(lapply(60:99, \(age0) {
      a_run(pars, age0 = age0) %>% mutate(Key = k)
    }))
    
    setTxtProgressBar(pb, k)
  }
  
  yss <- bind_rows(yss) %>% 
    relocate(Scenario, Age0, Arm, Key) %>% 
    select(Scenario, Age0, Arm, Key, 
           Q_All_d, C_Med_d, C_VacRZV_d, N_VacRZV_d)  %>% 
    pivot_longer(-c(Scenario, Age0, Arm, Key)) %>% 
    pivot_wider(names_from = Arm) %>% 
    mutate(
      Diff = Vac - SOC,
      name = paste0("d", name)
    ) %>% 
    select(-SOC, -Vac) %>% 
    pivot_wider(values_from = Diff)%>% 
    mutate(
      Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
      Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
    ) %>% 
    group_by(Age0) %>% 
    summarise(
      Thres = pmin(quantile(Thres20, 0.5), quantile(Thres30, 0.1))
    ) %>% 
    mutate(
      VE = d
    )
  
  collector[[d]] <- yss
}


yss <- bind_rows(collector)
ves <- bind_rows(ves)

save(yss, file = here::here("out", "sens_yss_waning.rdata"))





## Visualise

library(ggpubr)

theme_set(theme_bw())



labs_fn <- c(
  zle = "ZI Exp", 
  zlg = "ZI Gamma",
  long_zlg = "ZI Gamma, long lasting", 
  short_zlg = "ZI Gamma, drop at 10 yr"
)


g_ves <- ves %>% 
  filter(TimeVac <= 20) %>% 
  mutate(
    VE = factor(VE, levels = names(labs_fn))
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = TimeVac, ymin = L, ymax = U, fill = VE), alpha = 0.3) +
  geom_line(aes(x = TimeVac, y = M, colour = VE)) +
  scale_y_continuous("Vaccine effectiveness, %", limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous("Year since vaccination") +
  guides(fill = guide_none(), colour = guide_none()) +
  facet_wrap(.~VE, labeller = labeller(VE = labs_fn), nrow = 1)

g_ves



g_thres <- yss %>% 
  mutate(
    VE = factor(VE, levels = names(labs_fn))
  ) %>% 
  ggplot() +
  geom_smooth(aes(x = Age0, y = Thres)) +
  scale_y_continuous("Threshold price, £ per administration") +
  scale_x_continuous("Age of vaccination") +
  guides(fill = guide_none(), colour = guide_none()) +
  facet_wrap(.~VE, labeller = labeller(VE = labs_fn), nrow = 1) +
  expand_limits(y = 0)




g <- ggarrange(g_ves, g_thres, nrow = 2, align = "v")

g

ggsave(g, filename = here::here("docs", "figs", "g_sens_ves.png"), width = 9, height = 6.5)



