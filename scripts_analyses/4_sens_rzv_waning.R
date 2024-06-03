library(tidyverse)



source(here::here("models", "m_cohort_hz.R"))
source(here::here("models", "misc.R"))

# Load inputs
source(here::here("scripts_analyses", "fn_arrange_inputs.R"))
pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 1e3)



## Simulation -----
keys <- 1:pars_set$N_Sims
keys <- keys[1:100]

yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 


for(p in c("long_zlg", "short_zlg", "zle", "zlg")) {
  pars_set$VE_RZV <- local({
    load(here::here("pars", paste0("pars_ve_rzv_rw_", p, ".rdata")))
    sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
      select(Key, Vaccine, TimeVac = Yr, Protection = VE)
    
  })
  
  for(k in keys) {
    pars <- get_pars(pars_set, k)
    
    for (age0 in 60:95) {
      yss[[length(yss) + 1]] <- 
        sim_cohort_vac(pars, age0 = age0, vaccine0 = "Shingrix", agg = T) %>% 
        mutate(Key = k, Scenario = glue::as_glue("Vac_") + age0, Age0 = age0, Func = p)
    }
    setTxtProgressBar(pb, k)
  }
  
}



yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Arm, Type, Key)

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



ves <- bind_rows(lapply(c("long_zlg", "short_zlg", "zle", "zlg"), function(p) {
  load(here::here("pars", paste0("pars_ve_rzv_rw_", p, ".rdata")))
  sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
    select(Key, Vaccine, TimeVac = Yr, Protection = VE) %>% 
    group_by(TimeVac) %>% 
    summarise(
      M = mean(Protection),
      L = quantile(Protection, 0.025), 
      U = quantile(Protection, 0.975)
    ) %>% 
    filter(TimeVac <= 20) %>% 
    mutate(
      Func = p
    )
})) %>% 
  mutate(
    Func = factor(Func, levels = names(labs_fn))
  )


g_ves <- ves %>% 
  ggplot() +
  geom_ribbon(aes(x = TimeVac, ymin = L, ymax = U, fill = Func), alpha = 0.3) +
  geom_line(aes(x = TimeVac, y = M, colour = Func)) +
  scale_y_continuous("Vaccine effectiveness, %", limits = c(0, 1), labels = scales::percent) +
  guides(fill = guide_none(), colour = guide_none()) +
  facet_wrap(.~Func, labeller = labeller(Func = labs_fn), nrow = 1)

g_ves


g_icer <- yss %>% 
  mutate(
    Func = factor(Func, levels = names(labs_fn))
  ) %>% 
  filter(Arm == "Vac") %>%
  group_by(Age0, Func) %>% 
  summarise(M = mean(ICER), L = quantile(ICER, 0.025), U = quantile(ICER, 0.975)) %>% 
  ggplot() +
  geom_ribbon(aes(x = Age0, ymin = L, ymax = U, fill = Func), alpha = 0.3) +
  geom_line(aes(x = Age0, y = M, colour = Func)) +
  # geom_line(aes(x = Age0, y = L, colour = Func), linetype = 2) +
  # geom_line(aes(x = Age0, y = U, colour = Func), linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  expand_limits(y = 0) +
  guides(fill = guide_none(), colour = guide_none()) +
  facet_wrap(.~Func, labeller = labeller(Func = labs_fn), nrow = 1) +
  scale_y_continuous("ICER, GBP per QALY")


g <- ggarrange(g_ves, g_icer, nrow = 2, align = "v")

ggsave(g, filename = here::here("docs", "figs", "g_sens_ves.png"), width = 8, height = 6)



