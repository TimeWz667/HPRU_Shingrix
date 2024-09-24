library(tidyverse)

theme_set(theme_bw())

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
      mutate(Scenario = glue::as_glue("Vac_") + as.character(age0), Age0 = age0)
  })
}



ve_type <- "realworld"
ve_type <- glue::as_glue(ve_type)


load(file = here::here("pars", "parset_nic_c35q35y24n1k_" + ve_type + ".rdata"))


## Simulation -----
keys <- 1:pars_set$N_Sims
keys <- keys


yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- get_pars(pars_set, k)
  
  for (age0 in seq(70, 85, 5)) {
    yss[[length(yss) + 1]] <- a_run(pars, age0 = age0) %>% mutate(Key = k)
  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Arm, Key) %>% 
  select(Scenario, Age0, Arm, Key, 
         Q_HZ_d, Q_Life_d, C_Hosp_d, C_GP_NonPHN_d, C_All_d,
         C_GP_PHN_d, C_VacRZV_d, N_VacRZV_d)  %>% 
  pivot_longer(-c(Scenario, Age0, Arm, Key)) %>% 
  pivot_wider(names_from = Arm) %>% 
  mutate(
    Diff = Vac - SOC,
    name = paste0("d", name)
  ) %>% 
  select(-SOC, -Vac) %>% 
  pivot_wider(values_from = Diff)


save(yss, file = here::here("out", "yss_sens_1way.rdata"))


fn_thres <- function(df) {
  df %>% 
    mutate(
      dC_Med_d = dC_Hosp_d + dC_GP_NonPHN_d + dC_GP_PHN_d,
      Thres20 = ((dQ_HZ_d + dQ_Life_d) * 2e4 - dC_Med_d) / dN_VacRZV_d,
      Thres30 = ((dQ_HZ_d + dQ_Life_d) * 3e4 - dC_Med_d) / dN_VacRZV_d,
    ) %>% 
    group_by(Age0) %>% 
    summarise(
      T20_50 = quantile(Thres20, 0.5),
      T30_90 = quantile(Thres30, 0.9),
      Thres = pmin(T20_50, T30_90)
    )
}


case0 <- yss %>% fn_thres


case1 <- bind_rows(
  yss %>% mutate(
    dC_Hosp_d = dC_Hosp_d * (1 + 0.1)
  ) %>% fn_thres %>% mutate(Pars = "C_Hosp", Direction = +0.1),
  yss %>% mutate(
    dC_Hosp_d = dC_Hosp_d * (1 - 0.1)
  ) %>% fn_thres %>% mutate(Pars = "C_Hosp", Direction = -0.1),
  
  yss %>% mutate(
    dC_GP_NonPHN_d = dC_GP_NonPHN_d * (1 + 0.1)
  ) %>% fn_thres %>% mutate(Pars = "C_GP_NonPHN", Direction = +0.1),
  yss %>% mutate(
    dC_GP_NonPHN_d = dC_GP_NonPHN_d * (1 - 0.1)
  ) %>% fn_thres %>% mutate(Pars = "C_GP_NonPHN", Direction = -0.1),
  
  yss %>% mutate(
    dC_GP_PHN_d = dC_GP_PHN_d * (1 + 0.1)
  ) %>% fn_thres %>% mutate(Pars = "C_GP_PHN", Direction = +0.1),
  yss %>% mutate(
    dC_GP_PHN_d = dC_GP_PHN_d * (1 - 0.1)
  ) %>% fn_thres %>% mutate(Pars = "C_GP_PHN", Direction = -0.1),
  
  yss %>% mutate(
    dQ_HZ_d = dQ_HZ_d * (1 + 0.1)
  ) %>% fn_thres %>% mutate(Pars = "Q_HZ", Direction = +0.1),
  yss %>% mutate(
    dQ_HZ_d = dQ_HZ_d * (1 - 0.1)
  ) %>% fn_thres %>% mutate(Pars = "Q_HZ", Direction = -0.1),
  
  yss %>% mutate(
    dQ_Life_d = dQ_Life_d * (1 + 0.1)
  ) %>% fn_thres %>% mutate(Pars = "Q_Life", Direction = +0.1),
  yss %>% mutate(
    dQ_Life_d = dQ_Life_d * (1 - 0.1)
  ) %>% fn_thres %>% mutate(Pars = "Q_Life", Direction = -0.1)
)


lvs <- case1 %>% 
  filter(Age0 == 80) %>% 
  group_by(Pars) %>% 
  summarise(R = diff(range(Thres))) %>% 
  arrange(R) %>% 
  pull(Pars)

lvs

labs_lvs <- c(
  "C_GP_PHN" = "Cost, GP, PHN",
  "C_GP_NonPHN" = "Cost, GP, No PHN",
  "Q_Life" = "QALY, Survival",
  "C_Hosp" = "Cost, Hospitalisation",     
  "Q_HZ" = "QALY, HZ"
)

sens_ce <- case1 %>% 
  select(Pars, Direction, Thres, Age0) %>% 
  left_join(case0 %>% select(Age0, Thres0 = Thres)) 


g_sens_ce <- sens_ce %>%
  filter(Age0 %in% c(80, 85)) %>% 
  mutate(
    Pars = factor(Pars, lvs),
    i = as.numeric(Pars),
    D = ifelse(Direction > 0, "+10%", "-10%"),
    a0 = paste0("Age of vaccination: ", Age0)
  ) %>% 
  ggplot() +
  geom_rect(aes(xmin = Thres0, xmax = Thres, ymin = i - 0.45, ymax = i + 0.45, 
                y = Pars, fill = D)) +
  scale_fill_discrete("Changes") +
  scale_x_continuous("Threshold price, GBP per administration") +
  scale_y_discrete("Components", labels = labs_lvs) +
  facet_wrap(a0~.)


g_sens_ce

ggsave(g_sens_ce, filename = here::here("docs", "figs", "g_sens_ce.png"), width = 9, height = 4)

 