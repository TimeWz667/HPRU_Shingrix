library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("R", "misc.R"))

dir.create(here::here("docs", "figs", "inputs"), showWarnings = F)



load(here::here("pars", "pars_base_15_rw.rdata"))

load(here::here("data", "fitted_coverage.rdata"))

lu <- as.matrix(1 - exp(confint(pred1$fit)))

tab_vac <- bind_rows(
  pars$VE_RZV_2d %>% mutate(Vaccine = "RZV_2d"),
  pars$VE_RZV_1d %>% mutate(Vaccine = "RZV_1d"),
  pars$VE_ReRZV_2d %>% mutate(Vaccine = "ReRZV_2d"),
  pars$VE_ReRZV_1d %>% mutate(Vaccine = "ReRZV_1d"),
  pars$VE_ZVL %>% 
    filter(Age == 70) %>% 
    select(TimeVac, Protection) %>% 
    mutate(Vaccine = "ZVL_70")
) %>% 
  select(TimeVac, Protection, Vaccine) %>% 
  filter(TimeVac %in% c(1, 6)) %>%
  mutate(
    Vaccine = factor(Vaccine, levels = c("RZV_2d", "RZV_1d", "ReRZV_2d", "ReRZV_1d", "ZVL_70"))
  ) %>% 
  group_by(Vaccine, TimeVac) %>% 
  summarise_all(amlu) %>%
  bind_rows(
    tibble(
      Vaccine = c("Uptake_1y", "Catch-up"),
      TimeVac = NA,
      M = 1 - exp(coef(pred1$fit)),
      L = lu[, 2],
      U = lu[, 1]
    )
  ) %>% 
  mutate(
    M = scales::percent(M), 
    CrI = sprintf("(%s - %s)", scales::percent(L), scales::percent(U)),
    TimeVac = TimeVac - 1
  ) %>% 
  select(TimeVac, Vaccine, M, CrI)

tab_vac

tab_inputs <- bind_rows(
  pars$Epidemiology %>% 
    mutate(
      p_mor_hz = r_mor_hz / r_hz,
      p_hosp = 1 - p_gp
    ) %>% 
    select(Age, r_hz, p_hosp, p_phn, p_mor_hz) %>% 
    filter(Age %in% seq(60, 90, 10)) %>% 
    pivot_longer(-c(Age)),
  pars$CostEff %>% 
    select(Age, QL_ph, cost_GP_pp_non_PHN_HZ_inf, cost_GP_pp_PHN_inf, cost_hosp_pp_inf) %>% 
    filter(Age %in% seq(60, 90, 10)) %>% 
    pivot_longer(-c(Age))
) %>% 
  mutate(
    name = factor(name, levels = c("r_hz", "p_mor_hz", "p_hosp", "p_phn",
                                   "QL_ph", "cost_GP_pp_PHN_inf", "cost_GP_pp_non_PHN_HZ_inf", "cost_hosp_pp_inf"))
  ) %>% 
  group_by(name, Age) %>% 
  summarise_all(c(n=length, amlu)) %>% 
  mutate(
    M = case_when(
      name %in% c("r_hz", "p_mor_hz") ~ sprintf("%.1f", M * 1e5),
      name %in% c("p_hosp", "p_phn") ~ scales::percent(M),
      name == "QL_ph" ~ sprintf("%.3f", M), 
      #startsWith(name, "cost") ~ sprintf("%.3f", M),
      T ~ sprintf("%.0f", M)
    ), 
    CrI = case_when(
      name %in% c("r_hz", "p_mor_hz") ~ sprintf("(%.1f - %.1f)", L * 1e5, U * 1e5),
      name %in% c("p_hosp", "p_phn") ~ sprintf("(%s - %s)", scales::percent(L), scales::percent(U)),
      name == "QL_ph" ~ sprintf("(%.3f - %.3f)", L, U), 
      #startsWith(name, "cost") ~ sprintf("%.3f", M),
      T ~ sprintf("(%.0f - %.0f)", L, U)
    ), 
  ) %>% 
  select(Age, Variable = name, M, CrI) %>% 
  filter(
    !(Variable %in% c("cost_GP_pp_PHN_inf", "cost_GP_pp_non_PHN_HZ_inf")) | Age == 60
  )

tab_inputs

write_csv(tab_inputs, here::here("docs", "tabs", "tab_inputs_15_rw.csv"))
write_csv(tab_vac, here::here("docs", "tabs", "tab_inputs_vac_rw.csv"))
