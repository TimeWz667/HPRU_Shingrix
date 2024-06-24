library(tidyverse)


load(here::here("data", "processed_demography", "Population_ONS.rdata"))
load(here::here("data", "fitted_coverage.rdata"))
load(file = here::here("pars", "parset_nic_c35q35y24n1k.rdata"))


source(here::here("models", "sim_profile.R"))


profile <- sim_profile(demo_ons, pars_cov = pred1$pars, ve = pars_set$VE_ZVL)


write_csv(profile, here::here("docs", "tabs", "sim_profile.csv"))




tab_profile <- profile %>% 
  group_by(Agp) %>% 
  summarise(
    N_All = sum(N, na.rm = T),
    N_ZVL = sum(N * (TimeVac > 0), na.rm = T),
    N_ZVL2 = sum(N * (TimeVac >= 2), na.rm = T),
    N_ZVL5 = sum(N * (TimeVac >= 5), na.rm = T),
    N_Protected = sum(Protection * N, na.rm = T),
    P_ZVL = N_ZVL / N_All,
    P_ZVL2 = N_ZVL2 / N_All,
    P_ZVL5 = N_ZVL5 / N_All,
    P_Protected = N_Protected / N_All
  )

write_csv(tab_profile, here::here("docs", "tabs", "tab_profile.csv"))

  

## Cost-effective burden by Vaccine
profile <- read_csv(here::here("docs", "tabs", "sim_profile.csv"))
stats_ce <- read_csv(here::here("docs", "tabs", "stats_ce_rzv.csv"))
stats_cer <- read_csv(here::here("docs", "tabs", "stats_ce_zvl2rzv.csv"))
load(here::here("data", "fitted_coverage.rdata"))

sel_cols <- c("N0", "dQ_All_d", "dC_All_d", "dC_VacRZV_d", "dN_VacRZV_d", "ICER")

vaccine <- bind_rows(
  stats_ce %>% 
    select(Age = Age0, Arm, Index, M) %>% 
    filter(Index %in% sel_cols) %>% 
    pivot_wider(names_from = Index, values_from = M) %>% 
    mutate(TimeVac = -1),
  stats_cer %>% 
    filter(Scenario != "Overall") %>% 
    mutate(TimeVac = Age1 - Age0) %>% 
    select(Age = Age1, TimeVac, Arm, Index, M) %>% 
    filter(Index %in% sel_cols) %>% 
    pivot_wider(names_from = Index, values_from = M)
) %>% 
  mutate(
    dQ_All_d = dQ_All_d / N0,
    dC_All_d = dC_All_d / N0,
    dC_VacRZV_d = dC_VacRZV_d / N0,
    dN_VacRZV_d = dN_VacRZV_d / N0
  ) %>% 
  select(-N0)


vaccine


ce0 <- profile %>% 
  mutate(
    Eligibility = case_when(
      Age < 65 & TimeVac == -1 ~ "",
      Age < 70 & TimeVac == -1 ~ "New2023",
      Age < 80 & TimeVac == -1 ~ "SOC",
      Age < 80 ~ "",
      Age < 85 & TimeVac == -1 ~ "UV_85",
      Age < 85 ~ "ZVL_85",
      T ~ ""
    ),
    Arm = case_when(
      Eligibility %in% c("New2023", "SOC") ~ "Vac",
      Eligibility == "ZVL_85" ~ "ReVac_RZV1",
      Eligibility %in% c("UV_85") ~ "Vac1",
      T ~ NA
    ) 
  ) %>% 
  filter(!is.na(Arm)) %>% 
  left_join(vaccine) %>% 
  mutate(
    Eligibility = factor(Eligibility, c("SOC", "New2023", "UV_85", "ZVL_85", ""))
  ) %>% 
  arrange(Eligibility)


tab_programme0 <- ce0 %>% 
  group_by(Eligibility, Agp) %>% 
  summarise(
    across(starts_with("d"), \(x) sum(x * N)),
    ICER = weighted.mean(ICER, w = N),
    N = sum(N),
    Arm = paste(unique(Arm), collapse = "")
  ) %>% 
  ungroup() %>% 
  mutate(
    cum_N = cumsum(N),
    across(starts_with("d"), \(x) cumsum(x * N), .names = "cum_{.col}"),
    Thres = (cum_dQ_All_d * 2e4 - (cum_dC_All_d - cum_dC_VacRZV_d)) / cum_dN_VacRZV_d,
    cum_ICER = cum_dC_All_d / cum_dQ_All_d,
    pr_cum_N = cumsum(N) / sum(N),
    pr_cum_dC = cum_dC_All_d / max(cum_dC_All_d),
    pr_cum_dQ = cum_dQ_All_d / max(cum_dQ_All_d),
  ) %>% 
  select(Eligibility, Agp, Arm, N, dC_All_d, dQ_All_d, pr_cum_N, pr_cum_dC, pr_cum_dQ, Thres, cum_ICER)

 
tab_programme <- tab_programme0
write_csv(tab_programme, here::here("docs", "tabs", "tab_programme_all.csv"))



tab_programme1 <- ce0 %>% 
  mutate(
    N = ifelse(Age %in% c(65, 70, 75, 80), N * pred1$pars$p_initial, N * pred1$pars$p_catchup)
  ) %>% 
  group_by(Eligibility, Agp) %>% 
  summarise(
    across(starts_with("d"), \(x) sum(x * N)),
    ICER = weighted.mean(ICER, w = N),
    N = sum(N),
    Arm = paste(unique(Arm), collapse = "")
  ) %>% 
  ungroup() %>% 
  mutate(
    cum_N = cumsum(N),
    across(starts_with("d"), \(x) cumsum(x * N), .names = "cum_{.col}"),
    Thres = (cum_dQ_All_d * 2e4 - (cum_dC_All_d - cum_dC_VacRZV_d)) / cum_dN_VacRZV_d,
    cum_ICER = cum_dC_All_d / cum_dQ_All_d,
    pr_cum_N = cumsum(N) / sum(N),
    pr_cum_dC = cum_dC_All_d / max(cum_dC_All_d),
    pr_cum_dQ = cum_dQ_All_d / max(cum_dQ_All_d),
  ) %>% 
  select(Eligibility, Agp, Arm, N, dC_All_d, dQ_All_d, pr_cum_N, pr_cum_dC, pr_cum_dQ, Thres, cum_ICER)


tab_programme <- tab_programme1
write_csv(tab_programme, here::here("docs", "tabs", "tab_programme_65_75_5.csv"))


tab_programme2 <- ce0 %>% 
  mutate(
    N = ifelse(Age %in% c(65, 70, 75), N * pred1$pars$p_initial, N * pred1$pars$p_catchup)
  ) %>% 
  group_by(Eligibility, Agp) %>% 
  summarise(
    across(starts_with("d"), \(x) sum(x * N)),
    ICER = weighted.mean(ICER, w = N),
    N = sum(N),
    Arm = paste(unique(Arm), collapse = "")
  ) %>% 
  ungroup() %>% 
  mutate(
    cum_N = cumsum(N),
    across(starts_with("d"), \(x) cumsum(x * N), .names = "cum_{.col}"),
    Thres = (cum_dQ_All_d * 2e4 - (cum_dC_All_d - cum_dC_VacRZV_d)) / cum_dN_VacRZV_d,
    cum_ICER = cum_dC_All_d / cum_dQ_All_d,
    pr_cum_N = cumsum(N) / sum(N),
    pr_cum_dC = cum_dC_All_d / max(cum_dC_All_d),
    pr_cum_dQ = cum_dQ_All_d / max(cum_dQ_All_d),
  ) %>% 
  select(Eligibility, Agp, Arm, N, dC_All_d, dQ_All_d, pr_cum_N, pr_cum_dC, pr_cum_dQ, Thres, cum_ICER)


tab_programme <- tab_programme2
write_csv(tab_programme, here::here("docs", "tabs", "tab_programme_65_80_5.csv"))




tab_programme0 %>% 
  summarise(across(N:dQ_All_d, sum))

