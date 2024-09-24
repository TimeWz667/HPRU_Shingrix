library(tidyverse)


load(here::here("data", "processed_demography", "Population_ONS.rdata"))
load(here::here("data", "fitted_coverage.rdata"))
load(file = here::here("pars", "parset_nic_c35q35y24n1k_realworld.rdata"))


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
stats_ce <- read_csv(here::here("docs", "tabs", "stats_ce_rzv_realworld.csv"))
stats_cer <- read_csv(here::here("docs", "tabs", "stats_ce_zvl2rzv_realworld.csv"))
load(here::here("data", "fitted_coverage.rdata"))

sel_cols <- c("N0", "dRisk_HZ", "dQ_All_d", "dC_All_d", "dC_Med_d", "dC_VacRZV_d", "dN_VacRZV_d")


n_all <- profile %>% 
  group_by(Agp) %>% 
  summarise(N_All = sum(N))


vaccine <- bind_rows(
  stats_ce %>% 
    select(Age = Age0, Arm, starts_with(sel_cols)) %>% 
    pivot_longer(ends_with(c("_A", "_L", "_M", "_U")), names_to = c("name", "stats"), 
                 names_pattern = "(\\S+)_(A|M|L|U)") %>% 
    filter(stats == "M") %>% 
    select(-stats) %>% 
    pivot_wider() %>% 
    mutate(TimeVac = -1),
  stats_cer %>% 
    filter(Scenario != "Overall") %>% 
    mutate(TimeVac = Age1 - Age0) %>% 
    select(Age = Age1, TimeVac, Arm, starts_with(sel_cols)) %>% 
    pivot_longer(ends_with(c("_A", "_L", "_M", "_U")), names_to = c("name", "stats"), 
                 names_pattern = "(\\S+)_(A|M|L|U)") %>% 
    filter(stats == "M") %>% 
    select(-stats) %>% 
    pivot_wider()
) %>% 
  mutate(
    nd = ifelse(endsWith(Arm, "1d"), 1, 2),
    Price = dC_VacRZV_d / dN_VacRZV_d,
    fac = nd / dN_VacRZV_d,
    across(starts_with("d"), \(x) x * fac)
  ) %>% 
  select(-N0, -nd, -Price, fac)


ce0 <- profile %>% 
  mutate(
    #TimeVac = ifelse(Age >= 85, -1, TimeVac),
    Eligibility = case_when(
      Age < 65 & TimeVac == -1 ~ "",
      Age == 65 & TimeVac == -1 ~ "New2023",
      Age < 70 & TimeVac == -1 ~ "",
      Age < 80 & TimeVac == -1 ~ "SOC",
      Age < 80 ~ "",
      Age < 85 & TimeVac == -1 ~ "UV_85",
      Age < 85 ~ "ZVL_85",
      Age < 95 & TimeVac == -1 ~ "UV_95",
      Age < 95 ~ "ZVL_90",
      T ~ ""
    ),
    Arm = case_when(
      Eligibility %in% c("New2023", "SOC") ~ "Vac_2d",
      Eligibility %in% c("ZVL_85", "ZVL_90") ~ "ReVac_RZV_1d",
      Eligibility %in% c("UV_85", "UV_95") ~ "Vac_1d",
      T ~ NA
    ) 
  ) %>% 
  filter(!is.na(Arm)) %>% 
  left_join(vaccine) %>% 
  mutate(
    Eligibility = factor(Eligibility, c("New2023", "SOC", "ZVL_85", "UV_85", "ZVL_90", "UV_95", ""))
  ) %>% 
  arrange(Eligibility)



make_profile <- function(df) {
  df %>% 
    group_by(Eligibility, Agp) %>% 
    summarise(
      across(starts_with("d"), \(x) sum(x * N_Uptake)),
      N_Doses = sum(N_Uptake * ifelse(endsWith(Arm, "1"), 1, 2)),
      N = sum(N),
      N_Uptake = sum(N_Uptake),
      Arm = paste(unique(Arm), collapse = "")
    ) %>% 
    ungroup() %>% 
    mutate(
      across(c(N, N_Uptake, N_Doses), \(x) cumsum(x), .names = "cum_{.col}"),
      across(starts_with("d"), \(x) cumsum(x), .names = "cum_{.col}"),
      Thres = (cum_dQ_All_d * 2e4 - cum_dC_Med_d) / cum_dN_VacRZV_d,
      cum_dC_All_d = cum_dC_Med_d + cum_dN_VacRZV_d * 60,
      dC_All_d = dC_Med_d + dN_VacRZV_d * 60,
      CumICER = cum_dC_All_d / cum_dQ_All_d,
      Prop_Uptake = cum_N_Uptake / cum_N_Uptake[3],
      Prop_Doses = cum_N_Doses / cum_N_Doses[3],
      Prop_Case = cum_dRisk_HZ / cum_dRisk_HZ[3],
      Prop_dQ = cum_dQ_All_d / cum_dQ_All_d[3],
      Prop_dC_Med = cum_dC_Med_d / cum_dC_Med_d[3],
      ICER = dC_All_d / dQ_All_d
    ) %>% 
    left_join(n_all) %>% 
    select(Eligibility, Agp, Arm, N_All, N, N_Uptake, N_Doses, 
           dRisk_HZ, dC_Med_d, dQ_All_d, Thres, starts_with("Prop"))
  
}



tab_programme <- tab_programme1 <- ce0 %>% 
  mutate(
    N_Uptake = N * ifelse(Age %in% c(65, 70), pred1$pars$p_initial, pred1$pars$p_catchup)
  ) %>% 
  make_profile()
  
write_csv(tab_programme, here::here("docs", "tabs", "tab_programme_cont.csv"))


# tab_programme <- tab_programme1 <- ce0 %>% 
#   mutate(
#     N_Uptake = N * case_when(
#       Age %in% c(65, 70, 80) ~ pred1$pars$p_initial,
#       Age < 80 ~ pred1$pars$p_catchup,
#       T ~ 0
#     )
#   ) %>% 
#   make_profile()
# 
# write_csv(tab_programme, here::here("docs", "tabs", "tab_programme_call80.csv"))


tab_programme <- tab_programme2 <- ce0 %>% 
  mutate(
    N_Uptake = N * ifelse(Age %in% c(65, 70, 80), pred1$pars$p_initial, pred1$pars$p_catchup)
  ) %>% 
  make_profile()

write_csv(tab_programme, here::here("docs", "tabs", "tab_programme_call80_cont.csv"))



# tab_programme0 %>% tail(5)
tab_programme1 %>% tail(5)
tab_programme2 %>% tail(5)




