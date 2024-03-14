library(tidyverse)

theme_set(theme_bw())


### 
# Age: 70
# IC: Non-IC
###


# Exogenous inputs
## -- Szende A, Janssen B, Cabases J. Self-reported population health: an international perspective based on EQ-5D. Dordrecht: Springer; 2014. TTO value set England. p30
qol <- tibble(Age = 0:100) %>% 
  mutate(
    QOL = case_when(
      Age < 18 ~ 1,
      Age < 25 ~ 0.929,
      Age < 35 ~ 0.919,
      Age < 45 ~ 0.893,
      Age < 55 ~ 0.855,
      Age < 65 ~ 0.810,
      Age < 75 ~ 0.773,
      T ~ 0.703
    )
  )


cohort_size <- 4869.075

discount_rate_costs <- 0.035
discount_rate_effects <- 0.035
cost_vac_pp <- 170


load(here::here("data", "test_ver2021.rdata"))


## AC's version -----

res_ac <- df_r %>% 
  group_by(Key) %>% 
  mutate(
    N0 = cohort_size,
    dis_e = (1 + discount_rate_effects) ^ - (Age - min(Age)),
    dis_c = (1 + discount_rate_costs) ^ - (Age - min(Age)),
    p_hz = pexp(1, r_inc_hz),
    p_hz_gp = pexp(1, r_inc_hz_gp),
    p_hz_hosp = pexp(1, r_hosp_hz),
    p_mor_hz = 1 - exp(- r_mor_hz),
    p_survival = c(1, cumprod(1 - r_mor_bg)[-n()]),
    N_Alive = p_survival * cohort_size,
    N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
    N_HZ_All_soc = N_Alive * p_hz,
    N_HZ_GP_soc = N_Alive * p_hz_gp,
    N_HZ_Hosp_soc = N_Alive * p_hz_hosp,
    N_Death_soc = N_Alive * p_mor_hz,
    N_HZ_All_alt = N_Alive * p_hz * (1 - VE),
    N_HZ_GP_alt = N_Alive * p_hz_gp * (1 - VE),
    N_HZ_Hosp_alt = N_Alive * p_hz_hosp * (1 - VE),
    N_Death_alt = N_Alive * p_mor_hz * (1 - VE),
    N_PHN_GP_soc = N_Alive * p_hz_gp * p_phn,
    N_PHN_GP_alt = N_Alive * p_hz_gp * p_phn * (1 - VE),
    dN_All = 0,
    dN_HZ_All = N_HZ_All_alt - N_HZ_All_soc,
    dN_HZ_GP = N_HZ_GP_alt - N_HZ_GP_soc,
    dN_HZ_Hosp = N_HZ_Hosp_alt - N_HZ_Hosp_soc,
    dN_Death = N_Death_alt - N_Death_soc,
    dN_PHN = -N_Alive * p_hz * p_phn * VE,
    dQ_HZ_d = dN_HZ_All * QL_HZ * dis_e,
    dQ_Death_d = - QL_death * N0 * p_mor_hz * VE * dis_e, ## to fix,
    Q_Soc_d = (N_HZ_All_soc * QL_HZ + QL_death * N0 * p_mor_hz) * dis_e, ## to fix
    Q_Alt_d = (N_HZ_All_alt * QL_HZ + QL_death * N0 * (1 - VE) * p_mor_hz) * dis_e, ## to fix
    dQ_All_d = - (dQ_Death_d + dQ_HZ_d),
    C_GP_Soc_d = (N_PHN_GP_soc * cost_GP_pp_PHN_inf + (N_HZ_GP_soc - N_PHN_GP_soc) * cost_GP_pp_non_PHN_HZ_inf) * dis_c,
    C_GP_Alt_d = (N_PHN_GP_alt * cost_GP_pp_PHN_inf + (N_HZ_GP_alt - N_PHN_GP_alt) * cost_GP_pp_non_PHN_HZ_inf) * dis_c,
    dC_GP_d = - (C_GP_Soc_d - C_GP_Alt_d),
    dC_Hosp_d = - N0 * p_hz_hosp * VE * cost_Hospitalisation_pp_inf * dis_c, ## to fix
    dC_Intv_d = c(cohort_size * cost_vac_pp, rep(0, n() - 1)),
    dC_All_d = dC_Intv_d + dC_GP_d + dC_Hosp_d
  ) %>% 
  select(starts_with(c("dN_", "dQ_", "Q_", "dC_")),) %>% 
  summarise_all(sum) %>% 
  mutate(ICER = dC_All_d / dQ_All_d)



## Fix survival rates on QALY and Cost calculation -----

res_surv <- df_r %>% 
  group_by(Key) %>% 
  mutate(
    N0 = cohort_size,
    dis_e = (1 + discount_rate_effects) ^ - (Age - min(Age)),
    dis_c = (1 + discount_rate_costs) ^ - (Age - min(Age)),
    p_hz = pexp(1, r_inc_hz),
    p_hz_gp = pexp(1, r_inc_hz_gp),
    p_hz_hosp = pexp(1, r_hosp_hz),
    p_mor_hz = 1 - exp(- r_mor_hz),
    p_survival = c(1, cumprod(1 - r_mor_bg)[-n()]),
    N_Alive = p_survival * cohort_size,
    N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
    N_HZ_All_soc = N_Alive * p_hz,
    N_HZ_GP_soc = N_Alive * p_hz_gp,
    N_HZ_Hosp_soc = N_Alive * p_hz_hosp,
    N_Death_soc = N_Alive * p_mor_hz,
    N_HZ_All_alt = N_Alive * p_hz * (1 - VE),
    N_HZ_GP_alt = N_Alive * p_hz_gp * (1 - VE),
    N_HZ_Hosp_alt = N_Alive * p_hz_hosp * (1 - VE),
    N_Death_alt = N_Alive * p_mor_hz * (1 - VE),
    N_PHN_GP_soc = N_Alive * p_hz_gp * p_phn,
    N_PHN_GP_alt = N_Alive * p_hz_gp * p_phn * (1 - VE),
    dN_All = 0,
    dN_HZ_All = N_HZ_All_alt - N_HZ_All_soc,
    dN_HZ_GP = N_HZ_GP_alt - N_HZ_GP_soc,
    dN_HZ_Hosp = N_HZ_Hosp_alt - N_HZ_Hosp_soc,
    dN_Death = N_Death_alt - N_Death_soc,
    dN_PHN = - N_Alive * p_hz * p_phn * VE,
    dQ_HZ_d = dN_HZ_All * QL_HZ * dis_e,
    dQ_Death_d = dN_Death * QL_death * dis_e,
    Q_Soc_d = (N_HZ_All_soc * QL_HZ + N_Death_soc * QL_death) * dis_e, 
    Q_Alt_d = (N_HZ_All_alt * QL_HZ + N_Death_alt * QL_death) * dis_e,
    dQ_All_d = - (dQ_Death_d + dQ_HZ_d),
    C_GP_Soc_d = (N_PHN_GP_soc * cost_GP_pp_PHN_inf + (N_HZ_GP_soc - N_PHN_GP_soc) * cost_GP_pp_non_PHN_HZ_inf) * dis_c,
    C_GP_Alt_d = (N_PHN_GP_alt * cost_GP_pp_PHN_inf + (N_HZ_GP_alt - N_PHN_GP_alt) * cost_GP_pp_non_PHN_HZ_inf) * dis_c,
    dC_GP_d = - (C_GP_Soc_d - C_GP_Alt_d),
    dC_Hosp_d = dN_HZ_Hosp * cost_Hospitalisation_pp_inf * dis_c,
    dC_Intv_d = c(cohort_size * cost_vac_pp, rep(0, n() - 1)),
    dC_All_d = dC_Intv_d + dC_GP_d + dC_Hosp_d,
    ICER = dC_All_d / dQ_All_d
  ) %>% 
  select(starts_with(c("dN_", "dQ_", "Q_", "dC_")),) %>% 
  summarise_all(sum) %>% 
  mutate(ICER = dC_All_d / dQ_All_d)


## Fix QOL loss due to HZ-related death -----

res_qol <- df_r %>% 
  group_by(Key) %>% 
  left_join(qol) %>% 
  mutate(
    N0 = cohort_size,
    dis_e = (1 + discount_rate_effects) ^ - (Age - min(Age)),
    dis_c = (1 + discount_rate_costs) ^ - (Age - min(Age)),
    p_hz = pexp(1, r_inc_hz),
    p_hz_gp = pexp(1, r_inc_hz_gp),
    p_hz_hosp = pexp(1, r_hosp_hz),
    
    r_mor = r_mor_hz + r_mor_bg,
    p_mor_hz = r_mor_hz,
    p_mor_hz = p_mor_hz * (1 - exp(- r_mor)) / r_mor,
    p_survival = c(1, cumprod(1 - r_mor)[-n()]),
    N_Alive = p_survival * cohort_size,
    N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
    N_All_soc = N_Alive,
    N_HZ_All_soc = N_Alive * p_hz,
    N_HZ_GP_soc = N_Alive * p_hz_gp,
    N_HZ_Hosp_soc = N_Alive * p_hz_hosp,
    N_PHN_All_soc = N_HZ_All_soc * p_phn,
    N_PHN_GP_soc = N_HZ_GP_soc * p_phn,
    N_Death_soc = N_Alive * p_mor_hz,
    
    r_mor = r_mor_hz * (1 - VE) + r_mor_bg,
    p_mor_hz = r_mor_hz * (1 - VE),
    p_mor_hz = p_mor_hz * (1 - exp(- r_mor)) / r_mor,
    p_survival = c(1, cumprod(1 - r_mor)[-n()]),
    N_Alive = p_survival * cohort_size,
    N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
    N_All_alt = N_Alive,
    N_HZ_All_alt = N_Alive * p_hz * (1 - VE),
    N_HZ_GP_alt = N_Alive * p_hz_gp * (1 - VE),
    N_HZ_Hosp_alt = N_Alive * p_hz_hosp * (1 - VE),
    N_PHN_All_alt = N_HZ_All_alt * p_phn,
    N_PHN_GP_alt = N_HZ_GP_alt * p_phn,
    N_Death_alt = N_Alive * p_mor_hz,

    dN_All = N_All_alt - N_All_soc,
    dN_HZ_All = N_HZ_All_alt - N_HZ_All_soc,
    dN_HZ_GP = N_HZ_GP_alt - N_HZ_GP_soc,
    dN_HZ_Hosp = N_HZ_Hosp_alt - N_HZ_Hosp_soc,
    dN_Death = N_Death_alt - N_Death_soc,
    dN_PHN = N_PHN_All_alt - N_PHN_All_soc,
    dQ_HZ_d = dN_HZ_All * QL_HZ * dis_e,
    dQ_Death_d = - dN_All * QOL * dis_e,
    Q_Soc_d = (N_HZ_All_soc * QL_HZ + N_Death_soc * QL_death) * dis_e, 
    Q_Alt_d = (N_HZ_All_alt * QL_HZ + N_Death_alt * QL_death) * dis_e,
    dQ_All_d = - (dQ_Death_d + dQ_HZ_d),
    C_GP_Soc_d = (N_PHN_GP_soc * cost_GP_pp_PHN_inf + (N_HZ_GP_soc - N_PHN_GP_soc) * cost_GP_pp_non_PHN_HZ_inf) * dis_c,
    C_GP_Alt_d = (N_PHN_GP_alt * cost_GP_pp_PHN_inf + (N_HZ_GP_alt - N_PHN_GP_alt) * cost_GP_pp_non_PHN_HZ_inf) * dis_c,
    dC_GP_d = - (C_GP_Soc_d - C_GP_Alt_d),
    dC_Hosp_d = dN_HZ_Hosp * cost_Hospitalisation_pp_inf * dis_c,
    dC_Intv_d = c(cohort_size * cost_vac_pp, rep(0, n() - 1)),
    dC_All_d = dC_Intv_d + dC_GP_d + dC_Hosp_d,
    ICER = dC_All_d / dQ_All_d
  ) %>% 
  select(starts_with(c("dN_", "dQ_", "Q_", "dC_")),) %>% 
  summarise_all(sum) %>% 
  mutate(ICER = dC_All_d / dQ_All_d)



## Remodel incidence and hospitalisation -----


## Move to 2023 population and background mortality -----


## Use new Shingrix data


## Use real-world VE
