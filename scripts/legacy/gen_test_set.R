library(tidyverse)

theme_set(theme_bw())


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


df0 <- read_csv(here::here("data", "raw_previous", "test_nic_rzv_70.csv"))
res0 <- read_csv(here::here("data", "raw_previous", "results_nic_rzv_70.csv")) %>% 
  mutate(
    dC_GP_d = - total_CS_GP_d * cohort_size,
    dC_Hosp_d = - total_CS_hospital_d * cohort_size,
    dC_Intv_d = total_costs_intervention,
    C_Soc_d = total_C_no_vac_d,
    C_Alt_d = total_C_post_vac_d + total_costs_intervention,
    dC_All_d = total_costs_intervention + (dC_GP_d + dC_Hosp_d),
    dC_All_d2 = C_Alt_d - C_Soc_d,
    dQ_HZ_d = total_QG_HZ_d * cohort_size,
    dQ_Death_d = total_QG_death_HZ_d * cohort_size,
    Q_Soc_d = total_QL_no_vac_d,
    Q_Alt_d = total_QL_post_vac_d,
    dQ_All_d = total_QALYs_gained_d,
    dQ_All_d1 = dQ_HZ_d + dQ_Death_d,
    dQ_All_d2 = - (Q_Alt_d - Q_Soc_d),
    ICER = dC_All_d / dQ_All_d
  ) %>% 
  select(
    N = cohort_size,
    starts_with(c("dC", "C", "dQ", "Q")),
    ICER
  )


df_r <- df0 %>% 
  filter(Key <= 5) %>% 
  select(Key, Age = age, r_inc_hz = Incidence_HZ, r_inc_hz_gp = Incidence_HZ_GP_only,
         r_hosp_hz = Hospitalisation_rate_HZ, r_mor_hz = Incidence_HZ_mortality, p_phn = p_PHN,
         r_mor_bg = Background_mortality_rate, VE, 
         QL_y1, QL_y2, QL_HZ, QL_y1_o3m, QL_death,
         cost_Hospitalisation_pp_inf = Hospitalisation_costs_pp_HZ_inf,
         cost_GP_pp_non_PHN_HZ_inf = GP_cost_pp_non_PHN_HZ_inf,
         cost_GP_pp_PHN_inf = GP_cost_pp_PHN_inf
         )


QL_Death <- df0 %>% filter(Key == 1) %>% 
  select(Age = age, QL_death)

save(res0, df_r, QL_Death, file = here::here("data", "test_ver2021.rdata"))





df_r %>% 
  group_by(Key) %>% 
  mutate(
    N0 = cohort_size,
    dis_e = (1 + discount_rate_effects) ^ - (Age - 70),
    dis_c = (1 + discount_rate_costs) ^ - (Age - 70),
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
    N_PHN_All = N_Alive * p_hz * p_phn,
    N_PHN_GP = N_Alive * p_hz_gp * p_phn,
    N_PHN_Hosp = N_Alive * p_hz_hosp * p_phn,
    dN_HZ_All = N_HZ_All_soc - N_HZ_All_alt,
    dN_HZ_GP = N_HZ_GP_soc - N_HZ_GP_alt,
    dN_Death = N_Death_soc - N_Death_alt,
    dQ_HZ_d = dN_HZ_All * QL_HZ * dis_e,
    dQ_Death_d = QL_death * N0 * p_mor_hz * VE * dis_e, ## to fix,
    Q_Soc_d = (N_HZ_All_soc * QL_HZ + QL_death * N0 * p_mor_hz) * dis_e,
    Q_Alt_d = (N_HZ_All_alt * QL_HZ + QL_death * N0 * (1 - VE) * p_mor_hz) * dis_e,
    dQ_All_d = dQ_Death_d + dQ_HZ_d,
    dC_GP_d = 0, 
    dC_Hosp_d = - N0 * p_hz_hosp * VE * cost_Hospitalisation_pp_inf * dis_c, 
    dC_Intv_d = c(cohort_size * cost_vac_pp, rep(0, n() - 1))
  ) %>% 
  select(Age, starts_with(c("dN_", "dQ_", "Q_", "dC_"))) %>% 
  summarise_all(sum)



df_r %>% 
  group_by(Key) %>% 
  mutate(
    dis_e = (1 + discount_rate_effects) ^ - (Age - 70),
    dis_c = (1 + discount_rate_costs) ^ - (Age - 70),
    p_hz = pexp(1, r_inc_hz),
    p_hz_gp = pexp(1, r_inc_hz_gp),
    p_hz_hosp = pexp(1, r_hosp_hz),
    #r_death = r_mor_bg + r_mor_hz,
    p_survival = c(1, cumprod(1 - r_mor_bg)[-n()]),
    N_Alive = p_survival * cohort_size,
    N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
    N_HZ_All = N_Start * p_hz,
    N_HZ_GP = N_Start * p_hz_gp,
    N_HZ_Hosp = N_Start * p_hz_hosp,
    N_PHN_All = N_Start * p_hz * p_phn,
    N_PHN_GP = N_Start * p_hz_gp * p_phn,
    N_PHN_Hosp = N_Start * p_hz_hosp * p_phn,
    
  ) %>% 
  select(Age, p_survival, starts_with(c("N_", "Q_", "C_")))

