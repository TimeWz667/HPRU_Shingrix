library(tidyverse)

theme_set(theme_bw())



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



