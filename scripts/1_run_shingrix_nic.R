# Both vaccines in one file
# All data in the IC added
# Incidence and p_PHN from CPRD Jemma study
# VE zostavax now includes ZEST study
# VE updated to incorporate age pyramid for ZVL and age variation for data in time for both vaccines
# item of service fee updated to ?10.06  (for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
# QALYs updated new data Brisson and Drolet vs Rampakakis Canada
# incidences transformed to probabilities
# mortality of HZ included
# updated p_PHN excluding individuals vaccinated from 2013
# oputpatients costs age 90+ all the same as age 90
# updated costs of hospitalisation HZ using robust regression
# 29-05-2018 extra QL avoided o3m PHN only for ZVL
# Decide whether to include the extra reduction in QL against PHN for breakthrough HZ  (reduction in QL for the first 6 months meant for Zostavax but included for Sept 2017 CEA JCVI for both and o3m QL included for both in May 2018)
# change vaccine_cost_per_dose, number_courses and vaccine effectiveness, extra QL adverted, for other vaccine  
# For both vaccines QL-> Area between model and 1, up to when model reaches one (LE)
# QL with transformation 0.3-0.7 (QL 22-11-2017 file) (vs. transformation 0.25-0.75 as per Sept 2017 CEA JCVI)
# PSA


#########################################################################################################################
# Cost-effectiveness analysis of herpes zoster vaccination
#########################################################################################################################

library(tidyverse)
folder_data <- function(x) here::here("data", x)
folder_tab <- function(x) here::here("outputs", "tabs", x)
folder_temp <- function(x) here::here("outputs", "temp", x)


rand_table <- function(df, n_iter) {
  tibble(ID = 1:n_iter, Key = sample(unique(df$Key), N_Iter, rep = T)) %>% 
    left_join(df, relationship = "many-to-many") %>% 
    select(- Key)
}


set.seed(11667)

IC_status<- "NIC"
vaccine<-"shingrix"
N_Iter <- 500

### discount rate costs
discount_rate_costs <- 0.035
### discount rate effects
discount_rate_effects <- 0.035

### vaccine coverage
vaccine_coverage <- 0.483 
### cost per vaccine dose
vaccine_cost_per_dose <- 75
### admin cost per vaccine dose (item of service fee for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
admin_cost_per_dose <- 10
### number of doses
number_courses <- 2


##########################
#### Data
### Demography
load(folder_data("Population_NIC_Ons_2015.rdata"))

### Incidence HZ 
load(folder_data("Epi_HZ_NIC.rdata"))
load(folder_data("P_PHN_NIC_CPRD.rdata"))

### Hospitalisation
load(folder_data("R_Hospitalisation_HZ_NIC.rdata"))

### QALY loss HZ
load(folder_data("QOL_LE.rdata"))

### Cost 
load(folder_data("Cost_GP_Gauthier.rdata"))
load(folder_data("Cost_Hospitalisation_NIC.rdata"))

### Vaccine efficacy
load(folder_data("VE_Shingrix_NIC.rdata"))


### Shared properties
sims0 <- crossing(ID = 1:N_Iter, age = 0:100) %>% 
  left_join(Pop) %>% 
  left_join(rand_table(Incidence_HZ, N_Iter)) %>% 
  left_join(rand_table(Incidence_HZ_GP_only, N_Iter)) %>% 
  left_join(rand_table(Mortality_HZ, N_Iter)) %>% 
  left_join(rand_table(P_PHN, N_Iter)) %>% 
  left_join(rand_table(Rate_Hospitalisation_HZ, N_Iter)) %>% 
  left_join(rand_table(QOL, N_Iter))  %>% 
  left_join(rand_table(Cost_Hospitalisation_HZ, N_Iter)) %>% 
  left_join(rand_table(Cost_GP, N_Iter)) %>% 
  mutate(
    p_death_hz = Death_HZ,
    p_hospitalised_hz = 1 - exp(-Hospitalisation_rate_HZ),
    QL_death0 = QOL * LE,
    QL_y2_d = QL_y2 / (1 + discount_rate_effects),
    QL_HZ = QL_y1 + QL_y2,
    QL_HZ_d = QL_y1 + QL_y2_d,
    QL_o3m_pre_vac_d = QL_y1_o3m + QL_y2_d
  ) %>% 
  group_by(ID) %>% 
  arrange(ID, age)



### start loop

results <- list()


for (vaccination_age in 18:95){
  print(vaccination_age)
  scenario <- sprintf("CEA_%s_%s_%s", vaccine, IC_status, vaccination_age)
  scenario <- glue::as_glue(scenario)
  
  ##########################
  #### cohort size
  # population in England at vaccination age * vaccine_coverage
  cohort_size <- Pop %>% filter(age == vaccination_age) %>% pull(Pop) * vaccine_coverage
  
  ## Attach VE
  sims <- sims0 %>% 
    mutate(Scenario = scenario) %>% 
    left_join(bind_rows(lapply(1:N_Iter, function(i) {
      sample_ve(VE, vaccination_age = vaccination_age) %>% mutate(ID = i)
    }))) %>% 
    mutate(
      VE_PHN_w_HZ = 0
    ) %>% 
    filter(age >= vaccination_age) %>% 
    #### Survival_probability_HZ
    mutate(
      age_vaccination = vaccination_age,
      p_survival = case_when(
        age <= vaccination_age ~ 1,
        T ~ 1 - Background_mortality
      ),
      p_survival = cumprod(p_survival)
    ) %>% 
    #### function for calculating probabilities
    mutate(
      p_HZ_alive = p_HZ * p_survival,
      p_HZ_GP_only_alive = p_HZ_GP_only * p_survival,
      p_HZ_post_vac = p_HZ_alive * (1 - VE),  # Probability pre-vaccination * (1- VE)
      p_deaths_HZ_post_vac = p_death_hz * (1 - VE), # Probability pre-vaccination * (1- VE)
      p_hospitalisation = p_hospitalised_hz,
      p_hospitalisation_post_vac = p_hospitalisation * (1 - VE)
    ) %>% 
    #### QALY loss HZ
    mutate(
      discount_ql = 1 / ((1 + discount_rate_effects)^(age - vaccination_age)),
      QL_HZ_pre_vac = p_HZ_alive * QL_HZ_d,
      QL_HZ_pre_vac_d = QL_HZ_pre_vac * discount_ql, # QALY loss HZ in the cohort
      QL_o3m_pre_vac_d = QL_y1_o3m + QL_y2_d,
      QL_o3m_post_vac_d = QL_o3m_pre_vac_d * (1 - VE_PHN_w_HZ),
      QL_o3m_post_vac_with_VE_d = p_HZ_post_vac * QL_o3m_post_vac_d,
      QL_o3m_post_vac_no_VE_d = p_HZ_post_vac * QL_o3m_pre_vac_d,
      additional_QL_o3m_d = QL_o3m_post_vac_no_VE_d - QL_o3m_post_vac_with_VE_d,
      QL_HZ_post_vac = p_HZ_post_vac * QL_HZ_d - additional_QL_o3m_d,
      QL_HZ_post_vac_d = QL_HZ_post_vac * discount_ql,
      QL_death_pre_vac = p_death_hz * QL_death0,
      QL_death_pre_vac_d = QL_death_pre_vac * discount_ql,
      QL_death_post_vac = p_deaths_HZ_post_vac * QL_death0,
      QL_death_post_vac_d = QL_death_post_vac * discount_ql
    ) %>%
    mutate(
      discount_cost = 1 / ((1+discount_rate_costs)^(age - vaccination_age)),
      # Costs for hospitalisations due to HZ in the cohort
      Cost_hospitalisation = p_hospitalisation * Hospitalisation_costs_pp_HZ_inf,
      Cost_hospitalisation_d = Cost_hospitalisation * discount_cost,
      # Costs for hospitalisations due to HZ in the cohort
      Cost_hospitalisation_post_vac = p_hospitalisation_post_vac* Hospitalisation_costs_pp_HZ_inf,
      Cost_hospitalisation_post_vac_d = Cost_hospitalisation_post_vac  * discount_cost,
      p_PHN_GP = p_HZ_GP_only_alive * p_phn,
      p_non_PHN_HZ_GP = p_HZ_GP_only_alive * (1 - p_phn),
      # Costs for GP due to HZ in the cohort
      Cost_GP = (p_PHN_GP * GP_cost_pp_PHN_inf) + (p_non_PHN_HZ_GP * GP_cost_pp_non_PHN_HZ_inf),
      Cost_GP_d = Cost_GP * discount_cost,
      # ensure VE_PHN_w_HZ only applies to zostavax
      p_PHN_post_vac = (p_HZ_GP_only_alive * (1 - VE))*(1 - VE_PHN_w_HZ) * p_phn, #different to p_PHN_AJ
      p_non_PHN_HZ_post_vac = (p_HZ_GP_only_alive*(1 - VE)) * (1-((1 - VE_PHN_w_HZ) * p_phn)),
      # Costs for GP due to HZ in the cohort
      Cost_GP_post_vac = (p_PHN_post_vac * GP_cost_pp_PHN_inf) + (p_non_PHN_HZ_post_vac * GP_cost_pp_non_PHN_HZ_inf),
      Cost_GP_post_vac_d = Cost_GP_post_vac * discount_cost
    ) 
  
  
  tab <- sims %>% 
    group_by(ID, Scenario) %>% 
    summarise(
      total_QL_no_vac_d = sum(QL_HZ_pre_vac_d + QL_death_pre_vac_d) * cohort_size,
      total_QL_post_vac_d = sum(QL_HZ_post_vac_d + QL_death_post_vac_d) * cohort_size,
      total_QG_HZ_d = sum(QL_HZ_pre_vac_d - QL_HZ_post_vac_d),
      total_QG_death_HZ_d = sum(QL_death_pre_vac_d - QL_death_post_vac_d),
      total_C_no_vac_d = sum(Cost_hospitalisation_d + Cost_GP_d) * cohort_size,
      total_C_post_vac_d = sum(Cost_hospitalisation_post_vac_d + Cost_GP_post_vac_d) * cohort_size,
      total_CS_hospital_d = sum(Cost_hospitalisation_d - Cost_hospitalisation_post_vac_d),
      total_CS_GP_d = sum(Cost_GP_d- Cost_GP_post_vac_d)
    ) %>% 
    mutate(
      cohort_size = cohort_size,
      total_QALYs_gained_d = (total_QG_HZ_d + total_QG_death_HZ_d) * cohort_size,
      total_CS_d = total_CS_hospital_d + total_CS_GP_d,
      total_saved_costs_d = total_CS_d * cohort_size,
      total_costs_intervention = cohort_size*((vaccine_cost_per_dose + admin_cost_per_dose)*number_courses),
      total_net_cost = total_costs_intervention - total_saved_costs_d,
      ICER = total_net_cost / total_QALYs_gained_d
    ) %>% 
    ungroup()
  
  results[[scenario]] <- tab
  write_csv(tab, file = folder_temp(scenario + ".csv"))
  
  # BoD<-data.frame(Scenario=c("No vaccination", "vaccination"),
  #                 N_HZ_cases=c(p_HZ_alive,HZ_post_vac),
  #                 N_hospitalisations=c(Hospitalisation, Hospitalisation_post_vac),
  #                 GP_costs_undiscounted=c(Cost_GP, Cost_GP_post_vac),
  #                 Hospitalisation_cost_undiscounted=c(Cost_hospitalisation,Cost_hospitalisation_post_vac),
  #                 QL_undiscounted=c(QL_HZ,QL_HZ_post_vac),
  #                 Cost_intervention=c(0,results_df$total_costs_intervention[1]))
  # 
  
}


results <- bind_rows(results)

write_csv(results, file = folder_temp(sprintf("CEA_%s_%s.csv", vaccine, IC_status)))

save(results, file = folder_temp(sprintf("CEA_%s_%s.rdata", vaccine, IC_status)))



### Summarise results

summ <- results %>% 
  select(Scenario, total_QALYs_gained_d, total_saved_costs_d, total_costs_intervention, total_net_cost, ICER) %>% 
  pivot_longer(-Scenario, names_to = "Index") %>% 
  group_by(Scenario, Index) %>%
  summarise(
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    q025 = quantile(value, 0.025, na.rm = T),
    q250 = quantile(value, 0.25, na.rm = T),
    q500 = quantile(value, 0.5, na.rm = T),
    q750 = quantile(value, 0.75, na.rm = T),
    q975 = quantile(value, 0.975, na.rm = T),
  ) %>% 
  ungroup() %>% 
  extract(Scenario, "Vaccination_Age", "_(\\d+)", convert = T, remove = F) %>% 
  arrange(Index, vaccination_age)

write_csv(summ, file = folder_tab(sprintf("Summary_CEA_%s_%s.csv", vaccine, IC_status)))
