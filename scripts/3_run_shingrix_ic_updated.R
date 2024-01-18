## Update function with dynamic modelling
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

IC_status<- "IC"
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
load(folder_data("Population_IC_Ons_2015.rdata"))
load(folder_data("Epi_IC.rdata"))
load(folder_data("QOL_LE.rdata"))
load(folder_data("Cost_GP_Gauthier.rdata"))
load(folder_data("Cost_Hospitalisation_IC.rdata"))
load(folder_data("VE_Shingrix_IC.rdata"))


### Shared properties
sims0 <- crossing(ID = 1:N_Iter, age = 0:100) %>% 
  left_join(Pop) %>% 
  left_join(rand_table(Epi_HZ, N_Iter) %>% rename(age = Age)) %>% 
  left_join(rand_table(QL, N_Iter))  %>% 
  left_join(rand_table(Cost_Hospitalisation_HZ, N_Iter)) %>% 
  left_join(rand_table(Cost_GP, N_Iter)) %>%
  # left_join(QL_death0) %>% 
  rename(r_mor_bg = Background_mortality, Age = age) %>% 
  mutate(
    #QL_death0 = QOL * LE,
    QL_y2_d = QL_y2 / (1 + discount_rate_effects),
    QL_HZ = QL_y1 + QL_y2,
    QL_HZ_d = QL_y1 + QL_y2_d,
    QL_o3m_pre_vac_d = QL_y1_o3m + QL_y2_d
  ) %>% 
  group_by(ID) %>% 
  arrange(ID, Age)


### start loop

results <- list()


calc_ce <- function(df, n0) {
  df %>% 
    mutate(
      N_Alive = p_survival * n0,
      N_Start = c(n0, N_Alive[-length(N_Alive)]),
      N_HZ_All = N_Start * p_hz,
      N_HZ_GP = N_Start * p_hz_gp,
      N_HZ_Hosp = N_Start * p_hz_hosp,
      N_PHN_All = N_Start * p_hz * p_phn,
      N_PHN_GP = N_Start * p_hz_gp * p_phn,
      N_PHN_Hosp = N_Start * p_hz_hosp * p_phn,
      N_Death = N_Start - N_Alive,
      N_DeathBg = N_Start * p_death_bg,
      N_DeathHZ = N_Death - N_DeathBg,
      # N_Death = ifelse(Age == 100, 0, N_Death),
      # N_DeathBg = ifelse(Age == 100, 0, N_DeathBg),
      # N_DeathHZ = ifelse(Age == 100, 0, N_DeathHZ),
      dis_ql = 1 / ((1 + discount_rate_effects)^(Age - vaccination_age)),
      dis_cost = 1 / ((1 + discount_rate_costs)^(Age - vaccination_age)),
      Q_Life = N_Alive * QOL,
      Q_HZ = - N_HZ_All * QL_HZ,
      Q_All = Q_Life + Q_HZ,
      Q_Life_d = Q_Life * dis_ql,
      Q_HZ_d = - N_HZ_All * QL_HZ_d * dis_ql,
      Q_All_d = Q_Life_d + Q_HZ_d,
      C_Hosp = N_HZ_Hosp * Hospitalisation_costs_pp_HZ_inf,
      C_GP_NonPHN = (N_HZ_GP - N_PHN_GP) * GP_cost_pp_non_PHN_HZ_inf,
      C_GP_PHN = N_PHN_GP * GP_cost_pp_PHN_inf,
      C_GP = C_GP_NonPHN + C_GP_PHN,
      C_Vac = c((vaccine_cost_per_dose + admin_cost_per_dose) * number_courses, rep(0, n() - 1)) * N_Vac,
      C_All = C_Hosp + C_GP + C_Vac,
      C_Hosp_d = C_Hosp * dis_cost,
      C_GP_NonPHN_d = C_GP_NonPHN * dis_cost,
      C_GP_PHN_d = C_GP_PHN * dis_cost,
      C_GP_d = C_GP * dis_cost,
      C_All_d = C_Hosp_d + C_GP_d + C_Vac
    ) %>% 
    select(ID, Age, starts_with(c("N_", "Q_", "C_")))
  
}


for (vaccination_age in 18:95){
  scenario <- sprintf("CEA_24_%s_%s_%s", vaccine, IC_status, vaccination_age)
  scenario <- glue::as_glue(scenario)
  print(scenario)
  
  ##########################
  #### cohort size
  # population in England at vaccination age * vaccine_coverage
  cohort_size <- Pop %>% filter(age == vaccination_age) %>% pull(Pop) * vaccine_coverage
  
  ## Attach VE
  sims_baseline <- sims0 %>% 
    mutate(Scenario = scenario) %>% 
    left_join(bind_rows(lapply(1:N_Iter, function(i) {
      sample_ve(VE, vaccination_age = vaccination_age) %>% mutate(ID = i) %>% rename(Age = age)
    }))) %>% 
    filter(Age >= vaccination_age)
  
  
  sims_soc <- sims_baseline %>% 
    group_by(ID) %>% 
    mutate(
      N_Vac = 0,
      p_hz = pexp(1, r_inc_hz),
      p_hz_gp = pexp(1, r_inc_hz_gp),
      p_hz_hosp = pexp(1, r_hosp_hz),
      r_death = r_mor_bg + r_mor_hz,
      # p_death_bg = pexp(1, r_mor_bg),
      # p_death_hz = pexp(1, r_mor_hz),
      # p_death = p_death_bg + p_death_hz,
      # Competing risk approach
      p_death = pexp(1, r_death),
      p_death = c(p_death[1:(n() - 1)], 1),
      p_death_bg = p_death * r_mor_bg / r_death,
      p_death_hz = p_death - p_death_bg,
      p_survival = cumprod(1 - p_death)
    ) %>% 
    calc_ce(cohort_size)  %>% 
    summarise_all(sum) %>% 
    select(-Age) %>% 
    ungroup()
  
  
  sims_alt <- sims_baseline %>% 
    group_by(ID) %>% 
    mutate(
      N_Vac = c(cohort_size, rep(0, n() - 1)),
      p_hz = pexp(1, r_inc_hz * (1 - VE)),
      p_hz_gp = pexp(1, r_inc_hz_gp * (1 - VE)),
      p_hz_hosp = pexp(1, r_hosp_hz * (1 - VE)),
      r_death = r_mor_bg + r_mor_hz * (1 - VE),
      # p_death_bg = pexp(1, r_mor_bg),
      # p_death_hz = pexp(1, r_mor_hz * (1 - VE)),
      # p_death = p_death_bg + p_death_hz,
      # Competing risk approach
      p_death = pexp(1, r_death),
      p_death = c(p_death[1:(n() - 1)], 1),
      p_death_bg = p_death * r_mor_bg / r_death,
      p_death_hz = p_death - p_death_bg,
      p_survival = cumprod(1 - p_death)
    ) %>% 
    calc_ce(cohort_size)  %>% 
    summarise_all(sum) %>% 
    select(-Age) %>% 
    ungroup()
  
  
  sims <- bind_rows(
    sims_soc %>% mutate(Group = "SOC"),
    sims_alt %>% mutate(Group = "Vac")
  ) %>% 
    pivot_longer(-c(ID, Group), names_to = "Stat") %>% 
    pivot_wider(names_from = Group) %>% 
    mutate(
      Diff = Vac - SOC
    ) %>% 
    pivot_longer(-c(ID, Stat), names_to = "Group")
  
  sims <- bind_rows(
    sims, 
    sims %>% 
      filter(Stat %in% c("Q_All", "C_All") & Group == "Diff") %>% 
      pivot_wider(names_from = Stat) %>% 
      mutate(
        Stat = "ICER",
        value = C_All / Q_All,
      ) %>% 
      select(ID, Group, Stat, value),
    sims %>% 
      filter(Stat %in% c("Q_All_d", "C_All_d") & Group == "Diff") %>% 
      pivot_wider(names_from = Stat) %>% 
      mutate(
        Stat = "ICER_d",
        value = C_All_d / Q_All_d,
      ) %>% 
      select(ID, Group, Stat, value)
  )
  
  
  tab <- sims %>% 
    select(-ID) %>% 
    group_by(Stat, Group) %>% 
    summarise_all(
      list(
        Mean = function(x) mean(x, na.rm = T),
        STD = function(x) sd(x, na.rm = T),
        Q025 = function(x) quantile(x, 0.025, na.rm = T),
        Q25 = function(x) quantile(x, 0.25, na.rm = T),
        Q5 = function(x) quantile(x, 0.5, na.rm = T),
        Q75 = function(x) quantile(x, 0.75, na.rm = T),
        Q975 = function(x) quantile(x, 0.975, na.rm = T)
      )
    ) %>% 
    mutate(
      Scenario = scenario
    )
  
  results[[scenario]] <- tab
  write_csv(tab, file = folder_temp(scenario + ".csv"))

}


results <- bind_rows(results)

write_csv(results, file = folder_temp(sprintf("CEA_dy_%s_%s.csv", vaccine, IC_status)))

save(results, file = folder_temp(sprintf("CEA_dy_%s_%s.rdata", vaccine, IC_status)))

