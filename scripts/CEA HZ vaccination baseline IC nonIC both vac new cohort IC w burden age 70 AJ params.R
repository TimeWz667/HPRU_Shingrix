# Corrected QALY gain calculation (not multiplying QALY loss post vaccination twice by p_HZ)
# BoD denominator is correct for IC 
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



###################
#### Meta data ####
###################
library(tidyverse)
library(betareg)

set.seed(11667)


folder_data <- function(x) here::here("data", x)
folder_figs <- function(x) here::here("outputs", "figs", x)
folder_tabs <- function(x) here::here("outputs", "tabs", x)

n_run <- 100


### Settings

### vaccine
vaccine <- "zostavax"
# vaccine<-"shingrix"

IC_status <- "non IC"
# IC_status<- "IC"


discount_rate_costs <- 0.035
discount_rate_effects <- 0.035

### vaccine coverage
## AJ's model
# vaccine_coverage<-0.735 
## PHE report 0.483 in 2016/17, 0.549 in 2015/16, 0.59 in 2014/15, 0.618 in 2013/14
vaccine_coverage<-0.735 

### cost per vaccine dose
vaccine_cost_per_dose <- switch(
  vaccine, 
  zostavax = 55, 
  shingrix = 75
)

### admin cost per vaccine dose (item of service fee for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
admin_cost_per_dose<-10

### number of doses
number_courses <- switch(
  vaccine,
  zostavax = 1, 
  shingrix = 2
)


#### Parameters ----
### Incidence
Incidence_HZ <- local({
  df <- read_csv(file = folder_data("Incidence_HZ_old_AJ_model.csv")) %>% 
    rename(age = Age, Incidence_HZ = Incidence)
  
  # false positives when determining incidence
  p_Incidence_false_positives <- 0.05
  # proportion of HZ cases in IC
  p_Incidence_IC <- 0.089523809524

  df %>% 
    full_join(tibble(age = 50:100)) %>% 
    arrange(age) %>% 
    mutate(
      Incidence_HZ = case_when(
        age < 60 ~ df$Incidence_HZ[1],
        age < 100 ~ df$Incidence_HZ[nrow(df)],
        T ~ inc_hz1
      ), 
      Incidence_HZ_nIC = Incidence_HZ * (1-p_Incidence_false_positives)*(1-p_Incidence_IC)
    )
})


### Background mortality 
Background_mortality <- read_csv(folder_data("Background_mortality_old_AJ_model.csv")) %>% 
  rename(age = Age, Background_mortality = `Background mortality`)


QL_HZ <- local({
  df <- read_csv(folder_data("QL_AJ.csv"), col_names = c("age", "QL_HZ_d"))
  df %>% 
    full_join(tibble(age = 50:100)) %>% 
    arrange(age) %>% 
    mutate(
      QL_HZ_d = case_when(
        age < 60 ~  df$QL_HZ_d[1],
        age < 100 ~ df$QL_HZ_d[nrow(df)],
        T ~ QL_HZ_d
      )
    )
})


QL_HZ_6m <- local({
  df <- read_csv(folder_data("QL_6m_pre_vac_AJ.csv"))
  df %>% 
    full_join(tibble(age = 50:100)) %>% 
    arrange(age) %>% 
    mutate(
      QL_6m_pre_vac = case_when(
        age < 70 ~ df$QL_6m_pre_vac[1],
        age < 100 ~ df$QL_6m_pre_vac[nrow(df)],
        T ~ QL_6m_pre_vac
      ),
      QL_6m_post_vac = QL_6m_pre_vac*0.646914809
    )
})


additional_QL_HZ_6m <- read_csv(folder_data("23-07-2019 AJs additional QL over 6m zvl.csv")) %>% 
  rename(additional_QL_HZ_6m = `additional QL 6m`)

VE <- read_csv(folder_data("VE_old_AJ_model.csv"))


### HZ mortality
# AJ's old mortality

HZ_mortality <- read_csv(folder_data("CFR_HZ_old_AJ_model.csv")) %>% 
  rename(age = Age, HZ_mortality_rate = CFR) %>% 
  mutate(p_deaths_HZ = 0)


Hospitalisation_rate_HZ <- read_csv(folder_data("hospitalisation_rates_old_AJ_model.csv")) %>% 
  rename(age = Age, Hospitalisation_rate_HZ = Hospitalisation_rate)


### costs of hospitalisation per HZ case
## from AJ's model (based on average length of stay per age group)
Hospitalisation_costs_HZ <- read_csv(folder_data("hospital_costs_HZ_old_AJ_model.csv")) %>% 
  rename(age = Age, Hospitalisation_costs_pp_HZ_inf = hospital_costs)
  

### costs GP (outpatient direct cost per HZ episode)- excludes hospitalisations. Gauthier et al. 2009 Epidemiology and cost of herpes zoster and post-herpetic neuralgia in the United Kingdom. As used by AJ in his last model.
# AJ costs
Cost_GP_per_HZ <- read_csv(folder_data("AJ_p_PHN.csv")) %>% 
  mutate(
    GP_cost_pp_non_PHN_HZ_inf = 75.63,
    GP_cost_pp_PHN_inf = 340.04
  )


Parameters <- Incidence_HZ %>% 
  select(age, p_HZ = Incidence_HZ_nIC) %>% 
  mutate(
    p_HZ_GP_only = p_HZ
  ) %>% 
  left_join(Background_mortality, by = "age") %>% 
  left_join(QL_HZ, by = "age") %>% 
  left_join(QL_HZ_6m, by = "age") %>%
  left_join(additional_QL_HZ_6m, by = "age") %>% 
  left_join(VE) %>% 
  left_join(HZ_mortality) %>% 
  left_join(Hospitalisation_rate_HZ) %>% 
  left_join(Hospitalisation_costs_HZ) %>% 
  left_join(Cost_GP_per_HZ)


save(Parameters, file = here::here("pars", "HZ_IC_NonIC.rdata"))

#########################################################################################################################
# Populating model
#########################################################################################################################

### England population
# AJ's old model
Pop_2015 <- read_csv(folder_data("Background population AJ model.csv"), col_names = c("Age", "Pop"))


#########################################################################################################################
# Cost-effectiveness analysis of herpes zoster vaccination
#########################################################################################################################

load(file = here::here("pars", "HZ_IC_NonIC.rdata"))


### start loop
# for (k in 50:90){
k <- 70

### Initialise model 
vaccination_age <- k


##########################
#### cohort size
# target population population in England at vaccination age
if(IC_status=="non IC"){
  target_population <- Pop_2015 %>% filter(Age%in%vaccination_age) %>% pull(Pop)
} else if (IC_status=="IC"){
  target_population <- Pop_2015 %>% filter(Age%in%vaccination_age) %>% pull(Pop_IC)
} else {print("Error")}
# target population  * vaccine_coverage
if(IC_status=="non IC"){
  cohort_size <- target_population * vaccine_coverage
} else if (IC_status=="IC"){
  cohort_size <- target_population * vaccine_coverage
} else {print("Error")}

popest <- data.frame(age=0:100,
                   pe=c(rep(0.9, length(0:14)),
                        rep(0.88, length(15:19)),
                        rep(0.87, length(20:39)),
                        rep(0.85, length(40:49)),
                        rep(0.8,  length(50:69)),
                        rep(0.75, length(70:79)),
                        rep(0.725, length(80:100))))


sims <- lapply(1:n_run, function(i) {
  sim <- Parameters %>% 
    mutate(Key = i) %>% 
    #### Survival_probability_HZ
    mutate(
      p_survival = case_when(
        age <= vaccination_age ~ 1,
        T ~ 1 - Background_mortality
      ),
      p_survival = cumprod(p_survival)
    ) %>% 
    #### Model only from vaccination age
    filter(age >= vaccination_age) %>% 
    #### function for calculating probabilities
    mutate(
      p_HZ_alive = p_HZ * p_survival,
      p_HZ_GP_only_alive = p_HZ_GP_only * p_survival,
      p_HZ_post_vac = p_HZ_alive * (1 - VE),  # Probability pre-vaccination * (1- VE)
      p_deaths_HZ_post_vac = p_deaths_HZ * (1 - VE), # Probability pre-vaccination * (1- VE)
      p_hospitalisation = p_HZ_alive * Hospitalisation_rate_HZ,
      p_hospitalisation_post_vac = p_hospitalisation * (1 - VE)
    ) %>% 
    #### QALY loss HZ
    mutate(
      QL_HZ_pre_vac = p_HZ_alive * QL_HZ_d,
      QL_HZ_pre_vac_d = QL_HZ_pre_vac / ((1 + discount_rate_effects)^(age - vaccination_age)), # QALY loss HZ in the cohort
      additional_QL_o3m_d = additional_QL_HZ_6m,
      additional_QL_o3m_d = ifelse(vaccine == "shingrix", 0, additional_QL_o3m_d),
      QL_HZ_post_vac = p_HZ_post_vac * QL_HZ_d - additional_QL_o3m_d,
      QL_HZ_post_vac_d = QL_HZ_post_vac / ((1 + discount_rate_effects)^(age - vaccination_age)),
      QL_death_pre_vac_d = 0,
      QL_death_post_vac_d = 0
    ) %>% 
    #### function to calculate costs
    mutate(
      # Costs for hospitalisations due to HZ in the cohort
      Cost_hospitalisation = p_hospitalisation * Hospitalisation_costs_pp_HZ_inf,
      Cost_hospitalisation_d = Cost_hospitalisation/((1+discount_rate_costs)^(age - vaccination_age)),
      # Costs for hospitalisations due to HZ in the cohort
      Cost_hospitalisation_post_vac = p_hospitalisation_post_vac* Hospitalisation_costs_pp_HZ_inf,
      Cost_hospitalisation_post_vac_d = Cost_hospitalisation_post_vac / ((1+discount_rate_costs) ^ (age-vaccination_age)),
      p_PHN_GP = p_HZ_GP_only_alive * p_PHN,
      p_non_PHN_HZ_GP = p_HZ_GP_only_alive * (1 - p_PHN),
      # Costs for GP due to HZ in the cohort
      Cost_GP = (p_PHN_GP * GP_cost_pp_PHN_inf) + (p_non_PHN_HZ_GP * GP_cost_pp_non_PHN_HZ_inf),
      Cost_GP_d = Cost_GP/((1 + discount_rate_costs) ^ (age-vaccination_age)),
      # ensure VE_PHN_w_HZ only applies to zostavax
      VE_PHN_w_HZ = ifelse(vaccine == "shingrix", 0, 0),
      p_PHN_post_vac = (p_HZ_GP_only_alive * (1 - VE))*(1 - VE_PHN_w_HZ) * p_PHN, #different to p_PHN_AJ
      p_non_PHN_HZ_post_vac = (p_HZ_GP_only_alive*(1 - VE)) * (1-((1 - VE_PHN_w_HZ) * p_PHN)),
      # Costs for GP due to HZ in the cohort
      Cost_GP_post_vac = (p_PHN_post_vac * GP_cost_pp_PHN_inf) + (p_non_PHN_HZ_post_vac * GP_cost_pp_non_PHN_HZ_inf),
      Cost_GP_post_vac_d = Cost_GP_post_vac / ((1 + discount_rate_costs) ^ (age-vaccination_age))
    )
})


sims <- bind_rows(sims) %>% arrange(Key, age)




##########################
#### Results data frame to store results
results_df <- sims %>% 
  filter(age < 100) %>% 
  group_by(Key) %>% 
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


# write.csv(results_df, file=paste(Output,"02-07-2019_CEA_", vaccine, "_", IC_status, "_", "age", vaccination_age, ".csv", sep=""))


finished_run <- paste("14-01-2019_CEA_", vaccine, "_", IC_status, "_", "age", vaccination_age, ".csv", sep="")
print(finished_run)



##########################
#### Table burden of disease

# N cases HZ pre vac
p_HZ_alive<-rep(NA, length(l))
for (i in 1: length(l)){
  p_HZ_alive[i]<-sum(l[[i]]$p_HZ_alive)*(target_population)
}
mean(p_HZ_alive)
quantile(p_HZ_alive, prob=c(0.05,0.5, 0.95))
p_HZ_alive<-paste(prettyNum(signif(round(quantile(p_HZ_alive, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                  " (",prettyNum(signif(round(quantile(p_HZ_alive, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                  prettyNum(signif(round(quantile(p_HZ_alive, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")

cohort_size<-(target_population) * vaccine_coverage

# N cases HZ post vac
HZ_post_vac<-rep(NA, length(l))
for (i in 1: length(l)){
  HZ_post_vac[i]<-(sum(l[[i]]$p_HZ_post_vac)*cohort_size)+
    (sum(l[[i]]$p_HZ_alive)*((target_population)-cohort_size)) # is p_HZ_alive here correct?
  
}
mean(HZ_post_vac)
quantile(HZ_post_vac, prob=c(0.05,0.5, 0.95))
HZ_post_vac<-paste(prettyNum(signif(round(quantile(HZ_post_vac, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                   " (",prettyNum(signif(round(quantile(HZ_post_vac, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                   prettyNum(signif(round(quantile(HZ_post_vac, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")


# N hospitalisations pre vac
Hospitalisation<-rep(NA, length(l))
for (i in 1: length(l)){
  Hospitalisation[i]<-sum(l[[i]]$p_hospitalisation)*(target_population)
}
mean(Hospitalisation)
quantile(Hospitalisation, prob=c(0.05,0.5, 0.95))
Hospitalisation<-paste(prettyNum(signif(round(quantile(Hospitalisation, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                       " (",prettyNum(signif(round(quantile(Hospitalisation, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                       prettyNum(signif(round(quantile(Hospitalisation, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")

# N hospitalisations post vac
Hospitalisation_post_vac<-rep(NA, length(l))
for (i in 1: length(l)){
  Hospitalisation_post_vac[i]<-(sum(l[[i]]$p_hospitalisation_post_vac)*cohort_size)+
    (sum(l[[i]]$p_hospitalisation)*((target_population)-cohort_size))
  
}
mean(Hospitalisation_post_vac)
quantile(Hospitalisation_post_vac, prob=c(0.05,0.5, 0.95))
Hospitalisation_post_vac<-paste(prettyNum(signif(round(quantile(Hospitalisation_post_vac, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                                " (",prettyNum(signif(round(quantile(Hospitalisation_post_vac, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                                prettyNum(signif(round(quantile(Hospitalisation_post_vac, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")

#GP cost undiscounted pre vac
Cost_GP<-rep(NA, length(l))
for (i in 1: length(l)){
  Cost_GP[i]<-sum(l[[i]]$Cost_GP)*(target_population)
}
mean(Cost_GP)
quantile(Cost_GP, prob=c(0.05,0.5, 0.95))
Cost_GP<-paste(prettyNum(signif(round(quantile(Cost_GP, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
               " (",prettyNum(signif(round(quantile(Cost_GP, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
               prettyNum(signif(round(quantile(Cost_GP, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")


#GP cost undiscounted post vac
Cost_GP_post_vac<-rep(NA, length(l))
for (i in 1: length(l)){
  Cost_GP_post_vac[i]<-(sum(l[[i]]$Cost_GP_post_vac)*cohort_size)+
    (sum(l[[i]]$Cost_GP)*((target_population)-cohort_size))
  
}
mean(Cost_GP_post_vac)
quantile(Cost_GP_post_vac, prob=c(0.05,0.5, 0.95))
Cost_GP_post_vac<-paste(prettyNum(signif(round(quantile(Cost_GP_post_vac, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                        " (",prettyNum(signif(round(quantile(Cost_GP_post_vac, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                        prettyNum(signif(round(quantile(Cost_GP_post_vac, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")

#Hospital cost undiscounted pre vac
Cost_hospitalisation<-rep(NA, length(l))
for (i in 1: length(l)){
  Cost_hospitalisation[i]<-sum(l[[i]]$Cost_hospitalisation)*(target_population)
}
mean(Cost_hospitalisation)
quantile(Cost_hospitalisation, prob=c(0.05,0.5, 0.95))
Cost_hospitalisation<-paste(prettyNum(signif(round(quantile(Cost_hospitalisation, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                            " (",prettyNum(signif(round(quantile(Cost_hospitalisation, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                            prettyNum(signif(round(quantile(Cost_hospitalisation, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")

#Hospital cost undiscounted post vac
Cost_hospitalisation_post_vac<-rep(NA, length(l))
for (i in 1: length(l)){
  Cost_hospitalisation_post_vac[i]<-(sum(l[[i]]$Cost_hospitalisation_post_vac)*cohort_size)+
    (sum(l[[i]]$Cost_hospitalisation)*((target_population)-cohort_size))
  
}
mean(Cost_hospitalisation_post_vac)
quantile(Cost_hospitalisation_post_vac, prob=c(0.05,0.5, 0.95))
Cost_hospitalisation_post_vac<-paste(prettyNum(signif(round(quantile(Cost_hospitalisation_post_vac, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                                     " (",prettyNum(signif(round(quantile(Cost_hospitalisation_post_vac, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                                     prettyNum(signif(round(quantile(Cost_hospitalisation_post_vac, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")


#QL pre vac undiscounted
QL_HZ<-rep(NA, length(l))
for (i in 1: length(l)){
  QL_HZ[i]<-sum(l[[i]]$QL_HZ_pre_vac)*(target_population)
}
mean(QL_HZ)
quantile(QL_HZ, prob=c(0.05,0.5, 0.95))
QL_HZ<-paste(prettyNum(signif(round(quantile(QL_HZ, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
             " (",prettyNum(signif(round(quantile(QL_HZ, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
             prettyNum(signif(round(quantile(QL_HZ, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")

#QL post vac undiscounted
QL_HZ_post_vac<-rep(NA, length(l))
for (i in 1: length(l)){
  QL_HZ_post_vac[i]<-(sum(l[[i]]$QL_HZ_post_vac)*cohort_size)+
    (sum(l[[i]]$QL_HZ_pre_vac)*((target_population)-cohort_size))
  
}
mean(QL_HZ_post_vac)
quantile(QL_HZ_post_vac, prob=c(0.05,0.5, 0.95))
QL_HZ_post_vac<-paste(prettyNum(signif(round(quantile(QL_HZ_post_vac, prob=c(0.5)),1),3),big.mark=",", preserve.width="none"),
                      " (",prettyNum(signif(round(quantile(QL_HZ_post_vac, prob=c(0.05)),1),3),big.mark=",", preserve.width="none"),"-",
                      prettyNum(signif(round(quantile(QL_HZ_post_vac, prob=c(0.95)),1),3),big.mark=",", preserve.width="none"),")", sep="")



BoD<-data.frame(Scenario=c("No vaccination", "vaccination"),
                N_HZ_cases=c(p_HZ_alive,HZ_post_vac),
                N_hospitalisations=c(Hospitalisation, Hospitalisation_post_vac),
                GP_costs_undiscounted=c(Cost_GP, Cost_GP_post_vac),
                Hospitalisation_cost_undiscounted=c(Cost_hospitalisation,Cost_hospitalisation_post_vac),
                QL_undiscounted=c(QL_HZ,QL_HZ_post_vac),
                Cost_intervention=c(0,prettyNum(signif(results_df$total_costs_intervention[1],3),big.mark=",", preserve.width="none")))
