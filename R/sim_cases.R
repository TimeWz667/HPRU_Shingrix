
calc_ce <- function(df, n0, age_vac, cost_vac_pp, discounting_e, discounting_c) {
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
      dis_ql = 1 / ((1 + discounting_e)^max(Age - age_vac, 0)),
      dis_cost = 1 / ((1 + discounting_c)^max(Age - age_vac, 0)),
      QL_y2_d = QL_y2 / (1 + discounting_e),
      QL_HZ = QL_y1 + QL_y2,
      QL_HZ_d = QL_y1 + QL_y2_d,
      Q_Life = N_Alive * QOL,
      Q_HZ = - N_HZ_All * QL_HZ,
      Q_All = Q_Life + Q_HZ,
      Q_Life_d = Q_Life * dis_ql,
      Q_HZ_d = - N_HZ_All * QL_HZ_d * dis_ql,
      Q_All_d = Q_Life_d + Q_HZ_d,
      C_Hosp = N_HZ_Hosp * cost_Hospitalisation_pp_inf,
      C_GP_NonPHN = (N_HZ_GP - N_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf ,
      C_GP_PHN = N_PHN_GP * cost_GP_pp_PHN_inf,
      C_GP = C_GP_NonPHN + C_GP_PHN,
      C_Vac = cost_vac_pp * N_Vac,
      C_All = C_Hosp + C_GP + C_Vac,
      C_Hosp_d = C_Hosp * dis_cost,
      C_GP_NonPHN_d = C_GP_NonPHN * dis_cost,
      C_GP_PHN_d = C_GP_PHN * dis_cost,
      C_GP_d = C_GP * dis_cost,
      C_All_d = C_Hosp_d + C_GP_d + C_Vac
    ) %>% 
    select(ID, Age, starts_with(c("N_", "Q_", "C_")))
}


sim_cases <- function(sims0, n0, age_vac, cost_vac_pp, discounting_e, discounting_c) {
  sims_soc <- sims0 %>% 
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
    calc_ce(n0, age_vac, cost_vac_pp, discounting_e, discounting_c) %>% 
    summarise_all(sum) %>% 
    select(-Age) %>% 
    ungroup()
  
  sims_alt <- sims0 %>% 
    group_by(ID) %>% 
    mutate(
      N_Vac = ifelse(Age == vaccination_age, cohort_size, 0),
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
    calc_ce(n0, age_vac, cost_vac_pp, discounting_e, discounting_c) %>% 
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
  
  return(sims)
}

