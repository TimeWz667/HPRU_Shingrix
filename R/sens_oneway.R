sens_ce <- function(yss_uv) { 
  require(tidyverse)
  
  fn_thres <- function(df) {
    df %>% 
      mutate(
        dC_Med_d = dC_Hosp_d + dC_GP_NonPHN_d + dC_GP_PHN_d,
        Thres20 = ((dQ_HZ_d + dQ_Life_d) * 2e4 - dC_Med_d) / dN_VacRZV_d,
        Thres30 = ((dQ_HZ_d + dQ_Life_d) * 3e4 - dC_Med_d) / dN_VacRZV_d,
      ) %>% 
      group_by(Age0) %>% 
      summarise(
        T20_50 = quantile(Thres20, 0.5),
        T30_90 = quantile(Thres30, 0.9),
        Thres = pmin(T20_50, T30_90)
      )
  }
  
  
  yss <- local({
    temp <- yss_uv %>% 
      pivot_longer(-c(Scenario, Age0, Arm, Key, N0, Year0), names_to = "Index")
    
    temp %>% 
      filter(Arm != "SOC") %>% 
      left_join(
        temp %>% 
          filter(Arm == "SOC") %>% 
          select(Scenario, Age0, Key, Index, value0 = value),
        relationship = "many-to-many"
      ) %>% 
      mutate(
        Index = paste0("d", Index),
        Diff = value - value0
      ) %>% 
      select(-value, -value0) %>% 
      pivot_wider(names_from = Index, values_from = "Diff") %>% 
      mutate(
        ICER = dC_All_d / dQ_All_d,
        Price0 = dC_VacRZV_d / dN_VacRZV_d,
        Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
        Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
      )
    
  }) %>% 
    filter(Arm == "RZV_2d")
  
  
  
  case0 <- yss %>% fn_thres
  
  
  case1 <- bind_rows(
    yss %>% mutate(
      dC_Hosp_d = dC_Hosp_d * (1 + 0.1)
    ) %>% fn_thres %>% mutate(Pars = "C_Hosp", Direction = +0.1),
    yss %>% mutate(
      dC_Hosp_d = dC_Hosp_d * (1 - 0.1)
    ) %>% fn_thres %>% mutate(Pars = "C_Hosp", Direction = -0.1),
    
    yss %>% mutate(
      dC_GP_NonPHN_d = dC_GP_NonPHN_d * (1 + 0.1)
    ) %>% fn_thres %>% mutate(Pars = "C_GP_NonPHN", Direction = +0.1),
    yss %>% mutate(
      dC_GP_NonPHN_d = dC_GP_NonPHN_d * (1 - 0.1)
    ) %>% fn_thres %>% mutate(Pars = "C_GP_NonPHN", Direction = -0.1),
    
    yss %>% mutate(
      dC_GP_PHN_d = dC_GP_PHN_d * (1 + 0.1)
    ) %>% fn_thres %>% mutate(Pars = "C_GP_PHN", Direction = +0.1),
    yss %>% mutate(
      dC_GP_PHN_d = dC_GP_PHN_d * (1 - 0.1)
    ) %>% fn_thres %>% mutate(Pars = "C_GP_PHN", Direction = -0.1),
    
    yss %>% mutate(
      dQ_HZ_d = dQ_HZ_d * (1 + 0.1)
    ) %>% fn_thres %>% mutate(Pars = "Q_HZ", Direction = +0.1),
    yss %>% mutate(
      dQ_HZ_d = dQ_HZ_d * (1 - 0.1)
    ) %>% fn_thres %>% mutate(Pars = "Q_HZ", Direction = -0.1),
    
    yss %>% mutate(
      dQ_Life_d = dQ_Life_d * (1 + 0.1)
    ) %>% fn_thres %>% mutate(Pars = "Q_Life", Direction = +0.1),
    yss %>% mutate(
      dQ_Life_d = dQ_Life_d * (1 - 0.1)
    ) %>% fn_thres %>% mutate(Pars = "Q_Life", Direction = -0.1)
  )
  
  
  lvs <- case1 %>% 
    filter(Age0 == 80) %>% 
    group_by(Pars) %>% 
    summarise(R = diff(range(Thres))) %>% 
    arrange(R) %>% 
    pull(Pars)
  
  sens_ce <- case1 %>% 
    select(Pars, Direction, Thres, Age0) %>% 
    left_join(case0 %>% select(Age0, Thres0 = Thres)) 

  return(sens_ce)
    
}


summarise_sens_ce <- function(sens_ce, prefix = "", folder = NA, ext = ".pdf") {
  require(tidyverse)
  
  if (!is.na(folder)) {
    root_tab <- here::here("docs", "tabs", folder)
    root_fig <- here::here("docs", "figs", folder)
    dir.create(root_tab, showWarnings = F)
    dir.create(root_fig, showWarnings = F)
  } else {
    root_tab <- here::here("docs", "tabs")
    root_fig <- here::here("docs", "figs")
  }
  
  if (prefix != "") {
    prefix_fig <- glue::as_glue("g_") + prefix + "_"
    prefix_tab <- glue::as_glue(prefix) + "_"
  } else {
    prefix_fig <- glue::as_glue("g_")
    prefix_tab <- glue::as_glue("")
  }

  write_csv(sens_ce, here::here(root_tab, prefix_tab + "sens_ce1d.csv"))
  
  
  labs_lvs <- c(
    "C_GP_PHN" = "Cost, GP, PHN",
    "C_GP_NonPHN" = "Cost, GP, No PHN",
    "Q_Life" = "QALY, Survival",
    "C_Hosp" = "Cost, Hospitalisation",     
    "Q_HZ" = "QALY, HZ"
  )
  
  g_sens_ce <- sens_ce %>%
    filter(Age0 %in% c(75, 80, 85)) %>% 
    mutate(
      Pars = factor(Pars, names(labs_lvs)),
      i = as.numeric(Pars),
      D = ifelse(Direction > 0, "+10%", "-10%"),
      a0 = paste0("Age of vaccination: ", Age0)
    ) %>% 
    ggplot() +
    geom_rect(aes(xmin = Thres0, xmax = Thres, ymin = i - 0.45, ymax = i + 0.45, 
                  y = Pars, fill = D)) +
    scale_fill_discrete("Changes") +
    scale_x_continuous("Threshold price, GBP per administration") +
    scale_y_discrete("Components", labels = labs_lvs) +
    facet_wrap(a0~.)
  
  ggsave(g_sens_ce, filename = here::here(root_fig, prefix_fig + "sens_ce1d" + ext), width = 8, height = 4)
  
  
  
}
