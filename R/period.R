

projection <- function(pars, year0 = 2013, year1 = 2050) {
  
}


pars <- tar_read(pars_proj)


p_uptake <- pars$pars_proj_c334c586f7cb2b6d_Uptake
p_demo <- pars$pars_proj_c334c586f7cb2b6d_Demography


p_uptake


n0 <- tibble(Year = 2013, Age = 50:100, Vaccine = "None", Prop = 1)



strategy_null <- function(df, p_uptake, year) {
  require(tidyverse)
  return(df %>% mutate(eli = NA, p_uptake = 0))
}


strategy_zvl <- function(df, p_uptake, year) {
  require(tidyverse)
  p_ini <- p_uptake$p_initial
  p_cat <- p_uptake$p_catchup
  
  df <- df %>% 
    mutate(
      eli = case_when(
        Vaccine != "None" ~ NA,
        Age < 70 ~ NA,
        Age < 80 ~ "ZVL",
        T ~ NA
      ),
      p_uptake = case_when(
        is.na(eli) ~ 0,
        Age == 70 ~ p_ini,
        T ~ p_cat
      )
    )
  
  return(df)
}


strategy_changeonly <- function(df, p_uptake, year) {
  require(tidyverse)
  p_ini <- p_uptake$p_initial
  p_cat <- p_uptake$p_catchup
  
  df <- df %>% 
    mutate(
      eli = case_when(
        Vaccine != "None" ~ NA,
        Age < 70 ~ NA,
        Age < 80 ~ ifelse(yr < 2023, "ZVL", "RZV_2d"),
        T ~ NA
      ),
      p_uptake = case_when(
        is.na(eli) ~ 0,
        Age == 70 ~ p_ini,
        T ~ p_cat
      )
    )
  
  return(df)
}


strategy_scheduled <- function(df, p_uptake, year) {
  require(tidyverse)
  p_ini <- p_uptake$p_initial
  p_cat <- p_uptake$p_catchup
  
  if (year < 2023) {
    df <- df %>% 
      mutate(
        eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 70 ~ NA,
          Age < 80 ~ "ZVL",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age == 70 ~ p_ini,
          T ~ p_cat
        )
      )
  } else if (year < 2028) {
    df <- df %>% 
      mutate(
        eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 65 ~ NA,
          Age >= 80 ~ NA,
          Age >= 70 ~ "RZV_2d",
          Age < (65 + year - 2023) ~ "RZV_2d",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age %in% c(65, 70) ~ p_ini,
          T ~ p_cat
        )
      )
  } else if (year < 2033) {
    df <- df %>% 
      mutate(
        eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age >= 80 ~ NA,
          Age >= 65 ~ "RZV_2d",
          Age < (60 + year - 2028) ~ "RZV_2d",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age %in% c(60, 65) ~ p_ini,
          T ~ p_cat
        )
      )
  } else {
    df <- df %>% 
      mutate(
        eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV_2d",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age == 60 ~ p_ini,
          T ~ p_cat
        )
      )
  }
}


strategy_scheduled65 <- function(df, p_uptake, year) strategy_scheduled(df, p_uptake, min(year, 2027))


strategy_scheduled_o1d <- function(df, p_uptake, year, year0 = 2024, cap_age = 85) {
  require(tidyverse)
  p_ini <- p_uptake$p_initial
  p_cat <- p_uptake$p_catchup
  
  
  sch <- strategy_scheduled(df, p_uptake, year)
  
  if (year >= year0) { 
    sch <- sch %>% 
      mutate(
        eli = case_when(
          !is.na(eli) ~ eli,
          Age < 80 ~ NA,
          Age >= cap_age ~ NA,
          Vaccine == "None" ~ "RZV_1d",
          T ~ "ReRZV_1d"
        ),
        p_uptake = case_when(
          p_uptake > 0 ~ p_uptake,
          is.na(eli) ~ 0,
          Age == 80 ~ p_ini,
          T ~ p_cat
        )
      )
  }
  sch
}


strategy_scheduled_o2d <- function(df, p_uptake, year, year0 = 2024, cap_age = 85) {
  require(tidyverse)
  p_ini <- p_uptake$p_initial
  p_cat <- p_uptake$p_catchup
  
  
  sch <- strategy_scheduled(df, p_uptake, year)
  
  if (year >= year0) { 
    sch <- sch %>% 
      mutate(
        eli = case_when(
          !is.na(eli) ~ eli,
          Age < 80 ~ NA,
          Age >= cap_age ~ NA,
          Vaccine == "None" ~ "RZV_2d",
          T ~ "ReRZV_2d"
        ),
        p_uptake = case_when(
          p_uptake > 0 ~ p_uptake,
          is.na(eli) ~ 0,
          Age == 80 ~ p_ini,
          T ~ p_cat
        )
      )
  }
  sch
}


strategy_scheduled_1d85 <- function(df, p_uptake, year) strategy_scheduled_o1d(df, p_uptake, year, year0 = 2024, cap_age = 85) 
strategy_scheduled_1d <- function(df, p_uptake, year) strategy_scheduled_o1d(df, p_uptake, year, year0 = 2024, cap_age = 101) 

strategy_scheduled_2d85 <- function(df, p_uptake, year) strategy_scheduled_o2d(df, p_uptake, year, year0 = 2024, cap_age = 85) 
strategy_scheduled_2d <- function(df, p_uptake, year) strategy_scheduled_o2d(df, p_uptake, year, year0 = 2024, cap_age = 101) 








up <- lapply(2013:2050, \(year) {
  crossing(Age = 50:100, Vaccine = c("None", "ZVL", "RZV_2d"))  %>% 
    strategy_scheduled(p_uptake, year) %>% 
    mutate(Year = year)
}) %>% 
  bind_rows()

up %>% 
  filter(Age >= 60) %>% 
  filter(Year == 2034) %>% 
  filter(!is.na(eli))



up <- lapply(2013:2050, \(year) {
  crossing(Age = 50:100, Vaccine = c("None", "ZVL", "RZV_2d"))  %>% 
    strategy_scheduled_1d(p_uptake, year) %>% 
    mutate(Year = year)
}) %>% 
  bind_rows()


up %>% 
  filter(Age >= 79) %>% 
  filter(Year == 2027) %>% 
  filter(!is.na(eli))





