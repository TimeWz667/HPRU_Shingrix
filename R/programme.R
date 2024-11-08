
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
        Age < 80 ~ ifelse(year < 2023, "ZVL", "RZV_2d"),
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


strategy_scheduled_old <- function(df, p_uptake, year, year0 = 2024, vaccine_old = "1d", cap_age = 85) {
  require(tidyverse)
  p_ini <- p_uptake$p_initial
  p_cat <- p_uptake$p_catchup
  
  if (vaccine_old == "1d") {
    vo_pri <- "RZV_1d"
    vo_re <- "ReRZV_1d"
  } else if (vaccine_old == "2d") {
    vo_pri <- "RZV_2d"
    vo_re <- "ReRZV_2d"
  } else {
    vo_pri <- NA
    vo_re <- NA
  }
  
  
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
          Age >= cap_age ~ NA,
          (Age >= 80) & (Vaccine == "None") ~ vo_pri,
          (Age >= 80) & (Vaccine == "ZVL") ~ vo_re,
          Age < 65 ~ NA,
          Vaccine != "None" ~ NA,
          Age >= 70 ~ "RZV_2d",
          Age < (65 + year - 2023) ~ "RZV_2d",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age %in% c(65, 70, 80) ~ p_ini,
          T ~ p_cat
        )
      )
  } else if (year < 2033) {
    df <- df %>% 
      mutate(
        eli = case_when(
          Age >= cap_age ~ NA,
          (Age >= 80) & (Vaccine == "None") ~ vo_pri,
          (Age >= 80) & (Vaccine == "ZVL") ~ vo_re,
          Age < 60 ~ NA,         
          Vaccine != "None" ~ NA,
          Age >= 65 ~ "RZV_2d",
          Age < (60 + year - 2028) ~ "RZV_2d",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age %in% c(60, 65, 80) ~ p_ini,
          T ~ p_cat
        )
      )
  } else {
    df <- df %>% 
      mutate(
        eli = case_when(
          Age >= cap_age ~ NA,
          (Age >= 80) & (Vaccine == "None") ~ vo_pri,
          (Age >= 80) & (Vaccine == "ZVL") ~ vo_re,
          Age < 60 ~ NA,         
          Vaccine != "None" ~ NA,
          Age < 80 ~ "RZV_2d",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(eli) ~ 0,
          Age %in% c(60, 80) ~ p_ini,
          T ~ p_cat
        )
      )
  }
}


strategy_scheduled_1d85 <- function(df, p_uptake, year) strategy_scheduled_old(df, p_uptake, year, year0 = 2024, vaccine_old = "1d", cap_age = 85) 
strategy_scheduled_1d90 <- function(df, p_uptake, year) strategy_scheduled_old(df, p_uptake, year, year0 = 2024, vaccine_old = "1d", cap_age = 90) 
strategy_scheduled_1d95 <- function(df, p_uptake, year) strategy_scheduled_old(df, p_uptake, year, year0 = 2024, vaccine_old = "1d", cap_age = 95)

strategy_scheduled_2d85 <- function(df, p_uptake, year) strategy_scheduled_old(df, p_uptake, year, year0 = 2024, vaccine_old = "2d", cap_age = 85) 
strategy_scheduled_2d90 <- function(df, p_uptake, year) strategy_scheduled_old(df, p_uptake, year, year0 = 2024, vaccine_old = "2d", cap_age = 90) 
strategy_scheduled_2d95 <- function(df, p_uptake, year) strategy_scheduled_old(df, p_uptake, year, year0 = 2024, vaccine_old = "2d", cap_age = 95) 

