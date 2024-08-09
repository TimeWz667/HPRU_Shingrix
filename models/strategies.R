strategy_null <- function(df, pars, yr) {
  return(df %>% mutate(Eli = NA, p_uptake = 0))
}

strategy_zvl <- function(df, pars, yr) {
  require(tidyverse)
  
  df <- df %>% 
    mutate(
      Eli = case_when(
        Vaccine != "None" ~ NA,
        Age < 70 ~ NA,
        Age < 80 ~ "ZVL",
        T ~ NA
      ),
      p_uptake = case_when(
        is.na(Eli) ~ 0,
        Age == 70 ~ pars$Uptake$p_initial,
        T ~ pars$Uptake$p_catchup
      )
    )
  
  return(df)
}


strategy_changeonly <- function(df, pars, yr) {
  require(tidyverse)
  
  df <- df %>% 
    mutate(
      Eli = case_when(
        Vaccine != "None" ~ NA,
        Age < 70 ~ NA,
        Age < 80 ~ ifelse(yr < 2023, "ZVL", "RZV"),
        T ~ NA
      ),
      p_uptake = case_when(
        is.na(Eli) ~ 0,
        Age == 70 ~ pars$Uptake$p_initial,
        T ~ pars$Uptake$p_catchup
      )
    )
  
  return(df)
}


strategy_scheduled <- function(df, pars, yr) {
  require(tidyverse)
  
  if (yr < 2023) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 70 ~ NA,
          Age < 80 ~ "ZVL",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age == 70 ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else if (yr < 2028) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 65 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(65, 70) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else if (yr < 2033) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(60, 65) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age == 60 ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  }
}



strategy_scheduled65 <- function(df, pars, yr) strategy_scheduled(df, pars, min(yr, 2027))


strategy_scheduled_e85 <- function(df, pars, yr) {
  require(tidyverse)
  
  if (yr < 2023) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 70 ~ NA,
          Age < 80 ~ "ZVL",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age == 70 ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else if (yr < 2028) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Age >= 80 & Age < 85 ~ "ReRZV1",
          Vaccine != "None" ~ NA,
          Age < 65 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(65, 70) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else if (yr < 2033) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Age >= 80 & Age < 85 ~ "ReRZV1",
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(60, 65) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Age >= 80 & Age < 85 ~ "ReRZV1",
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age == 60 ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  }
}


strategy_scheduled_e85b80 <- function(df, pars, yr) {
  require(tidyverse)
  
  if (yr < 2023) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Vaccine != "None" ~ NA,
          Age < 70 ~ NA,
          Age < 80 ~ "ZVL",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age == 70 ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else if (yr < 2028) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Age >= 80 & Age < 85 ~ "ReRZV1",
          Vaccine != "None" ~ NA,
          Age < 65 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(65, 70, 80) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else if (yr < 2033) {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Age >= 80 & Age < 85 ~ "ReRZV1",
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(60, 65, 80) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  } else {
    df <- df %>% 
      mutate(
        Eli = case_when(
          Age >= 80 & Age < 85 ~ "ReRZV1",
          Vaccine != "None" ~ NA,
          Age < 60 ~ NA,
          Age < 80 ~ "RZV",
          T ~ NA
        ),
        p_uptake = case_when(
          is.na(Eli) ~ 0,
          Age %in% c(60, 80) ~ pars$Uptake$p_initial,
          T ~ pars$Uptake$p_catchup
        )
      )
  }
}



