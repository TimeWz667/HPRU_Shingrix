library(tidyverse)



coverage <- read_csv(here::here("data", "Coverage.csv")) %>% 
  mutate(
    Cov_2018 = ifelse(Cov_2018 == "Not eligible", NA, Cov_2018),
    Cov_2018 = as.numeric(Cov_2018)
  ) %>% 
  pivot_longer(-Age, 
               names_pattern = "Cov_(\\d+)", names_to = "Year") %>% 
  mutate(value = value / 100) 


dat <- coverage %>% 
  mutate(
    VacT = Age - 71,
    Cohort = as.numeric(Year) - VacT
  ) %>% 
  filter(Cohort >= 2014) %>% 
  mutate(Cohort = as.character(Cohort))


pred1 <- local({
  fit <- lm(log(1 - value) ~ VacT, data = dat)
  
  co <- coef(fit)
  co <- unname(co)
  p_initial <- 1 - exp(co[1])
  p_catchup <- 1 - exp(co[2])
  
  pars <- list(
    p_initial = p_initial,
    p_catchup = p_catchup
  )
  
  ds <- as_tibble(expand.grid(
    Cohort = as.character(2014:2022),
    VacT = 0:9  
  ))
  
  pred <- predict(fit, ds, interval = "prediction")
  
  list(
    fit = fit,
    pars = pars,
    pred = ds %>% 
      mutate(
        Coverage = 1 - exp(pred[, 1]),
        Coverage_l = 1 - exp(pred[, 3]),
        Coverage_u = 1 - exp(pred[, 2]),
        Age = VacT + 71,
        Year = as.numeric(as.character(Cohort)) + VacT
      )
  )
})


pred2 <- local({
  fit <- lm(log(1 - value) ~ Cohort + VacT - 1, data = dat)
  
  co <- coef(fit)
  co <- unname(co)
  p_initial <- 1 - exp(co[-length(co)])
  names(p_initial) <- paste0("Cohort_", 2014:2022)
  p_catchup <- 1 - exp(co[length(co)])
  
  pars <- list(
    p_initial = p_initial,
    p_catchup = p_catchup
  )
  
  ds <- as_tibble(expand.grid(
    Cohort = as.character(2014:2022),
    VacT = 0:9  
  ))
  
  pred <- predict(fit, ds, interval = "prediction")
  
  list(
    fit = fit,
    pars = pars,
    pred = ds %>% 
      mutate(
        Coverage = 1 - exp(pred[, 1]),
        Coverage_l = 1 - exp(pred[, 3]),
        Coverage_u = 1 - exp(pred[, 2]),
        Age = VacT + 71,
        Year = as.numeric(as.character(Cohort)) + VacT
      )
  )
})

save(pred1, pred2, file = here::here("data", "fit_coverage.csv"))
