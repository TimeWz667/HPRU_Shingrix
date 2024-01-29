library(tidyverse)
library(readxl)
library(GauPro)
library(tidybayes)


epi_zh <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 1)
epi_gp <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 2)
epi_phn <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 3)


epi <- epi %>% 
  select(Age, IC = Immunocompromised, X = `Num zoster events`, N = `Person- years`) %>% 
  left_join(epi_gp %>% select(Age, IC = Immunocompromised, X_gp = `Num zoster events`)) %>% 
  mutate(X_hosp = X - X_gp) %>% 
  filter(Age != "101+") %>% 
  mutate(
    Age = as.numeric(Age)
  )

epi %>% 
  ggplot() +
  geom_point(aes(x = Age, y = X_hosp / N)) +
  facet_grid(IC~.)



epi

gp <- with(epi %>% filter(IC == 1) %>% filter(Age < 90), {
  s <- gpkm(Age, X / N)
})

gp$plot()

gp <- with(epi %>% filter(IC == 1) %>% filter(Age < 90), {
  s <- gpkm(Age, X_hosp / N)
})
gp$plot()


gp <- with(epi %>% filter(IC == 0) %>% filter(Age < 90), {
  s <- gpkm(Age, X / N)
})

gp$plot()

gp <- with(epi %>% filter(IC == 0) %>% filter(Age < 90), {
  s <- gpkm(Age, X_hosp / N)
})
gp$plot()

gp <- with(epi %>% filter(IC == 0) %>% filter(Age < 90), {
  y <- X_gp / X
  y <- log(y / (1 - y))
  s <- gpkm(Age, y)
})
gp$plot()


gp$plot()



sims <- gp$sample(50:100, 300)

colnames(sims) <- paste0("Age", 50:100)

as_tibble(sims) %>% 
  mutate(
    ID = 1:n()
  ) %>% 
  pivot_longer(-ID) %>% 
  extract(name, "Age", "Age(\\d+)") %>% 
  mutate(
    Age = as.numeric(Age)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = value, group = ID), alpha = 0.05)


