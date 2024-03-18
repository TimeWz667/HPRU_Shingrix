library(tidyverse)


theme_set(theme_bw())

load(here::here("data", "processed_vaccine", "coverage.rdata"))



d <- coverage %>% 
  mutate(
    Age = Age - 1,
    Year = as.numeric(Year),
    Cohort = Year - Age + 70
  ) 

g_cov_yr <- d %>% 
  ggplot() +
  geom_line(aes(x = Age, y = value, colour = as.character(Year))) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_colour_discrete("Calendar year") +
  scale_x_continuous("Age", breaks = seq(70, 80, 2)) +
  expand_limits(y = c(0, 1))



g_cov_cohort <- d %>% 
  filter(Cohort >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = value, colour = as.character(Cohort))) +
  scale_y_continuous("Coverage, %", labels = scales::percent) +
  scale_colour_discrete("Cohort (at 70yr)") +
  scale_x_continuous("Age", breaks = seq(70, 80, 2)) +
  expand_limits(y = c(0, 1))

g_cov_yr
g_cov_cohort

ggsave(g_cov_yr, filename = here::here("outputs", "figs", "data", "g_cov_yr.png"), width = 7, height = 5.5)
ggsave(g_cov_cohort, filename = here::here("outputs", "figs", "data", "g_cov_cohort.png"), width = 7, height = 5.5)
