library(tidyverse)
library(tidybayes)


load(file = here::here("out", "yss_back.rdata"))
load(file = here::here("out", "yss_soc.rdata"))


yss_back %>% 
  group_by(Year, Key, Scenario) %>% 
  summarise(
    IncR = sum(N_HZ) / sum(N)
  ) %>% 
  ggplot() +
  stat_lineribbon(aes(x = Year, y = IncR), .width = c(.99, .95, .8, .5), color = "#08519C") +
  scale_fill_brewer() +
  scale_y_continuous("Incidence, per 100,000", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)
  





