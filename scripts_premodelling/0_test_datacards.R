library(knitr)
library(tidyverse)


env <- list(
  title = "Test data",
  desc = c(
    "Source" = "Here",
    "Model" = "Model A",
    "Strata" = "Age, Sex"
  ),
  g_gof = ggplot(tibble(x = rnorm(1000)) %>% mutate(y = 0.5 * x + rnorm(1000))) +
    geom_point(aes(x = x, y = y))
)


rmarkdown::render(
  input = here::here("docs", "templates", "template_data.Rmd"),
  output_file = here::here("docs", "cards_data", "Test.html"),
  envir = env
)

