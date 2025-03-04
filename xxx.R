library(tidyverse)

df <- read_csv("clean_data/clean_data_2.csv", show_col_types = FALSE) %>%
  mutate(
    suicidal         = as.factor(suicidal),
    sex              = as.factor(sex),
    period           = as.factor(period),
    date_of_birth    = as.Date(date_of_birth, format = "%B %d, %Y"),
    date_of_death    = as.Date(date_of_death, format = "%B %d, %Y"),
    country_of_birth = as.factor(country_of_birth)
  )


df %>% group_by(poet) %>%
  summarise(heterosexual = first(heterosexual)) %>%
  getElement("poet") %>% cat(sep=", ")


df %>% group_by(poet) %>%
  summarise(heterosexual = first(heterosexual)) %>%
  nrow()
