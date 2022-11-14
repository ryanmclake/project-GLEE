# Initial code for project-GLEE
# Author: Ryan McClure

# read in data
library(dplyr)

obs <- vroom::vroom("./data/observed/observed_ebullition.csv") %>%
  arrange(country) %>%
  mutate(area_ha = surface_area_km2 * 100) %>%
  filter(country %in% c("United States","Alaska")) %>%
  filter(area_ha >= 10) %>%
  readr::write_csv("./data/observed/united_states.csv")




