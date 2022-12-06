# Initial code for project-GLEE
# Author: Ryan McClure

SE <- function(x) sd(x) / sqrt(length(x)) 

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, MCMCvis, lubridate, tidybayes,
               ncdf4, reshape2, zoo, patchwork, hydroGOF, viridis,
               imputeTS, devtools, scales, forecast, coda, rjags, R2jags,gridExtra)

nasa_ebu <- read_csv("./data/observed/bastviken_data.csv") %>%
  select(lat, lon, type, eb_flux, temp) %>%
  na.omit(.) %>%
  filter(temp != "-") %>%
  mutate(temp = as.numeric(temp),
         eb_flux = as.numeric(eb_flux),
         sd = 1.5) %>%
  tibble::rownames_to_column(., "row_names")

g_res_ebu <- read_csv("./data/observed/g_res_data.csv") %>%
  filter(!is.na(bubble_correct_mgC_m2_d)) %>%
  select(Longitude, Latitude, effective_temp_ch4, Littoral_frac, z_max, z_mean, Cum_radiance, bubble_correct_mgC_m2_d) %>%
  na.omit(.) %>%
  filter(bubble_correct_mgC_m2_d < 245) %>%
  mutate(sd = 1.5, 
         Cum_radiance = Cum_radiance) %>%
  tibble::rownames_to_column(., "row_names")




