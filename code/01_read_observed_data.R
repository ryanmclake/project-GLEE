# Initial code for project-GLEE
# Author: Ryan McClure

SE <- function(x) sd(x) / sqrt(length(x)) 

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, MCMCvis, lubridate, tidybayes,
               ncdf4, reshape2, zoo, patchwork, hydroGOF, viridis,
               imputeTS, devtools, scales, forecast, coda, rjags, R2jags,gridExtra)

eb1 <- read_csv("./data/observed/bastviken_data.csv") %>%
  select(lat, lon, eb_flux, temp) %>%
  na.omit(.) %>%
  filter(temp != "-") %>%
  mutate(temp = as.numeric(temp),
         eb_flux = as.numeric(eb_flux),
         sd = 1.5)

eb2 <- read_csv("./data/observed/BAWLD_CH4_Aquatic.csv") %>%
  select(LAT, LONG, CH4.E.FLUX, TEMP) %>%
  filter(CH4.E.FLUX != "-") %>%
  filter(TEMP != "-") %>%
  rename(lat = LAT, lon = LONG, eb_flux = CH4.E.FLUX, temp = TEMP) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         eb_flux = as.numeric(eb_flux),
         temp = as.numeric(temp),
         sd = 1.5) %>%
  na.omit(.) %>%
  bind_rows(., eb1)

eb3 <- read_csv("./data/observed/ebullition_melak.csv") %>%
  mutate(sd = 1.5) %>%
  na.omit(.) %>%
  bind_rows(., eb2)

eb4 <- read_csv("./data/observed/data_for_ebu_scaling.csv") %>%
  mutate(sd = 1.5) %>%
  na.omit(.) %>%
  bind_rows(., eb3)

eb5 <- read_csv("./data/observed/chamber_flux_ameriflux.csv") %>%
  select(LOC_LATITUDE, LOC_LONGITUDE, CMB_FCH4_Ebullitive, CMB_TA) %>%
  rename(lat = LOC_LATITUDE, lon = LOC_LONGITUDE, eb_flux = CMB_FCH4_Ebullitive, temp = CMB_TA) %>%
  mutate(eb_flux = eb_flux/1000000/16.04*86400, 
         sd = 1.5) %>%
  bind_rows(., eb4) %>%
  tibble::rownames_to_column(., "row_names") %>%
  filter(eb_flux > 0) %>%
  filter(eb_flux < 1830)


max(eb5$eb_flux)


eb_high <- eb5 %>% filter(lat >= 65)
eb_mid1 <- eb5 %>% filter(lat < 65 & lat >= 23.5)
eb_mid2 <- eb5 %>% filter(lat < 23.5 & lat > -23.5)
eb_mid3 <- eb5 %>% filter(lat < -23.5 & lat > -65)
eb_low <- eb5 %>% filter(lat <= -65)



dif1 <- read_csv("./data/observed/bastviken_data.csv") %>%
  select(lat, lon, diff_flux, temp) %>%
  na.omit(.) %>%
  filter(temp != "-") %>%
  mutate(temp = as.numeric(temp),
         diff_flux = as.numeric(diff_flux),
         sd = 1.5) %>%
  na.omit(.)


dif2 <- read_csv("./data/observed/BAWLD_CH4_Aquatic.csv") %>%
  select(LAT, LONG, CH4.D.FLUX, TEMP) %>%
  filter(CH4.D.FLUX != "-") %>%
  filter(TEMP != "-") %>%
  rename(lat = LAT, lon = LONG, diff_flux = CH4.D.FLUX, temp = TEMP) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         diff_flux = as.numeric(diff_flux),
         temp = as.numeric(temp),
         sd = 1.5) %>%
  na.omit(.) %>%
  bind_rows(., dif1)


dif3 <- read_csv("./data/observed/rosentreter_all.csv") %>%
  select(lat, long, dfch4_mgCH4m2d, Tw_degc) %>%
  rename(lat = lat, lon = long, diff_flux = dfch4_mgCH4m2d, temp = Tw_degc) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         diff_flux = as.numeric(diff_flux),
         temp = as.numeric(temp),
         sd = 1.5) %>%
  na.omit(.) %>%
  bind_rows(., dif2)

dif4 <- read_csv("./data/observed/yvon-data.csv") %>%
  select(latitude, longitude, flux, temp) %>%
  rename(lat = latitude, lon = longitude, diff_flux = flux, temp = temp) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         diff_flux = as.numeric(diff_flux),
         temp = as.numeric(temp),
         sd = 1.5) %>%
  na.omit(.) %>%
  bind_rows(., dif3) %>%
  tibble::rownames_to_column(., "row_names") %>%
  filter(diff_flux < 1500)






# g_res_ebu <- read_csv("./data/observed/g_res_data.csv") %>%
#   filter(!is.na(bubble_correct_mgC_m2_d)) %>%
#   select(Longitude, Latitude, effective_temp_ch4, Littoral_frac, z_max, z_mean, Cum_radiance, bubble_correct_mgC_m2_d) %>%
#   na.omit(.) %>%
#   filter(bubble_correct_mgC_m2_d < 245) %>%
#   mutate(sd = 1.5, 
#          Cum_radiance = Cum_radiance) %>%
#   tibble::rownames_to_column(., "row_names")






