


# littoral_fraction <- function(Zmax, Zmean){
#   lit_frac_g_res <- (1-(1-3/(Zmax))^((Zmax)/(Zmean )- 1))*100
#   return(lit_frac_g_res)
# }

# Tg CO2 /yr  equivalent calculator (16.55*(16/12*34*365/1000))



# G-Res Models
ebullition_base_model <- function(per_littoral_area, I_cumu){
  10^(-0.98574 + 1.0075 * log10(per_littoral_area) + 0.04928 * ((I_cumu)/30.4))
}

ebullition_base_model <- Vectorize(ebullition_base_model)

g_res <- g_res %>%
  mutate(flux = ebullition_base_model(per_littoral_area = Littoral_frac, I_cumu = Cum_radiance))
#Where --> 
# Parameters
# B1 = -0.98574
# B2 = 1.0075
# B3 = 0.04928

# Inputs - covariates
# per_littoral_area = littoral fraction above 3 meters
# I_cumu = Cumulative global Hiorizontal radiance 










ebullition_g_res_sub_model <- function(per_littoral_area, I_mean, M_ice){
  10^(-1.310432 + 0.8515131 * log10(per_littoral_area) + 0.051977 * (I * M_ice))
}

#Where --> 
# Parameters
# B1 = -1.310432
# B2 = 0.8515131
# B3 = 0.051977
# process and observation

# Inputs - covariates
# I_mean = mean global Hiorizontal radiance 
# M_ice = Number of months with average temperatures < 0C


