# Title: Pull appropriate data files for model calibration runs
# History:
# created MEL 27MAR20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse,zoo,rstatix)

get_calibration_data <- function(model_name){

  # read in g-res_data
  g_res <- read_csv("./data/observed/g_res_data.csv") %>%
    filter(!is.na(bubble_correct_mgC_m2_d)) %>%
    select(area_km2, Longitude, Latitude, effective_temp_ch4, Littoral_frac, z_max, z_mean, Cum_radiance, bubble_correct_mgC_m2_d) %>%
    na.omit(.) %>%
    filter(bubble_correct_mgC_m2_d < 245) %>%
    mutate(sd = ifelse(bubble_correct_mgC_m2_d<100, 2.5, 0.1))
  
  # subset according to species
    y.obs <- g_res %>%
      select(bubble_correct_mgC_m2_d)
    y <- as.matrix(y.obs)
  
  #subset drivers according to model
  #THIS IS WHERE YOU ADD IN INFO FOR NEW MODELS
    #create a new if statement for your model name and write code
    #to pull the correct driver data
  if(model_name == "GRESBASEMODEL"){
    x.obs <- g_res %>%
      select(Littoral_frac, Cum_radiance)
    x <- as.matrix(x.obs)

    #number of betas
    N.pred.beta = 3
  }

  #set counters
  N = length((g_res$bubble_correct_mgC_m2_d))

  return(list(N = N, y = y, x = x, N.pred.beta = N.pred.beta))
}


