# Title: Pull appropriate data files for model calibration runs
# History:
# created MEL 27MAR20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse,zoo)

get_calibration_data <- function(model_name, site, plot, species, cal_years, epi_weeks){

  # read in tick data
  ticks <- read_csv("./0_Data_files/ticks-targets.csv.gz")

  # subset to site, plot, and correct time period
  ticks_calibration <- ticks %>%
    filter(siteID == site,
           plotID == plot,
           Year %in% cal_years,
           epiWeek %in% epi_weeks)

  # subset according to species
  if(species == "A. americanum"){
  y.obs <- ticks_calibration %>%
    select(Year, epiWeek, ambloyomma_americanum) %>%
    pivot_wider(names_from = epiWeek, values_from = ambloyomma_americanum) %>%
    select(-Year)
  y <- as.matrix(y.obs)
  }

  if(species == "I. scapularis"){
  y.obs <- ticks_calibration %>%
    select(Year, epiWeek, ixodes_scapularis) %>%
    pivot_wider(names_from = epiWeek, values_from = ixodes_scapularis)%>%
    select(-Year)
  y <- as.matrix(y.obs)
  }

  #subset drivers according to model
  #THIS IS WHERE YOU ADD IN INFO FOR NEW MODELS
    #create a new if statement for your model name and write code
    #to pull the correct driver data
  if(model_name == "ZIP"){
    x.obs <- ticks_calibration %>%
      select(Year, epiWeek, airTempMin_degC) %>%
      pivot_wider(names_from = epiWeek, values_from = airTempMin_degC) %>%
      select(-Year)
    x <- as.matrix(x.obs)

    week_mean_x_temp <- colMeans(x, na.rm = TRUE)
    week_mean_x <- zoo::na.approx(week_mean_x_temp)

    #number of betas
    N.pred.pois = 3
    N.pred.bern = 2
  }

  #set counters
  YR = length(cal_years)
  N = length(epi_weeks)

  return(list(YR = YR, N = N, y = y, x = x, week_mean_x = week_mean_x, N.pred.pois = N.pred.pois, N.pred.bern = N.pred.bern))

}


