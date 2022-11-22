# Title: Plug and play scripts to set conditions for model calibration runs
# History:
# created JAZ 2019-02-15
# WB Updates
# MEL updates for seasonal for-loop 30JUL19
# MEL updates for final publication 27MAR20

jags_plug_ins <- function(model_name, cal_data){

#JAGS Plug-ins: Add each separate model here
#variable.names are variables you would like to plot for model convergence (e.g., excludes mu)
#variable.names.out are all variables you would like to monitor in the jags run
#init are a range of initial conditions for parameters in each of 3 chains

#GRESBASEMODEL
  data.GRESBASEMODEL <- list(y=cal_data$y, 
                             N=cal_data$N, 
                             Littoral_frac = cal_data$x[,1],
                             Cum_radiance = cal_data$x[,2],
                             x = cal_data$x, 
                             N.pred.beta = cal_data$N.pred.beta,
                             `ebullition[1]` = 4.46)
  variable.names.GRESBASEMODEL <- c("beta","tau_obs", "tau_proc")
  variable.namesout.GRESBASEMODEL <- c("beta","tau_obs", "tau_proc")
  init.GRESBASEMODEL <- list(beta = c(-0.98574,1.0075,0.04928),tau_obs = 0.01, tau_proc = 0.01)
  params.GRESBASEMODEL <- c("beta[1]","beta[2]","beta[3]","tau_obs", "tau_proc")

  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  init = eval(parse(text = paste0('init.', model_name)))
  params = eval(parse(text = paste0('params.', model_name)))

  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params))
}





