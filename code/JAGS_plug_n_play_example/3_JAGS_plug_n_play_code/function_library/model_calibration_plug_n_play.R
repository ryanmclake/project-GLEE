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

#ZIP
  data.ZIP <- list(y=cal_data$y, YR = cal_data$YR, N=cal_data$N, x = cal_data$x, week_mean_x = cal_data$week_mean_x,
                   N.pred.pois = cal_data$N.pred.pois, N.pred.bern = cal_data$N.pred.bern)
  variable.names.ZIP <- c("beta.pois", "beta.bern","tau_x")
  variable.namesout.ZIP <- c("beta.pois", "beta.bern","tau_x","m")
  init.ZIP <- list(list(beta.pois = c(0.5,0.5,0.5),beta.bern = c(0.5,0.5),tau_x = 0.01), list(beta.pois = c(0,0,0),beta.bern = c(0,0), tau_x = 0.1), list(beta.pois = c(-0.5,-0.5,-0.5),beta.bern = c(-0.5,-0.5), tau_x = 1))
  params.ZIP <- c("beta.pois[1]","beta.pois[2]","beta.pois[3]", "beta.bern[1]","beta.bern[2]","tau_x")

  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  init = eval(parse(text = paste0('init.', model_name)))
  params = eval(parse(text = paste0('params.', model_name)))

  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params))
}





