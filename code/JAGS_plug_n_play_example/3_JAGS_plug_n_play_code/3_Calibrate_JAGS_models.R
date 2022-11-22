# Title: 3_Calibrate_JAGS_models
# History:
# State-space model - revised by S. LaDeau (11/2017) from the EcoForecast Activity
# by Michael Dietze, with reference "Ecological Forecasting", chapter 8
# updates JAZ, WB, MEL for Lofton et al. 2020

##################################SET-UP##############################################

#####IMPORTANT NOTES:

# 1. Right now this will only work with NEON core sites (KONZ, ORNL, SCBI,
# TALL), because we are using air temp data subset from the provided target
# dataset.

# 2. You need to set a plotting directory if you want plot output.

# 3. We are currently running on a weekly timestep but looks like most data is
# fortnightly? Something to think about. It makes the models really slow to run
# and more difficult to converge because there is a low proportion of data to
# NAs. You'll see that in the trace plots - not too many fuzzy caterpillars :-(
# Alternatively this may be because we are trying to constrain a fair number
# of parameters in this model (n = 6); a simpler model might converge better.

# 4. To add new models, you need to do the following:
#    A. add your new model script to the 2_JAGS_model_templates folder
#    B. add the relevant info for your model to two function scripts, found
#       in 3_JAGS_plug_n_play_code/function_library:
#         - model_calibration_get_data.R
#         - model_calibration_plug_n_play.R

# 5. We can easily nest this for-loop to also run through several different
# sites in addition to different models


#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, readxl, rjags, runjags, moments, coda, zoo)


#set a directory to use as a local file repository for plots if desire to write to file
#my_directory <- "C:/Users/Mary Lofton/Desktop/VTicks"
my_directory <- "~Desktop/EF 2021/VTicks"
write_plots <- TRUE

#make vector of model names for for-loop
my_models <- c("GRESBASEMODEL")
length(my_models)

########################CALIBRATE MODELS##############################################

#for (i in 1:length(my_models)){

#1) Source helper functions ---------------------------------------------------------
  source('/Users/ryanmcclure/Documents/project-GLEE/code/JAGS_plug_n_play_example/3_JAGS_plug_n_play_code/function_library/model_calibration_plug_n_play.R')
  source('/Users/ryanmcclure/Documents/project-GLEE/code/JAGS_plug_n_play_example/3_JAGS_plug_n_play_code/function_library/model_calibration_get_data.R')
  source('/Users/ryanmcclure/Documents/project-GLEE/code/JAGS_plug_n_play_example/3_JAGS_plug_n_play_code/function_library/model_calibration_plots.R')

#2) Model options => pick model -----------------------------------------------------

model_name = my_models[i] # options are found in 2_JAGS_model_templates
model=paste0("/Users/ryanmcclure/Documents/project-GLEE/code/JAGS_plug_n_play_example/2_JAGS_model_templates/",model_name, '.R') #Do not edit


#3) Read in data for model ------------------------------------------------------------------------------------------------------------

#see 3 for this function
cal_data <- get_calibration_data(model_name = model_name)


#4) JAGS Plug-Ins => initial conditions, priors, data, etc. --------------------------------------------------------------------------------------

#see 0_Function_library/model_calibration_plug_n_play.R for this function
jags_plug_ins <- jags_plug_ins(model_name = model_name, cal_data = cal_data)


#5) Run model (no edits, unless you want to change # of iterations) -------------------------------------------------------------
j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = jags_plug_ins$init.model,
                         n.chains = 3)

jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     adapt =  5000,
                     burnin =  10000,
                     sample = 50000,
                     n.chains = 3,
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#convert to an MCMC list to calculate cross-correlation later
jags.out.mcmc <- as.mcmc.list(jags.out)


#6) Save output for calibration assessment

#save predicted states
Nmc = 10000
out <- as.matrix(jags.out.mcmc)
srow <- sample.int(nrow(out),Nmc,replace=TRUE)
mus <- out[srow,grep("m",colnames(out))]
#write.csv(mus,file = file.path("./5_Model_output/5.1_Calibration",paste0(model_name,'_predicted_states.csv')),row.names = FALSE)

#plot parameters
plot_parameters(params = jags_plug_ins$params.model,
                write_plots = write_plots,
                my_directory = my_directory)

#calculate parameter summaries, effective sample size, and cross-correlations
sum <- summary(jags.out, vars = jags_plug_ins$variable.names.model)
crosscorr <- crosscorr(jags.out.mcmc[,c(jags_plug_ins$params.model)])

#save results
#sink(file = file.path("./5_Model_output/5.1_Calibration",paste0(model_name,'_param_summary.txt')))
print("Parameter summary")
print(sum)
print("Parameter cross-correlations")
print(crosscorr)
#sink()


#7) Save runjags output
# write.jagsfile(jags.out, file=file.path("./5_Model_output/5.1_Calibration",paste0(model_name,'_calibration.txt')),
#                remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

}

#Congratulations! You have run all the models. You may have a cookie.
