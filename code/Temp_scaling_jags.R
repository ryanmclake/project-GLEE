
if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, MCMCvis, lubridate, tidybayes,
               ncdf4, reshape2, zoo, patchwork, hydroGOF, viridis,
               imputeTS, devtools, scales, forecast, coda, rjags, R2jags,gridExtra)

set.seed(332)

data <- read_csv("./data/observed/bastviken_data.csv") %>%
  select(eb_flux, temp) %>%
  na.omit(.) %>%
  filter(temp != "-") %>%
  mutate(temp = as.numeric(temp),
         sd = 0.1)

g_res_data <- g_res %>%
  select(bubble_correct_mgC_m2_d, effective_temp_ch4, sd) %>%
  rename(eb_flux = bubble_correct_mgC_m2_d,
         temp = effective_temp_ch4)

data_cal <- bind_rows(data, g_res_data) %>%
  sample_n(., 100) %>%
  mutate(temp = as.numeric(temp))

eb_cals <- c(data$eb_flux)

data_val <- bind_rows(data, g_res_data) %>%
  filter(!eb_flux %in% eb_cals)

model= ("model.txt")
jagsscript = cat("
model {  
   
   #priors===================================================
   
   theta ~ dunif(0,100000)
   phi ~ dunif(0,100000)
   sd.pro ~ dunif(0, 1000)
   
   #end priors===============================================
   
   for(i in 1:N) {
      
      #process model=============================================
      
      tau.pro[i] <- 1/((sd.pro)*(sd.pro))
      predX[i] <- theta * (phi^(temp[i]-20))
      X[i] ~ dnorm(predX[i],tau.pro[i])
      
      #end of process model======================================
      
      #data model================================================
      
      Y[i] ~ dnorm(X[i], tau.obs[i]) # Observation variation
            #end of data model=========================================
   }
  }", file = model)


jags.data = list(Y = data_cal$eb_flux, 
                 tau.obs = 1/((data_cal$sd)) ^ 2,
                 N = nrow(data_cal), 
                 temp = data_cal$temp)

nchain = 3
chain_seeds <- c(200,800,1400)
init <- list()
for(i in 1:nchain){
  init[[i]] <- list(sd.pro = runif(1, 0.01, 2),
                    phi = runif(1, 0.5,1.5),
                    theta = runif(1, 50, 150),
                    .RNG.name = "base::Wichmann-Hill",
                    .RNG.seed = chain_seeds[i])
}

j.model   <- jags.model(file = model,
                        data = jags.data,
                        inits = init,
                        n.chains = 3)

eval_ebu  <- coda.samples(model = j.model,
                          variable.names = c("sd.pro","phi","theta"),
                          n.iter = 200000, n.burnin = 20000, thin = 200)

plot(eval_ebu)
print(gelman.diag(eval_ebu))

jags.out.mcmc <- as.mcmc.list(eval_ebu)


parameter <- eval_ebu %>%
  spread_draws(sd.pro, phi, theta) %>%
  filter(.chain == 1) %>%
  rename(ensemble = .iteration) %>%
  ungroup() %>%
  select(sd.pro, phi, theta)


ebu_validation <- function(theta, phi, temp, Q){
  est = (theta * phi ^ (temp - 20)) + rnorm(nrow(data_val),0, sd = Q)
  return(est)
}

parms <- sample_n(parameter, nrow(data_val), replace=TRUE)

validation <- tibble(ebu_validation(temp = data_val$temp,
                             phi = parms$phi,
                             theta = parms$theta,
                             Q = parms$sd.pro)) 

compare <- bind_cols(data_val$eb_flux, validation$eb_flux_validate)







val_temps <- c(data_val$temp)
out <- list()

for(i in 1:length(data_val$temp)){
  val <- data_val %>% filter(temp == val_temps[i]) %>%
}
# initial condition uncertainty


