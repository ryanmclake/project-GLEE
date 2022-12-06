
SE <- function(x) sd(x) / sqrt(length(x)) 

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, MCMCvis, lubridate, tidybayes,
               ncdf4, reshape2, zoo, patchwork, hydroGOF, viridis,
               imputeTS, devtools, scales, forecast, coda, rjags, R2jags,gridExtra)

model= ("model.txt")
jagsscript = cat("
model {  
   
   #priors===================================================
   
   theta ~ dunif(0,100000)
   phi ~ dunif(0,1000)
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


jags.data = list(Y = nasa_ebu$eb_flux, 
                 tau.obs = 1/((nasa_ebu$sd)) ^ 2,
                 N = nrow(nasa_ebu), 
                 temp = nasa_ebu$temp)

nchain = 3
chain_seeds <- c(200,800,1400)
init <- list()
for(i in 1:nchain){
  init[[i]] <- list(sd.pro = runif(1, 0.01, 2),
                    phi = runif(1, 1.2,1.4),
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

parameter <- eval_ebu %>%
  spread_draws(sd.pro, phi, theta) %>%
  filter(.chain == 1) %>%
  rename(ensemble = .iteration) %>%
  ungroup() %>%
  select(sd.pro, phi, theta)


ebu_validation <- function(theta, phi, temp, Q){
  est = (theta * 1.3 ^ (temp - 20)) + rnorm(10000,0, sd = Q)
  return(est)
}

parms <- sample_n(parameter, 10000, replace=TRUE)

out <- list()

for(s in 1:length(nasa_ebu$temp)){

  validation <- ebu_validation(temp = nasa_ebu$temp[s],
                                    phi = parms$phi,
                                    theta = parms$theta,
                                    Q = parms$sd.pro)
  out[[s]] <- validation
}

model_validate = as.data.frame(do.call(rbind, out)) %>%
  tibble::rownames_to_column(., "row_names") %>%
  pivot_longer(!row_names, names_to = "iteration", values_to = "value") %>%
  group_by(row_names) %>%
  summarize(mean_eb_flux = mean(value),
            sd_eb_flux = SE(value)) %>%
  arrange(row_names) %>%
  left_join(., nasa_ebu, by = "row_names")

mean <- as.data.frame(rowMeans(model_validate))

bind_cols(data_val$eb_flux, mean$`rowMeans(model_validate)`) %>%
  summarize(rmse = rmse(`...1`, `...2`))


