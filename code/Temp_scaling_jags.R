
SE <- function(x) sd(x) / sqrt(length(x)) 

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, MCMCvis, lubridate, tidybayes,
               ncdf4, reshape2, zoo, patchwork, hydroGOF, viridis,
               imputeTS, devtools, scales, forecast, coda, rjags, R2jags,gridExtra)

model= ("ebu_model.txt")
jagsscript = cat("
model {  
   
   #priors===================================================
   
   ebtwenty ~ dunif(0,1000)
   theta ~ dunif(0,100)
   sd.pro ~ dunif(0, 1000)
   
   #end priors===============================================
   
   for(i in 1:N) {
      
      #process model=============================================
      
      tau.pro[i] <- 1/((sd.pro)*(sd.pro))
      predX[i] <- ebtwenty * (theta^(temp[i]-20))
      X[i] ~ dnorm(predX[i],tau.pro[i])
      
      #end of process model======================================
      
      #data model================================================
      
      Y[i] ~ dnorm(X[i], tau.obs[i]) # Observation variation
            #end of data model=========================================
   }
  }", file = model)


set.seed(1)
cal <- eb_mid1 %>% sample_n(., 100)
cal_vals <- c(cal$row_names)
val <- eb_mid1 %>% filter(!row_names %in% cal_vals) %>%
  select(-row_names) %>%
  tibble::rownames_to_column(., "row_names") %>%
  mutate(row_names = as.numeric(row_names))


jags.data = list(Y = cal$eb_flux, 
                 tau.obs = 1/((cal$sd)) ^ 2,
                 N = nrow(cal), 
                 temp = cal$temp)

nchain = 3
chain_seeds <- c(200,800,1400)
init <- list()
for(i in 1:nchain){
  init[[i]] <- list(sd.pro = runif(1, 0.01, 2),
                    theta = runif(1, 1.2,1.4),
                    ebtwenty = runif(1, 50, 150),
                    .RNG.name = "base::Wichmann-Hill",
                    .RNG.seed = chain_seeds[i])
}

j.model   <- jags.model(file = model,
                        data = jags.data,
                        inits = init,
                        n.chains = 3)

eval_ebu  <- coda.samples(model = j.model,
                          variable.names = c("sd.pro","ebtwenty","theta"),
                          n.iter = 200000, n.burnin = 20000, thin = 200)

plot(eval_ebu)
print(gelman.diag(eval_ebu))

parameter <- eval_ebu %>%
  spread_draws(sd.pro, ebtwenty, theta) %>%
  filter(.chain == 1) %>%
  rename(ensemble = .iteration) %>%
  ungroup() %>%
  select(sd.pro, ebtwenty, theta)


ebu_validation <- function(theta, ebtwenty, temp, Q){
  est = (ebtwenty *theta ^ (temp - 20)) + rnorm(1000,0, sd = Q)
  return(est)
}

parms <- sample_n(parameter, 1000, replace=TRUE)

out <- list()

for(s in 1:length(val$temp)){

  validation <- ebu_validation(temp = val$temp[s],
                               ebtwenty = 100,
                               theta = 1.08,
                               Q = 0)
  out[[s]] <- validation
}

model_validate = as.data.frame(do.call(rbind, out)) %>%
  tibble::rownames_to_column(., "row_names") %>%
  pivot_longer(!row_names, names_to = "iteration", values_to = "value") %>%
  mutate(row_names = as.numeric(row_names)) %>%
  group_by(row_names) %>%
  summarize(mean_eb_flux = mean(value),
            sd_eb_flux = SE(value), 
            var_flux = var(value)) %>%
  arrange(row_names) %>%
  left_join(., val, by = "row_names") %>%
  summarize(rmse = rmse(mean_eb_flux, eb_flux))

plot(model_validate$mean_eb_flux, model_validate$eb_flux)

mean <- as.data.frame(rowMeans(model_validate))

bind_cols(data_val$eb_flux, mean$`rowMeans(model_validate)`) %>%
  summarize(rmse = rmse(`...1`, `...2`))

