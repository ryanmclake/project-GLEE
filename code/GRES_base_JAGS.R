#* G RES BASE  MODEL ----

model = ("g_res_base.txt")
jagsscript = cat("
model {  
   mu ~ dnorm(0,100)  # intercet
   beta ~ dnorm(0,100) # cat temp paramter
   phi ~ dnorm(0,100) 
   sd.pro ~ dunif(0, 1000)
   tau.pro <-  pow(sd.pro, -2)
   
   for(i in 1:N) {
      predX[i] <- 10^(mu + beta*log(Littoral_frac[i]) + phi * (Cum_radiance[i]/30.4))
      Y[i] ~ dnorm(predX[i],tau.pro) # model error
   }
}  
", 
                 file = model)


#* RUNJAGS FOR 2017 TEMP SCALE ----



nchain = 3
chain_seeds <- c(200,800,1400)
init <- list()
for(i in 1:nchain){
  init[[i]] <- list(sd.pro = runif(1, 0.5,1),
                    mu = runif(1,-1,-0.95),
                    beta = runif(1, 1, 1.01), 
                    phi = runif(1, 0.04, 0.06),
                    .RNG.name = "base::Wichmann-Hill",
                    .RNG.seed = chain_seeds[i])
}

jags.data.lm = list(Y= g_res_ebu$bubble_correct_mgC_m2_d,
                    N = nrow(g_res_ebu),
                    Littoral_frac = g_res_ebu$Littoral_frac, 
                    Cum_radiance = g_res_ebu$Cum_radiance)

jags.params.lm.eval = c("sd.pro", "mu", "beta", "phi")

j.lm.model   <- jags.model(file = model,
                           data = jags.data.lm,
                           n.chains = 3)

eval_gres  <- coda.samples(model = j.lm.model,
                           variable.names = jags.params.lm.eval,
                           n.iter = 200000, n.burnin = 20000, thin = 200)
plot(eval_gres)
print("TEMP SCALE MODEL DIAGNOSTICS")
print(gelman.diag(eval_gres))


parameter <- eval_gres %>%
  spread_draws(sd.pro, mu, beta, phi) %>%
  filter(.chain == 1) %>%
  rename(ensemble = .iteration) %>%
  ungroup() %>%
  select(sd.pro, mu, beta, phi)


g_res_validation <- function(sd.pro, mu, beta, phi, Cum_radiance, Littoral_frac, Q){
  
  estimate = (10^(mu + beta*log(Littoral_frac) + phi * (Cum_radiance/30.4))) + rnorm(10000,0, sd = Q)

  return(estimate)
}

parms <- sample_n(parameter, 10000, replace=TRUE)

out <- list()

for(s in 1:length(g_res_ebu$effective_temp_ch4)){
  
  validation <- g_res_validation(Littoral_frac = g_res_ebu$Littoral_frac[s],
                               Cum_radiance = g_res_ebu$Cum_radiance[s],
                               mu = parms$mu,
                               beta = parms$beta,
                               phi = parms$phi,
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
  left_join(., g_res_ebu, by = "row_names")

mean <- as.data.frame(rowMeans(model_validate))

bind_cols(data_val$eb_flux, mean$`rowMeans(model_validate)`) %>%
  summarize(rmse = rmse(`...1`, `...2`))

