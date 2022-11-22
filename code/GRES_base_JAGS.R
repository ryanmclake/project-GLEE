#* TEMPERATURE SCALING MODEL ----

model = ("g_res_base.txt")
jagsscript = cat("
model {  
   mu ~ dnorm(0,1e-3)  # intercet
   beta ~ dnorm(0,1e-3) # cat temp paramter
   phi ~ dnorm(0,1e-3) 
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

jags.data.lm = list(Y= g_res$bubble_correct_mgC_m2_d,
                    N = nrow(g_res),
                    Littoral_frac = g_res$Littoral_frac, 
                    Cum_radiance = g_res$Cum_radiance)

jags.params.lm.eval = c("sd.pro", "mu", "beta", "phi")

j.lm.model   <- jags.model(file = model,
                           data = jags.data.lm,
                           n.chains = 3)

eval_temp  <- coda.samples(model = j.lm.model,
                           variable.names = jags.params.lm.eval,
                           n.iter = 200000, n.burnin = 20000, thin = 200)
plot(eval_temp)
print("TEMP SCALE MODEL DIAGNOSTICS")
print(gelman.diag(eval_temp))