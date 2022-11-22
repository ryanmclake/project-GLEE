model= ("model.txt")
jagsscript = cat("
model {  
   
   #priors===================================================
   
   beta[1] ~ dnorm(-0.98574,0.5)
   beta[2] ~ dnorm(1.0075,0.5)
   beta[3] ~ dnorm(0.04928,0.5)
   sd.pro ~ dunif(0, 1000)
   
   #Informative priors on initial conditions based on first observation
   X[1] ~ dnorm(x_init, tau.obs[1])
   
   #end priors===============================================
   
   for(i in 1:N) {
      
      #process model=============================================
      
      tau.pro[i] <- 1/((sd.pro*area[i])*(sd.pro*area[i]))
      predX[i] <- 10^(mu2 + omega*log(Littoral_frac[i]) + phi * (Cum_radiance[i]/30.4))
      X[i] ~ dnorm(predX[i],tau.pro[i])
      
      #end of process model======================================
      
      #data model================================================
      
      Y[i] ~ dnorm(X[i], tau.obs[i]) # Observation variation
            #end of data model=========================================
   }
  }", file = model)



#* RUNJAGS FOR 2017 EBULLITION MODEL ----


jags.data.ar = list(x_init = g_res$bubble_correct_mgC_m2_d[1],
                    Y = g_res$bubble_correct_mgC_m2_d, 
                    tau.obs = 1/((g_res$sd)) ^ 2,
                    N = nrow(g_res), 
                    Littoral_frac = g_res$Littoral_frac,
                    Cum_radiance = g_res$Cum_radiance,
                    area = g_res$area_km2)

nchain = 3
chain_seeds <- c(200,800,1400)
init <- list()
for(i in 1:nchain){
  init[[i]] <- list(sd.pro = runif(1, 0.01, 2),
                    mu2 = runif(1, -20,0),
                    omega = runif(1, 0.3,1), 
                    phi = runif(1, -0.2, 0.3),
                    .RNG.name = "base::Wichmann-Hill",
                    .RNG.seed = chain_seeds[i])
}

j.model   <- jags.model(file = model.ar17,
                        data = jags.data.ar,
                        inits = init,
                        n.chains = 3)

eval_ebu  <- coda.samples(model = j.model,
                          variable.names = c("sd.pro", "mu2", "phi", "omega"),
                          n.iter = 200000, n.burnin = 20000, thin = 200)


