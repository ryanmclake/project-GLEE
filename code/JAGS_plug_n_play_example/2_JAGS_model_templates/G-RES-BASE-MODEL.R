### 

model{  # data model 

  for(i in 1:N){ # is number of lakes for calibration something like 300

    #this fits the blended model to your observed data.
    y[i] ~ dnorm(x[i], tau_obs) # observations model

  } # close model loop

  #THIS IS THE STATE-SPACE MODEL --> Currently not recursive so it is 1:N, not 2:N
  for(i in 1:N){
    
    #x[i] is the linear combination of predictors and parameters for the component of the model
    
    x[i]~dnorm(ebullition[i],tau_proc)
    
    ebullition[i] <- 10^(beta[1] + beta[2]*log10(per_littoral_area[i]) + beta.pois[3] * (I[i]/30.4))

  } #close state-space model loop

#in order to define a gamma dist in jags, we need to specify the rate and shape of the distribution
# this is annoying, because it does not output a shape and rate. if we have mean + var() we can back calculate the shape and Rate

# how ever we use obseravtions to get tau_obs, we will calculate some SD and Var() arond obs... we see var() of X amount. MEL get twisted
# beacue once we calc that variace, the variance is the mean of your gamma distribution. HOw do we calculate the variance of the variance? 
# MEL happy to be a sounding board 

beta[1] ~ dnorm(-0.98574,0.5)
beta[2] ~ dnorm(1.0075,0.5)
beta[3] ~ dnorm(0.04928,0.5)
tau_obs ~ dgamma(0, 100)#find a justifiable value
tau_proc ~ dgamma(0, 1000)#find a justifiable value

} #close model loop.
