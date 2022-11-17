# A few notes on what this is doing:

# We are combining the probability of observing any ticks at all (0 or 1)
# with the probability of observing a certain number of ticks

# The probability of observing any ticks at all is modeled using a Bernoulli
# distribution

# The probability of observing any particular number of ticks is modeled using
# a Poisson distribution

# The combination of the two, or "blended model", is called a zero-inflated
# Poisson, or ZIP for short.

# Major credit for this model template to John Foster at BU, who helped some
# of my teammates on the GLEON Bayesian working group team develop this model
# as part of a forecasting short course in 2018.

model{  # data model 

  for(i in 1:N){ # is number of lakes for calibration something like 300

    #this fits the blended model to your observed data.
    y[i] ~ dnorm(mu[i], tau_obs) # observations model

    #gap-filling model for covariate
    #x[k,i]~dnorm(week_mean_x[i],tau_x)

    # #This blends the poisson and zero inflation models
    # m[k,i] <- mu[k,i]*b[k,i] + 1E-10 #adding the tiny value is important (i forget why...)
    # 
    # #this is the bernoulli outcome of the zero inflation
    # b[k,i] ~ dbern(theta[k,i])
    # 
    # #theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
    # logit(theta[k,i]) <- beta.bern[1] + beta.bern[2]*x[k,i]

  } # close model loop

  #THIS IS THE STATE-SPACE MODEL - must be 2:N because it's recursive
  for(i in 1:N){
    #mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
    #x is temperature
    mu[i]~dnorm(ebullition[i],tau_proc)
    ebullition[i] <- 10^(beta[1] + beta[2]*log10(per_littoral_area[i]) + beta.pois[3] * (I[i] * M_ice[i]))

  } #close state-space model loop

    # #setup your initial conditions
    # mu[k,1] ~ dpois(1)

  } #close year loop.

#in order to define a gamma dist in jags, we need to specify the rate and shape of the distribution
# this is annoying, because it does not output a shape and rate. if we have mean + var() we can back calculate the shape and Rate

# how ever we use obseravtions to get tau_obs, we will calculate some SD and Var() arond obs... we see var() of X amount. MEL get twisted
# beacue once we calc that variace, the variance is the mean of your gamma distribution. HOw do we calculate the variance of the variance? 
# MEL happy to be a sounding board 

beta[1] ~ dnorm(<previous param estimate>,<some variance>)
beta[2] ~ dnorm(<previous param estimate>,<some variance>)
beta[3] ~ dnorm(<previous param estimate>,<some variance>)
tau_obs ~ dgamma(#find a justifiable value
tau_proc ~ dgamma(#find a justifiable value

} #close model loop.
