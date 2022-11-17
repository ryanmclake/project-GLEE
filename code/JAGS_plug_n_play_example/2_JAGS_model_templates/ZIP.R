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

model{

  #THIS IS THE ANNUAL LOOP
  for (k in 1:YR){


  for(i in 1:N){

    #this fits the blended model to your observed data.
    y[k,i] ~ dpois(m[k,i])

    #gap-filling model for covariate
    x[k,i]~dnorm(week_mean_x[i],tau_x)

    #This blends the poisson and zero inflation models
    m[k,i] <- mu[k,i]*b[k,i] + 1E-10 #adding the tiny value is important (i forget why...)

    #this is the bernoulli outcome of the zero inflation
    b[k,i] ~ dbern(theta[k,i])

    #theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
    logit(theta[k,i]) <- beta.bern[1] + beta.bern[2]*x[k,i]

  } # close model loop

  #THIS IS THE STATE-SPACE MODEL - must be 2:N because it's recursive
  for(i in 2:N){
    #mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
    #x is temperature
    log(mu[k,i]) <- beta.pois[1] + beta.pois[2]*log(mu[k,i-1]) + beta.pois[3]*x[k,i]

  } #close state-space model loop

    #setup your initial conditions
    mu[k,1] ~ dpois(1)

  } #close year loop.

  #setup your priors for beta. These are flat, uninformative priors.
  for(j in 1:N.pred.pois){
    beta.pois[j] ~ dnorm(0, 1E-3)
  } # close prior loop

  for(j in 1:N.pred.bern){
    beta.bern[j] ~ dnorm(0, 1E-3)
  } # close prior loop

  #setup your prior for missing covariate model
  tau_x ~ dgamma(0.01, 0.01)

} #close model loop.
