con <- function(payoff,ntrials,w,A,a,theta) {

  ntrials <- 18
  
  
  
  # model parameters - for calculating transition probabilities between each states
  
  # from Cohen's paper
  
  #alpha <- .06 # 
  
  #beta <- .18
  
  #gamma <- .27
  
  #eta <- .04
  
  
  
  # simulation arrays to be filled in simulation loop
  
  # won't need this in jags model
  
  p <- array(NA,c(4,ntrials))
  
  state <- array(NA,c(ntrials))
  
  theta <- array(NA,c(ntrials))
  
  X <- array(NA,c(ntrials))
  
  
  
  #-----------
  
  # what should go into jags for priors for free parameters
  
  #alpha ~ dbeta(1,1)
  
  #beta ~ dbeta(1,1)
  
  #gamma ~ dbeta(1,1)
  
  #eta ~ dbeta(1,1)
  
  
  
  # transition probability matrix - equations from paper
  
  # this way of setting up the matrix won't work in jags
  
  # THIS IS NOT DOING ANYTHING IN THIS SCRIPT. JUST FOR UNDERSTANDING
  
  # SO YOU CAN SEE HOW THE MATRIX LOOKS
  
  # COPY THE ONE BELOW INTO JAGS

  P <- t(matrix(
    
    c( 1, 0, 0, 0, # absorbtion state
       
       alpha, (1-alpha-beta), beta,  0, #state 4 is not accessible
       
       0, gamma, (1-gamma-eta), eta, #state 1 is not accessible
       
       0, 0, 0, 1), # absorbtion state
    
    nrow=4, ncol=4))
  
  
  
  
  # simulation loop
  # we can assume that a "normal" agent always starts with stage 2,
  # but thinking about special cases, we can also assign a "flat" probability structure to the first stage
  
  # state[1] <- 2 # set first trial
  state[1] ~ dcat(c(.25,.25,.25,.25))
  
  #-----------
  
  
  for (t in 2:ntrials) {
    
    
    
    p[,t] <- P[state[t-1],] # set p vector for rcat, depends in previous state
    
    state[t] <- rcat(1, p[,t]) #current state sampled from cat. dist.
    
    
    
    theta[t] <- ifelse(state[t]<3,1,0) # probability of being on stage t
    
    
    
    X[t] <- rbinom(1,1,theta[t]) # agent's answer
    
    
    
  }
  
  
  
  result <- list(p=p,
                 state=state,
                 X=X)
  
  return(result)

}