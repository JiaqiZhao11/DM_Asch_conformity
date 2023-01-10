model {
  
  mu_alpha ~ dbeta(1,1)T(0,.5)
  mu_beta ~ dbeta(1,1)T(0,.5)
  mu_gamma ~ dbeta(1,1)T(0,.5)
  mu_eta ~ dbeta(1,1)T(0,.5)
  
  lambda_alpha ~ dgamma(2.5/2,0.01/2)
  lambda_beta ~ dgamma(2.5/2,0.01/2)
  lambda_gamma ~ dgamma(2.5/2,0.01/2)
  lambda_eta ~ dgamma(2.5/2,0.01/2) 
  
  
  for (s in 1:nsubs){                                       #subject loop
    alpha[s] ~ dnorm(mu_alpha, lambda_alpha)T(0,.5)
    beta[s] ~ dnorm(mu_beta, lambda_beta)T(0,.5)
    gamma[s] ~ dnorm(mu_gamma, lambda_gamma)T(0,.5)
    eta[s] ~ dnorm(mu_eta, lambda_eta)T(0,.5)
    
    P[s,1,1] <- 1
    
    P[s,1,2] <- 0
    
    P[s,1,3] <- 0
    
    P[s,1,4] <- 0
    
    
    
    P[s,2,1] <- alpha
    
    P[s,2,2] <- 1-alpha-beta
    
    P[s,2,3] <- beta
    
    P[s,2,4] <- 0
    
    
    
    P[s,3,1] <- 0
    
    P[3,2] <- gamma
    
    P[3,3] <- 1-gamma-eta
    
    P[3,4] <- eta
    
    
    
    P[4,1] <- 0
    
    P[4,2] <- 0
    
    P[4,3] <- 0
    
    P[4,4] <- 1
    
    
    
    state[1] ~ dcat(c(.25,.25,.25,.25))
    
    
    
    
    for (t in 2:ntrials) {
      
      p[1:4,t] <- P[state[t-1],] # set p vector for rcat, depends in previous state
      
      state[t] ~ dcat(p[1:4,t]) #current state sampled from cat. dist.
      
      
      
      theta[t] <- ifelse(state[t]<3,0,1)
      
      
      X[t] ~ dbin(1,theta[t])
      
    }
    
    Ev[s,1,1] ~ dnorm(0,0.01)
    Ev[s,1,2] ~ dnorm(0,0.01)
    Ev[s,1,3] ~ dnorm(0,0.01)
    Ev[s,1,4] ~ dnorm(0,0.01)
    
    for (t in 2:ntrials[s]) {   # ensuring that we don't process NAs for those subjects that have less than 100 trials 
      
      for (d in 1:4) {
        u[s, t, d] <- ifelse(X[s, t-1] < 0, -w[s] * abs(X[s, t-1])^A[s], X[s,t-1]^A[s]) # implementing Prospect Theory (w and A)
        Ev_update[s, t, d] <- Ev[s, t-1, d] + (a[s] * (u[s, t, d] - Ev[s, t-1, d])) # value to update Ev by (based on delta rule (incl. learning rate))
        Ev[s,t,d] <- ifelse(x[s,t-1] == d, Ev_update[s,t,d], Ev[s, t-1, d]) # updating Ev for only the chosen deck
        exp_p[s, t, d] <- exp(theta[s]*Ev[s, t, d]) # first step of softmax
      }
      
      for (d in 1:4) {
        p[s, t, d] <- exp_p[s, t, d]/sum(exp_p[s, t, ]) # second step of softmax (convertin to probability space)
      }
      
      x[s,t] ~ dcat(p[s,t, ]) # the actual choice
    }
  }
}
  
  
  
  
  
}