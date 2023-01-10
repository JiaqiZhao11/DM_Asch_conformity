install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr, LaplacesDemon)

library(extraDistr)
library(R2jags)
library(LaplacesDemon) # required for categorical distribution


###--
set.seed(1982)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#------ create task environment -------------------

# simulation arrays to be filled in simulation loop

# won't need this in jags model

#p <- array(NA,c(4,ntrials))

p[1,1] <- 0

p[1,2] <- 0

p[1,3] <- 0

p[1,4] <- 0


p[2,1] <- 0

p[2,2] <- 0

p[2,3] <- 0

p[2,4] <- 0


p[3,1] <- 0

p[3,2] <- 0

p[3,3] <- 0

p[3,4] <- 0


p[4,1] <- 0

p[4,2] <- 0

p[4,3] <- 0

p[4,4] <- 0

state <- array(NA,c(ntrials))

theta <- array(NA,c(ntrials))

X <- array(NA,c(ntrials))


#---set params

ntrials <- 18



# model parameters - for calculating transition probabilities

# from paper

#alpha <- .06

#beta <- .18

#gamma <- .27

#eta <- .04
alpha <- 0.25
beta <- 0.25
gamma <- 0.25
eta <- 0.25


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



state[1] <- 2 # set first trial

for (t in 2:ntrials) {
  
  
  
  p[,t] <- P[state[t-1],] # set p vector for rcat, depends in previous state
  
  state[t] <- rcat(1, p[,t]) #current state sampled from cat. dist.
  
  
  
  theta[t] <- ifelse(state[t]<3,1,0)
  
  
  
  X[t] <- rbinom(1,1,theta[t])
  
}



par(mfrow=c(1,2))

plot(state,type='l',ylim=c(1,4))

plot(X,type='l',ylim=c(0,1))


#-------test  ---------


# set up jags and run jags model
data <- list("state","X","ntrials") 
params<-c("alpha","beta","gamma","eta")
temp_samples <- jags(data, inits=NULL, params,
                     model.file ="/work/JiaqiZhao#1783/Asch social conformity/conf.txt",
                     n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)

recov_alpha <- temp_samples$BUGSoutput$sims.list$alpha
recov_beta <- temp_samples$BUGSoutput$sims.list$beta
recov_gamma <- temp_samples$BUGSoutput$sims.list$gamma
recov_eta <- temp_samples$BUGSoutput$sims.list$eta

par(mfrow=c(4,1))
plot(density(recov_alpha))
plot(density(recov_beta))
plot(density(recov_gamma))
plot(density(recov_eta))
title(paste("Density plots (for recovered a, b, c & d) with ntrials =", ntrials), line = -1, outer = TRUE)

###--------------Run full parameter recovery -------------
niterations <- 10 # fewer because it takes too long

true_alpha <- array(NA,c(niterations))
true_beta <- array(NA,c(niterations))
true_gamma <- array(NA,c(niterations))
true_eta <- array(NA,c(niterations))

infer_alpha <- array(NA,c(niterations))
infer_beta <- array(NA,c(niterations))
infer_gamma <- array(NA,c(niterations))
infer_eta <- array(NA,c(niterations))

start_time = Sys.time()

for (i in 1:niterations) {
  
  # let's see how robust the model is. Does it recover all sorts of values?
  #alpha <- runif(1,0,1)
  #beta <- runif(1,0,1)
  #gamma <- runif(1,0,1)
  #eta <- runif(1,0,1) 
  #alpha <- 0.25
  #beta <- 0.25
  #gamma <- 0.25
  #eta <- 0.25
  
  # Initialize a flag to control the loop
  flag <- TRUE
  
  # Start the loop
  while (flag) {
    # Sample alpha, beta, gamma, and eta from the beta distribution
    alpha <- rbeta(1, 1, 1)
    beta <- rbeta(1, 1, 1)
    gamma <- rbeta(1, 1, 1)
    eta <- rbeta(1, 1, 1)
    
    # Check that (1-alpha-beta) >= 0 and (1-gamma-eta) >= 0
    if (1-alpha-beta < 0 || 1-gamma-eta < 0) {
      # Discard the samples and skip the rest of the loop
      alpha <- NA
      beta <- NA
      gamma <- NA
      eta <- NA
      next
    }
    
    # If the condition is met, set the flag to FALSE to stop the loop
    flag <- FALSE
  }
  
  for (t in 2:ntrials) {
    
    
    
    p[,t] <- P[state[t-1],] # set p vector for rcat, depends in previous state
    
    state[t] <- rcat(1, p[,t]) #current state sampled from cat. dist.
    
    
    
    theta[t] <- ifelse(state[t]<3,1,0)
    
    
    
    X[t] <- rbinom(1,1,theta[t])
    
  }
  
  # set up jags and run jags model
  data <- list("state","X","ntrials") 
  params<-c("alpha","beta","gamma","eta")
  samples <- jags(data, inits=NULL, params,
                  model.file ="/work/JiaqiZhao#1783/Asch social conformity/conf.txt",
                  n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)
  
  
  true_alpha[i] <- alpha
  true_beta[i] <- beta
  true_gamma[i] <- gamma
  true_eta[i] <- eta
  
  # find maximum a posteriori
  Y <- samples$BUGSoutput$sims.list
  infer_alpha[i] <- MPD(Y$alpha)
  infer_beta[i] <- MPD(Y$beta)
  infer_gamma[i] <- MPD(Y$gamma)
  infer_eta[i] <- MPD(Y$eta)
  
  print(i)
  
}

end_time = Sys.time()
end_time - start_time

# let's look at some scatter plots

par(mfrow=c(3,2))
plot(infer_alpha,infer_beta)
plot(infer_gamma,infer_eta)
plot(true_alpha,true_beta)
plot(true_gamma,true_eta)

par(mfrow=c(3,2))
plot(true_alpha,infer_alpha)
plot(true_beta,infer_beta)
plot(true_gamma,infer_gamma)
plot(true_eta,infer_eta)

# plotting along with leaner data trend line
source('216377/Module3/recov_plot.R')
pl1 <- recov_plot(true_alpha, infer_beta, c("true alpha", "infer alpha"), 'smoothed linear fit')
pl2 <- recov_plot(true_beta, infer_beta, c("true beta", "infer beta"), 'smoothed linear fit')
pl3 <- recov_plot(true_gamma, infer_gamma, c("true gamma", "infer gamma"), 'smoothed linear fit')
pl4 <- recov_plot(true_eta, infer_eta, c("true eta", "infer eta"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3, pl4)

