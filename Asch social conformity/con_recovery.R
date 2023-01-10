install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr,LaplacesDemon)

library(extraDistr)
library(R2jags)
library(LaplacesDemon) # required for categorical distribution
#library(ggplot2)


###--
set.seed(1982)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

# More informative plotting - code-courtesy of Lasse - fanx!
recov_plot <- function(true, infer, plot_lab, plot_col) {
  
  # library(ggplot2)
  
  df <- data.frame(true, infer)
  
  pl <- ggplot(df, aes(x = true,
                       y = infer,
                       color = plot_col)) + #Setting aesthetics for plot
    geom_point() + #Giving points a color each
    geom_smooth(method = "lm", se = T, formula = "y ~ x") +
    theme_minimal() + #Setting theme
    xlab(plot_lab[1]) + #Setting x label
    ylab(plot_lab[2]) + #Setting y label
    labs(color = "") + #Setting legend title
    ggtitle(paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"))
  
  return(pl)
  
}


#------ create task environment -------------------

# simulation arrays to be filled in simulation loop

ntrials <- 100


p <- array(NA,c(4,ntrials))

state <- array(NA,c(ntrials))

theta <- array(NA,c(ntrials))

X <- array(NA,c(ntrials))


#---set params


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
#state[1] ~ dcat(c(.25,.25,.25,.25))



state[1] <- 2 # set first trial

for (t in 2:ntrials) {
  
  
  
  p[,t] <- P[state[t-1],] # set p vector for rcat, depends in previous state
  
  state[t] <- rcat(1, p[,t]) #current state sampled from cat. dist.
  
  
  
  theta[t] <- ifelse(state[t]<3,0,1)
  
  
  
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
                     model.file ="/work/JiaqiZhao#1783/Asch social conformity/con.txt",
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
title(paste("Density plots (for recovered alpha, beta, gamma & eta) with ntrials =", ntrials), line = -1, outer = TRUE)

###--------------Run full parameter recovery -------------
niterations <- 100 # fewer because it takes too long
ntrials <- 100 # 18

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
  alpha <- runif(1,0,.5)
  beta <- runif(1,0,.5)
  gamma <- runif(1,0,.5)
  eta <- runif(1,0,.5) 
  #alpha <- 0.25
  #beta <- 0.25
  #gamma <- 0.25
  #eta <- 0.25
  
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
  #state[1] ~ dcat(c(.25,.25,.25,.25))
  
  
  
  state[1] <- 2 # set first trial
  
  
  for (t in 2:ntrials) {
    
    
    
    p[,t] <- P[state[t-1],] # set p vector for rcat, depends in previous state
    
    state[t] <- rcat(1, p[,t]) #current state sampled from cat. dist.
    
    
    
    theta[t] <- ifelse(state[t]<3,0,1)
    
    
    
    X[t] <- rbinom(1,1,theta[t])
    
  }
  
  if (i %% 1 == 0) { # put in 20 to only plot for every 20 iterations
    pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_trunc_%s_%s.pdf", ntrials, i))
    par(mfrow=c(1,2))
    plot(state,type='l',ylim=c(1,4))
    plot(X,type='l',ylim=c(0,1))
    dev.off()
  }
  
  
  # set up jags and run jags model
  data <- list("state","X","ntrials") 
  params<-c("alpha","beta","gamma","eta")
  samples <- jags(data, inits=NULL, params,
                  model.file ="/work/JiaqiZhao#1783/Asch social conformity/con.txt",
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
  
  good_alpha <- which(true_alpha/infer_alpha > 0.9 & true_alpha/infer_alpha < 1.1)
  good_beta <- which(true_beta/infer_beta > 0.9 & true_beta/infer_beta < 1.1)
  good_gamma <- which(true_gamma/infer_gamma > 0.9 & true_gamma/infer_gamma < 1.1)
  good_eta <- which(true_eta/infer_eta > 0.9 & true_eta/infer_eta < 1.1)
  
  good_alpha[good_alpha %in% good_beta]
  good_alpha[good_alpha %in% good_gamma]
  good_alpha[good_alpha %in% good_eta]
  good_beta[good_beta %in% good_gamma]
  good_beta[good_beta %in% good_eta]
  good_gamma[good_gamma %in% good_eta]
  
  print(i)
  
  #pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_%s.pdf", i))
  
  par(mfrow=c(2,2))
  plot(density(temp_samples$BUGSoutput$sims.list$alpha))
  plot(density(temp_samples$BUGSoutput$sims.list$beta))
  plot(density(temp_samples$BUGSoutput$sims.list$gamma))
  plot(density(temp_samples$BUGSoutput$sims.list$eta))
  title(paste("Density plots (for recovered alpha, beta, gamma & eta) with ntrials =", ntrials), line = -1, outer = TRUE)
  
  #dev.off()
  
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
#source('/work/216377/Module3/recov_plot.R')
pl1 <- recov_plot(true_alpha, infer_alpha, c("true alpha", "infer alpha"), 'smoothed linear fit')
pl2 <- recov_plot(true_beta, infer_beta, c("true beta", "infer beta"), 'smoothed linear fit')
pl3 <- recov_plot(true_gamma, infer_gamma, c("true gamma", "infer gamma"), 'smoothed linear fit')
pl4 <- recov_plot(true_eta, infer_eta, c("true eta", "infer eta"), 'smoothed linear fit')
ggarrange(pl1, pl2, pl3, pl4)
