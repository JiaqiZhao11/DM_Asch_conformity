install.packages("pacman")
pacman::p_load(hesim, extraDistr, R2jags, parallel, ggpubr,LaplacesDemon)

library(extraDistr)
library(R2jags)
library(LaplacesDemon) # required for categorical distribution
library(tidyverse)
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


library(readxl)
data_eye_contact <- read_csv("JiaqiZhao#1783/Asch social conformity/Data_eye_contact.csv")
View(Data_eye_contact)

#----------prepare data for jags models - want trial x subject arrays for choice and gain & loss ----
# identify and count unique subject IDs
subIDs <- unique(data_eye_contact$`Subject ID`)
nsubs <- length(subIDs)
ntrials_max <- 18

# all states (state) and responses (X)
state_raw <- data_eye_contact$state
X_raw <- data_eye_contact$X

#--- assign choices and outcomes in trial x sub matrix

#different number of trials across subjects. We'll need to fix this by padding arrays of < 100
#this is just so we can make the array
#then we'll also need to record number of valid trials for each sub, 
#then run the JAGS model on only valid trials

# empty arrays to fill
ntrials_all <- array(0,c(nsubs))
state_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))


P <- t(matrix(
  
  c( 1, 0, 0, 0, # absorbtion state
     
     alpha, (1-alpha-beta), beta,  0, #state 4 is not accessible
     
     0, gamma, (1-gamma-eta), eta, #state 1 is not accessible
     
     0, 0, 0, 1), # absorbtion state
  
  nrow=4, ncol=4))



for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(state_raw[data_eye_contact$`Subject ID`==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  state_sub <- state_raw[data_eye_contact$`Subject ID`==subIDs[s]] 
  length(state_sub) <- ntrials_max
  
  X_sub <- X_raw[data_eye_contact$`Subject ID`==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  state_all[s,] <- state_sub
  X_all[s,] <- X_sub
  
}


#----------testing our data curation by running JAGS on one subject

# Now we'll fit one subject just to make sure everything works



state <- state_all[1,]
X <- X_all[1,]

ntrials <- ntrials_all[1]


# set up jags and run jags model
data <- list("state","X","ntrials") 
params<-c("alpha","beta","gamma","eta")
temp_samples <- jags(data, inits=NULL, params,
                     model.file ="/work/JiaqiZhao#1783/Asch social conformity/con.txt",
                     n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)


for (i in 1:nsubs) {
  
  
  state <- state_all[i,]
  X <- X_all[i,]
  
  ntrials <- ntrials_all[i]
  
  
  # set up jags and run jags model
  data <- list("state","X","ntrials") 
  params<-c("alpha","beta","gamma","eta")
  temp_samples <- jags(data, inits=NULL, params,
                       model.file ="/work/JiaqiZhao#1783/Asch social conformity/con.txt",
                       n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)
  
  
  
  
  
  
  # find maximum a posteriori
  Y <- samples$BUGSoutput$sims.list
  estim_alpha[i] <- MPD(Y$alpha)
  estim_beta[i] <- MPD(Y$beta)
  estim_gamma[i] <- MPD(Y$gamma)
  estim_eta[i] <- MPD(Y$eta)
  
  print(i)
  print(estim_alpha[i])
  
  pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_%s.pdf", subIDs))
  #pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_trunc_%s_%s.pdf", ntrials, i))
  par(mfrow=c(2,2))
  plot(density(temp_samples$BUGSoutput$sims.list$alpha))
  plot(density(temp_samples$BUGSoutput$sims.list$beta))
  plot(density(temp_samples$BUGSoutput$sims.list$gamma))
  plot(density(temp_samples$BUGSoutput$sims.list$eta))
  title(paste("Density plots (for estimated alpha, beta, gamma & eta) of subject =", subIDs), line = -1, outer = TRUE)
  
  dev.off()
  
  
  
}

mean_alpha <- 



##PVL 
# set up jags and run jags model on one subject
data <- list("x","X","ntrials") 
params<-c("w","A","theta","a","p")
temp_samples <- jags.parallel(data, inits=NULL, params,
                              model.file ="216377/Module3/PVL.txt",
                              n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=4)

# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
plot(density(temp_samples$BUGSoutput$sims.list$w))
plot(density(temp_samples$BUGSoutput$sims.list$A))
plot(density(temp_samples$BUGSoutput$sims.list$theta))
plot(density(temp_samples$BUGSoutput$sims.list$a))

# Question: how would you expect the data to look on the basis of these posteriors?



###########################################################
#---------- run the hierarchical model on controls --------
###########################################################

state <- x_all
X <- X_all

ntrials <- ntrials_all

#P <- array(NA,c(nsubs,4,ntrials_max))
p <- array(NA,c(nsubs,4,ntrials_max))
theta <- array(NA,c(nsubs,ntrials_max))

# set up jags and run jags model
data <- list("state","X","ntrials","nsubs") 
params<-c("mu_alpha","mu_beta","mu_gamma","mu_eta","lambda_alpha","lambda_beta","lambda_gamma","lambda_eta")

samples <- jags.parallel(data, inits=NULL, params,
                         model.file ="/work/JiaqiZhao#1783/Asch social conformity/hier_con.txt",
                         n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=4)

# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
plot(density(samples$BUGSoutput$sims.list$mu_alpha))
plot(density(samples$BUGSoutput$sims.list$mu_beta))
plot(density(samples$BUGSoutput$sims.list$mu_gamma))
plot(density(samples$BUGSoutput$sims.list$mu_eta))

# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
plot(density(samples$BUGSoutput$sims.list$lambda_alpha))
plot(density(samples$BUGSoutput$sims.list$lambda_beta))
plot(density(samples$BUGSoutput$sims.list$lambda_gamma))
plot(density(samples$BUGSoutput$sims.list$lambda_eta))





















#------ create task environment -------------------

# simulation arrays to be filled in simulation loop

ntrials <- 100


p <- array(NA,c(4,ntrials))
theta <- array(NA,c(ntrials))

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
  
  print(i)
  
  pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_%s.pdf", i))
  
  par(mfrow=c(2,2))
  plot(density(temp_samples$BUGSoutput$sims.list$alpha))
  plot(density(temp_samples$BUGSoutput$sims.list$beta))
  plot(density(temp_samples$BUGSoutput$sims.list$gamma))
  plot(density(temp_samples$BUGSoutput$sims.list$eta))
  title(paste("Density plots (for recovered alpha, beta, gamma & eta) with ntrials =", ntrials), line = -1, outer = TRUE)
  
  dev.off()
  
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
