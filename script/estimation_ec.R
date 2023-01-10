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

library(readxl)
data_eye_contact <- read_csv("JiaqiZhao#1783/Asch social conformity/Data_eye_contact.csv")
# View(Data_eye_contact)

#----------prepare data for jags models - want trial x subject arrays for choice and gain & loss ----
# identify and count unique subject IDs

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


state <- state_all[4,]
X <- X_all[4,]

ntrials <- ntrials_all[4]

# set up jags and run jags model on one subject
data <- list("state","X","ntrials") 
params<-c("alpha","beta","gamma","eta")
temp_samples <- jags.parallel(data, inits=NULL, params,
                              model.file ="/work/JiaqiZhao#1783/Asch social conformity/con.txt",
                              n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=4)

# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
plot(density(temp_samples$BUGSoutput$sims.list$alpha))
plot(density(temp_samples$BUGSoutput$sims.list$beta))
plot(density(temp_samples$BUGSoutput$sims.list$gamma))
plot(density(temp_samples$BUGSoutput$sims.list$eta))
title(paste("Density plots (for estimated alpha, beta, gamma & eta) of subject =", subIDs[4]), line = -1, outer = TRUE)


Y <- temp_samples$BUGSoutput$sims.list
alpha <- MPD(Y$alpha)
beta <- MPD(Y$beta)
gamma <- MPD(Y$gamma)
eta <- MPD(Y$eta)


# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
plot(density(temp_samples$BUGSoutput$sims.list$alpha))
plot(density(temp_samples$BUGSoutput$sims.list$beta))
plot(density(temp_samples$BUGSoutput$sims.list$gamma))
plot(density(temp_samples$BUGSoutput$sims.list$eta))
title(paste("Density plots (for estimated alpha, beta, gamma & eta) of subject =", subIDs[4]), line = -1, outer = TRUE)


print(alpha)
print(beta)
print(gamma)
print(eta)

#---loop all subjects--

for (i in 1:nsubs) {
  
  
  state <- state_all[i,]
  X <- X_all[i,]
  
  ntrials <- ntrials_all[i]
  
  
  # set up jags and run jags model
  data <- list("state","X","ntrials") 
  params<-c("alpha","beta","gamma","eta")
  samples <- jags(data, inits=NULL, params,
                       model.file ="/work/JiaqiZhao#1783/Asch social conformity/con.txt",
                       n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)
  
  
  # find maximum a posteriori
  Y <- samples$BUGSoutput$sims.list
  alpha[i] <- MPD(Y$alpha)
  beta[i] <- MPD(Y$beta)
  gamma[i] <- MPD(Y$gamma)
  eta[i] <- MPD(Y$eta)
  
  
  pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_%s.pdf", subIDs))
  #pdf(file=sprintf("/work/JiaqiZhao#1783/Asch social conformity/plot_trunc_%s_%s.pdf", ntrials, i))
  par(mfrow=c(2,2))
  plot(density(temp_samples$BUGSoutput$sims.list$alpha))
  plot(density(temp_samples$BUGSoutput$sims.list$beta))
  plot(density(temp_samples$BUGSoutput$sims.list$gamma))
  plot(density(temp_samples$BUGSoutput$sims.list$eta))
  title(paste("Density plots (for estimated alpha, beta, gamma & eta) of subject =", subIDs[4]), line = -1, outer = TRUE)
  
  dev.off()
  
  print(i)
  
  print(samples$BUGSoutput$summary[,8])

}


print(mean(alpha))
print(sd(alpha))
print(mean(beta))
print(sd(beta))
print(mean(gamma))
print(sd(gamma))
print(mean(eta))
print(sd(eta))

