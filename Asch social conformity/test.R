# arrays to populate for simulation
sub_ans <- array(NA,ntrials)
c_index <- array(NA,ntrials)
p1 <- array(NA,ntrials)
p2 <- array(NA,ntrials)
p3 <- array(NA,ntrials)
p4 <- array(NA,ntrials)
# free parameters - turn back on when constructing
#w <- 2
#A <- .5
#theta <- 3
#a <- .1
#--- plot prospect theory function
#x <- seq(1,100,1)
#y <- x^A
#plot(x,y)
# assigning a random probability for the four stages)
p1[1] <- .25 
p2[1] <- .25
p3[1] <- .25 
p4[1] <- .25



a <- .03 # 
b <- .38 # 
c <- .07 # 
d <- .41 # 

for (t in 2:ntrials) {
  
  p1[t] <- p1[t-1] + (a * p2[t-1])
  
  p2[t] <- ((1-a-b) * p2[t-1]) + (c * p3[t-1])
  
  p3[t] <- (b * p2[t-1]) + ((1-c-d) * p3[t-1])
  
  p4[t] <- (d * p3[t-1]) + p4[t-1]

  
  
  
}

for (t in 1:ntrials) {
  
  
  sub_ans [t] <- ifelse(((p1[t]+p2[t]) >= (p3[t]+p4[t])),0,1)
  
  c_index [t] <- ifelse(conf_ans [t]==1, sub_ans[t], - sub_ans[t])
  
  
}
