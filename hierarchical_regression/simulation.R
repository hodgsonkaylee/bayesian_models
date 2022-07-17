library(R2jags)
library(MASS)
library(HotDeckImputation)
library(Amelia)

# different number of response variables
m5 <- 5
m10 <- 10
m30 <- 30

k2 <- 2

# different sample sizes
N50 <- 50
N160 <- 160
N300 <- 300

# Create different Bayesian models for different number of explanatory variables (k=2,10)
mdl.sim.m5k2 <- "
model {
for (i in 1:N){
y1[i] ~ dnorm(y.hat1[i],tau)
y.hat1[i] <- b1.0 + b1.1*x1[i] + b1.2*x2[i]
y2[i] ~ dnorm(y.hat2[i],tau)
y.hat2[i] <- b2.0 + b2.1*x1[i] + b2.2*x2[i]
y3[i] ~ dnorm(y.hat3[i],tau)
y.hat3[i] <- b3.0 + b3.1*x1[i] + b3.2*x2[i]
y4[i] ~ dnorm(y.hat4[i],tau)
y.hat4[i] <- b4.0 + b4.1*x1[i] + b4.2*x2[i]
y5[i] ~ dnorm(y.hat5[i],tau)
y.hat5[i] <- b5.0 + b5.1*x1[i] + b5.2*x2[i]
}
b1.0 ~ dnorm(0,1)
b1.1 ~ dnorm(mu[1],lambda[1])
b1.2 ~ dnorm(mu[2],lambda[2])
b2.0 ~ dnorm(0,1)
b2.1 ~ dnorm(mu[1],lambda[1])
b2.2 ~ dnorm(mu[2],lambda[2])
b3.0 ~ dnorm(0,1)
b3.1 ~ dnorm(mu[1],lambda[1])
b3.2 ~ dnorm(mu[2],lambda[2])
b4.0 ~ dnorm(0,1)
b4.1 ~ dnorm(mu[1],lambda[1])
b4.2 ~ dnorm(mu[2],lambda[2])
b5.0 ~ dnorm(0,1)
b5.1 ~ dnorm(mu[1],lambda[1])
b5.2 ~ dnorm(mu[2],lambda[2])
sigma2 ~ dunif(0,1)
tau <- 1/sigma2
for(j in 1:2){
s2[j] ~ dunif(0, 2)
lambda[j] <- 1 / (s2[j]) 
mu[j] ~ dnorm(0,1)
}
}
"
writeLines(mdl.sim.m5k2,'hiermodsim.m5k2.txt')
data.jags.m5k2 <- c('x1','x2','y1','y2','y3','y4','y5','N')
parms.jags.m5k2 <- c('mu','lambda','tau',
                   'b1.0','b1.1','b1.2',
                   'b2.0','b2.1','b2.2',
                   'b3.0','b3.1','b3.2',
                   'b4.0','b4.1','b4.2',
                   'b5.0','b5.1','b5.2')

mdl.sim.m10k2 <- "
model {
for (i in 1:N){
y1[i] ~ dnorm(y.hat1[i],tau)
y.hat1[i] <- b1.0 + b1.1*x1[i] + b1.2*x2[i]
y2[i] ~ dnorm(y.hat2[i],tau)
y.hat2[i] <- b2.0 + b2.1*x1[i] + b2.2*x2[i]
y3[i] ~ dnorm(y.hat3[i],tau)
y.hat3[i] <- b3.0 + b3.1*x1[i] + b3.2*x2[i]
y4[i] ~ dnorm(y.hat4[i],tau)
y.hat4[i] <- b4.0 + b4.1*x1[i] + b4.2*x2[i]
y5[i] ~ dnorm(y.hat5[i],tau)
y.hat5[i] <- b5.0 + b5.1*x1[i] + b5.2*x2[i]
y6[i] ~ dnorm(y.hat6[i],tau)
y.hat6[i] <- b6.0 + b6.1*x1[i] + b6.2*x2[i]
y7[i] ~ dnorm(y.hat7[i],tau)
y.hat7[i] <- b7.0 + b7.1*x1[i] + b7.2*x2[i]
y8[i] ~ dnorm(y.hat8[i],tau)
y.hat8[i] <- b8.0 + b8.1*x1[i] + b8.2*x2[i]
y9[i] ~ dnorm(y.hat9[i],tau)
y.hat9[i] <- b9.0 + b9.1*x1[i] + b9.2*x2[i]
y10[i] ~ dnorm(y.hat10[i],tau)
y.hat10[i] <- b10.0 + b10.1*x1[i] + b10.2*x2[i]
}
b1.0 ~ dnorm(0,1)
b1.1 ~ dnorm(mu[1],lambda[1])
b1.2 ~ dnorm(mu[2],lambda[2])
b2.0 ~ dnorm(0,1)
b2.1 ~ dnorm(mu[1],lambda[1])
b2.2 ~ dnorm(mu[2],lambda[2])
b3.0 ~ dnorm(0,1)
b3.1 ~ dnorm(mu[1],lambda[1])
b3.2 ~ dnorm(mu[2],lambda[2])
b4.0 ~ dnorm(0,1)
b4.1 ~ dnorm(mu[1],lambda[1])
b4.2 ~ dnorm(mu[2],lambda[2])
b5.0 ~ dnorm(0,1)
b5.1 ~ dnorm(mu[1],lambda[1])
b5.2 ~ dnorm(mu[2],lambda[2])
b6.0 ~ dnorm(0,1)
b6.1 ~ dnorm(mu[1],lambda[1])
b6.2 ~ dnorm(mu[2],lambda[2])
b7.0 ~ dnorm(0,1)
b7.1 ~ dnorm(mu[1],lambda[1])
b7.2 ~ dnorm(mu[2],lambda[2])
b8.0 ~ dnorm(0,1)
b8.1 ~ dnorm(mu[1],lambda[1])
b8.2 ~ dnorm(mu[2],lambda[2])
b9.0 ~ dnorm(0,1)
b9.1 ~ dnorm(mu[1],lambda[1])
b9.2 ~ dnorm(mu[2],lambda[2])
b10.0 ~ dnorm(0,1)
b10.1 ~ dnorm(mu[1],lambda[1])
b10.2 ~ dnorm(mu[2],lambda[2])
sigma2 ~ dunif(0,1)
tau <- 1/sigma2
for(j in 1:2){
s2[j] ~ dunif(0, 2)
lambda[j] <- 1 / (s2[j]) 
mu[j] ~ dnorm(0,1)
}
}
"
writeLines(mdl.sim.m10k2,'hiermodsim.m10k2.txt')
data.jags.m10k2 <- c('x1','x2','y1','y2','y3','y4','y5','y6','y7','y8','y9','y10','N')
parms.jags.m10k2 <- c('mu','lambda','tau',
                     'b1.0','b1.1','b1.2',
                     'b2.0','b2.1','b2.2',
                     'b3.0','b3.1','b3.2',
                     'b4.0','b4.1','b4.2',
                     'b5.0','b5.1','b5.2',
                     'b6.0','b6.1','b6.2',
                     'b7.0','b7.1','b7.2',
                     'b8.0','b8.1','b8.2',
                     'b9.0','b9.1','b9.2',
                     'b10.0','b10.1','b10.2')

B <- 100

N <- 300
k <- 2
m <- 10
mdl.sim <- mdl.sim.m10k2
data.jags <- data.jags.m10k2
parms.jags <- parms.jags.m10k2
hiermodelsim <- "hiermodsim.m10k2.txt"

####################################################
# Simulate Data
####################################################

# Randomly draw values of X from iid Standard Normal
X <- cbind(1,matrix(rnorm(N*k,0,1),nrow=N,ncol=k))

for(i in 1:k){
  assign(paste("x",i,sep=""),X[,(i+1)])
}

# Select betahat values
#beta.hat <- rbind(1,matrix(rnorm(m*k,0,1),nrow=k,ncol=m))

# Function to optimize
#x.fun <- function(Beta) {
#  sum.betax <- 0
#  for(i in 1:(k+1)){
#    sum.betax <- sum.betax + Beta[i]*X[,i]
#  }
#  mean(sum.betax)^2
#}

# Find betahat values so that the expected value of x_i*beta=0
#beta.hat.init <- matrix(0,nrow=k+1,ncol=m)
#for(i in 1:m) beta.hat.init[,i] <- optim(rep(i*.1,k+1),x.fun)$par

sum.unique <- 0
for(i in 1:(m-1)) sum.unique <- sum.unique + m-i
v <- sample(seq(from=.1,to=1,by=.1),sum.unique,replace=TRUE) 
cov.beta <- diag(m)
cov.beta[lower.tri(cov.beta, diag = FALSE)] <- v
cov.beta[upper.tri(cov.beta)] <- t(cov.beta)[upper.tri(cov.beta)]
E <- eigen(cov.beta)
CM <- E$vectors %*% tcrossprod(diag(pmax(E$values, 0), m), E$vectors)
Balance <- diag(1/sqrt(diag(CM)))
cov.beta <- Balance %*% CM %*% Balance  
beta.hat <- mvrnorm((k+1),rep(0,m),cov.beta)

# Create symmetric correlation matrix
sum.unique <- 0
for(i in 1:(m-1)) sum.unique <- sum.unique + m-i
v <- sample(seq(from=.1,to=1,by=.1),sum.unique,replace=TRUE) 
A <- diag(m)
A[lower.tri(A, diag = FALSE)] <- v
A[upper.tri(A)] <- t(A)[upper.tri(A)]

# Make the covariance matrix for epsilon positive definite: 
# https://www.r-bloggers.com/correcting-a-pseudo-correlation-matrix-to-be-positive-semidefinite/
E <- eigen(A)
CM <- E$vectors %*% tcrossprod(diag(pmax(E$values, 0), m), E$vectors)
Balance <- diag(1/sqrt(diag(CM)))
cov <- Balance %*% CM %*% Balance  

# Create list to save model comparison estimates
modelComp <- list()

for(l in 1:B){
  
  cor.1 <- diag(2,m)
  cor.1[lower.tri(cor.1, diag = FALSE)] <- rep(.9,sum.unique)
  cor.1[upper.tri(cor.1)] <- t(cor.1)[upper.tri(cor.1)]
  cor.y <- cor.1
  
  while(any(cor.y>=cor.1)){
    # Randomly draw epsilon values
    eps <- mvrnorm(N,rep(0,m),cov)
    
    # Generate Response variables
    sim.response <- X%*%beta.hat + eps
    
    #resmean <- ressd <- rep(0,m)
    #for(i in 1:m) {
    #  resmean[i]<-mean(sim.response[,i])
    #  ressd[i] <- sd(sim.response[,i])
    #  for(j in 1:N){
    #    sim.response[j,i] <- (sim.response[j,i] - resmean[i])/ressd[i]
    #  }
    #}
    
    #beta.hat <- solve(t(X)%*%X)%*%t(X)%*%sim.response
    
    # Not missing at random
    #n.missing <- sample(1:(.40*N),m,replace=TRUE)
    #for(i in 1:m){
    #  sort.y <- sort(sim.response[,i],decreasing=TRUE)
    #  nmar <- ifelse(n.missing[i]==0,100,sort.y[ceiling(n.missing[i])])
    #  sim.response[,i] <- ifelse(sim.response[,i]>nmar,NA,sim.response[,i])
    #}
    
    # Not missing at random
    isna <- rep(NA,N)
    sumisna <- N
    while(sumisna==N){
      sim.response.miss <- sim.response
      n.missing <- sample(0:(.30*N),m,replace=TRUE)
      for(i in 1:m){
        which.missing <- sample(1:N,n.missing[i],replace=FALSE,prob=(1:N)/sum(1:N))
        sim.response.miss[which.missing,i] <- NA
      }
      for(j in 1:N) isna[j] <- any(is.na(sim.response.miss[j,]))*1
      sumisna <- sum(isna)
    }
    sim.response <- sim.response.miss
    cor.y <- cor(sim.response,use="complete.obs")
  }
  
  ####################################################
  
  ####################################################
  # Impute missing values with different methods
  
  # Mean Hot Deck Imputation - imputes the column mean of the complete cases for the missing cases
  sim.response.complete.meanhd <- sim.response
  sim.response.complete.meanhd <- impute.mean(sim.response.complete.meanhd)
  
  # EM and Multiple Imputation
  M <- 1
  sim.response.complete.mi <- amelia(sim.response,m=M)
  ####################################################
  
  ####################################################
  # Estimate coefficients Using Different Methods:
  
  ##### 1. Multivariate Regression using Data with Missing Values #####
  mv.miss.model <- lm(sim.response ~ X[,-1])
  # parameter estimates:
  mvparam.miss.est <- mv.miss.model$coef
  # parameter standard errors and CI:
  mvparam.miss.se <- mvparam.miss.ciwidth <- matrix(0,nrow=k+1,ncol=m)
  for(i in 1:m){
    mvparam.miss.se[,i] <- coef(summary(mv.miss.model))[[i]][,"Std. Error"]
    upper <- mvparam.miss.est[,i] + qnorm(0.975)*mvparam.miss.se[,i]
    lower <- mvparam.miss.est[,i] - qnorm(0.975)*mvparam.miss.se[,i]
    mvparam.miss.ciwidth[,i] <- upper - lower
  }
  # Model comparison values
  mv.miss.abser <- mean(abs(beta.hat-mvparam.miss.est))
  mv.miss.rmse <- sqrt(mean((beta.hat-mvparam.miss.est)^2))
  mv.miss.avwidth <- mean(mvparam.miss.ciwidth)
  
  ##### 2. Simple Linear Regression using Data with Missing Values #####
  simpparam.est <- simpparam.se <- simpparam.ciwidth <- matrix(0,nrow=k+1,ncol=m)
  for(i in 1:m){
    simp.model <- lm(sim.response[,i] ~ X[,-1])
    # parameter estimates
    simpparam.est[,i] <- simp.model$coeff
    # parameter standard errors and CI
    simpparam.se[,i] <- coef(summary(simp.model))[,"Std. Error"]
    upper <- simpparam.est[,i] + qnorm(0.975)*simpparam.se[,i]
    lower <- simpparam.est[,i] - qnorm(0.975)*simpparam.se[,i]
    simpparam.ciwidth[,i] <- upper - lower
  }
  # Model comparison values
  simp.abser <- mean(abs(beta.hat-simpparam.est))
  simp.rmse <- sqrt(mean((beta.hat-simpparam.est)^2))
  simp.avwidth <- mean(simpparam.ciwidth)
  
  ##### 3. Multivariate Regression using Data with Mean HD Imputed Values #####
  mv.meanhd.model <- lm(sim.response.complete.meanhd ~ X[,-1])
  # parameter estimates:
  mvparam.meanhd.est <- mv.meanhd.model$coef
  # parameter standard errors and CI:
  mvparam.meanhd.se <- mvparam.meanhd.ciwidth <- matrix(0,nrow=k+1,ncol=m)
  for(i in 1:m){
    mvparam.meanhd.se[,i] <- coef(summary(mv.meanhd.model))[[i]][,"Std. Error"]
    upper <- mvparam.meanhd.est[,i] + qnorm(0.975)*mvparam.meanhd.se[,i]
    lower <- mvparam.meanhd.est[,i] - qnorm(0.975)*mvparam.meanhd.se[,i]
    mvparam.meanhd.ciwidth[,i] <- upper - lower
  }
  # Model comparison values
  mv.meanhd.abser <- mean(abs(beta.hat-mvparam.meanhd.est))
  mv.meanhd.rmse <- sqrt(mean((beta.hat-mvparam.meanhd.est)^2))
  mv.meanhd.avwidth <- mean(mvparam.meanhd.ciwidth)
  
  ##### 4. Multivariate Regression using Data with Multiple Imputation Values #####
  mvmodel <- list()
  mvmodel.vals <- mvparam.ciwidth <- matrix(0,nrow=M,ncol=m*(k+1))
  mvmodel.se <- matrix(0,nrow=k+1,ncol=m)
  for(i in 1:M){
    mvmodel[[i]] <- lm(sim.response.complete.mi$imputations[[i]] ~ X[,-1])
    # parameter estimates for each model
    mvmodel.vals[i,] <- as.vector(mvmodel[[i]]$coef)
    for(j in 1:m){
      # standard error estimates for each model
      mvmodel.se[,j] <- coef(summary(mvmodel[[i]]))[[j]][,"Std. Error"]
    }
    # confidence intervals for each model
    upper <- mvmodel.vals[i,] + qnorm(0.975)*as.vector(mvmodel.se)
    lower <- mvmodel.vals[i,] - qnorm(0.975)*as.vector(mvmodel.se)
    mvparam.ciwidth[i,] <- upper - lower
  }
  # Final Average Estimates
  mvparam.mi.est <- colMeans(mvmodel.vals)
  mvparam.mi.est <- matrix(mvparam.mi.est,nrow=k+1,ncol=m)
  mvparam.mi.ciwidth <- colMeans(mvparam.ciwidth)
  mvparam.mi.ciwidth <- matrix(mvparam.ciwidth,nrow=k+1,ncol=m)
  # Model comparison values
  mv.abser <- mean(abs(beta.hat-mvparam.mi.est))
  mv.rmse <- sqrt(mean((beta.hat-mvparam.mi.est)^2))
  mv.avwidth <- mean(mvparam.ciwidth)
  
  ##### 5. Bayesian Hierarchical Linear Regression #####
  for(i in 1:m){
    assign(paste("y",i,sep=""),sim.response[,i])
  }
  mod.sim.sim <- jags(data=data.jags, inits=NULL,
                        parameters.to.save=parms.jags,
                        model.file=hiermodelsim,n.iter=12000,
                        n.burnin=2000,n.chains=5,n.thin=5)
  # parameter estimates
  hiermodel.est <- matrix(mod.sim.sim$BUGSoutput$summary[c(1:3,7:30,4:6),1],nrow=k+1,ncol=m,byrow=F)
  # parameter CI
  hiermodel.ciwidth <- mod.sim.sim$BUGSoutput$summary[1:((k+1)*m),7] - mod.sim.sim$BUGSoutput$summary[1:((k+1)*m),3]
  # Model comparison values
  hier.abser <- mean(abs(beta.hat-hiermodel.est))
  hier.rmse <- sqrt(mean((beta.hat-hiermodel.est)^2))
  hier.avwidth <- mean(hiermodel.ciwidth)
  ####################################################
  
  ####################################################
  # Compare Models
  
  comp.mods <- data.frame(matrix(c(mv.miss.abser,mv.miss.rmse,mv.miss.avwidth,
                                   simp.abser,simp.rmse,simp.avwidth,
                                   mv.meanhd.abser,mv.meanhd.rmse,mv.meanhd.avwidth,
                                   mv.abser,mv.rmse,mv.avwidth,
                                   hier.abser,hier.rmse,hier.avwidth),nrow=5,ncol=3,byrow=T))
  colnames(comp.mods) <- c("Absolute Error","RMSE","Average CI Width")
  rownames(comp.mods) <- c("Multivariate (Missing)","Simple","Multivariate (Mean HD)",
                           "Multivariate (MI)","Hierarchical")
  modelComp[[l]] <- comp.mods
  save(modelComp,file="Sim.N300k2m10.NMAR3.Rdata")
}

load("~/Desktop/Kaylee Work and School/Research/Masters Project/Project/Sim.N50k2m5.NMAR.Rdata")
modelComp <- Sim.N50k2m5.NMAR
which.best.abser <- which.best.rmse <- which.best.ciwidth <- rep(0,B)
all.abser <- all.rmse <- all.ciwidth <- matrix(0,nrow=B,ncol=5)
for(i in 1:B){
  which.best.abser[i] <- which.min(modelComp[[i]][,1])
  which.best.rmse[i] <- which.min(modelComp[[i]][,2])
  which.best.ciwidth[i] <- which.min(modelComp[[i]][4:5,3])
  all.abser[i,] <- modelComp[[i]][,1]
  all.rmse[i,] <- modelComp[[i]][,2]
  all.ciwidth[i,] <- modelComp[[i]][,3]
}

apply(all.rmse,2,function(x) quantile(x,c(0.025,0.975)))

Sim.N50k2m5.NMAR <- modelComp
save(Sim.N50k2m5.NMAR,file="Sim.N50k2m5.NMAR.Rdata")

Sim.N50k2m10.NMAR <- modelComp
save(Sim.N50k2m10.NMAR,file="Sim.N50k2m10.NMAR.Rdata")

Sim.N160k2m5.NMAR <- modelComp
save(Sim.N160k2m5.NMAR,file="Sim.N160k2m5.NMAR.Rdata")

Sim.N160k2m10.NMAR <- modelComp
save(Sim.N160k2m10.NMAR,file="Sim.N160k2m10.NMAR.Rdata")

Sim.N300k2m5.NMAR <- modelComp
save(Sim.N300k2m5.NMAR,file="Sim.N300k2m5.NMAR.Rdata")

Sim.N300k2m10.NMAR2 <- modelComp
save(Sim.N300k2m10.NMAR2,file="Sim.N300k2m10.NMAR2.Rdata")

Sim.N300k2m10.NMARcomp <- c(Sim.N300k2m10.NMAR,Sim.N300k2m10.NMAR2,modelComp)
save(Sim.N300k2m10.NMARcomp,file="Sim.N300k2m10.NMARcomp.Rdata")


Sim.N300k2m10.NMARcomp <- c(Sim.N300k2m10.NMARcomp,modelComp)
save(Sim.N300k2m10.NMARcomp,file="Sim.N300k2m10.NMARcomp.Rdata")

