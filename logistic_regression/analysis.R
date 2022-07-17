#############################################################################
####### STAT 651 Project - Subordination of Women and the Environment #######
#############################################################################

# At least 10 parameters
# Explain the dataset and goals of analysis
# Justify prior distribution
# Entertain alternative prior distributions
# Investigate sensitivity of priors to conclusions
# Give details of computational approach and burden
# Perform diagnostics to justify computations
# Entertain a non-Bayesian analysis of the data (okay to use software)
# Paper: no more than 8 pages, including references, tables, figures, etc.

# Due Dec 13 11:59pm
# Submit paper with code and data by email with subject "STAT 651 Project" 

#############################################################################

### Go to working directory and load the data
envdat <- read.csv(file="environment_data.csv",header=TRUE)
head(envdat)

#############################################################################

# DEPENDENT VARIABLES

### create combined variable clusters (based on exploratory factor analysis: Hudson, et al. 2018, Submitted)
# Water and Environmental Wellbeing Cluster: water and sanitation, EPI, wastewater treatment, foundations of wellbeing
# Clean Air Cluster: indoor air pollution and greenhouse gases
# Biodiversity and Pest Regulation Cluster: biodiversity and pest regulation
envdat$Wellbeing <- scale(envdat$WaterSanitat) + scale(envdat$EnvPI) + scale(envdat$WastewaterTreat) + scale(envdat$FoundaWellbeing)
envdat$AirPollution <- scale(envdat$IndoorAirDeath16) + scale(envdat$GreenhseEmission)
envdat$AirPollution0 <- ifelse(envdat$Country=="Central African Rep",NA,envdat$AirPollution)
envdat$BioPest <- scale(envdat$BiodiversHabitat) + scale(envdat$PestReg)

### Dichotomize the variables for Logistic Regression
Wellbeing <- ifelse(is.na(envdat$Wellbeing),NA,ifelse(envdat$Wellbeing<=2,1,0))
AirPollution <- ifelse(is.na(envdat$AirPollution0),NA,ifelse(envdat$AirPollution0>-.10,1,0))
BioPestReg <- ifelse(is.na(envdat$BioPest),NA,ifelse(envdat$BioPest<=0,1,0))
OutdoorAirDeath <- ifelse(is.na(envdat$OutdoorAirDeath16),NA,ifelse(envdat$OutdoorAirDeath16>40,1,0))
AirQual <- ifelse(is.na(envdat$AirQual),NA,ifelse(envdat$AirQual<=87,1,0))
ClimateRiskIndex <- ifelse(is.na(envdat$ClimateRiskIndex),NA,ifelse(envdat$ClimateRiskIndex>78.2,1,0))
# 0 = "good" level, 1 = "bad" level

### Put all dependent variables in a data frame
Y <- cbind(Wellbeing,AirPollution,BioPestReg,
           OutdoorAirDeath,AirQual,ClimateRiskIndex)
colnames(Y) <- c('Wellbeing','AirPollution','BioPestReg',
                 'OutdoorAirDeath','AirQual','ClimateRiskIndex')

#############################################################################

# INDEPENDENT VARIABLES

### Change 4-level categorical variable into 3 dummy variables
# 1) majority Western, Orthodox, and Latin civilizations
# 2) majority Muslim civilizations
# 3) majority Hindu, Sinic, and Buddhist civilizations
# 4) African countries that are not majority Muslim
CIV1 <- ifelse(is.na(envdat$CIV),NA,ifelse(envdat$CIV==1,1,0))
CIV2 <- ifelse(is.na(envdat$CIV),NA,ifelse(envdat$CIV==2,1,0))
CIV4 <- ifelse(is.na(envdat$CIV),NA,ifelse(envdat$CIV==4,1,0))

### Standardize the dichotomous input variables (Gelman Page 415)
prctCol1 <- sum(envdat$ColStatus==1)/(sum(envdat$ColStatus==0)+sum(envdat$ColStatus==1))
prctCIV1 <- sum(CIV1==1,na.rm=TRUE)/(sum(CIV1==0,na.rm=TRUE)+sum(CIV1==1,na.rm=TRUE))
prctCIV2 <- sum(CIV2==1,na.rm=TRUE)/(sum(CIV2==0,na.rm=TRUE)+sum(CIV2==1,na.rm=TRUE))
prctCIV4 <- sum(CIV4==1,na.rm=TRUE)/(sum(CIV4==0,na.rm=TRUE)+sum(CIV4==1,na.rm=TRUE))
ColStatus <- ifelse(is.na(envdat$ColStatus),NA,ifelse(envdat$ColStatus==0,prctCol1,prctCol1-1))
CIV1 <- ifelse(is.na(CIV1),NA,ifelse(CIV1==0,prctCIV1,prctCIV1-1))
CIV2 <- ifelse(is.na(CIV2),NA,ifelse(CIV2==0,prctCIV2,prctCIV2-1))
CIV4 <- ifelse(is.na(CIV4),NA,ifelse(CIV4==0,prctCIV4,prctCIV4-1))

### Standardized the continuous variables. Mean=0,SD=0.5 (Gelman Page 415)
Syndrome <- scale(envdat$Syndrm2017Imp, center=TRUE, scale=2*sd(envdat$Syndrm2017Imp,na.rm=TRUE))
Urbanization <- scale(envdat$Urbanization2015, center=TRUE, scale=2*sd(envdat$Urbanization2015,na.rm=TRUE))
LandNeighbors <- scale(envdat$Landneighbors, center=TRUE, scale=2*sd(envdat$Landneighbors,na.rm=TRUE))
Terrain <- scale(envdat$Terrain2014, center=TRUE, scale=2*sd(envdat$Terrain2014,na.rm=TRUE))
RelFrac <- scale(envdat$RelFrac2003, center=TRUE, scale=2*sd(envdat$RelFrac2003,na.rm=TRUE))
EthnicFrac <- scale(envdat$EthnicFrac2003, center=TRUE, scale=2*sd(envdat$EthnicFrac2003,na.rm=TRUE))

### Put all independent variables in a data frame
indepvar <- cbind(rep(1,176),
           CIV1,CIV2,CIV4,RelFrac,EthnicFrac,ColStatus,Urbanization,
           Terrain,LandNeighbors,Syndrome)
colnames(indepvar) <- c('Intercept',
                 'CIV1','CIV2','CIV4','RelFrac','EthnicFrac','ColStatus','Urbanization',
                 'Terrain','LandNeighbors','Syndrome')

### Check Distribution of independent variables
apply(indepvar,2,sd,na.rm=TRUE)
apply(indepvar,2,mean,na.rm=TRUE)

#############################################################################

# DESCRIPTIVE STATISTICS FOR SYNDROME
descript <- matrix(0,nrow=dim(Y)[2],ncol=2)
rownames(descript) <- colnames(Y)
colnames(descript) <- c("good","bad")
for(i in 1:dim(Y)[2]){
  for(j in 1:2){
  descript[i,j] <- mean(envdat$Syndrm2017Imp[Y[,i]==(j-1)],na.rm=TRUE)
  }
}

### plot these descriptive values ###
pdf("barplot.pdf",width=11,height=8)
par(mfrow=c(1,1))
plot(1:6,rep(0,6),type="l",lwd=0, bty="n",
     ylim=c(0,11.5),xaxt="n",yaxt="n",
     xlab="",ylab="",
     main="",xlim=c(1,6.5))
for(i in 1:nrow(descript)){
  segments(i,0,i,descript[i,1],lwd=20,col="green")
  segments(i+.2,0,i+.2,descript[i,2],lwd=20,col="red")
}
dev.off()

#### SYNDROME MAP ####
envdat$iso3 <- countrycode(envdat$Country,"country.name","iso3c")
envdat$iso3[32] <- "CAF"
envdat$iso3[45] <- "COD"

envdat$Syndrm2017Imp[which(envdat$Country=="Syria")] <- 13.0

envdat <- envdat[order(envdat$Syndrm2017Imp),]

MyWorldMap <- joinCountryData2Map(envdat, joinCode = "ISO3", 
                                  nameJoinColumn = "iso3",
                                  nameCountryColumn = "Country")
summary(MyWorldMap)

# create color palette
my.colors <- brewer.pal(8, 'Blues')
my.colors <- colorRampPalette(my.colors)(21)

# map the data and export
pdf("map.pdf")
mapCountryData(MyWorldMap, nameColumnToPlot = "Syndrm2017Imp", numCats=17,catMethod="categorical",
               colourPalette = my.colors[5:21], addLegend = TRUE, borderCol="white", missingCountryCol = "gray",
               mapRegion = "world", mapTitle = "The Syndrome Scale")
dev.off()

#############################################################################
#############################################################################
### BAYESIAN MODEL ###
# LEVEL 1: y_ij|p_ij ~ Bernoulli(p_ij),
#          logit(pi_ij) = beta_j0 + sum_{k=1}^10(x_ijk*beta_jk) + epsilon_ij
# LEVEL 2: beta_j ~ p_k(0, 2.5), alpha_k ~ p_k(2,2)
# i=1,...,176, j=1,...,11, k=1,...,10

# Gelman Page 422: Example of hierarchical logistic regression model
#############################################################################
#############################################################################

# Generic MH algorithm 
mh.update <- function(state, log.target.density, proposal, log.proposal.density = function(x) { 0.0 } ) {
  log.mh.ratio <- 0
  log.mh.ratio <- log.mh.ratio + log.target.density(proposal) - log.target.density(state)
  log.mh.ratio <- log.mh.ratio + log.proposal.density(state) - log.proposal.density(proposal)
  if ( log(runif(1)) < log.mh.ratio ) proposal
  else state
}

# Prior values
sigma <- 5
theta <- 2.5
mu <- 2
phi <- 2

# Log Posterior
log.posterior <- function(params){
  beta.hat <- params
  eta <- X%*%beta.hat
  p <- 1/(1+exp(-eta))
  sum(dbinom(y,1,p,log=T),na.rm=T) + dcauchy(beta.hat[1],scale=sigma,log=T) + sum(dcauchy(beta.hat[2:8],scale=theta,log=T)) +
    sum(dcauchy(beta.hat[9:11],location=mu,scale=phi,log=T))
}

library(MASS)

# MCMC function
MCMC <- function(y,init.vals,nSamples){
  
  ptm <- proc.time()
  
  # Initial values
  X <- indepvar
  beta.hat <- init.vals[1:11]
  
  # Reserve space for draws
  J <- length(beta.hat[2:8])
  K <- length(beta.hat[9:11])
  names <- c(paste0("beta",0:J),paste0("alpha",1:K))
  samples <- matrix(0, nrow=nSamples, ncol=length(names))
  colnames(samples) <- names
  samples[1,] <- beta.hat
  
  # Normal approximation
  result <- optim(init.vals, function(par) -log.posterior(par), hessian=TRUE)
  cov.mat <- observedInformation <- result$hessian
  mean.vec <- result$par
  
  # Covariance values
  A <- cov.mat
  B <- t(chol(A))
  C <- solve(A)
  
  # envelope function - use for proposal density
  envelope.ir <- function(x) -0.5 * t(x-mean.vec) %*% C %*% (x-mean.vec)
  
  # MH updates
  for ( l in 2:nSamples ) {
    
    proposal <- mean.vec + .2*B %*% rnorm(11)
    samples[l,] <- mh.update(samples[l-1,], log.posterior, proposal, envelope.ir)
    
  }
  
  cpu <- proc.time()-ptm
  list(samples,cpu)
}

# Draw from the MCMC algorithm
init.vals <- apply(Y,2,function(y) summary(glm(y ~ indepvar[,-1],family=binomial))$coef[,1])
init.vals_0 <- c(rep(0,8),rep(2,3))
init.vals_o <- c(rep(0,11))
MCMC.draws <- MCMC.draws_0 <- MCMC.draws_o <- list()
mcmc.proctime <- matrix(0,nrow=6,ncol=3)
for(i in 1:ncol(init.vals)) {
  y <- Y[,i]
  # draws from posterior
  mcmcvals <- MCMC(y,init.vals=init.vals[,i],nSamples=50000)
  MCMC.draws[[i]] <- mcmcvals[[1]]
  MCMC.draws[[i]] <- MCMC.draws[[i]][-(1:20000),]
  MCMC.draws[[i]] <- MCMC.draws[[i]][-seq(0,30000,by=5),]
  mcmcvals_0 <- MCMC(y,init.vals=init.vals_0,nSamples=50000)
  MCMC.draws_0[[i]] <- mcmcvals_0[[1]]
  MCMC.draws_0[[i]] <- MCMC.draws_0[[i]][-(1:20000),]
  MCMC.draws_0[[i]] <- MCMC.draws_0[[i]][-seq(0,30000,by=5),]
  mcmcvals_o <- MCMC(y,init.vals=init.vals_o,nSamples=50000)
  MCMC.draws_o[[i]] <- mcmcvals_o[[1]]
  MCMC.draws_o[[i]] <- MCMC.draws_o[[i]][-(1:20000),]
  MCMC.draws_o[[i]] <- MCMC.draws_o[[i]][-seq(0,30000,by=5),]
  mcmc.proctime[i,] <- c(mcmcvals[[2]][3],mcmcvals_0[[2]][3],mcmcvals_o[[2]][3])
}

MCMC.draws.final <- list()
for(i in 1:6) MCMC.draws.final[[i]] <- rbind(MCMC.draws_0[[i]],MCMC.draws_o[[i]],MCMC.draws[[i]])

#############################################################################
# Estimates from the draws

library(coda)
draws.est <- draws.ci <- draws.ci9 <- draws.mce <- draws.mce9 <- ess <- list()
for(i in 1:6){
  draws.est[[i]] <- apply(MCMC.draws.final[[i]],2,mean)
  draws.ci[[i]] <- apply(MCMC.draws.final[[i]],2,quantile,c(0.025,0.975))
  draws.ci9[[i]] <- apply(MCMC.draws.final[[i]],2,quantile,c(0.005,0.995))
  var.est <- apply(MCMC.draws.final[[i]],2,var)
  ess[[i]] <- effectiveSize(MCMC.draws.final[[i]])
  draws.mce[[i]] <- rbind(draws.est[[i]] - qnorm(0.975) * sqrt(var.est/ess[[i]]),
                          draws.est[[i]] + qnorm(0.975) * sqrt(var.est/ess[[i]]))
  draws.mce9[[i]] <- rbind(draws.est[[i]] - qnorm(0.995) * sqrt(var.est/ess[[i]]),
                           draws.est[[i]] + qnorm(0.975) * sqrt(var.est/ess[[i]]))
}

est.vals <- rep(0,6)
for(i in 1:6) est.vals[i] <- draws.est[[i]][11]
est.ci <- est.ci9 <- matrix(0,nrow=6,ncol=2)
for(i in 1:6) est.ci[i,] <- draws.ci[[i]][,11]
for(i in 1:6) est.ci9[i,] <- draws.ci9[[i]][,11]

pdf("coefficient.pdf",height=8,width=11)
plot(est.vals,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Coefficient Estimate", ylab="",xlim=c(-.5,4.5),yaxt="n")
for(i in 1:length(est.vals)){
  segments(est.ci9[i,1],i,est.ci9[i,2],i,lwd=2,col="grey")
}
for(i in 1:length(est.vals)){
  segments(est.ci[i,1],i,est.ci[i,2],i,lwd=4,col="cornflowerblue")
}
axis(2,at=1:6,labels=c("Water and Environmental Wellbeing",
                           "Air Pollution",
                           "Biodiversity and Pest Regulation",
                           "Deaths Caused by Outdoor Air Pollution",
                           "Air Quality",
                           "Global Climate Risk Index"),las=1,cex=.3)
points(est.vals,1:6,cex=1,pch=19)
abline(v=0,lty=2)
dev.off()

#############################################################################

# Convergence Diagnostics

# Convergence for Syndrome parameter

ylimits <- matrix(c(c(1.5,4.8),c(1.5,3.8),c(.9,3.3),c(1.7,4.3),c(1.2,4),c(-.2,1.8)),byrow=T,ncol=2)
ylabels <- c("Water and Environmental Wellbeing","Air Pollution","Biodiversity and Pest Regulation",
             "Deaths Caused by Outdoor Air Pollution","Air Quality","Global Climate Risk Index")
png("conv.png",height = 480, width = 660)
par(mfrow=c(2,3))
for(i in 1:6){
  plot(MCMC.draws_0[[i]][,11],type="l",col="#BEBEBEFF",ylim=ylimits[i,],
       ylab=ylabels[i],xlab="",xaxt="n")
  axis(side=1,at=c(0,6000,12000,18000,24000))
  lines(MCMC.draws_o[[i]][,11],col="#40E0D0FF")
  lines(MCMC.draws[[i]][,11],col="#FF000022")
  abline(h=draws.est[[i]][11],col="blue")
}
dev.off()

pdf("acf.pdf")
par(mfrow=c(2,3))
for(i in 1:6){
  acf(MCMC.draws.final[[i]][,11])
}
dev.off()

# effective samples size calculated above
ess

# Statistical diagnostics
ar <- rhat <-rhatm <- list()
for(i in 1:6){
  ar[[i]] <- apply(MCMC.draws.final[[i]],2, function(x) length(unique(x))/length(x))
  chain.list <- list(as.mcmc(MCMC.draws[[i]][1:12000,]),as.mcmc(MCMC.draws[[i]][12001:24000,]),
                     as.mcmc(MCMC.draws_0[[i]][1:12000,]),as.mcmc(MCMC.draws_0[[i]][12001:24000,]),
                     as.mcmc(MCMC.draws_o[[i]][1:12000,]),as.mcmc(MCMC.draws_o[[i]][12001:24000,]))
  rhat[[i]] <- gelman.diag(chain.list)  # Gelman's rhat
  #rhatm[[i]] <- Rhat.mult(MCMC.draws.final[[i]])
}

#############################################################################

# Frequentist approach

coef.est <- apply(Y,2,function(y) summary(glm(y ~ indepvar[,-1],family=binomial))$coef[,1])
coef.se <- apply(Y,2,function(y) summary(glm(y ~ indepvar[,-1],family=binomial))$coef[,2])

f.est.vals <- coef.est[11,]
f.est.se <- coef.se[11,]
f.est.ci <- f.est.ci9 <- matrix(0,nrow=6,ncol=2)
for(i in 1:6){
  f.est.ci[i,] <- f.est.vals[i] + c(-1,1)*qnorm(0.975)*f.est.se[i]
  f.est.ci9[i,] <- f.est.vals[i] + c(-1,1)*qnorm(0.995)*f.est.se[i]
}

# Comparison of frequentist and Bayesian
pdf("fcoefficient.pdf",height=8,width=13)
par(mfrow=c(1,2))
plot(est.vals,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Coefficient Estimate", ylab="",xlim=c(-.5,7.5),yaxt="n")
for(i in 1:length(est.vals)){
  segments(est.ci9[i,1],i,est.ci9[i,2],i,lwd=2,col="grey")
}
for(i in 1:length(est.vals)){
  segments(est.ci[i,1],i,est.ci[i,2],i,lwd=4,col="cornflowerblue")
}
axis(2,at=1:6,labels=c("Water and Environmental Wellbeing",
                       "Air Pollution",
                       "Biodiversity and Pest Regulation",
                       "Deaths Caused by Outdoor Air Pollution",
                       "Air Quality",
                       "Global Climate Risk Index"),las=1,cex=.3)
points(est.vals,1:6,cex=1,pch=19)
abline(v=0,lty=2)
plot(f.est.vals,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Coefficient Estimate", ylab="",xlim=c(-.5,7.5),yaxt="n")
for(i in 1:length(f.est.vals)){
  segments(f.est.ci9[i,1],i,f.est.ci9[i,2],i,lwd=2,col="grey")
}
for(i in 1:length(f.est.vals)){
  segments(f.est.ci[i,1],i,f.est.ci[i,2],i,lwd=4,col="cornflowerblue")
}
points(f.est.vals,1:6,cex=1,pch=19)
abline(v=0,lty=2)
dev.off()

#############################################################################

# Sensitivity Analysis

#### NONINFORMATIVE PRIORS
# Prior values
sigma <- 5
theta <- 5
mu <- 0
phi <- 5
# Log Posterior
log.posterior <- function(params){
  beta.hat <- params
  eta <- X%*%beta.hat
  p <- 1/(1+exp(-eta))
  sum(dbinom(y,1,p,log=T),na.rm=T) + dcauchy(beta.hat[1],scale=sigma,log=T) + sum(dcauchy(beta.hat[2:8],scale=theta,log=T)) +
    sum(dcauchy(beta.hat[9:11],location=mu,scale=phi,log=T))
}
# Draw from the MCMC algorithm
MCMC.draws.ni <- MCMC.draws_0.ni <- MCMC.draws_o.ni <- list()
mcmc.proctime.ni <- matrix(0,nrow=6,ncol=3)
for(i in 1:ncol(init.vals)) {
  y <- Y[,i]
  # draws from posterior
  mcmcvals.ni <- MCMC.sens(y,init.vals=init.vals[,i],nSamples=50000)
  MCMC.draws.ni[[i]] <- mcmcvals.ni[[1]]
  MCMC.draws.ni[[i]] <- MCMC.draws.ni[[i]][-(1:20000),]
  MCMC.draws.ni[[i]] <- MCMC.draws.ni[[i]][-seq(0,30000,by=5),]
  mcmcvals_0.ni <- MCMC.sens(y,init.vals=init.vals_0,nSamples=50000)
  MCMC.draws_0.ni[[i]] <- mcmcvals_0.ni[[1]]
  MCMC.draws_0.ni[[i]] <- MCMC.draws_0.ni[[i]][-(1:20000),]
  MCMC.draws_0.ni[[i]] <- MCMC.draws_0.ni[[i]][-seq(0,30000,by=5),]
  mcmcvals_o.ni <- MCMC.sens(y,init.vals=init.vals_o,nSamples=50000)
  MCMC.draws_o.ni[[i]] <- mcmcvals_o.ni[[1]]
  MCMC.draws_o.ni[[i]] <- MCMC.draws_o.ni[[i]][-(1:20000),]
  MCMC.draws_o.ni[[i]] <- MCMC.draws_o.ni[[i]][-seq(0,30000,by=5),]
  mcmc.proctime.ni[i,] <- c(mcmcvals.ni[[2]][3],mcmcvals_0.ni[[2]][3],mcmcvals_o.ni[[2]][3])
}

#### MORE INFORMATIVE PRIORS
# Prior values
sigma <- 5
theta <- 2.5
mu <- 2
phi <- 2
# Log Posterior
log.posterior <- function(params){
  beta.hat <- params
  eta <- X%*%beta.hat
  p <- 1/(1+exp(-eta))
  sum(dbinom(y,1,p,log=T),na.rm=T) + dnorm(beta.hat[1],sd=sigma,log=T) + sum(dnorm(beta.hat[2:8],sd=theta,log=T)) +
    sum(dnorm(beta.hat[9:11],mean=mu,sd=phi,log=T))
}
# Draw from the MCMC algorithm
MCMC.draws.mi <- MCMC.draws_0.mi <- MCMC.draws_o.mi <- list()
mcmc.proctime.mi <- matrix(0,nrow=6,ncol=3)
for(i in 1:ncol(init.vals)) {
  y <- Y[,i]
  # draws from posterior
  mcmcvals.mi <- MCMC.sens(y,init.vals=init.vals[,i],nSamples=50000)
  MCMC.draws.mi[[i]] <- mcmcvals.mi[[1]]
  MCMC.draws.mi[[i]] <- MCMC.draws.mi[[i]][-(1:20000),]
  MCMC.draws.mi[[i]] <- MCMC.draws.mi[[i]][-seq(0,30000,by=5),]
  mcmcvals_0.mi <- MCMC.sens(y,init.vals=init.vals_0,nSamples=50000)
  MCMC.draws_0.mi[[i]] <- mcmcvals_0.mi[[1]]
  MCMC.draws_0.mi[[i]] <- MCMC.draws_0.mi[[i]][-(1:20000),]
  MCMC.draws_0.mi[[i]] <- MCMC.draws_0.mi[[i]][-seq(0,30000,by=5),]
  mcmcvals_o.mi <- MCMC.sens(y,init.vals=init.vals_o,nSamples=50000)
  MCMC.draws_o.mi[[i]] <- mcmcvals_o.mi[[1]]
  MCMC.draws_o.mi[[i]] <- MCMC.draws_o.mi[[i]][-(1:20000),]
  MCMC.draws_o.mi[[i]] <- MCMC.draws_o.mi[[i]][-seq(0,30000,by=5),]
  mcmc.proctime.mi[i,] <- c(mcmcvals.mi[[2]][3],mcmcvals_0.mi[[2]][3],mcmcvals_o.mi[[2]][3])
}

# Estimates from Sensitivity Analyses
MCMC.draws.final.ni <- list()
for(i in 1:6) MCMC.draws.final.ni[[i]] <- rbind(MCMC.draws_0.ni[[i]],MCMC.draws_o.ni[[i]],MCMC.draws.ni[[i]])
MCMC.draws.final.mi <- list()
for(i in 1:6) MCMC.draws.final.mi[[i]] <- rbind(MCMC.draws_0.mi[[i]],MCMC.draws_o.mi[[i]],MCMC.draws.mi[[i]])

draws.est.ni <- draws.ci.ni <- draws.ci9.ni <- draws.mce.ni <- draws.mce9.ni <- ess.ni <- list()
draws.est.mi <- draws.ci.mi <- draws.ci9.mi <- draws.mce.mi <- draws.mce9.mi <- ess.mi <- list()
for(i in 1:6){
  draws.est.ni[[i]] <- apply(MCMC.draws.final.ni[[i]],2,mean)
  draws.ci.ni[[i]] <- apply(MCMC.draws.final.ni[[i]],2,quantile,c(0.025,0.975))
  draws.ci9.ni[[i]] <- apply(MCMC.draws.final.ni[[i]],2,quantile,c(0.005,0.995))
  var.est <- apply(MCMC.draws.final.ni[[i]],2,var)
  ess.ni[[i]] <- effectiveSize(MCMC.draws.final.ni[[i]])
  draws.mce.ni[[i]] <- rbind(draws.est.ni[[i]] - qnorm(0.975) * sqrt(var.est/ess.ni[[i]]),
                          draws.est.ni[[i]] + qnorm(0.975) * sqrt(var.est/ess.ni[[i]]))
  draws.mce9.ni[[i]] <- rbind(draws.est.ni[[i]] - qnorm(0.995) * sqrt(var.est/ess.ni[[i]]),
                           draws.est.ni[[i]] + qnorm(0.975) * sqrt(var.est/ess.ni[[i]]))
  draws.est.mi[[i]] <- apply(MCMC.draws.final.mi[[i]],2,mean)
  draws.ci.mi[[i]] <- apply(MCMC.draws.final.mi[[i]],2,quantile,c(0.025,0.975))
  draws.ci9.mi[[i]] <- apply(MCMC.draws.final.mi[[i]],2,quantile,c(0.005,0.995))
  var.est <- apply(MCMC.draws.final.mi[[i]],2,var)
  ess.mi[[i]] <- effectiveSize(MCMC.draws.final.mi[[i]])
  draws.mce.mi[[i]] <- rbind(draws.est.mi[[i]] - qnorm(0.975) * sqrt(var.est/ess.mi[[i]]),
                             draws.est.mi[[i]] + qnorm(0.975) * sqrt(var.est/ess.mi[[i]]))
  draws.mce9.mi[[i]] <- rbind(draws.est.mi[[i]] - qnorm(0.995) * sqrt(var.est/ess.mi[[i]]),
                              draws.est.mi[[i]] + qnorm(0.975) * sqrt(var.est/ess.mi[[i]]))
}

# Syndrome Estimates
est.vals.ni <- est.vals.mi <- rep(0,6)
est.ci.ni <- est.ci9.ni <- est.ci.mi <- est.ci9.mi <- matrix(0,nrow=6,ncol=2)
for(i in 1:6) est.vals.ni[i] <- draws.est.ni[[i]][11]
for(i in 1:6) est.ci.ni[i,] <- draws.ci.ni[[i]][,11]
for(i in 1:6) est.ci9.ni[i,] <- draws.ci9.ni[[i]][,11]
for(i in 1:6) est.vals.mi[i] <- draws.est.mi[[i]][11]
for(i in 1:6) est.ci.mi[i,] <- draws.ci.mi[[i]][,11]
for(i in 1:6) est.ci9.mi[i,] <- draws.ci9.mi[[i]][,11]

pdf("coefficientsens.pdf",height=8,width=20)
par(mfrow=c(1,3))
plot(est.vals,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Coefficient Estimate", ylab="",xlim=c(-.5,4.5),yaxt="n")
for(i in 1:length(est.vals)){
  segments(est.ci9[i,1],i,est.ci9[i,2],i,lwd=2,col="grey")
}
for(i in 1:length(est.vals)){
  segments(est.ci[i,1],i,est.ci[i,2],i,lwd=4,col="cornflowerblue")
}
axis(2,at=1:6,labels=c("Water and Environmental Wellbeing",
                       "Air Pollution",
                       "Biodiversity and Pest Regulation",
                       "Deaths Caused by Outdoor Air Pollution",
                       "Air Quality",
                       "Global Climate Risk Index"),las=1,cex=.3)
points(est.vals,1:6,cex=1,pch=19)
abline(v=0,lty=2)
plot(est.vals.ni,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Coefficient Estimate", ylab="",xlim=c(-.5,4.5),yaxt="n")
for(i in 1:length(est.vals.ni)){
  segments(est.ci9.ni[i,1],i,est.ci9.ni[i,2],i,lwd=2,col="grey")
}
for(i in 1:length(est.vals.ni)){
  segments(est.ci.ni[i,1],i,est.ci.ni[i,2],i,lwd=4,col="cornflowerblue")
}
points(est.vals.ni,1:6,cex=1,pch=19)
abline(v=0,lty=2)
plot(est.vals.mi,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Coefficient Estimate", ylab="",xlim=c(-.5,4.5),yaxt="n")
for(i in 1:length(est.vals.mi)){
  segments(est.ci9.mi[i,1],i,est.ci9.mi[i,2],i,lwd=2,col="grey")
}
for(i in 1:length(est.vals.mi)){
  segments(est.ci.mi[i,1],i,est.ci.mi[i,2],i,lwd=4,col="cornflowerblue")
}
points(est.vals.mi,1:6,cex=1,pch=19)
abline(v=0,lty=2)
dev.off()

# Diagnostic comparisons
# effective samples size calculated above
ess.ni
ess.mi

# Statistical diagnostics
ar.ni <- rhat.ni <- ar.mi <- rhat.mi <- list()
for(i in 1:6){
  ar.ni[[i]] <- apply(MCMC.draws.final.ni[[i]],2, function(x) length(unique(x))/length(x))
  chain.list.ni <- list(as.mcmc(MCMC.draws.ni[[i]]),as.mcmc(MCMC.draws_0.ni[[i]],as.mcmc(MCMC.draws_o.ni[[i]])))
  rhat.ni[[i]] <- gelman.diag(chain.list.ni)  # Gelman's rhat
  ar.mi[[i]] <- apply(MCMC.draws.final.mi[[i]],2, function(x) length(unique(x))/length(x))
  chain.list.mi <- list(as.mcmc(MCMC.draws.mi[[i]]),as.mcmc(MCMC.draws_0.mi[[i]],as.mcmc(MCMC.draws_o.mi[[i]])))
  rhat.mi[[i]] <- gelman.diag(chain.list.mi)  # Gelman's rhat
}


