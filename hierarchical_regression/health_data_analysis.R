####################################################
# Multivariate Analysis of Global Health Variables #
####################################################

# Import the data
healthdat <- read.csv(file='health_data.csv',header=T)
healthdat <- healthdat[1:176,]

# Assign factor variables
healthdat$ColStatus <- as.factor(healthdat$ColStatus)
healthdat$CIV <- as.factor(healthdat$CIV)

####################################################
# Clean up Dependent Variables

# exclude wasting, underweight, HIVPrev, HIVFemPrev, HIVAIDs, GHI
# Create a multivariate response variable matrix
response <- data.frame(cbind(
  # Women's Health:
  healthdat$FemLife.Ex2015,healthdat$Dif.Life.Exp.MW.2015,healthdat$PrenatalCare2017,
  healthdat$X.BirthSkilledStaff2017,healthdat$BirthAge15.19,healthdat$MMR2015,
  healthdat$RiskMaternalDeath2015,healthdat$MRComDis2008,healthdat$HIVFemPrev2016,healthdat$FGM2015,
  # Children's Health:
  healthdat$Stunted2015,healthdat$IMR2016,healthdat$Wasting2015,healthdat$Underweight2015,
  healthdat$Diarrhea2010,healthdat$ImmunMeasles2016,
  # Overall Health:
  healthdat$OpenDef2015,healthdat$UrbanOpenDef2015,healthdat$LifeExpect2016,
  healthdat$HealthExpend2015,healthdat$HealthExpendCapita2015,healthdat$Tuberculosis2015,
  healthdat$HIVPrev2015,healthdat$HIVAIDs2016,healthdat$Undernourish2015,
  healthdat$Alcohol2015,healthdat$Cigarette2016,healthdat$AccssImprovedWater2016,
  healthdat$AccssImprovedSanFclty2016,healthdat$GHI2016,healthdat$SSI2016,healthdat$Avgnrgysply2014.16))

names(response) <- c('FemLifeEx','DifLifeExMW','PrenatalCare','BrithSkilledStaff',
                     'BirthAge1519','MMR','RiskMaternalDeath','MRComDis','HIVFemPrev',
                     'FGM','Stunted','IMR','Wasting','Underweight','Diarrhea','ImmunMeasles',
                     'OpenDef','UrbanOpenDef','LifeExpect','HealthExpend','HealthExpendCapita',
                     'Tuberculosis','HIVPrev','HIVAIDs','Undernourish','Alcohol','Cigarette',
                     'AccssImprovedWater','AccssImprovedSanFclty','GHI','SSI','Avgnrgysply')
rownames(response) <- healthdat$Country

# Restrict to only continuous responses. Say how much I'm not considering
# Exclude Ordinal Variables: DifLifeExMW, MMR, FGM
response <- response[,-c(2,6,10)]

response[,2] <- response[,2]^10
response[,3] <- response[,3]^5
response[,13] <- response[,13]^5
response[,25] <- response[,25]^3
response[,26] <- response[,26]^2
response[,4] <- response[,4]^.5
response[,8] <- response[,8]^.5
response[,11] <- response[,11]^.5
response[,24] <- response[,24]^.25
response[,5] <- ifelse(response[,5]==0,log(.0001),log(response[,5]))
response[,6] <- ifelse(response[,6]==0,log(.0001),log(response[,6]))
response[,9] <- ifelse(response[,9]==0,log(.0001),log(response[,9]))
response[,10] <- ifelse(response[,10]==0,log(.1),log(response[,10]))
response[,14] <- ifelse(response[,14]==0,log(.0001),log(response[,14]))
response[,15] <- ifelse(response[,15]==0,log(.0001),log(response[,15]))
response[,17] <- ifelse(response[,17]==0,log(.0001),log(response[,17]))
response[,19] <- ifelse(response[,19]==0,log(.0001),log(response[,19]))
response[,20] <- response[,20]^-.25 
response[,21] <- response[,21]^-.25
response[,22] <- response[,22]^-.5

boxcox(lm(response[,24]~indepvar[,2]+indepvar[,3]+indepvar[,4]+indepvar[,5]
          +indepvar[,6]+indepvar[,7]+indepvar[,8]+indepvar[,9]+indepvar[,10]),
       lambda=seq(-1,1))
# 2,3,4,5,6,8,9,10,11,13,14,15,17,19,20,21,22,25,26
# 2: ^10
# 3,13: ^5
# 25: ^3
# 26: ^2
# 4,8,11: ^.5
# 5,6,9,10,14,15,17,19: log
# 20,21: -.25
# 22: -.5

### Variables where 'higher scores are worse': ###
# 'BirthAge1519', 'RiskMaternalDeath','MRComDis','HIVFemPrev'
# 'Stunted','IMR','Wasting','Underweight','Diarrhea'
# 'OpenDef','UrbanOpenDef', 'Tuberculosis','HIVPrev','HIVAIDs','Undernourish','Alcohol','Cigarette', 'GHI'
# from transformations: "HIVPrev","HIVAIDs","Undernourish" - don't transform here
higher.names <- c('BirthAge1519', 'RiskMaternalDeath','MRComDis','HIVFemPrev',
                  'Stunted','IMR','Wasting','Underweight','Diarrhea',
                  'OpenDef','UrbanOpenDef', 'Tuberculosis','Alcohol','Cigarette', 'GHI')

# Standardize the response variables
stresponse <- data.frame(matrix(rep(0,dim(response)[2]*dim(response)[1]),ncol=dim(response)[2]))
names(stresponse) <- c('FemLifeEx','PrenatalCare','BirthSkilledStaff',
                       'BirthAge1519','RiskMaternalDeath','MRComDis','HIVFemPrev',
                       'Stunted','IMR','Wasting','Underweight','Diarrhea','ImmunMeasles',
                       'OpenDef','UrbanOpenDef','LifeExpect','HealthExpend','HealthExpendCapita',
                       'Tuberculosis','HIVPrev','HIVAIDs','Undernourish','Alcohol','Cigarette',
                       'AccssImprovedWater','AccssImprovedSanFclty','GHI','SSI','Avgnrgysply')
resmean <- ressd <- rep(0,dim(response)[2])
for(i in 1:dim(response)[2]) {
  resmean[i]<-mean(response[,i],na.rm=TRUE)
  ressd[i] <- sd(response[,i],na.rm=TRUE)
  for(j in 1:dim(response)[1]){
    stresponse[j,i] <- (response[j,i] - resmean[i])/ressd[i]
  }
  if(any(names(stresponse)[i]==higher.names)) stresponse[,i] <- stresponse[,i]*(-1)
}

# Remove any rows where the independent variables have missing data (this still leaves us with 160 observations)
response <- response[-c(48,49,54,78,86,101,104,109,123,137,142,146,149,155,174,176),]
stresponse <- stresponse[-c(48,49,54,78,86,101,104,109,123,137,142,146,149,155,174,176),]

####################################################
# Clean up Independent Variables

### Change 4-level categorical variable into 3 dummy variables
# 1) majority Western, Orthodox, and Latin civilizations
# 2) majority Muslim civilizations
# 3) majority Hindu, Sinic, and Buddhist civilizations
# 4) African countries that are not majority Muslim
CIV1 <- ifelse(is.na(healthdat$CIV),NA,ifelse(healthdat$CIV==1,1,0))
CIV2 <- ifelse(is.na(healthdat$CIV),NA,ifelse(healthdat$CIV==2,1,0))
CIV4 <- ifelse(is.na(healthdat$CIV),NA,ifelse(healthdat$CIV==4,1,0))

### Standardize the dichotomous input variables (Gelman Page 415)
prctCol1 <- sum(healthdat$ColStatus==1)/(sum(healthdat$ColStatus==0)+sum(healthdat$ColStatus==1))
prctCIV1 <- sum(CIV1==1,na.rm=TRUE)/(sum(CIV1==0,na.rm=TRUE)+sum(CIV1==1,na.rm=TRUE))
prctCIV2 <- sum(CIV2==1,na.rm=TRUE)/(sum(CIV2==0,na.rm=TRUE)+sum(CIV2==1,na.rm=TRUE))
prctCIV4 <- sum(CIV4==1,na.rm=TRUE)/(sum(CIV4==0,na.rm=TRUE)+sum(CIV4==1,na.rm=TRUE))
ColStatus <- ifelse(is.na(healthdat$ColStatus),NA,ifelse(healthdat$ColStatus==0,prctCol1,prctCol1-1))
CIV1 <- ifelse(is.na(CIV1),NA,ifelse(CIV1==0,prctCIV1,prctCIV1-1))
CIV2 <- ifelse(is.na(CIV2),NA,ifelse(CIV2==0,prctCIV2,prctCIV2-1))
CIV4 <- ifelse(is.na(CIV4),NA,ifelse(CIV4==0,prctCIV4,prctCIV4-1))

### Standardized the continuous variables. Mean=0,SD=0.5 (Gelman Page 415)
Syndrome <- scale(healthdat$Syndrm2017Imp, center=TRUE, scale=2*sd(healthdat$Syndrm2017Imp,na.rm=TRUE))
Urbanization <- scale(healthdat$Urbanization2015, center=TRUE, scale=2*sd(healthdat$Urbanization2015,na.rm=TRUE))
LandNeighbors <- scale(healthdat$Landneighbors, center=TRUE, scale=2*sd(healthdat$Landneighbors,na.rm=TRUE))
Terrain <- scale(healthdat$Terrain2014, center=TRUE, scale=2*sd(healthdat$Terrain2014,na.rm=TRUE))
RelFrac <- scale(healthdat$RelFrac2003, center=TRUE, scale=2*sd(healthdat$RelFrac2003,na.rm=TRUE))
EthnicFrac <- scale(healthdat$EthnicFrac2003, center=TRUE, scale=2*sd(healthdat$EthnicFrac2003,na.rm=TRUE))

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

# Remove any rows where the independent variables have missing data (this still leaves us with 160 observations)
indepvar <- indepvar[-c(48,49,54,78,86,101,104,109,123,137,142,146,149,155,174,176),]
indepvar <- data.frame(indepvar)

####################################################
# Bayesian Hierarchical Approach
mdl <- "
  model {
for (i in 1:N){
y1[i] ~ dnorm(y.hat1[i],tau)
y.hat1[i] <- b1.0 + b1.1*x1[i] + b1.2*x2[i] + b1.3*x3[i] + b1.4*x4[i] + b1.5*x5[i] + b1.6*x6[i] + b1.7*x7[i] + b1.8*x8[i] + b1.9*x9[i] + b1.10*x10[i]
y2[i] ~ dnorm(y.hat2[i],tau)
y.hat2[i] <- b2.0 + b2.1*x1[i] + b2.2*x2[i] + b2.3*x3[i] + b2.4*x4[i] + b2.5*x5[i] + b2.6*x6[i] + b2.7*x7[i] + b2.8*x8[i] + b2.9*x9[i] + b2.10*x10[i]
y3[i] ~ dnorm(y.hat3[i],tau)
y.hat3[i] <- b3.0 + b3.1*x1[i] + b3.2*x2[i] + b3.3*x3[i] + b3.4*x4[i] + b3.5*x5[i] + b3.6*x6[i] + b3.7*x7[i] + b3.8*x8[i] + b3.9*x9[i] + b3.10*x10[i]
y4[i] ~ dnorm(y.hat4[i],tau)
y.hat4[i] <- b4.0 + b4.1*x1[i] + b4.2*x2[i] + b4.3*x3[i] + b4.4*x4[i] + b4.5*x5[i] + b4.6*x6[i] + b4.7*x7[i] + b4.8*x8[i] + b4.9*x9[i] + b4.10*x10[i]
y5[i] ~ dnorm(y.hat5[i],tau)
y.hat5[i] <- b5.0 + b5.1*x1[i] + b5.2*x2[i] + b5.3*x3[i] + b5.4*x4[i] + b5.5*x5[i] + b5.6*x6[i] + b5.7*x7[i] + b5.8*x8[i] + b5.9*x9[i] + b5.10*x10[i]
y6[i] ~ dnorm(y.hat6[i],tau)
y.hat6[i] <- b6.0 + b6.1*x1[i] + b6.2*x2[i] + b6.3*x3[i] + b6.4*x4[i] + b6.5*x5[i] + b6.6*x6[i] + b6.7*x7[i] + b6.8*x8[i] + b6.9*x9[i] + b6.10*x10[i]
y7[i] ~ dnorm(y.hat7[i],tau)
y.hat7[i] <- b7.0 + b7.1*x1[i] + b7.2*x2[i] + b7.3*x3[i] + b7.4*x4[i] + b7.5*x5[i] + b7.6*x6[i] + b7.7*x7[i] + b7.8*x8[i] + b7.9*x9[i] + b7.10*x10[i]
y8[i] ~ dnorm(y.hat8[i],tau)
y.hat8[i] <- b8.0 + b8.1*x1[i] + b8.2*x2[i] + b8.3*x3[i] + b8.4*x4[i] + b8.5*x5[i] + b8.6*x6[i] + b8.7*x7[i] + b8.8*x8[i] + b8.9*x9[i] + b8.10*x10[i]
y9[i] ~ dnorm(y.hat9[i],tau)
y.hat9[i] <- b9.0 + b9.1*x1[i] + b9.2*x2[i] + b9.3*x3[i] + b9.4*x4[i] + b9.5*x5[i] + b9.6*x6[i] + b9.7*x7[i] + b9.8*x8[i] + b9.9*x9[i] + b9.10*x10[i]
y10[i] ~ dnorm(y.hat10[i],tau)
y.hat10[i] <- b10.0 + b10.1*x1[i] + b10.2*x2[i] + b10.3*x3[i] + b10.4*x4[i] + b10.5*x5[i] + b10.6*x6[i] + b10.7*x7[i] + b10.8*x8[i] + b10.9*x9[i] + b10.10*x10[i]
y11[i] ~ dnorm(y.hat11[i],tau)
y.hat11[i] <- b11.0 + b11.1*x1[i] + b11.2*x2[i] + b11.3*x3[i] + b11.4*x4[i] + b11.5*x5[i] + b11.6*x6[i] + b11.7*x7[i] + b11.8*x8[i] + b11.9*x9[i] + b11.10*x10[i]
y12[i] ~ dnorm(y.hat12[i],tau)
y.hat12[i] <- b12.0 + b12.1*x1[i] + b12.2*x2[i] + b12.3*x3[i] + b12.4*x4[i] + b12.5*x5[i] + b12.6*x6[i] + b12.7*x7[i] + b12.8*x8[i] + b12.9*x9[i] + b12.10*x10[i]
y13[i] ~ dnorm(y.hat13[i],tau)
y.hat13[i] <- b13.0 + b13.1*x1[i] + b13.2*x2[i] + b13.3*x3[i] + b13.4*x4[i] + b13.5*x5[i] + b13.6*x6[i] + b13.7*x7[i] + b13.8*x8[i] + b13.9*x9[i] + b13.10*x10[i]
y14[i] ~ dnorm(y.hat14[i],tau)
y.hat14[i] <- b14.0 + b14.1*x1[i] + b14.2*x2[i] + b14.3*x3[i] + b14.4*x4[i] + b14.5*x5[i] + b14.6*x6[i] + b14.7*x7[i] + b14.8*x8[i] + b14.9*x9[i] + b14.10*x10[i]
y15[i] ~ dnorm(y.hat15[i],tau)
y.hat15[i] <- b15.0 + b15.1*x1[i] + b15.2*x2[i] + b15.3*x3[i] + b15.4*x4[i] + b15.5*x5[i] + b15.6*x6[i] + b15.7*x7[i] + b15.8*x8[i] + b15.9*x9[i] + b15.10*x10[i]
y16[i] ~ dnorm(y.hat16[i],tau)
y.hat16[i] <- b16.0 + b16.1*x1[i] + b16.2*x2[i] + b16.3*x3[i] + b16.4*x4[i] + b16.5*x5[i] + b16.6*x6[i] + b16.7*x7[i] + b16.8*x8[i] + b16.9*x9[i] + b16.10*x10[i]
y17[i] ~ dnorm(y.hat17[i],tau)
y.hat17[i] <- b17.0 + b17.1*x1[i] + b17.2*x2[i] + b17.3*x3[i] + b17.4*x4[i] + b17.5*x5[i] + b17.6*x6[i] + b17.7*x7[i] + b17.8*x8[i] + b17.9*x9[i] + b17.10*x10[i]
y18[i] ~ dnorm(y.hat18[i],tau)
y.hat18[i] <- b18.0 + b18.1*x1[i] + b18.2*x2[i] + b18.3*x3[i] + b18.4*x4[i] + b18.5*x5[i] + b18.6*x6[i] + b18.7*x7[i] + b18.8*x8[i] + b18.9*x9[i] + b18.10*x10[i]
y19[i] ~ dnorm(y.hat19[i],tau)
y.hat19[i] <- b19.0 + b19.1*x1[i] + b19.2*x2[i] + b19.3*x3[i] + b19.4*x4[i] + b19.5*x5[i] + b19.6*x6[i] + b19.7*x7[i] + b19.8*x8[i] + b19.9*x9[i] + b19.10*x10[i]
y20[i] ~ dnorm(y.hat20[i],tau)
y.hat20[i] <- b20.0 + b20.1*x1[i] + b20.2*x2[i] + b20.3*x3[i] + b20.4*x4[i] + b20.5*x5[i] + b20.6*x6[i] + b20.7*x7[i] + b20.8*x8[i] + b20.9*x9[i] + b20.10*x10[i]
y21[i] ~ dnorm(y.hat21[i],tau)
y.hat21[i] <- b21.0 + b21.1*x1[i] + b21.2*x2[i] + b21.3*x3[i] + b21.4*x4[i] + b21.5*x5[i] + b21.6*x6[i] + b21.7*x7[i] + b21.8*x8[i] + b21.9*x9[i] + b21.10*x10[i]
y22[i] ~ dnorm(y.hat22[i],tau)
y.hat22[i] <- b22.0 + b22.1*x1[i] + b22.2*x2[i] + b22.3*x3[i] + b22.4*x4[i] + b22.5*x5[i] + b22.6*x6[i] + b22.7*x7[i] + b22.8*x8[i] + b22.9*x9[i] + b22.10*x10[i]
y23[i] ~ dnorm(y.hat23[i],tau)
y.hat23[i] <- b23.0 + b23.1*x1[i] + b23.2*x2[i] + b23.3*x3[i] + b23.4*x4[i] + b23.5*x5[i] + b23.6*x6[i] + b23.7*x7[i] + b23.8*x8[i] + b23.9*x9[i] + b23.10*x10[i]
y24[i] ~ dnorm(y.hat24[i],tau)
y.hat24[i] <- b24.0 + b24.1*x1[i] + b24.2*x2[i] + b24.3*x3[i] + b24.4*x4[i] + b24.5*x5[i] + b24.6*x6[i] + b24.7*x7[i] + b24.8*x8[i] + b24.9*x9[i] + b24.10*x10[i]
y25[i] ~ dnorm(y.hat25[i],tau)
y.hat25[i] <- b25.0 + b25.1*x1[i] + b25.2*x2[i] + b25.3*x3[i] + b25.4*x4[i] + b25.5*x5[i] + b25.6*x6[i] + b25.7*x7[i] + b25.8*x8[i] + b25.9*x9[i] + b25.10*x10[i]
y26[i] ~ dnorm(y.hat26[i],tau)
y.hat26[i] <- b26.0 + b26.1*x1[i] + b26.2*x2[i] + b26.3*x3[i] + b26.4*x4[i] + b26.5*x5[i] + b26.6*x6[i] + b26.7*x7[i] + b26.8*x8[i] + b26.9*x9[i] + b26.10*x10[i]
y27[i] ~ dnorm(y.hat27[i],tau)
y.hat27[i] <- b27.0 + b27.1*x1[i] + b27.2*x2[i] + b27.3*x3[i] + b27.4*x4[i] + b27.5*x5[i] + b27.6*x6[i] + b27.7*x7[i] + b27.8*x8[i] + b27.9*x9[i] + b27.10*x10[i]
y28[i] ~ dnorm(y.hat28[i],tau)
y.hat28[i] <- b28.0 + b28.1*x1[i] + b28.2*x2[i] + b28.3*x3[i] + b28.4*x4[i] + b28.5*x5[i] + b28.6*x6[i] + b28.7*x7[i] + b28.8*x8[i] + b28.9*x9[i] + b28.10*x10[i]
y29[i] ~ dnorm(y.hat29[i],tau)
y.hat29[i] <- b29.0 + b29.1*x1[i] + b29.2*x2[i] + b29.3*x3[i] + b29.4*x4[i] + b29.5*x5[i] + b29.6*x6[i] + b29.7*x7[i] + b29.8*x8[i] + b29.9*x9[i] + b29.10*x10[i]
}

b1.0 ~ dnorm(0,1)
b1.1 ~ dnorm(mu[1],lambda[1])
b1.2 ~ dnorm(mu[2],lambda[2])
b1.3 ~ dnorm(mu[3],lambda[3])
b1.4 ~ dnorm(mu[4],lambda[4])
b1.5 ~ dnorm(mu[5],lambda[5])
b1.6 ~ dnorm(mu[6],lambda[6])
b1.7 ~ dnorm(mu[7],lambda[7])
b1.8 ~ dnorm(mu[8],lambda[8])
b1.9 ~ dnorm(mu[9],lambda[9])
b1.10 ~ dnorm(mu[10],lambda[10])
b2.0 ~ dnorm(0,1)
b2.1 ~ dnorm(mu[1],lambda[1])
b2.2 ~ dnorm(mu[2],lambda[2])
b2.3 ~ dnorm(mu[3],lambda[3])
b2.4 ~ dnorm(mu[4],lambda[4])
b2.5 ~ dnorm(mu[5],lambda[5])
b2.6 ~ dnorm(mu[6],lambda[6])
b2.7 ~ dnorm(mu[7],lambda[7])
b2.8 ~ dnorm(mu[8],lambda[8])
b2.9 ~ dnorm(mu[9],lambda[9])
b2.10 ~ dnorm(mu[10],lambda[10])
b3.0 ~ dnorm(0,1)
b3.1 ~ dnorm(mu[1],lambda[1])
b3.2 ~ dnorm(mu[2],lambda[2])
b3.3 ~ dnorm(mu[3],lambda[3])
b3.4 ~ dnorm(mu[4],lambda[4])
b3.5 ~ dnorm(mu[5],lambda[5])
b3.6 ~ dnorm(mu[6],lambda[6])
b3.7 ~ dnorm(mu[7],lambda[7])
b3.8 ~ dnorm(mu[8],lambda[8])
b3.9 ~ dnorm(mu[9],lambda[9])
b3.10 ~ dnorm(mu[10],lambda[10])
b4.0 ~ dnorm(0,1)
b4.1 ~ dnorm(mu[1],lambda[1])
b4.2 ~ dnorm(mu[2],lambda[2])
b4.3 ~ dnorm(mu[3],lambda[3])
b4.4 ~ dnorm(mu[4],lambda[4])
b4.5 ~ dnorm(mu[5],lambda[5])
b4.6 ~ dnorm(mu[6],lambda[6])
b4.7 ~ dnorm(mu[7],lambda[7])
b4.8 ~ dnorm(mu[8],lambda[8])
b4.9 ~ dnorm(mu[9],lambda[9])
b4.10 ~ dnorm(mu[10],lambda[10])
b5.0 ~ dnorm(0,1)
b5.1 ~ dnorm(mu[1],lambda[1])
b5.2 ~ dnorm(mu[2],lambda[2])
b5.3 ~ dnorm(mu[3],lambda[3])
b5.4 ~ dnorm(mu[4],lambda[4])
b5.5 ~ dnorm(mu[5],lambda[5])
b5.6 ~ dnorm(mu[6],lambda[6])
b5.7 ~ dnorm(mu[7],lambda[7])
b5.8 ~ dnorm(mu[8],lambda[8])
b5.9 ~ dnorm(mu[9],lambda[9])
b5.10 ~ dnorm(mu[10],lambda[10])
b6.0 ~ dnorm(0,1)
b6.1 ~ dnorm(mu[1],lambda[1])
b6.2 ~ dnorm(mu[2],lambda[2])
b6.3 ~ dnorm(mu[3],lambda[3])
b6.4 ~ dnorm(mu[4],lambda[4])
b6.5 ~ dnorm(mu[5],lambda[5])
b6.6 ~ dnorm(mu[6],lambda[6])
b6.7 ~ dnorm(mu[7],lambda[7])
b6.8 ~ dnorm(mu[8],lambda[8])
b6.9 ~ dnorm(mu[9],lambda[9])
b6.10 ~ dnorm(mu[10],lambda[10])
b7.0 ~ dnorm(0,1)
b7.1 ~ dnorm(mu[1],lambda[1])
b7.2 ~ dnorm(mu[2],lambda[2])
b7.3 ~ dnorm(mu[3],lambda[3])
b7.4 ~ dnorm(mu[4],lambda[4])
b7.5 ~ dnorm(mu[5],lambda[5])
b7.6 ~ dnorm(mu[6],lambda[6])
b7.7 ~ dnorm(mu[7],lambda[7])
b7.8 ~ dnorm(mu[8],lambda[8])
b7.9 ~ dnorm(mu[9],lambda[9])
b7.10 ~ dnorm(mu[10],lambda[10])
b8.0 ~ dnorm(0,1)
b8.1 ~ dnorm(mu[1],lambda[1])
b8.2 ~ dnorm(mu[2],lambda[2])
b8.3 ~ dnorm(mu[3],lambda[3])
b8.4 ~ dnorm(mu[4],lambda[4])
b8.5 ~ dnorm(mu[5],lambda[5])
b8.6 ~ dnorm(mu[6],lambda[6])
b8.7 ~ dnorm(mu[7],lambda[7])
b8.8 ~ dnorm(mu[8],lambda[8])
b8.9 ~ dnorm(mu[9],lambda[9])
b8.10 ~ dnorm(mu[10],lambda[10])
b9.0 ~ dnorm(0,1)
b9.1 ~ dnorm(mu[1],lambda[1])
b9.2 ~ dnorm(mu[2],lambda[2])
b9.3 ~ dnorm(mu[3],lambda[3])
b9.4 ~ dnorm(mu[4],lambda[4])
b9.5 ~ dnorm(mu[5],lambda[5])
b9.6 ~ dnorm(mu[6],lambda[6])
b9.7 ~ dnorm(mu[7],lambda[7])
b9.8 ~ dnorm(mu[8],lambda[8])
b9.9 ~ dnorm(mu[9],lambda[9])
b9.10 ~ dnorm(mu[10],lambda[10])
b10.0 ~ dnorm(0,1)
b10.1 ~ dnorm(mu[1],lambda[1])
b10.2 ~ dnorm(mu[2],lambda[2])
b10.3 ~ dnorm(mu[3],lambda[3])
b10.4 ~ dnorm(mu[4],lambda[4])
b10.5 ~ dnorm(mu[5],lambda[5])
b10.6 ~ dnorm(mu[6],lambda[6])
b10.7 ~ dnorm(mu[7],lambda[7])
b10.8 ~ dnorm(mu[8],lambda[8])
b10.9 ~ dnorm(mu[9],lambda[9])
b10.10 ~ dnorm(mu[10],lambda[10])
b11.0 ~ dnorm(0,1)
b11.1 ~ dnorm(mu[1],lambda[1])
b11.2 ~ dnorm(mu[2],lambda[2])
b11.3 ~ dnorm(mu[3],lambda[3])
b11.4 ~ dnorm(mu[4],lambda[4])
b11.5 ~ dnorm(mu[5],lambda[5])
b11.6 ~ dnorm(mu[6],lambda[6])
b11.7 ~ dnorm(mu[7],lambda[7])
b11.8 ~ dnorm(mu[8],lambda[8])
b11.9 ~ dnorm(mu[9],lambda[9])
b11.10 ~ dnorm(mu[10],lambda[10])
b12.0 ~ dnorm(0,1)
b12.1 ~ dnorm(mu[1],lambda[1])
b12.2 ~ dnorm(mu[2],lambda[2])
b12.3 ~ dnorm(mu[3],lambda[3])
b12.4 ~ dnorm(mu[4],lambda[4])
b12.5 ~ dnorm(mu[5],lambda[5])
b12.6 ~ dnorm(mu[6],lambda[6])
b12.7 ~ dnorm(mu[7],lambda[7])
b12.8 ~ dnorm(mu[8],lambda[8])
b12.9 ~ dnorm(mu[9],lambda[9])
b12.10 ~ dnorm(mu[10],lambda[10])
b13.0 ~ dnorm(0,1)
b13.1 ~ dnorm(mu[1],lambda[1])
b13.2 ~ dnorm(mu[2],lambda[2])
b13.3 ~ dnorm(mu[3],lambda[3])
b13.4 ~ dnorm(mu[4],lambda[4])
b13.5 ~ dnorm(mu[5],lambda[5])
b13.6 ~ dnorm(mu[6],lambda[6])
b13.7 ~ dnorm(mu[7],lambda[7])
b13.8 ~ dnorm(mu[8],lambda[8])
b13.9 ~ dnorm(mu[9],lambda[9])
b13.10 ~ dnorm(mu[10],lambda[10])
b14.0 ~ dnorm(0,1)
b14.1 ~ dnorm(mu[1],lambda[1])
b14.2 ~ dnorm(mu[2],lambda[2])
b14.3 ~ dnorm(mu[3],lambda[3])
b14.4 ~ dnorm(mu[4],lambda[4])
b14.5 ~ dnorm(mu[5],lambda[5])
b14.6 ~ dnorm(mu[6],lambda[6])
b14.7 ~ dnorm(mu[7],lambda[7])
b14.8 ~ dnorm(mu[8],lambda[8])
b14.9 ~ dnorm(mu[9],lambda[9])
b14.10 ~ dnorm(mu[10],lambda[10])
b15.0 ~ dnorm(0,1)
b15.1 ~ dnorm(mu[1],lambda[1])
b15.2 ~ dnorm(mu[2],lambda[2])
b15.3 ~ dnorm(mu[3],lambda[3])
b15.4 ~ dnorm(mu[4],lambda[4])
b15.5 ~ dnorm(mu[5],lambda[5])
b15.6 ~ dnorm(mu[6],lambda[6])
b15.7 ~ dnorm(mu[7],lambda[7])
b15.8 ~ dnorm(mu[8],lambda[8])
b15.9 ~ dnorm(mu[9],lambda[9])
b15.10 ~ dnorm(mu[10],lambda[10])
b16.0 ~ dnorm(0,1)
b16.1 ~ dnorm(mu[1],lambda[1])
b16.2 ~ dnorm(mu[2],lambda[2])
b16.3 ~ dnorm(mu[3],lambda[3])
b16.4 ~ dnorm(mu[4],lambda[4])
b16.5 ~ dnorm(mu[5],lambda[5])
b16.6 ~ dnorm(mu[6],lambda[6])
b16.7 ~ dnorm(mu[7],lambda[7])
b16.8 ~ dnorm(mu[8],lambda[8])
b16.9 ~ dnorm(mu[9],lambda[9])
b16.10 ~ dnorm(mu[10],lambda[10])
b17.0 ~ dnorm(0,1)
b17.1 ~ dnorm(mu[1],lambda[1])
b17.2 ~ dnorm(mu[2],lambda[2])
b17.3 ~ dnorm(mu[3],lambda[3])
b17.4 ~ dnorm(mu[4],lambda[4])
b17.5 ~ dnorm(mu[5],lambda[5])
b17.6 ~ dnorm(mu[6],lambda[6])
b17.7 ~ dnorm(mu[7],lambda[7])
b17.8 ~ dnorm(mu[8],lambda[8])
b17.9 ~ dnorm(mu[9],lambda[9])
b17.10 ~ dnorm(mu[10],lambda[10])
b18.0 ~ dnorm(0,1)
b18.1 ~ dnorm(mu[1],lambda[1])
b18.2 ~ dnorm(mu[2],lambda[2])
b18.3 ~ dnorm(mu[3],lambda[3])
b18.4 ~ dnorm(mu[4],lambda[4])
b18.5 ~ dnorm(mu[5],lambda[5])
b18.6 ~ dnorm(mu[6],lambda[6])
b18.7 ~ dnorm(mu[7],lambda[7])
b18.8 ~ dnorm(mu[8],lambda[8])
b18.9 ~ dnorm(mu[9],lambda[9])
b18.10 ~ dnorm(mu[10],lambda[10])
b19.0 ~ dnorm(0,1)
b19.1 ~ dnorm(mu[1],lambda[1])
b19.2 ~ dnorm(mu[2],lambda[2])
b19.3 ~ dnorm(mu[3],lambda[3])
b19.4 ~ dnorm(mu[4],lambda[4])
b19.5 ~ dnorm(mu[5],lambda[5])
b19.6 ~ dnorm(mu[6],lambda[6])
b19.7 ~ dnorm(mu[7],lambda[7])
b19.8 ~ dnorm(mu[8],lambda[8])
b19.9 ~ dnorm(mu[9],lambda[9])
b19.10 ~ dnorm(mu[10],lambda[10])
b20.0 ~ dnorm(0,1)
b20.1 ~ dnorm(mu[1],lambda[1])
b20.2 ~ dnorm(mu[2],lambda[2])
b20.3 ~ dnorm(mu[3],lambda[3])
b20.4 ~ dnorm(mu[4],lambda[4])
b20.5 ~ dnorm(mu[5],lambda[5])
b20.6 ~ dnorm(mu[6],lambda[6])
b20.7 ~ dnorm(mu[7],lambda[7])
b20.8 ~ dnorm(mu[8],lambda[8])
b20.9 ~ dnorm(mu[9],lambda[9])
b20.10 ~ dnorm(mu[10],lambda[10])
b21.0 ~ dnorm(0,1)
b21.1 ~ dnorm(mu[1],lambda[1])
b21.2 ~ dnorm(mu[2],lambda[2])
b21.3 ~ dnorm(mu[3],lambda[3])
b21.4 ~ dnorm(mu[4],lambda[4])
b21.5 ~ dnorm(mu[5],lambda[5])
b21.6 ~ dnorm(mu[6],lambda[6])
b21.7 ~ dnorm(mu[7],lambda[7])
b21.8 ~ dnorm(mu[8],lambda[8])
b21.9 ~ dnorm(mu[9],lambda[9])
b21.10 ~ dnorm(mu[10],lambda[10])
b22.0 ~ dnorm(0,1)
b22.1 ~ dnorm(mu[1],lambda[1])
b22.2 ~ dnorm(mu[2],lambda[2])
b22.3 ~ dnorm(mu[3],lambda[3])
b22.4 ~ dnorm(mu[4],lambda[4])
b22.5 ~ dnorm(mu[5],lambda[5])
b22.6 ~ dnorm(mu[6],lambda[6])
b22.7 ~ dnorm(mu[7],lambda[7])
b22.8 ~ dnorm(mu[8],lambda[8])
b22.9 ~ dnorm(mu[9],lambda[9])
b22.10 ~ dnorm(mu[10],lambda[10])
b23.0 ~ dnorm(0,1)
b23.1 ~ dnorm(mu[1],lambda[1])
b23.2 ~ dnorm(mu[2],lambda[2])
b23.3 ~ dnorm(mu[3],lambda[3])
b23.4 ~ dnorm(mu[4],lambda[4])
b23.5 ~ dnorm(mu[5],lambda[5])
b23.6 ~ dnorm(mu[6],lambda[6])
b23.7 ~ dnorm(mu[7],lambda[7])
b23.8 ~ dnorm(mu[8],lambda[8])
b23.9 ~ dnorm(mu[9],lambda[9])
b23.10 ~ dnorm(mu[10],lambda[10])
b24.0 ~ dnorm(0,1)
b24.1 ~ dnorm(mu[1],lambda[1])
b24.2 ~ dnorm(mu[2],lambda[2])
b24.3 ~ dnorm(mu[3],lambda[3])
b24.4 ~ dnorm(mu[4],lambda[4])
b24.5 ~ dnorm(mu[5],lambda[5])
b24.6 ~ dnorm(mu[6],lambda[6])
b24.7 ~ dnorm(mu[7],lambda[7])
b24.8 ~ dnorm(mu[8],lambda[8])
b24.9 ~ dnorm(mu[9],lambda[9])
b24.10 ~ dnorm(mu[10],lambda[10])
b25.0 ~ dnorm(0,1)
b25.1 ~ dnorm(mu[1],lambda[1])
b25.2 ~ dnorm(mu[2],lambda[2])
b25.3 ~ dnorm(mu[3],lambda[3])
b25.4 ~ dnorm(mu[4],lambda[4])
b25.5 ~ dnorm(mu[5],lambda[5])
b25.6 ~ dnorm(mu[6],lambda[6])
b25.7 ~ dnorm(mu[7],lambda[7])
b25.8 ~ dnorm(mu[8],lambda[8])
b25.9 ~ dnorm(mu[9],lambda[9])
b25.10 ~ dnorm(mu[10],lambda[10])
b26.0 ~ dnorm(0,1)
b26.1 ~ dnorm(mu[1],lambda[1])
b26.2 ~ dnorm(mu[2],lambda[2])
b26.3 ~ dnorm(mu[3],lambda[3])
b26.4 ~ dnorm(mu[4],lambda[4])
b26.5 ~ dnorm(mu[5],lambda[5])
b26.6 ~ dnorm(mu[6],lambda[6])
b26.7 ~ dnorm(mu[7],lambda[7])
b26.8 ~ dnorm(mu[8],lambda[8])
b26.9 ~ dnorm(mu[9],lambda[9])
b26.10 ~ dnorm(mu[10],lambda[10])
b27.0 ~ dnorm(0,1)
b27.1 ~ dnorm(mu[1],lambda[1])
b27.2 ~ dnorm(mu[2],lambda[2])
b27.3 ~ dnorm(mu[3],lambda[3])
b27.4 ~ dnorm(mu[4],lambda[4])
b27.5 ~ dnorm(mu[5],lambda[5])
b27.6 ~ dnorm(mu[6],lambda[6])
b27.7 ~ dnorm(mu[7],lambda[7])
b27.8 ~ dnorm(mu[8],lambda[8])
b27.9 ~ dnorm(mu[9],lambda[9])
b27.10 ~ dnorm(mu[10],lambda[10])
b28.0 ~ dnorm(0,1)
b28.1 ~ dnorm(mu[1],lambda[1])
b28.2 ~ dnorm(mu[2],lambda[2])
b28.3 ~ dnorm(mu[3],lambda[3])
b28.4 ~ dnorm(mu[4],lambda[4])
b28.5 ~ dnorm(mu[5],lambda[5])
b28.6 ~ dnorm(mu[6],lambda[6])
b28.7 ~ dnorm(mu[7],lambda[7])
b28.8 ~ dnorm(mu[8],lambda[8])
b28.9 ~ dnorm(mu[9],lambda[9])
b28.10 ~ dnorm(mu[10],lambda[10])
b29.0 ~ dnorm(0,1)
b29.1 ~ dnorm(mu[1],lambda[1])
b29.2 ~ dnorm(mu[2],lambda[2])
b29.3 ~ dnorm(mu[3],lambda[3])
b29.4 ~ dnorm(mu[4],lambda[4])
b29.5 ~ dnorm(mu[5],lambda[5])
b29.6 ~ dnorm(mu[6],lambda[6])
b29.7 ~ dnorm(mu[7],lambda[7])
b29.8 ~ dnorm(mu[8],lambda[8])
b29.9 ~ dnorm(mu[9],lambda[9])
b29.10 ~ dnorm(mu[10],lambda[10])

sigma2 ~ dunif(0,1)
tau <- 1/sigma2

for(j in 1:10){
s[j] ~ dunif(0, 2)
lambda[j] <- 1 / ( s[j]) 
mu[j] ~ dnorm(0,1)
}
}
"

library(R2jags)

writeLines(mdl,'hiermod.txt')

x1 <- indepvar$CIV1
x2 <- indepvar$CIV2
x3 <- indepvar$CIV4
x4 <- indepvar$ColStatus
x5 <- indepvar$RelFrac
x6 <- indepvar$EthnicFrac
x7 <- indepvar$Urbanization
x8 <- indepvar$Terrain
x9 <- indepvar$LandNeighbors
x10 <- indepvar$Syndrome

N <- nrow(stresponse)
k <- length(names(stresponse))
for(i in 1:k){
  assign(paste("y",i,sep=""),stresponse[,i])
}

data.jags <- c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10',
               'y1','y2','y3','y4','y5','y6','y7','y8','y9','y10',
               'y11','y12','y13','y14','y15','y16','y17','y18','y19','y20',
               'y21','y22','y23','y24','y25','y26','y27','y28','y29',
               'N')
parms <- c('mu','lambda','tau',
           'b1.0','b1.1','b1.2','b1.3','b1.4','b1.5','b1.6','b1.7','b1.8','b1.9','b1.10',
           'b2.0','b2.1','b2.2','b2.3','b2.4','b2.5','b2.6','b2.7','b2.8','b2.9','b2.10',
           'b3.0','b3.1','b3.2','b3.3','b3.4','b3.5','b3.6','b3.7','b3.8','b3.9','b3.10',
           'b4.0','b4.1','b4.2','b4.3','b4.4','b4.5','b4.6','b4.7','b4.8','b4.9','b4.10',
           'b5.0','b5.1','b5.2','b5.3','b5.4','b5.5','b5.6','b5.7','b5.8','b5.9','b5.10',
           'b6.0','b6.1','b6.2','b6.3','b6.4','b6.5','b6.6','b6.7','b6.8','b6.9','b6.10',
           'b7.0','b7.1','b7.2','b7.3','b7.4','b7.5','b7.6','b7.7','b7.8','b7.9','b7.10',
           'b8.0','b8.1','b8.2','b8.3','b8.4','b8.5','b8.6','b8.7','b8.8','b8.9','b8.10',
           'b9.0','b9.1','b9.2','b9.3','b9.4','b9.5','b9.6','b9.7','b9.8','b9.9','b9.10',
           'b10.0','b10.1','b10.2','b10.3','b10.4','b10.5','b10.6','b10.7','b10.8','b10.9','b10.10',
           'b11.0','b11.1','b11.2','b11.3','b11.4','b11.5','b11.6','b11.7','b11.8','b11.9','b11.10',
           'b12.0','b12.1','b12.2','b12.3','b12.4','b12.5','b12.6','b12.7','b12.8','b12.9','b12.10',
           'b13.0','b13.1','b13.2','b13.3','b13.4','b13.5','b13.6','b13.7','b13.8','b13.9','b13.10',
           'b14.0','b14.1','b14.2','b14.3','b14.4','b14.5','b14.6','b14.7','b14.8','b14.9','b14.10',
           'b15.0','b15.1','b15.2','b15.3','b15.4','b15.5','b15.6','b15.7','b15.8','b15.9','b15.10',
           'b16.0','b16.1','b16.2','b16.3','b16.4','b16.5','b16.6','b16.7','b16.8','b16.9','b16.10',
           'b17.0','b17.1','b17.2','b17.3','b17.4','b17.5','b17.6','b17.7','b17.8','b17.9','b17.10',
           'b18.0','b18.1','b18.2','b18.3','b18.4','b18.5','b18.6','b18.7','b18.8','b18.9','b18.10',
           'b19.0','b19.1','b19.2','b19.3','b19.4','b19.5','b19.6','b19.7','b19.8','b19.9','b19.10',
           'b20.0','b20.1','b20.2','b20.3','b20.4','b20.5','b20.6','b20.7','b20.8','b20.9','b20.10',
           'b21.0','b21.1','b21.2','b21.3','b21.4','b21.5','b21.6','b21.7','b21.8','b21.9','b21.10',
           'b22.0','b22.1','b22.2','b22.3','b22.4','b22.5','b22.6','b22.7','b22.8','b22.9','b22.10',
           'b23.0','b23.1','b23.2','b23.3','b23.4','b23.5','b23.6','b23.7','b23.8','b23.9','b23.10',
           'b24.0','b24.1','b24.2','b24.3','b24.4','b24.5','b24.6','b24.7','b24.8','b24.9','b24.10',
           'b25.0','b25.1','b25.2','b25.3','b25.4','b25.5','b25.6','b25.7','b25.8','b25.9','b25.10',
           'b26.0','b26.1','b26.2','b26.3','b26.4','b26.5','b26.6','b26.7','b26.8','b26.9','b26.10',
           'b27.0','b27.1','b27.2','b27.3','b27.4','b27.5','b27.6','b27.7','b27.8','b27.9','b27.10',
           'b28.0','b28.1','b28.2','b28.3','b28.4','b28.5','b28.6','b28.7','b28.8','b28.9','b28.10',
           'b29.0','b29.1','b29.2','b29.3','b29.4','b29.5','b29.6','b29.7','b29.8','b29.9','b29.10'           
           )

mod.sim <- jags(data=data.jags, inits=NULL,
                parameters.to.save=parms,
                model.file='hiermod.txt',n.iter=12000,
                n.burnin=2000,n.chains=5,n.thin=5)

sim <- as.mcmc(mod.sim) # gives you the chains
ess <- effectiveSize(sim)
chains <- as.matrix(sim)
sims <- as.mcmc(chains)
R2 <- raftery.diag(sims) # good to check if you want to make inference out on the tails, want them to all be about 1
autocor <- autocorr.diag(sims)
DIC <- mod.sim$BUGSoutput$DIC
est <- mod.sim$BUGSoutput$summary[,1]

syn.vals <- seq(from=3,to=319,by=11)
syn.vals <- syn.vals[c(1,12,23:29,2:11,13:22)]

syn.est <- mod.sim$BUGSoutput$summary[syn.vals,1]
syn.2.5 <- mod.sim$BUGSoutput$summary[syn.vals,3]
syn.97.5 <- mod.sim$BUGSoutput$summary[syn.vals,7]

library(xtable)
xtable(round(cbind(syn.est,syn.2.5,syn.97.5),3))

convplots.names <- c("Female Life Expectancy","Prenatal Care","Birth Skilled Staff","Births of Women Ages 15-19",
                     "Risk of Maternal Death", "Preventable Death (Women)","Prevalence of HIV (Women)",
                     "Stunted (Children)", "Infant Mortality Rate", "Wasting (Children)","Underweight (Children)","Diarrhea Deaths (Children)",
                     "Measles Immunization (Children)",
                     "Open Defecation","Urban Open Defecation","Life Expectancy","Health Expenditure (% GDP)",
                     "Health Expenditure (per Capita)","Tuberculosis","HIV","HIV/AIDS","Undernourishment",
                     "Alcohol Consumption","Cigarette Consumption","Access to Water","Access to Sanitation",
                     "Global Hunger Index","Human Wellbeing","Energy Supply Adequacy")

# Convergence Plots
pdf("convplots.pdf", width=15, height=18)
par(mfrow=c(6,5))
for(i in 1:29){
  plot(chains[,syn.vals[i]],type="l", main=convplots.names[i], xlab="",ylab="MCMC draws")
}
dev.off()


pdf("convplotss2.pdf", width=12, height=9)
par(mfrow=c(3,4))
plot(1/chains[,341],type="l", main=expression(sigma^2), xlab="",ylab="MCMC draws")
for(i in 1:10){
  plot(1/phi2[,i],type="l", main=phi2names[i], xlab="",ylab="MCMC draws")
}
dev.off()

phi2 <- cbind(chains[,c(321,323:330,322)])
phi2names <- c(expression(phi[1]^2),expression(phi[2]^2),expression(phi[3]^2),
               expression(phi[4]^2),expression(phi[5]^2),expression(phi[6]^2),
               expression(phi[7]^2),expression(phi[8]^2),expression(phi[9]^2),
               expression(phi[10]^2))


raftery.diag(sims)$resmatrix[syn.vals,]
raftery.diag(sims,q=.975)$resmatrix[syn.vals,]
