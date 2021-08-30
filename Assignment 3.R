### This assignment looked at forecasting wolf populations over time, but first I had to evaluate models to determine which was most parsimonious

Density <- c(4.91,2.47,2.8,3.62,2.53,2.23,2.82,2.75,2.33,3.04,1.59)
R <- numeric(10)
R[1] <- log(Density[2]/Density[1])
for(r in 1:10){R[r]<- log(Density[r+1]/Density[r])}
R
### Q1 B ###
###RICKER START###
N <- c(4.91,2.47,2.8,3.62,2.53,2.23,2.82,2.75,2.33,3.04)
Model <- lm(R~N)
summary(Model)
### Rmax Ricker = 0.91847
n = length(N)
rmax <- 0.91847
slope <- -0.34957
K <- -rmax/slope
K
## K Ricker = 2.730562
rpredict_2 <- rmax*(1-N/K)
resid_2 <- R-rpredict_2

#Ricker MSE
sigma_2 <- sqrt(sum(resid_2^2)/n)
sigma_2
#-log-liklihood
L2 <- n*(log(sigma_2)+0.5*log(2*pi))+sum(resid_2^2/(2*sigma_2^2))
L2
#parameters
p2 <- 3

#AIC
AIC_2 <- 2*L2+2*p2*n/(n-p2-1)
AIC_2
###RICKER END###


###GEOMETRIC START###
n <- length(N)

#Residuals
resid_1 <- R-mean(R)

#Intermediate calculations
sqresid_1 <- resid_1*resid_1
sumsq_1 <- sum(sqresid_1)
sigma_1 <-sqrt(sumsq_1/n)
sigma_1
#-log-Liklihood
L1 <- n*(log(sigma_1)+0.5*log(2*pi))+sum(sqresid_1/(2*sigma_1^2))

#parameters
p1 <- 2

#AIC
AIC_1 <- 2*L1+2*p1*n/(n-p1-1)

AIC_1

AICs <- c(AIC_1,AIC_2)
dAICs <- AICs - AIC_2
dAICs
w <- exp(-dAICs)/sum(exp(-dAICs))
w

##The basis of Question 1 was to compare the Ricker model and the Geometric model for predicting future population trends
## w denotes the Akaike weight, which means the %chance its the most parsimonious model when predicting population trends
## Ricker model was determined to be superior in this instance


##################### QUESTION 2 ################################
mean(R)
NumYear <- 100
Year <- 0:NumYear
F <- numeric(100+1)
F[1] <- 1.59

plot(F~Year, type = "l", ylim = c(0,10))

Critical <- numeric(1000)

abline(h=1.41, lwd = 2, lty = 2, col = "red")

for(i in 1:1000){
  noise <- rnorm(NumYear,0,sigma_2)
  for(t in 2:(NumYear+1)){F[t] <- F[t-1]*exp(rmax*(1-F[t-1]/K)+noise[t-1])}
  lines(Year,F)
  if(min(F) < 1.41){Critical[i] <- 1}
  else{Critical[i] <- 0}
}

Critical
sum(Critical)
Percent <- (sum(Critical)/1000)*100
Percent
### The code up until here is used to forcast wolf populations 100 years into the future using the Ricker model
## Critical denotes a critical population level that will severely impact wolf population longevity (1.41 wolves/100km2)
## Percent counts the number of times the model reaches that critical population level (which is half the average density)

plot(F~Year, type = 'l', ylim = c(0,6), main='Wolf Density Trajectories in the next 100 years', ylab='Wolf Density per 100 square km')
abline(h=1.41, lwd = 2, lty = 2, col = 'green')
for(i in 1:20){
  noise <- rnorm(NumYear,0,sigma_2)
  for(t in 2:(NumYear + 1)){F[t] <- F[t-1]*exp(rmax*(1-F[t-1]/K) + noise[t-1])}
  lines(Year,F)
}
## This creates a stochastic model showing 20 potential wolf population densities over 100 years, assuming density-dependence in the population
## Ricker model was used here as well as it was determined to be the most parsimonious above