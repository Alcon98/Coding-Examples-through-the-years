### The general goal of this assignment was to model populations of species given known information
### There are predator species, prey species, with resource competition between prey species and prey preferences for predators


NumYears <- 100
step <- 0.01 
tmax <- NumYears/step
t <- 1:tmax
Years <- t*step

rmaxFo <- 0.32*step
muFo <- 0.086*step
kFo <- 1544
lambdaFo <- 7.7e-4

rmaxS <- 0.32*step
muS <- 0.159*step
kS <- 2490
lambdaS <- 2.5e-4

rmaxP <- 0.78*step
muP <- 0.019*step
kP <- 15189
lambdaP <- 7.7e-4

v <- 0.09*step

Fo <- numeric(tmax)
S <- numeric(tmax)
P <- numeric(tmax)
E <- numeric(tmax)

Fo[1] <- 1312
S[1] <- 1000
P[1] <- 13827
E[1] <- 2

Bfs <- 0.36
Bsf <- 2.76

PEF <- 8.1
PES <- 3.1


for(t in 2:tmax){
  Fo[t] <- max(0,Fo[t-1] + rmaxFo*Fo[t-1]*(1-(Fo[t-1]+(Bfs*S[t-1]))/kFo) - (muFo*((PEF*Fo[t-1])/(PEF*Fo[t-1]+PES*S[t-1]+P[t-1]))*E[t-1]*Fo[t-1]))
  S[t] <- max(0,S[t-1] + rmaxS*S[t-1]*(1-(S[t-1]+Bsf*Fo[t-1])/kS) - muS*((PES*S[t-1])/(PEF*Fo[t-1]+PES*S[t-1]+P[t-1]))*E[t-1]*S[t-1])
  P[t] <- max(0,P[t-1] + rmaxP*P[t-1]*(1-P[t-1]/kP) - muP*(P[t-1]/(PEF*Fo[t-1]+PES*S[t-1]+P[t-1]))*E[t-1]*P[t-1])
  E[t] <- max(0,E[t-1] + (((lambdaFo*muFo*PEF*Fo[t-1]^2 + lambdaS*muS*PES*S[t-1]^2 + lambdaP*muP*P[t-1]^2)*E[t-1])/(PEF*Fo[t-1]+PES*S[t-1]+P[t-1]))-(v*E[t-1]))
}

par(mfrow=c(2,2))
plot(Fo~Years,type='l',ylab='Foxes')
plot(S~Years,type='l',ylab='Skunks')
plot(P~Years,type='l',ylab='Pigs')
plot(E~Years,type='l',ylab='Eagles')

#### Other Question - 3 species system ####
NumYears <- 100
step <- 0.01 
tmax <- NumYears/step
t <- 1:tmax
Years <- t*step

rmaxFo <- 0.32*step
muFo <- 0.086*step
kFo <- 1544
lambdaFo <- 7.7e-4

rmaxS <- 0.32*step
muS <- 0.159*step
kS <- 2490
lambdaS <- 2.5e-4


v <- 0.09*step

Fo <- numeric(tmax)
S <- numeric(tmax)
E <- numeric(tmax)

Fo[1] <- 1312
S[1] <- 1000
E[1] <- 2

Bfs <- 0.36
Bsf <- 2.76


for(t in 2:tmax){
  Fo[t] <- max(0,Fo[t-1] + rmaxFo*Fo[t-1]*(1-(Fo[t-1]+(Bfs*S[t-1]))/kFo) - (muFo*((Fo[t-1])/(Fo[t-1]+S[t-1]))*E[t-1]*Fo[t-1]))
  S[t] <- max(0,S[t-1] + rmaxS*S[t-1]*(1-(S[t-1]+Bsf*Fo[t-1])/kS) - muS*((S[t-1])/(Fo[t-1]+S[t-1]))*E[t-1]*S[t-1])
  E[t] <- max(0,E[t-1] + (((lambdaFo*muFo*Fo[t-1]^2 + lambdaS*muS*S[t-1]^2)/(Fo[t-1]+S[t-1]))-(v*E[t-1])))
}

par(mfrow=c(2,2))
plot(Fo~Years,type='l',ylab='Foxes')
plot(S~Years,type='l',ylab='Skunks')
plot(E~Years,type='l',ylab='Eagles')

