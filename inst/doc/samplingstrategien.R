## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE) 

## ------------------------------------------------------------------------
mu<-rep(0,100) 
nu<-rep(0,100) 
x<-10 
for(i in 2:100) { 
nu[i]<-rnorm(1,mu[i-1],1) 
mu[i]<-rnorm(1,(x+nu[i])/2,1/2) 
} 

## ------------------------------------------------------------------------
par(mfrow=c(2,3)) 
plot(nu, pch=19) 
points(mu,col="blue", pch=19) 
acf(nu) 
acf(mu) 

## ------------------------------------------------------------------------
mu<-rep(0,100)
nu<-rep(0,100)
x<-10
Q<-matrix(c(2,-1,-1,1),nrow=2)
Q1<-solve(Q)
for(i in 2:100)
{
  temp<-rnorm(2)+c(x,0)
  temp<-Q1%*%temp
  nu[i]<-temp[2]
  mu[i]<-temp[1]
}

## ------------------------------------------------------------------------
par(mfrow=c(2,3))
plot(nu, pch=19)
points(mu,col="blue", pch=19)
acf(nu)
acf(mu)

