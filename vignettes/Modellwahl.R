## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ------------------------------------------------------------------------
fire.counts <- c(75, 88, 84, 99, 79, 68, 86, 109, 73, 85, 101, 85,
                 75, 81, 64, 77, 83, 83, 88, 83, 78, 83, 78, 80,
                 82, 90, 74, 72, 69, 72, 76, 76, 104, 86, 92, 88)
hist(fire.counts, probability=TRUE, ylim=c(0, .08))
x <- 60:110
lines(x, dpois(x, lambda=mean(fire.counts)), col="red")
lines(x, dnorm(x, mean=mean(fire.counts), sd=12), col="blue")
lines(x, dnorm(x, mean=mean(fire.counts), sd=6), col="green")
legend("topright", legend=c("M1: Poisson(theta)",
                            "M2: N(theta, 12)",
                            "M3: N(theta, 6)"),
       col=c("red", "blue", "green"), lty=1)

## ----laplace-------------------------------------------------------------
laplace <- function (logpost, mode, ...) 
{
  fit = optim(mode, logpost, gr = NULL, 
    ..., hessian = TRUE, control = list(fnscale = -1))
  mode = fit$par
  h = -solve(fit$hessian)
  p = length(mode)
  int = p/2 * log(2 * pi) + 0.5 * log(det(h)) +
  logpost(mode, ...)
  stuff = list(mode = mode, var = h, int = int,
                  converge = fit$convergence == 0)
  return(stuff)
}

## ----model1, warning=FALSE-----------------------------------------------
model.1 <- function(theta, y){
  sum(log(dpois(y, theta))) + 
    dgamma(theta, shape=280, rate=4)
}
log.pred.1 <- laplace(model.1, 80, fire.counts)$int
log.pred.1

## ----model23, warning=FALSE----------------------------------------------
model.2 <- function(theta, y){
  sum(log(dnorm(y, theta, 6))) + 
    dgamma(theta, shape=280, rate=4)
}
model.3 <- function(theta, y){
  sum(log(dnorm(y, theta, 12))) + 
    dgamma(theta, shape=280, rate=4)
}
log.pred.2 <- laplace(model.2, 80, fire.counts)$int
log.pred.3 <- laplace(model.3, 80, fire.counts)$int

## ------------------------------------------------------------------------
data.frame(Model=1:3, log.pred=c(log.pred.1, log.pred.2, log.pred.3))
exp(log.pred.1 - log.pred.3)

## ----mcmc-pre, include=FALSE, echo=FALSE, message=FALSE, cache=TRUE------
library(Matrix)
library(bayeskurs)
library(INLA)
library(tictoc)
data(Drivers)
Drivers<-Drivers[1:192,]

T=192
y <- sqrt(Drivers$y)
belt <- Drivers$belt

a.c <- a.d <- 1
b.c <- 0.0005
b.d <- 0.1
a.s <- 1/4
b.s <- 1/4

## ----mcmc1, message=FALSE, cache=TRUE------------------------------------
rw=1
source("drivers_example_mcmc.R")

## ----deviance1-----------------------------------------------------------
mu<-array(0,c(T,nr.it))
D <- rep(NA,nr.it)
sigma<-sigma2.save
for (i in 1:nr.it)
     {
        mu[,i]=alpha.save[i]+belt*beta.save[i]+
          gamma.save[,i]+delta.save[,i]
        D[i] <- -2*sum(dnorm(y, mean=mu[,i], 
                    sd=sqrt(sigma[i]),log=TRUE))
}
plot(density(D))
D.theta.hat <- -2*sum(dnorm(y,apply(mu,1,median),
                  sqrt(median(sigma)),log=TRUE))
D.hat <- median(D)
pD <- D.hat - D.theta.hat
(DIC1<-data.frame("rw"=1,"D"=D.hat,"pD"=pD,
                                "DIC"=D.hat+pD))

