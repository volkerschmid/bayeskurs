## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)
library(bayeskurs)

## ----Fischdaten----------------------------------------------------------
data(fish, package="bayesmix")
hist(fish$fish, freq=FALSE, n=22)
lines(density(fish$fish))

## ----BMM-----------------------------------------------------------------
library(bayesmix)
model <- BMMmodel(fish, k = 4,
  initialValues = list(S0 = 2),
  priors = list(kind = "independence",
  parameter = "priorsFish", 
  hierarchical = "tau"))

## ----jags model----------------------------------------------------------
print(model)

## ----jagscontrol---------------------------------------------------------
control <- JAGScontrol(variables = c("mu", "tau", "eta",
    "S"), burn.in = 1000, n.iter = 5000, seed = 10)

## ----jagsrun-------------------------------------------------------------
z <- JAGSrun(fish, model = model, control = control)

## ----zsort---------------------------------------------------------------
zSort <- Sort(z, by = "mu")
zSort


## ----medianmodel---------------------------------------------------------
postmed<-apply(zSort$results, 2, median)
x<-seq(min(fish$fish), max(fish$fish), length=1000)
d1 <- postmed[257]*dnorm(x, postmed[261], sqrt(postmed[265]))
d2 <- postmed[258]*dnorm(x, postmed[262], sqrt(postmed[266]))
d3 <- postmed[259]*dnorm(x, postmed[263], sqrt(postmed[267]))
d4 <- postmed[260]*dnorm(x, postmed[264], sqrt(postmed[268]))
d <- d1 + d2 + d3 + d4
hist(fish$fish, freq=FALSE, n=22, ylim=c(0,max(d)))
lines(x, d)
lines(x, d1, col="grey")
lines(x, d2, col="grey")
lines(x, d3, col="grey")
lines(x, d4, col="grey")


## ----S-------------------------------------------------------------------
S <- apply(zSort$results[,1:256],2,
     bioimagetools::table.n,4, percentage=TRUE)
barplot(S, col=c("red","green","blue","orange"))

