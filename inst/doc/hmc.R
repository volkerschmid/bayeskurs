## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE-------------------------------------------------------
#install.packages('rstan')
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## ------------------------------------------------------------------------
# Simulate some data
N <- 1000
M <- 3
x <- cbind(rep(1,N), rpois(N,5), runif(N,2,3))
truebeta <- c(-14, 0, 5)
truesigma <- 1
y <- rnorm(N, x%*%truebeta, truesigma)

## ------------------------------------------------------------------------
print(limo)

## ------------------------------------------------------------------------
beta<-As.mcmc.list(limo,pars="beta")
beta0<-As.mcmc.list(limo,pars="beta[1]")
coda::traceplot(beta0)

## ------------------------------------------------------------------------
coda::densplot(beta0)

## ------------------------------------------------------------------------
  beta1<-As.mcmc.list(limo,pars="beta[2]")
  plot(beta1)

## ------------------------------------------------------------------------
  beta2<-As.mcmc.list(limo,pars="beta[3]")
  plot(beta2)

## ------------------------------------------------------------------------
coda::acfplot(beta)

## ------------------------------------------------------------------------
sigma<-As.mcmc.list(limo,pars="sigma")
plot(sigma)

## ------------------------------------------------------------------------
coda::acfplot(sigma)

## ------------------------------------------------------------------------
coda::gelman.diag(As.mcmc.list(limo))

