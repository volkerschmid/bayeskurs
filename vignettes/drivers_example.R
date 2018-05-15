#Drivers mcmc

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

source("drivers_example_mcmc.R")

plot.ts(alpha.save)
plot.ts(beta.save)
plot.ts(t(tau.save))
plot.ts(sigma2.save)

plot(density(alpha.save))
plot(density(beta.save))
gamma<-apply(gamma.save,1,quantile,c(.025,.5,.975))
plot(gamma[2,],type="l",ylim=range(gamma),xlab="Jahr")
lines(gamma[1,],lty=2)
lines(gamma[3,],lty=2)
delta<-apply(delta.save,1,quantile,c(.025,.5,.975))
plot(delta[2,],type="l",ylim=range(delta),xlab="Monat")
lines(delta[1,],lty=2)
lines(delta[3,],lty=2)
plot(density(tau.save[1,]))
plot(density(tau.save[2,]))
plot(density(sigma2.save))
plot(y-mean(alpha)-belt*mean(beta)-gamma[2,]-delta[2,])

## Jetzt mit BayesX
#install.packages("BayesXsrc", type = "source") 


library(R2BayesX)
#bayesx.term.options(bs = "rw1")
formula.bx  = sqrt(y) ~ belt + sx(trend, bs="rw1", a=1, b=0.0005) +
  sx(seasonal, bs="season", a=1, b=0.1)

system.time(
  mcmc.bx <- bayesx(formula = formula.bx, data=Drivers, 
                   control=bayesx.control(family="gaussian",method="MCMC", 
                   hyp.prior = c(4,4), iterations=10000L, burnin=5000L))
)

plot(mcmc.bx$effects$`sx(trend)`)
plot(mcmc.bx$effects$`sx(seasonal)`)
summary(mcmc.bx)

## BayesX mit REML

# Keine Hyperparameter (!)
formula.bx2 = sqrt(y) ~ belt + sx(trend, bs="rw1") + sx(seasonal, bs="season")

system.time(
  reml.bx <- bayesx(formula = formula.bx2, data=Drivers, 
                   control=bayesx.control(family="gaussian",method="REML"))
)
plot(reml.bx$effects$`sx(trend)`)
plot(reml.bx$effects$`sx(seasonal)`)
summary(reml.bx)

## Integrated Nested Laplace Approximation 

formula  = sqrt(y) ~ belt + f(trend, model="rw1", param=c(a.c,b.c)) +
  f(seasonal, model="seasonal", season.length=12, param=c(a.d,b.d))

#launch the inla function 
system.time(
inla.mod <- inla(formula, family="gaussian", data=Drivers, 
                 control.family=list(param=c(a.s,b.s)))
)
plot(inla.mod)
summary(inla.mod)

# Besser wäre vielleicht RW2

formula2  = sqrt(y) ~ belt + f(trend, model="rw2", param=c(a.c,b.c)) +
  f(seasonal, model="seasonal", season.length=12, param=c(a.d,b.d))

#lounch the inla function 
mod2 = inla(formula, family="gaussian", data=Drivers,
           control.family=list(param=c(a.s,b.s)))
plot(mod2)

