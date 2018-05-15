#' MCMC Beispiel für Poisson-Likelihood mit Lognormal-Priori
#'
#' @param sumx Summe der Daten
#' @param n Anzahl Daten
#' @param prior Liste mit Priori-Parametern, default: list("a"=1,"b"=1,"tau"=1000)
#' @param I Anzahl Iterationen
#' @param do.tuning Soll getunt werden?
#' @param tuning Anzahl Tuning-Iterationen
#' @param do.burnin Soll burnin beachtet werden
#' @param burnin Anzahl burnin-Iterationen
#'
#' @return coda::mcmc Objekt
#' @export 
#' @import coda 
#'
poisson.lognormal.mcmc<-function(sumx,n,prior=list("a"=1,"b"=1,"tau"=1000),I=1000, do.tuning=FALSE, tuning=50, do.burnin=FALSE, burnin=100){
mu<-rep(NA,I)
lambda<-rep(NA,I)
mu[1]<-1
lambda[1]<-sumx/n 
tau=prior[["tau"]]
a=prior[["a"]]
b=prior[["b"]]

#Berechnung der log Full Conditional von mu
log.fc.mu <- function(mu,lambda,sumx,tau)
{
  return(mu*log(lambda)-mu^2-mu^2/tau)
}

log.fc.lambda <- function(lambda,mu,sumx,n){
  return((sumx+mu-1)*log(lambda)-n*lambda-0.5*log(lambda)^2)
}

rw.sd <- .1
akzeptanz <- 0

#Ziehen der Zufallszahlen mittel MH-Algorithmus
i=1
while (i<=I)
{
  i<-i+1
  
  # Ziehe mu mittels random walk proposal
  mustern <- rnorm(1,mu[i-1],rw.sd)
  logalpha<-log.fc.mu(mustern,lambda[i-1],sumx,tau)-log.fc.mu(mu[i-1],lambda[i-1],sumx,tau)
  alpha<-exp(logalpha)
  ifelse(runif(1)<alpha,
         {mu[i]<-mustern; akzeptanz=akzeptanz+1},
         mu[i]<-mu[i-1])
  
  # Ziehe lambda mittels ad hoc Normal-Approximation
  c<-sumx + mu[i]+a
  d<-n+b
  lambdastern<-rnorm(1,mean=c/d,sd=sqrt(c/d^2))
  logalpha <- log.fc.lambda(lambdastern,mu[i],sumx,n)-log.fc.lambda(lambda[i-1],mu[i],sumx,n)
  logalpha <- logalpha+dnorm(lambda[i-1],mean=c/d,sd=sqrt(c/d^2),log=TRUE)-dnorm(lambdastern,mean=c/d,sd=sqrt(c/d^2),log=TRUE)
  alpha<-exp(logalpha)
  ifelse (runif(1)<alpha,
          lambda[i]<-lambdastern,
          lambda[i]<-lambda[i-1])
  
  if (i==tuning)if(do.tuning)
  {
    # Tuning des RW-Proposals
    ak.rate = akzeptanz/tuning
    if (ak.rate>.5){
      rw.sd <-rw.sd * ak.rate * 2
      akzeptanz = 0
      i = 1
      lambda[1] <- lambda[tuning]
      mu[1] <- mu[tuning]
    }
    if (ak.rate<.3){
      rw.sd <-rw.sd * ak.rate
      akzeptanz = 0
      i = 1
      lambda[1] <- lambda[tuning]
      mu[1] <- mu[tuning]
    }
    message(paste("Akzeptanzrate",ak.rate,"\n"))
  }
  if(i==burnin)if (do.burnin)
  {
    i=1
    do.burnin=FALSE
  }
  
}
lambda<-lambda[-1]
mu<-mu[-1]
return(coda::mcmc(cbind(mu,lambda)))
}