alpha <- mean(y)
beta <- 0
gamma <- rep(0,T)
delta <- rep(0,T)
tau.c <- a.c/b.c
tau.d <- a.d/b.d
sigma2 <- b.s/a.s

Qc <- Matrix::sparseMatrix(i = c(1:T,1:(T-1)), j=c(1:T,2:T),
                           x=c(1,rep(2,T-2),1,rep(-1,T-1)), symmetric = TRUE)
try(if (rw==2){Qc<-Matrix::sparseMatrix(i = c(1:n, 2:n, 3:n), j=c(1:n, 1:(n-1),1:(n-2)),
                                        x = c(1,5,rep(6,n-4),5,1,-2,rep(-4,n-3),-2,rep(1,n-2)), symmetric = TRUE)},silent=TRUE)
Qd <- Matrix::sparseMatrix(i=T,j=T,x=0,symmetric = TRUE)
EinsM<-Matrix::sparseMatrix(i=rep(1:12,12),j=rep(1:12,each=12),x=1)
for (i in 1:(T-12))
  Qd[i+(0:11),i+(0:11)]<-Qd[i+(0:11),i+(0:11)]+EinsM
sumx2 <- sum(belt)

burnin=5000
nr.it=5000
I = burnin+nr.it
iter = 0

alpha.save<-beta.save<-sigma2.save<-rep(0,nr.it)
gamma.save<-array(0,c(T,nr.it))
delta.save<-array(0,c(T,nr.it))
tau.save<-array(0,c(2,nr.it))

while (iter<I)
{
  iter=iter+1
  if (iter==1)tic()
  
  gamma<-gamma-mean(gamma)
  delta<-delta-mean(delta)
  
  m <- sum(y-gamma-delta-belt*beta)
  m <- m/sigma2
  Q <- T/sigma2
  alpha <- rnorm(1, m/Q, 1/Q)
  
  m <- sum(belt*(y-gamma-delta-alpha))
  m <- m/sigma2
  Q <- sumx2/sigma2
  beta <- rnorm(1, m/Q, 1/Q)
  
  if(rw!=0){
    m = (y-delta-beta*belt-alpha)/sigma2
    Q <- tau.c*Qc + Matrix::Diagonal(T)/sigma2
    gamma <- bayeskurs::rmvnormcanon(m,Q)
  }
  else
  {
    gamma<-rep(0,T)
  }
  
  m <- rep(0,T)
  m = (y-gamma-beta*belt-alpha)/sigma2
  Q <- tau.d*Qd + Matrix::Diagonal(T)/sigma2
  delta <- bayeskurs::rmvnormcanon(m,Q)
  
  tau.c <- rgamma(1, a.c + (T-1)/2, b.c + (t(gamma)%*%Qc%*%gamma/2)[1,1])
  tau.d <- rgamma(1, a.d + (T-12+1)/2, b.d + (t(delta)%*%Qd%*%delta/2)[1,1])
  sigma2 <- 1/rgamma(1, a.s + T/2, b.s + sum((y-alpha-gamma-delta-beta*belt)^2))
  
  if(iter==10)
  {
    temp<-toc(quiet=TRUE)
    temp<-(temp$toc-temp$tic)/10
    message(paste0("One iteration takes roughly ",round(temp,2)," seconds. Estimated total time of ",I," Iterations is ",round(temp*I)," seconds.\n"))
  }
  if (iter>burnin)
  {
    alpha.save[iter-burnin]<-alpha
    beta.save[iter-burnin]<-beta
    gamma.save[,iter-burnin]<-gamma
    delta.save[,iter-burnin]<-delta
    tau.save[,iter-burnin]<-c(tau.c,tau.d)
    sigma2.save[iter-burnin]<-sigma2
  }
}
