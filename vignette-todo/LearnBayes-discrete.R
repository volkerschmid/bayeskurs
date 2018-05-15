library(LearnBayes)

# Binomial

p <- seq(0, 1, by = 0.01)
prior <- 1 / 101 + 0 * p
plot(p, prior, type="h", main="Prior Distribution")
post <- pdisc(p, prior, c(20, 12))
plot(p, post, type="h", main="Posterior Distribution")

discint(cbind(p, post), 0.90)

n <- 20
s <- 0:20
pred.probs <- pdiscp(p, post, n, s)
plot(s, pred.probs, type="h", main="Predictive Distribution")

prior2 <- p/sum(p)
plot(prior2)
post2 <- pdisc(p, prior2, c(20, 12))
par(mfrow=c(2,1))
plot(p, post, type="h", main="Posterior Distribution")
plot(p, post2, type="h", main="Posterior Distribution")

prior3 <- dbeta(p,.001,.001)
prior3[1]<-prior3[101]<-1
plot(prior3)
post3 <- pdisc(p, prior3, c(20, 12))
par(mfrow=c(2,1))
plot(p, post, type="h", main="Posterior Distribution")
plot(p, post3, type="h", main="Posterior Distribution")

plot(post3-post2)

#Poisson

prior.p <- rep(1/11, 11)
names(prior.p) <- 20:30

print(prior.p)
plot(prior.p)

y <- c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22)

post <- discrete.bayes(dpois, prior.p, y)

print(post)
plot(post)
points(prior)
summary(post)

#Geometrisch

y<-rgeom(3,.4)

p <- seq(0.01, 1, by = 0.01)
prior <- 1 / 100 + 0 * p
prior2 <- p/sum(p)
prior3 <- dbeta(p,.001,.001)
prior3[1]<-prior3[100]<-1

par(mfrow=c(3,1))
names(prior)<-names(prior2)<-names(prior3)<-p
plot(discrete.bayes(dgeom, prior, y))
plot(discrete.bayes(dgeom, prior2, y))
plot(discrete.bayes(dgeom, prior3, y))

y<-rgeom(15,.4)
par(mfrow=c(3,1))
names(prior)<-names(prior2)<-names(prior3)<-p
plot(discrete.bayes(dgeom, prior, y))
plot(discrete.bayes(dgeom, prior2, y))
plot(discrete.bayes(dgeom, prior3, y))

dev.off()

#normal
set.seed(3427)
p1 = seq(0.4, 0.6, length = 99)
p2 = seq(0.01, 0.2, length=99)
prior = matrix(1/(99^2), 99, 99)
image(p1,p2,prior, xlab="mu", ylab="sigma")
y<-rnorm(20,.5,.1)
mean(y)
sd(y)
dimnames(prior)[[1]] = p1
dimnames(prior)[[2]] = p2
post<-discrete.bayes.2(dnorm,prior,y)
plot(post,xlab="mu",ylab="sigma")

plot(p1,490*apply(post$prob,1,sum),type="l",ylim=c(0,20),xlab="mu",ylab="") #marginal
lines(p1,23*490*post$prob[,49],col="blue") #bedingt
legend(.4,20,col=c("black","blue"),legend=c("marginal","bedingt"),pch=19)
