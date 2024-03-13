install.packages("stats4")
install.packages("methods")
library(stats4)
library(methods)
# question no 1
N<- 20
n<- 10
p<- 0.5
set.seed(100)
x<- rbinom(N,n,p)
x
mean(x)/10
nll.binom<- function(size,prob){
  -sum(dbinom(x,size=n,prob=prob,log = TRUE))
}
ans<- optim(fn=nll.binom,par = c(prob=0.1),size=10,method = "BFGS")
ans$par
# for N=500
N<-500
n<- 10
p<- 0.5
set.seed(100)
x<- rbinom(N,n,p)
mean(x)/n
nll.binom<- function(size,prob)
{
  -sum(dbinom(x,size=n,prob=prob,log=TRUE))
}
ans<- optim(fn=nll.binom,par = c(prob=0.1),size=10,method = "BFGS")
ans$par


# question no 2
# for size=10
set.seed(100)
N<- 10
lambda<- 1.5
x<- rpois(N,lambda = lambda)
mean(x)#mle
nll.pois<- function(lambda)
{
  -sum(dpois(x,lambda=lambda,log=TRUE))
}
ans<- optim(fn=nll.pois,par = c(lambda=1.5),method = "BFGS")
ans$par
#for size=50
set.seed(100)
N<- 50
lambda<- 1.5
x<- rpois(N,lambda = lambda)
mean(x)#mle
nll.pois<- function(lambda)
{
  -sum(dpois(x,lambda=lambda,log=TRUE))
}
ans<- optim(fn=nll.pois,par = c(lambda=1.5),method = "BFGS")
ans$par
# for size=100
set.seed(100)
N<- 100
lambda<- 1.5
x<- rpois(N,lambda = lambda)
mean(x)#mle
nll.pois<- function(lambda)
{
  -sum(dpois(x,lambda=lambda,log=TRUE))
}
ans<- optim(fn=nll.pois,par = c(lambda=1.5),method = "BFGS")
ans$par

# question no 3
set.seed(100)
N<- 20
mean<- 5
var<- 2
sd<- sqrt(2)
x<- rnorm(N,mean,sd)
mean(x)#mle
nll.norm<- function(size,prob)
{
  -sum(dnorm(x,mean,sd,log=TRUE))
}
ans<- optim(fn=nll.norm,par=c(mean=5,sd=sd),size=20,method = "BFGS")
ans$par

# for size=100
set.seed(100)
N<- 100
mean<- 5
var<- 2
sd<- sqrt(2)
x<- rnorm(N,mean,sd)
mean(x)#mle
nll.norm<- function(size,prob)
{
  -sum(dnorm(x,mean,sd,log=TRUE))
}
ans<- optim(fn=nll.norm,par=c(mean=5,sd=sd),size=20,method = "BFGS")
ans$par

# question no 5
# for exp
set.seed(100)
N<- 50
x<- rexp(N,rate = 2)
N/sum(x)#mle

nll.exp<- function(rate)
{
  -sum(dexp(x,rate=rate,log=TRUE))
}
ans<- optim(fn=nll.exp,par=c(rate=2),method="BFGS")
ans$par

# size=100
set.seed(100)
N<- 100
x<- rexp(N,rate = 2)
N/sum(x)#mle

nll.exp<- function(rate)
{
  -sum(dexp(x,rate=rate,log=TRUE))
}
ans<- optim(fn=nll.exp,par=c(rate=2),method="BFGS")
ans$par

# size=120
set.seed(100)
N<- 120
x<- rexp(N,rate = 2)
N/sum(x)#mle

nll.exp<- function(rate)
{
  -sum(dexp(x,rate=rate,log=TRUE))
}
ans<- optim(fn=nll.exp,par=c(rate=2),method="BFGS")
ans$par

# for gamma
N<- 50
x<- rgamma(N,shape=2,scale = 1)
50/sum(x)#mle
nll.gamma<- function(shape,scale)
{
  -sum(dgamma(x,shape = shape,scale=scale,log=TRUE))
}
ans<- optim(fn=nll.gamma,par=c(shape=2,scale=1),method="BFGS")
