install.packages("purrr") # package for unifor distribution
library(purrr)

# question no 1
rdunif(n=10,b=15,a=0)

rbinom(n=10,size = 1,prob=0.2)

rbinom(n=10,size=12,prob=0.5)

rgeom(n=10,prob=0.73)

rnbinom(n=10,size = 5,prob = 0.3)

rpois(n=10,lambda = 4)

# question no 2
x<- 0:10
x
p1<- dbinom(x,size = 10,prob = 0.8)
p2<- dbinom(x,size=10, prob = 0.5)
p3<- dbinom(x,size = 10, prob=0.2)
par(mfrow= c(1,3))
plot(x,p1,type = "h",main="plot for p = 0.8")
points(x,p1,pch=16)
plot(x,p2,type = "h",main = "plot for p=0.5")
points(x,p2,pch=16)
plot(x,p3,type="h",main="plot for p = 0.2")
points(x,p3,pch=16)

# question no 3
x<- 0:5
pmf<- dbinom(x,size=5,prob=0.7)
cdf<- pbinom(x,size = 5, prob = 0.7)
par(mfrow=c(1,2))
plot(x=x,y=pmf,type="h",main="plot of pmf", col="red")
points(x,pmf,pch=16)
plot(x=x,y=cdf,type="h",main='plot of cdf',col="orange")
points(x,cdf,pch=16)

# question no 4
obs<- rbinom(n=100,size = 10,prob = 0.2)
mean(obs)
var(obs)
x<- 0:10
pmf<- dbinom(x,size=10,prob=0.2)
cdf<- pbinom(q=x,size=10,prob=0.2)
par(mfrow=c(1,2))
plot(x,pmf,type="h",main="plot of pmf",col="blue")
points(x,pmf,pch=16)
plot(x,cdf,type="h",main="plot of cdf",col="red")
points(x,cdf,pch=16)

# question no 5
dgeom(x=3,prob = 0.20)

# question no 6
dpois(x=6,lambda = 4.5)
ppois(q=6,lambda = 4.5)

# question no 7
x<- 0:20
b<- dbinom(x,size=20,prob = 0.2)
lam<- 20*0.2
p<- dpois(x,lambda = lam)
par(mfrow=c(1,2))
plot(x=x,y=b,type = "h",main = "plot for binomial")
points(x,b,pch=16)
plot(x=x,y=p,type="h",main="plot for poisson")
points(x,p,pch=16)

# question no 9
x<- 0:20
pdf<- dpois(x,3)
plot(x,pdf,type = "l",col="black")
lines(x,dpois(x,5),type="l",col="blue")

lines(x,dpois(x,9),type="l",col="orange")
lines(x,dpois(x,13),type="l",col="red")
lines(x,dpois(x,22),type="l",col="green")
legend("topright",legend = c("lambda=3","lambda=5","lambda=9",
                             "lambda=13","lambda=22"),
       col = c("black","blue","orange","red","green"),
       lty = c(1,1,1,1,1))

# question no 10
x<- 0:30
b<- dbinom(x,size=30,prob = 0.1)
lam= 30*0.1
p<- dpois(x,lambda = lam)
par(mfrow=c(1,2))
plot(x,b,type="h",main = "plot of binomial",col="red")
points(x,b,pch=16)
plot(x,p,type="h",main="plot for poisson",col="green")
points(x,p,pch=16)
