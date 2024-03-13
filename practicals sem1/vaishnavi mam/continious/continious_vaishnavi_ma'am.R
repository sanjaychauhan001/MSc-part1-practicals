# question no 1
obs<- runif(n=1000,min=-1,max=1)
hist(obs, xlab = "x",col = "red")
obs2<- runif(n=10000,min=-1,max=1)
hist(obs2,density = 20, col="blue")

# question no 2
x<- seq(0,20,by=0.1)
x
pdf<- dexp(x,0.5)
cdf<- pexp(x,0.5)
par(mfrow=c(1,2))
plot(x,pdf,type="l",main="plot of pdf",col="red")
plot(x,cdf,type="l",main="plot of cdf", col="blue")

# question no 3
x<- seq(0,20,by=0.2)
pdf<- dgamma(x,shape=1,scale = 2)
par(mfrow=c(1,1))
plot(x,pdf,type = "l",main = "PDF of gamma",col="black")
lines(x,dgamma(x,shape=2, scale = 2), type="l",col="orange")
lines(x,dgamma(x,shape=3,scale=2),type="l",col="red")
lines(x,dgamma(x,shape=5,scale=1),type="l",col="blue")
lines(x,dgamma(x,shape=9,scale = 0.5),type="l",col="green")
legend("topright",legend = c("a=1,b=2","a=2,b=2","a=3,b=2","a=5,b=1",
                             "a=9,b=0.5"),
       col = c("black","orange","red","blue","green"),
       lty = c(1,1,1,1,1))

# question no 4
a<- rexp(1000,rate = 6)
y<- 1-(exp(-6*a))
par(mfrow=c(1,2))
hist(y)
lines(seq(0,1,0.01),dunif(seq(0,1,0.01),0,1),col="blue")
plot(ecdf(y),col="red")
lines(seq(0,10,0,01), punif(seq(0,1,0.01),0,1),col="green")

# question no 5
set.seed(100)
x<- runif(1000,min = 0,max=1)
lambda=1
y<- -(1/lambda)*log(1-x)
par(mfrow=c(2,2))
hist(x,col="red")
hist(y,probability = TRUE,col="blue")

plot(density(y), col="red")
lines(seq(0,20,0.01),dexp(seq(0,20,0.01),1),col="green")

plot(ecdf(y), col="red")
lines(seq(0,40,0.01), pexp(seq(0,40,0.01),1),col="green")


# question no 6
set.seed(1001)
x<- runif(1000,0,1)
y<- -log(x)
par(mfrow=c(2,2))
hist(y,probability = TRUE, col="red")
lines(seq(0,20,0.01),dexp(seq(0,20,0.01),1),col="blue")

plot(ecdf(y),col="red")
lines(seq(0,20,0.01),pexp(seq(0,20,0.01),1),col="blue")

y1<- -log(1-x)

hist(y1,probability=TRUE,col="red")
lines(seq(0,20,0.01), dexp(seq(0,20,0.01),1),col="blue")

plot(ecdf(y1), col="red")
lines(seq(0,20,0.01), pexp(seq(0,20,0.01),1),col="blue")


# question no 7
#a
pnorm(1,lower=F)
pnorm(75,mean = 75,sd = 15,lower=F)

#b
#mu = muy + rho*(sdy/sdx)*(x-mux)
# var = vary(1-rho^1q)

mu<- 60 + 0.6*(15/10)*(80-70)
var<- (15**2)*(1-(0.6^2))
sd<- sqrt(var)
mu

pnorm(75,mean = 69,sd = sd,lower=F)

#c
# r.v x1,x2~ bvn
# y = a1x1 + a2x2 + b
# y~ N(mean= a1*mu1 + a2*mu2 + b, variance = (a1^2)*(var1) + (a2^2)*(var2) + 2*rho*sd1*sd2)
# P(x1+x2 > 150)

mean=70+60
mean
variance<- 10^2 + 15^2 + 2*0.6*10*15
variance
sd<- sqrt(variance)
pnorm(150, mean = mean, sd = sd, lower=F)

#d
# z = 5x-4y

mu1<- 5*70-4*60
mu1
var1<- (5^2)*(10^2) + (4^2)*(15^2) - 2*0.6*5*4*10*15
var1

pnorm(150, mean=mu1, sd=var1^0.5, lower=F)


# question no 8
N<- 200
set.seed(123)
mu1<- 1
mu2<- 1
sigma1<- 2
sigma2<- 8
rho<- 0.6

mu<- c(mu1,mu2)
sigma<- matrix(c(sigma1^1, sigma1*sigma2*rho, sigma1*sigma2*rho,sigma2^2),nrow = 2)
install.packages("MASS")
library(MASS)
bvn<- mvrnorm(n=N,mu = mu,Sigma = sigma)
colnames(bvn)<- c("bvn_x1","bvn_x2")
bvn

# question no 9
mean_log<- 9
sd_log<- 2

obs<- rlnorm(1000, meanlog = mean_log, sdlog = sd_log)
x_val<- seq(0, 20, 0.01)
pdf_val<- dlnorm(x_val, meanlog = mean_log, sdlog = sd_log)
cdf_val<- plnorm(x_val, meanlog = mean_log, sdlog = sd_log)

par(mfrow=c(1,2))
plot(x_val, pdf_val, type = "l", main="pdf of lognormal distribution")
plot(x_val, cdf_val, type = "l", main="cdf of lognormal distribution")

