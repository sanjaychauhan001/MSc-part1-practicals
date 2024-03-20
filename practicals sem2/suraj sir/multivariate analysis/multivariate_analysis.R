# question no 1
mux<- 70
muy<- 60
sdx<- 10
sdy<- 15
rho<- 0.6
#i
pnorm(75,mean=muy,sd=sdy,lower=F) # 0.1586553
#ii
#mu = muy + rho*(sdy/sdx)*(x-mux)
# var = vary*(1-rho^2)
mu<- muy+rho*(sdy/sdx)*(80-mux)
mu  
var<- sdy^2*(1-rho^2)
sd<- sqrt(var)
sd
pnorm(75,mean=mu,sd=sd,lower=F) #0.3085375
#iii
mu<- mux+muy
mu
var<- (sdx^2) + (sdy^2) + (2*rho*sdx*sdy)
var
sd<- sqrt(var)
sd
pnorm(150,mean = mu,sd=sd, lower=F)#0.1867361
#iv
mu<- mux-muy
sd<- sqrt(sdx^2+sdy^2-2*rho)
pnorm(0,mean=mu,sd=sd) #0.2891987
#v
mu<- (5*mux)-(4*muy)
mu
sd<- sqrt(25*sdx^2+16*sdy^2-2*rho)
pnorm(150,mean=mu,sd=sd,lower=F) #0.3042563

# question no 2
install.packages('MASS')
library(MASS)
mu<- c(mux,muy)
sigma<- matrix(c(sdx^2,rho*sdx*sdy, rho*sdx*sdy, sdy^2),2)
sigma
bnv<- mvrnorm(n=500,mu=mu,Sigma = sigma)
colnames(bnv)<- c('bvn_1','bnv_2')
bnv

# question no 3
N<- 200
mu1<- 1
mu2<- 1
sig1<- 2
sig2<- 8
rho<- 0.6
set.seed(123)
#i
mu<- c(mu1,mu2)
sigma<- matrix(c(sig1^2,rho*sig1*sig2, rho*sig1*sig2, sig2^2),2)
sigma
bnv<- mvrnorm(n=N,mu=mu,Sigma = sigma)
colnames(bnv)<- c("bnv_1","bvn_2")
bnv
#ii
install.packages('mvtnorm')
library(mvtnorm)
bnv2<- mvtnorm::rmvnorm(n=N,mean=mu,sigma = sigma,method='svd')
bnv2
#iii
gibbs<- function(N,mu1,s1,mu2,s2,rho){
  mat<- matrix(ncol = 2,nrow = N)
  x<- 0
  y<- 0
  mat[1,]<- c(x,y)
  for (i in 2:N){
    x<- rnorm(1,mu1+
                (s1/s2)*rho*(y-mu2),sqrt((1-rho^2)*s1^2))
    y<- rnorm(1,mu2+
                (s2/s1)*rho*(x-mu1),sqrt((1-rho^2)*s2^2))
    mat[i,]<- c(x,y)
  }
  mat
}
bnv3<- gibbs(N=200,mu1,sig1,mu2,sig2,rho)
bnv3
#iv
rbnv<- function(n,mu1,s1,mu2,s2,rho){
  x1<- rnorm(n,mu1,s1)
  x2<- rnorm(1,mu2+
               (s2/s1)*rho*(x1-mu1), sqrt((1-rho^2)*s2^2))
  cbind(x1,x2)
}
bnv4<- rbnv(n=200,mu1,sig1,mu2,sig2,rho)
bnv4
