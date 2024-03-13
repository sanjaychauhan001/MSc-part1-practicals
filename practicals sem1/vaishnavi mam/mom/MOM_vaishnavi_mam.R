# question no 1
sample<- c(1.3,0.6,1.7,2.2,0.3,1.1,2.1,1.8,0.7)
theta<- 2*mean(sample)
theta

# question no 2
sample<-c(1.3,0.7,3.6,5.4,7.4,2.5,3.2,4.1,2.5,2.6)
theta<-(mean(sample))-1
theta


# question no 3
set.seed(121)
sample<- rexp(100,1/2)
lambda<- 1/mean(sample)
lambda
x<- seq(0,20,0.01)
plot(x,dexp(x,1/2),type='h')
lines(x,dexp(x,lambda),type="l",col="red")

# question no 4
set.seed(0)
sample<- rgamma(1000,5,7)
m1<- mean(sample)
m2<- (sum((sample)^2))/length(sample)
alpha<- (m1^2)/(m2-m1^2)
alpha
beta<- m1/(m2-m1^2)
beta

# question no 5
set.seed(456)
sample<- runif(150,min=2,max=8)
m1<- mean(sample)
m1
m2<- (sum((sample^2)))/length(sample)
m2
a<- m1-sqrt(3*(m2-(m1^2)))
a
b<- m1+sqrt(3*(m2-(m1^2)))
b

# question no 6
x<- 0:5
x
f<- c(241,123,45,13,4,1)
f
data_frame<- data.frame(x=x,f=f)
data_frame
m1<- (sum(f*x))/sum(f)
m1
m2<- (sum(f*(x^2)))/sum(f)
m2
p<- m1/(m2-(m1^2))
p
q<- 1-p
q
k<- (p*m1)/q
k

# question no7
sample7<-c(0.85,0.76,0.82,0.78,0.90,0.9,0.98,0.78)
sample7
mean<- mean(sample7)
mean
theta<- mean/(1-mean)
theta


# question no 8
sample<- c(2,9,7,7,8,4,3,3,6,6)
m1<- mean(sample)
m1
m2<-mean((sample^2))
m2
var<- m2-(m1^2)
var
