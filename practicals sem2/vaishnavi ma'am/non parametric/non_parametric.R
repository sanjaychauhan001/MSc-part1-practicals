install.packages('BSDA')
library(BSDA)
# question no 1
x<- c(10,15,20,17,11,25,30,27,36,40,5,26)
# H0 md=20 VS H1 md<20
test<- SIGN.test(x,md=20,alternative = 'less')
test
#s = 6, p-value = 0.7256
#since p-value > 0.05 we fail to reject the null hypothesis

# question no 2
x<- c(10,15,20,17,11,25,30,27,36,40,5,26)
# H0 md=20 VS H1 md!=20
test<- wilcox.test(x,mu=20,alternative = 'two.sided')
test
# V = 40, p-value = 0.5629
# since p-value > 0.05 we fail to reject the null hypothesis

#question no 3
x<- c(-1, 6, 13, 4, 2, 3, 5, 9)
# H0 md=8.9 VS H1: md!=8.9
test<- wilcox.test(x,mu=8.9,alternative = 'two.sided')
test
#V = 5, p-value = 0.07813
# since the p-value > 0.05 we fail to reject the null hypothesis

# question no 4
x=c(51.2,46.5,24.1,10.2,65.3,92.1,30.3,49.2)
y=c(45.8,41.3,15.8,11.1,58.5,70.3,31.6,35.4)
# H0: md1=md2 VS H1: md1>md2
# i
test<- SIGN.test(x,y,alternative = 'greater')
test
#S = 6, p-value = 0.1445
# since p-value is greater than 0.05 we fail to reject the null hypothesis
# ii
test<- wilcox.test(x,y,alternative = 'greater')
test
#W = 37, p-value = 0.3227
# since p-value>0.05 we fail to reject the null hypothesis

# question no 5
x<- c(1.5,2.3,4.2,7.1,10.4, 8.4,9.3,6.5,2.5,4.6)
#H0: data follows exp H1: doen't
test<- ks.test(x,pexp)
test
#D = 0.79974, p-value = 2.238e-07
# since p-value<0.05 we reject H0

# question no 6
set.seed(50)
x<- runif(20,0,1)
# H0: data follows uniform Vs H1: doen't
test<- ks.test(x,punif)
test
#D = 0.19127, p-value = 0.406
# we do not reject the null hypothesis
#ii
y<- sqrt(x)
test<- ks.test(y,punif)
test
#D = 0.20501, p-value = 0.3243
#we fail to reject the null hypothesis

# question no 7
x<- rnorm(50,mean = 5,sd = sqrt(2))
#H0 : data follows normal distribution vs H1;doesn't
test<- ks.test(x,pnorm)
test
#D = 0.94678, p-value = 8.882e-1
# we reject the null hypothesis