# question no 1
lam=3
t=2
dpois(x=4,lambda = lam*t)

# question no 2
#i
dbinom(x=2,size = 2,prob = 2/5)
#ii
1 - dbinom(x = 0,size = 2,prob = 2/5)

# question no 3
lam<- 5
p<- 0.6
t<- 4
dpois(x=10,lambda = lam*t*p)

# question no 4
1-pexp(1,rate = 2)

# question no 5
#i
birth_hour<- 5
birth_year<- 5*24*365
birth_year
#ii
dpois(x=0,lambda = 5*24)
#iii
1-pexp(q=8,rate = 1/12)

# question no 6
#i
lam<- 0.5
t<- 2
dpois(x=0,lambda = lam*t)
#ii
t<-1
a<- dpois(x=1,lambda = lam*t)
b<- a**4
b

# question no 7
#i
lam<- 3
a<- dpois(x=2,lambda = 3*1)
b<- dpois(x=5, lambda = 3*2)
c<- a*b
c
#ii
a<- dpois(x=2,lambda = 3)
b<- dpois(x=1, lambda = 3)
c<- (a*b)/a
c
