# question no 1
#i
p0<- 1/4
p1<- 1/4
p2<- 1/2
mean<- 0*p0+1*p1+2*p2
mean
#ii
pgf<- function(s){
  (1/4)+(1/4)*s+(1/4)*s^2
}
pgf(0)
#iii
roots<- polyroot(c(p0,p1-1,p2))
roots                 
a<- Re(roots[1])
a
s1=c()
ps=c()
for(s in seq(0,1,0.1))
  {
  p1=pgf(s)
  s1=rbind(s1,s)
  ps=rbind(ps,p1)
}
df<- cbind(s1,ps)
plot(s1,ps,type='l',ylim = c(0,1))
lines(s1,s1,col='red')
abline(v=0.5)

# question no 2