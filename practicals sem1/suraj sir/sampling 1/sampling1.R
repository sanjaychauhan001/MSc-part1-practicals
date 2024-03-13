#question 1
N<-5 #population household
n<-2 #sample household
i<-1:5;i #household number
xi<-c(156,149,166,164,155);xi #income of household
#a
pm<-sum(xi)/N;pm #population mean
pms<-(1/(N-1))*sum((xi-pm)^2);pms #population mean square
pv<-(1/(N))*sum((xi-pm)^2);pv #population variance
#OR
a1<-mean(xi);a1 #population mean
b1<-var(xi);b1 #population mean square
c1<-((N-1)/N)*var(xi);c1 #population variance
#OR
d<-mean(xi^2);d 
b2<-(N/(N-1))*(d-a1^2);b2
c2<-d-a1^2;c2

install.packages("gtools") #for permutation & combination
library(gtools) #for permutation & combination
#b (sampling with replacement)
samples<-N^n;samples
set.seed(121)
s<-permutations(n=5,r=2,v=i,repeats.allowed = TRUE)
s<-as.data.frame(s)
s
v<-permutations(n=5,r=2,v=xi,repeats.allowed = TRUE)
v<-as.data.frame(v)
v
c<-cbind(s,v)
c
colnames(c)[3]<-"m1"
colnames(c)[4]<-"m2"
names(c)
c$sample_mean<-((c$m1)+(c$m2))/n
c
c$sample_mean_sqr<-(1/(n-1))*((c$m1-c$sample_mean)^2)+((c$m2-c$sample_mean)^2)
c
c$sample_var<-(1/(n))*((c$m1-c$sample_mean)^2)+((c$m2-c$sample_mean)^2)
c
c$mean_sq<-(c$sample_mean)^2
c
#i
E_sm<-sum(c$sample_mean)/samples ;E_sm
#sample mean is u.e. of pop. mean
#ii
E_sms<-sum(c$sample_mean_sqr)/samples ;E_sms
#sample mean sqr is u.e. of pop. variance
#iii
V_sm<-(sum(c$mean_sq)/samples)-((sum(c$sample_mean)/samples)^2) ;V_sm
V_sm1<-((N-1)/(N*n))*pms ;V_sm1
V_sm2<-pv/n ;V_sm2

#c
combinations(N,n)
samples<-length(combinations(N,n))/2;samples
set.seed(121)
a<-combinations(5,2,v=i)
as.data.frame(a)
t<-combinations(5,2,v=xi)
as.data.frame(t)
w<-cbind(a,t)
w<-as.data.frame(w)
w
colnames(w)[3]<-"m1"
colnames(w)[4]<-"m2"
names(w)
w$sample_mean<-((w$m1)+(w$m2))/n
w
w$sample_mean_sqr<-(1/(n-1))*((w$m1-w$sample_mean)^2)+((w$m2-w$sample_mean)^2)
w
w$sample_var<-(1/(n))*((w$m1-w$sample_mean)^2)+((w$m2-w$sample_mean)^2)
w
w$mean_sq<-(w$sample_mean)^2
w
#i
E_sm<-sum(w$sample_mean)/samples ;E_sm
#sample mean is u.e. of pop. mean
#ii
E_sms<-sum(w$sample_mean_sqr)/samples ;E_sms
#sample mean sqr is u.e. of pop. variance
#iii
V_sm<-(sum(w$mean_sq)/samples)-((sum(w$sample_mean)/samples)^2) ;V_sm
V_sm1<-((N-n)/(N*n))*pms ;V_sm1
V_sm2<-((N-n)/(N-1))*pv/n ;V_sm2

#question 2
Nh<-c(12,24,19,13,9)
N<-sum(Nh);N
nh<-c(4,6,5,4,2)
Wh<-Nh/N
Wh2<-(Wh)^2
Yhi<-c(9.2,21.8,10.2,3.8,10.7) #sum of elements of each stratum 
Yh_bar<-Yhi/nh
Sh<-c(5.2,3.7,6.5,3.4,2.7)
Sh2<-(Sh)^2
y<-(Wh2*Sh2)/nh
z<-(Wh2*Sh2)/Nh
WhSh2<-Wh*Sh2
data<-data.frame(Nh,nh,Wh,Wh2,Yhi,Yh_bar,Sh,Sh2,y,WhSh,z,WhSh2)
data
y_bar_st<-sum(data$Wh*data$Yh_bar);y_bar_st
v_y_bar_st<-(sum(data$y)-sum(data$z));v_y_bar_st
n<-21
v_prop<-((N-n)/(N*n))*sum(data$WhSh2);v_prop
v_opt<-(((sum(data$WhSh))^2)*(1/n))-((sum(data$WhSh2))*(1/N));v_opt
v_prop/v_opt #efficiency w.r.t. optimum allocation 

#question 3
h<-c(1,2,3)
Nh<-c(2461,2385,543)
nh<-c(19,27,6)
N<-sum(Nh)
Wh<-Nh/N ;Wh
Whsq<-(Wh)^2 ;Whsq
fh<-nh/Nh ;fh
a<-sum(27,1068,81,418,162,397);a
b<-sum(960,56,513,170,843,1627,661,511,1361,232,981,477,906,864,2422,2055,803,655);b
c<-sum(1835,1744,1821,2496,7974,10238);c
(a+b+c)/18
yhi<-c(a,b,c);yhi
Yh1<-c(0,27,0,0,0,0,0,0,1068,81,418,0,162,397,0,0,0,0,0)
Yh2<-c(960,56,0,0,0,0,513,170,843,1627,661,0,511,0,1361,232,0,981,477,906,864,2422,2055,803,0,0,655)
Yh3<-c(1835,1744,1821,2496,7974,10238)
Yhi<-c(Yh1,Yh2,Yh3)
yh_bar<-yhi/nh ;yh_bar
Whyh_bar<-Wh*yh_bar ;Whyh_bar
y_bar_st<-sum(Whyh_bar);y_bar_st
Shsq1<-(1/(19-1))*sum((Yh1-113.3158)^2) ;Shsq1
Shsq2<-(1/(27-1))*sum((Yh2-596.1852)^2) ;Shsq2
Shsq3<-(1/(6-1))*sum((Yh3-4351.3333)^2) ;Shsq3
p<-((1-fh[1])*(Whsq[1]*Shsq1))/nh[1]
p
q<-((1-fh[2])*(Whsq[2]*Shsq2))/nh[2]
q
r<-((1-fh[3])*(Whsq[3]*Shsq3))/nh[3]
r
v_y_bar_st<-p+q+r
v_y_bar_st

#question 4
Nh<-c(63,199,53,25)
N<-340
n<-34
ybarh<-c(112.1,276.7,558.1,860.1)
ybarN<-sum(112.1*(63/340),276.7*(199/340),558.1*(53/340),860.1*(25/340)) ;ybarN
Sh<-c(56.3,116.4,186,363.1) 
Shsq<-Sh^2 ;Shsq
Wh<-Nh/sum(Nh) ;Wh
Whybarh<-Wh*ybarh ;Whybarh
f<-n/N ;f
s2<-(sum((Nh-1)*Shsq))/339+(sum(Nh*(ybarh-ybarN)^2))/339 ;s2 #s square
Vsrs<-((1-f)/n)*s2
Vsrs
VNh<-((1-f)/n)*sum(Wh*Shsq) ;VNh #propotional
VWh<-(((sum(Wh*Sh))^2)/n)-((sum(Wh*Shsq))/N) ;VWh #neyman optimum
VNh/Vsrs #eff of prop
VWh/Vsrs #eff of neyman 

#question 5
Nh<-c(18260,4315,2233,1057,567)
N<-sum(Nh);N
ybarh<-c(100000,250000,500000,1760000,2250000)
ybarN<-sum(100000*(18260/N),250000*(4315/N),500000*(2233/N),1760000*(1057/N),2250000*(567/N));ybarN
Sh<-c(80000,200000,600000,1900000,2500000)
Ch<-c(6,9,10,12,15)
n<-600
nh<-c(49,49,49,49,49)
Shsq<-Sh^2 ;Shsq
Wh<-Nh/sum(Nh) ;Wh
Whybarh<-Wh*ybarh ;Whybarh
f<-n/N ;f
s2<-(sum((Nh-1)*Shsq))/(N-1)+(sum(Nh*(ybarh-ybarN)^2))/(N-1) ;s2 #s square
Vsrs<-((1-f)/n)*s2
Vsrs
VNh<-((1-f)/n)*sum(Wh*Shsq) ;VNh #propotional
VWh<-(((sum(Wh*Sh))^2)/n)-((sum(Wh*Shsq))/N) ;VWh #neyman optimum

####repeat 
#q1
N<-5
n<-2
xi<-c(156,149,166,164,155)
#a
a<-xi^2
b<-mean(xi)
b
pms<-var(xi) #wrong
pms
pm<-sum(xi)/N
pm
pv<-mean(a)-b^2
pv
pms1<-(1/(N-1))*sum((xi-b)^2);pms1
pvar<-(1/(N))*sum((xi-b)^2);pvar
#b
sample<-N^n
sample
#c
a<-combn(N,n)
sample<-length(a)/2
sample

install.packages("gtools")
library(gtools)
#b
b<-c(156,149,166,164,155)
set.seed(121)
v<-permutations(n=5,r=2,v=b,repeats.allowed = TRUE)
v<-as.data.frame(v)
v
s<-permutations(n=5,r=2,v=1:5,repeats.allowed = TRUE)
s<-as.data.frame(s)
s
c<-cbind(s,v)
c
colnames(c)[3]<-"m1"
colnames(c)[4]<-"m2"
names(c)
c$mean=((c$m1)+(c$m2))/2
c
c$sum_sq<-(1/(2-1))*((c$m1-c$mean)^2)+((c$m2-c$mean)^2)
c
c$mean_sq<-(c$mean)^2
c
x<-print(sum(c$mean))
y<-print(sum(c$sum_sq))
z<-print(sum(c$mean_sq))
sample_mean<-x/25 #sample mean is u.e. of pop. mean
sample_mean
sample_mean_sq<-y/25 #sample mean sqr is u.e. of pop. variance
sample_mean_sq

pv/n
#sample_var<-sum(c$mean-c$mean()))^2
#sample_var  
sv<-(sum(c$mean_sq)/25)-158^2
sv

#c
#combn(5,2) horizontal
combinations(5,2)
set.seed(121)
a<-combinations(5,2,v=1:5)
as.data.frame(a)
t<-combinations(5,2,v=b)
as.data.frame(t)
w<-cbind(a,t)
w<-as.data.frame(w)
w
colnames(w)[3]<-"m1"
colnames(w)[4]<-"m2"
names(w)
w$mean=((w$m1)+(w$m2))/2
w
w$sum_sq<-(1/(2-1))*((w$m1-w$mean)^2)+((w$m2-w$mean)^2)
w
w$mean_sq<-(w$mean)^2
w
x<-print(sum(w$mean))
y<-print(sum(w$sum_sq))
z<-print(sum(w$mean_sq))
sample_mean<-x/10 #sample mean is u.e. of pop. mean
sample_mean
sample_mean_sq<-y/10 #sample mean sqr is u.e. of pop. variance
sample_mean_sq
pv/10 #wrong
sv<-(sum(w$mean_sq)/10)-158^2
sv
((1/2)-(1/5))*pms1

#q2
Nh<-c(12,24,19,13,9)
N<-sum(Nh)
nh<-c(4,6,5,4,2)
Wh<-Nh/N
Wh2<-(Wh)^2
Yhi<-c(9.2,21.8,10.2,3.8,10.7)
Yh_bar<-Yhi/nh
Sh<-c(5.2,3.7,6.5,3.4,2.7)
Sh2<-(Sh)^2
y<-(Wh2*Sh2)/nh
WhSh<-Wh*Sh
WhYh_bar<-Wh*Yh_bar
z<-(Wh2*Sh2)/Nh
Wh2Sh2<-Wh2*Sh2
Wh.Sh2<-(Wh*Sh2)
data<-data.frame(Nh,nh,Wh,Wh2,Yhi,Yh_bar,Sh,Sh2,y,WhSh,WhYh_bar,z,Wh2Sh2,Wh.Sh2)
total<-c(sum(Nh),sum(nh),sum(Wh),sum(Wh2),sum(Yhi),sum(Yh_bar),sum(Sh),sum(Sh2),sum(y),sum(WhSh),sum(WhYh_bar),sum(z),sum(Wh2Sh2),sum(Wh.Sh2))
data
y_bar_st<-sum(data$WhYh_bar)
y_bar_st
N
1-(21/N) #n=21
v_y_bar_st<-(sum(data$y))-((sum(data$Wh.Sh2)/N))
v_y_bar_st

#q3
h<-c(1,2,3)
Nh<-c(2461,2385,543)
nh<-c(19,27,6)
N<-sum(Nh)
Wh<-Nh/N ;Wh
Whsq<-(Wh)^2 ;Whsq
fh<-nh/Nh ;fh
a<-sum(27,1068,81,418,162,397);a
b<-sum(960,56,513,170,843,1627,661,511,1361,232,981,477,906,864,2422,2055,803,655);b
c<-sum(1835,1744,1821,2496,7974,10238);c
(a+b+c)/18
yhi<-c(a,b,c);yhi
Yh1<-c(0,27,0,0,0,0,0,0,1068,81,418,0,162,397,0,0,0,0,0)
Yh2<-c(960,56,0,0,0,0,513,170,843,1627,661,0,511,0,1361,232,0,981,477,906,864,2422,2055,803,0,0,655)
Yh3<-c(1835,1744,1821,2496,7974,10238)
Yhi<-c(Yh1,Yh2,Yh3)
yh_bar<-yhi/nh ;yh_bar
Whyh_bar<-Wh*yh_bar ;Whyh_bar
sum(Whyh_bar)
Shsq1<-(1/(19-1))*sum((Yh1-113.3158)^2) ;Shsq1
Shsq2<-(1/(27-1))*sum((Yh2-596.1852)^2) ;Shsq2
Shsq3<-(1/(6-1))*sum((Yh3-4351.3333)^2) ;Shsq3
p<-((1-fh[1])*(Whsq[1]*Shsq1))/nh[1]
p
q<-((1-fh[2])*(Whsq[2]*Shsq2))/nh[2]
q
r<-((1-fh[3])*(Whsq[3]*Shsq3))/nh[3]
r
z<-p+q+r
z
#or
#question 3
h<-c(1,2,3)
Nh<-c(2461,2385,543)
nh<-c(19,27,6)
N<-sum(Nh)
Wh<-Nh/N ;Wh
Wh2<-(Wh)^2 ;Wh2
fh<-nh/Nh ;fh
a<-sum(27,1068,81,418,162,397)
b<-sum(960,56,513,170,843,1627,661,511,1361,232,981,477,906,864,2422,2055,803,655)
c<-sum(1835,1744,1821,2496,7974,10238)
(a+b+c)/18
yhi<-c(a,b,c)
yh1_bar<-a/nh[1] ;yh1_bar
yh2_bar<-b/nh[2] ;yh2_bar
yh3_bar<-c/nh[3] ;yh3_bar
Wh1yh1_bar<-Wh[1]*yh1_bar ;Wh1yh1_bar
Wh2yh2_bar<-Wh[2]*yh2_bar ;Wh2yh2_bar
Wh3yh3_bar<-Wh[3]*yh3_bar ;Wh3yh3_bar
y_bar_st<-Wh1yh1_bar+Wh2yh2_bar+Wh3yh3_bar;y_bar_st
Yh1<-c(0,27,0,0,0,0,0,0,1068,81,418,0,162,397,0,0,0,0,0)
Yh2<-c(960,56,0,0,0,0,513,170,843,1627,661,0,511,0,1361,232,0,981,477,906,864,2422,2055,803,0,0,655)
Yh3<-c(1835,1744,1821,2496,7974,10238)
Sh21<-(1/(nh[1]-1))*sum((Yh1-yh1_bar)^2) ;Sh21
Sh22<-(1/(nh[2]-1))*sum((Yh2-yh2_bar)^2) ;Sh22
Sh23<-(1/(nh[3]-1))*sum((Yh3-yh3_bar)^2) ;Sh23
p<-((1-fh[1])*(Wh2[1]*Sh21))/nh[1]
p
q<-((1-fh[2])*(Wh2[2]*Sh22))/nh[2]
q
r<-((1-fh[3])*(Wh2[3]*Sh23))/nh[3]
r
z<-p+q+r
z



#q4
Nh<-c(63,199,53,25)
N<-340
n<-34
ybarh<-c(112.1,276.7,558.1,860.1)
ybarN<-sum(112.1*(63/340),276.7*(199/340),558.1*(53/340),860.1*(25/340)) ;ybarN
Sh<-c(56.3,116.4,186,363.1) 
Shsq<-Sh^2 ;Shsq
Wh<-Nh/sum(Nh) ;Wh
Whybarh<-Wh*ybarh ;Whybarh
f<-n/N ;f
s2<-(sum((Nh-1)*Shsq))/339+(sum(Nh*(ybarh-ybarN)^2))/339 ;s2 #s square
Vsrs<-((1-f)/n)*s2
Vsrs
VNh<-((1-f)/n)*sum(Wh*Shsq) ;VNh #propotional
VWh<-(((sum(Wh*Sh))^2)/n)-((sum(Wh*Shsq))/N) ;VWh #neyman optimum
VNh/Vsrs #eff of prop
VWh/Vsrs #eff of neyman 

#Q5
Nh<-c(18260,4315,2233,1057,567)
N<-sum(Nh);N
ybarh<-c(100000,250000,500000,1760000,2250000)
ybarN<-sum(100000*(18260/N),250000*(4315/N),500000*(2233/N),1760000*(1057/N),2250000*(567/N));ybarN
Sh<-c(80000,200000,600000,1900000,2500000)
Ch<-c(6,9,10,12,15)
n<-600
nh<-c(49,49,49,49,49)
Shsq<-Sh^2 ;Shsq
Wh<-Nh/sum(Nh) ;Wh
Whybarh<-Wh*ybarh ;Whybarh
f<-n/N ;f
s2<-(sum((Nh-1)*Shsq))/(N-1)+(sum(Nh*(ybarh-ybarN)^2))/(N-1) ;s2 #s square
Vsrs<-((1-f)/n)*s2
Vsrs
VNh<-((1-f)/n)*sum(Wh*Shsq) ;VNh #propotional
VWh<-(((sum(Wh*Sh))^2)/n)-((sum(Wh*Shsq))/N) ;VWh #neyman optimum

