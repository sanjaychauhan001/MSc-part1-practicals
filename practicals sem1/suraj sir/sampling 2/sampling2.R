#question1
n<-5
N<-20
x<-c(368,512,384,503,475)
y<-c(31,31,33,29,38)
f<-n/N
xbar<-mean(x)
ybar<-mean(y)
Rhat<-ybar/xbar
X<-8830
Xbar<-X/N
S2x<-var(x);S2x
S2y<-var(y);S2y
r<-cor(x,y);r
#a
Ybar<-ybar #sample mean is ue of pop. mean
N*Ybar #total
V_Ybar<-((N-n)/(N*n))*S2y
V_Ybar
V_y<-var(y);V_y
#b
Ybar_hat<-Rhat*Xbar
Ybar_hat #sample mean estimator
N*Ybar_hat #total
V_Ybar_hat<-((1-f)/n)*(S2y+(Rhat^2)*S2x-2*Rhat*sqrt(S2x*S2y)*r)
V_Ybar_hat
#c
#ratio estimator and regression estimator are same if b=ybar/xbar
b<-r*sqrt(S2x*S2y)/S2x
Ybar_hat<-ybar+b*(Xbar-xbar)
Ybar_hat
N*Ybar_hat #total
V_Ybar_hat<-((1-f)/n)*(S2y+(b^2)*S2x-2*b*sqrt(S2x*S2y)*r)
V_Ybar_hat
V_Y_hat<-(N^2)*((1-f)/n)*(S2y+(b^2)*S2x-2*b*sqrt(S2x*S2y)*r);V_Y_hat

#question 2
N<-200
X<-11600
n<-10
yi<-c(61,42,50,58,67,45,39,57,71,53)
xi<-c(59,47,52,60,67,48,44,58,76,58)
b<-1
Ybar_hatlr<-ybar+b*(Xbar-xbar);Ybar_hatlr
N*Ybar_hatlr


#question3
N<-442
n<-67
ynbar<-42.6
xnbar<-67.6
Sx2<-129.6
Sy2<-79.2
Sxy<-57.2
f<-n/N
Rhat<-ynbar/xnbar
V_Yhat<-(N^2)*((1-f)/n)*(Sy2+(Rhat^2)*Sx2-2*Rhat*Sxy)
V_Yhat


#question 4
N<-82
n<-10
f<-n/N
Xbar<-72.9
yi<-c(17.6,18.2,19.4,10.3,21.4,22.2,17.4,15.9,24.4,21.8)
xi<-c(80.6,82.4,67.6,71.4,76.4,73.2,81.0,79.4,68.2,78.6)
ybar<-mean(yi)
xbar<-mean(xi)
Rhat<-ybar/xbar
#1
Ybar_hat<-Rhat*Xbar;Ybar_hat
#2
S2x<-var(xi);S2x
S2y<-var(yi);S2y
Sxy<-cov(xi,yi)
Sxy
b<-Sxy/S2x;b
Ybar_hatlr<-ybar+b*(Xbar-xbar);Ybar_hatlr
#3
rbar<-(1/n)*sum(yi/xi)
R_hathr<-rbar+((N-1)/((n-1)*(Xbar*N)))*sum((yi/xi)*(xi-xbar));R_hathr
Ybar_hathr<-R_hathr*Xbar
Ybar_hathr
#4
V_hatr<-((1-f)/n)*(S2y+(Rhat^2)*S2x-2*Rhat*Sxy);V_hatr #72.1
V_hat_reg<-((1-f)/n)*(S2y+(b^2)*S2x-2*b*Sxy);V_hat_reg #1.04


#question 5
N<-18
N1<-8
N2<-10
n1<-3
n2<-3
Xh1<-sum(630,729,865,305,569,427,326,481);Xh1
Yh1<-sum(250,248,359,129,223,335,412,503);Yh1
x1bar<-sum(865,569,481)/n1;x1bar
y1bar<-sum(359,223,503)/n1;y1bar
xi<-c(865,569,481)
yi<-c(359,223,503)
Sx12<-var(xi);Sx12
Sy12<-var(yi);Sy12
r1<-cov(xi,yi)/sqrt(Sx12*Sy12);r1
R1hat<-y1bar/x1bar;R1hat
f1<-n1/N1;f1
Xh2<-sum(1012,1181,780,815,1120,659,897,783,689,1217);Xh2
Yh2<-sum(340,416,247,306,403,271,357,295,218,398);Yh2
x2bar<-sum(1181,659,689)/n2;x2bar
y2bar<-sum(416,271,218)/n2;y2bar
xi<-c(1181,659,689)
yi<-c(416,271,218)
Sx22<-var(xi);Sx22
Sy22<-var(yi);Sy22
r2<-cov(xi,yi)/sqrt(Sx22*Sy22);r2
R2hat<-y2bar/x2bar;R2hat
f2<-n2/N2;f2
Yhat_rs<-sum(R1hat*Xh1,R2hat*Xh2);Yhat_rs
V1<-(N1^2)*((1-f1)/n1)*(Sy12+R1hat^2*Sx12-2*r1*R1hat*sqrt(Sx12*Sy12));V1
V2<-(N2^2)*((1-f2)/n2)*(Sy22+R2hat^2*Sx22-2*r2*R2hat*sqrt(Sx22*Sy22));V2
V_Yhat_rs<-V1+V2;V_Yhat_rs
#similarly for combined


