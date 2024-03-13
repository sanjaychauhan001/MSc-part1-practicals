#question 1
d<-c(36,49,44,55,42,09,35,47,27,44,42,57,50,48,32,54,22,17,29,05,56,16,40,06,50,23,39,35,32,51,38,09,33,33,22,31,24,42,38,38)
N<-length(d);N
n<-5
k<-N/n
m<-matrix(d,nrow = k,ncol = n,byrow = FALSE);m
D<-as.data.frame(m);D
#q1
f<-function(x){
  s=c()
  for(t in 1:k){
    y<-c(x[t],x[t+k],x[t+2*k],x[t+3*k],x[t+4*k])
    print(y)
    yi<-sum(y);print(yi)
    ybar<-yi/n;print(ybar)
    yibar<-sum(yi)/n;print(yibar)
    q<-(y-yibar)^2
    sisq<-sum(q)/(n-1)
    s=c(s,sisq)
    
  }
  print(s)
}
f(d)

#q2
r<-6
p<-c(d[r],d[r+k],d[r+2*k],d[r+3*k],d[r+4*k])
ybar<-sum(p)/n;ybar
sisq<-sum((p-ybar)^2)/(n-1);sisq

#q3
f1<-function(x){
  m=c()
  for(t in 1:k){
    y<-c(x[t],x[t+k],x[t+2*k],x[t+3*k],x[t+4*k])
    yi<-sum(y)
    ybar<-sum(x)/length(x)
    yibar<-sum(yi)/n
    u<-(yibar-ybar)^2
    m=c(m,u)
    
  }
  l=sum(m)/k
  print(l)
}
f1(d)
#q4
y1<-c(sum(D$V1),sum(D$V2),sum(D$V3),sum(D$V4),sum(D$V5))
y11<-y1^2
Ti_sq<-sum(y11)
f2<-function(x){
  G=sum(x)
  Gsq=G^2
  w=Gsq/(n*k)
  t1=c()
  w1=c()
  s=c()
  for(t in 1:k){
    y<-c(x[t],x[t+k],x[t+2*k],x[t+3*k],x[t+4*k])
    print(y)
    ts<-sum(y^2)
    t1<-c(t1,ts)
    yi<-sum(y);print(yi)
    ybar<-yi/n;print(ybar)
    yibar<-sum(yi)/n;print(yibar)
    q<-(y-yibar)^2
    sisq<-sum(q)/(n-1)
    s<-c(s,sisq)
    
  }
  TSS=sum(t1)-w
  print(TSS)
  Bsamp=sum(s)/k
  print(Bsamp)
  Bstrat=(Ti_sq/k)-w
  print(Bstrat)
  sv=c("Bsamp","within sample","Bstrat","within strata","total")
  df=c(k-1,N-k,n-1,N-n,N-1)
  SS=c(Bsamp,TSS-Bsamp,Bstrat,TSS-Bstrat,TSS)
  MSS=SS/df
  data=data.frame(sv,df,SS,MSS)
  print(data)
}
f2(d)
#v_y_bar_sy<-a*[1+rho(n-1)]
a<-(k-1)/(n*k)*168.85714 #s^2wst=168.85714
b<-8.11/a
rho_wst<-(b-1)/4
rho_wst
#v_ybar_sy<-a*[1+rho(n-1)]
a<-((N-1)/(N*n))*200.05128
b<-8.11/a
rho_w<-(b-1)/4
rho_w

#question 2
#q1
N=40
n=4
k=10
f=n/N
d=c(0,1,1,2,5,4,7,7,8,6,6,8,9,10,13,12,15,16,16,17,18,19,20,20,24,23,25,28,29,27,26,30,31,31,33,32,35,37,38,38)
length(d)
m=matrix(d,ncol=10,byrow = T)
D=as.data.frame(m,row.names=c("S1","S2","S3","S4"))
colnames(D)=c("1","2","3","4","5","6","7","8","9","10")
D$Yi=rowSums(D[,c("1","2","3","4","5","6","7","8","9","10")])
D$Yi_sq=(D$Yi)^2
D$Yi_bar=rowMeans(D[,c("1","2","3","4","5","6","7","8","9","10")])
D
s_Yi=sum(D$Yi)
s_Yi_sq=sum(D$Yi_sq)
s_Yi_bar=sum(D$Yi_bar)
Yj=colSums(D[,c("1","2","3","4","5","6","7","8","9","10")]^2)
s_Yj_sq=52927
s_Yij_sq=sum(Yj)
CF=(s_Yi*s_Yi)/N
TSS=s_Yij_sq-CF
Bet_samp=(s_Yj_sq/n)-CF
Bet_st=(s_Yi_sq/k)-CF
sv=c("Bet_samp","within sample","Bet_strata","within strata","total")
df=c(k-1,N-k,n-1,N-n,N-1)
SS=c(Bet_samp,TSS-Bet_samp,Bet_st,TSS-Bet_st,TSS)
MSS=SS/df
data=data.frame(sv,df,SS,MSS)
print(data)
S_sq_wsy=data$MSS[2]
S_sq_wst=data$MSS[4]
S_sq=data$MSS[5]
v_Ybar_SRS=((N-n)/(N*n))*S_sq
v_Ybar_sy=(((N-1)/N)*S_sq)-(((n-1)/n)*S_sq_wsy)
v_Ybar_st=((N-n)/N*n)*S_sq_wst
v1=v_Ybar_SRS/v_Ybar_sy
v2=v_Ybar_st/v_Ybar_sy
v1
v2
v_Ybar_SRS
v_Ybar_sy
v_Ybar_st
#q2
N=40
n=4
k=10
f=n/N
d=c(6,8,7,7,4,5,2,1,1,0,6,8,9,10,13,12,15,16,16,17,27,29,28,25,23,24,20,20,19,18,26,30,31,31,33,32,35,37,38,38)
length(d)
m=matrix(d,ncol=10,byrow = T)
D=as.data.frame(m,row.names=c("S1","S2","S3","S4"))
colnames(D)=c("1","2","3","4","5","6","7","8","9","10")
D$Yi=rowSums(D[,c("1","2","3","4","5","6","7","8","9","10")])
D$Yi_sq=(D$Yi)^2
D$Yi_bar=rowMeans(D[,c("1","2","3","4","5","6","7","8","9","10")])
D
s_Yi=sum(D$Yi)
s_Yi_sq=sum(D$Yi_sq)
s_Yi_bar=sum(D$Yi_bar)
Yj=colSums(D[,c("1","2","3","4","5","6","7","8","9","10")]^2)
s_Yj_sq=52927
s_Yij_sq=sum(Yj)
CF=(s_Yi*s_Yi)/N
TSS=s_Yij_sq-CF
Bet_samp=(s_Yj_sq/n)-CF
Bet_st=(s_Yi_sq/k)-CF
sv=c("Bet_samp","within sample","Bet_strata","within strata","total")
df=c(k-1,N-k,n-1,N-n,N-1)
SS=c(Bet_samp,TSS-Bet_samp,Bet_st,TSS-Bet_st,TSS)
MSS=SS/df
data=data.frame(sv,df,SS,MSS)
print(data)
S_sq_wsy=data$MSS[2]
S_sq_wst=data$MSS[4]
S_sq=data$MSS[5]
v_Ybar_SRS=((N-n)/(N*n))*S_sq
v_Ybar_sy=(((N-1)/N)*S_sq)-(((n-1)/n)*S_sq_wsy)
v_Ybar_st=((N-n)/N*n)*S_sq_wst
v1=v_Ybar_SRS/v_Ybar_sy
v2=v_Ybar_st/v_Ybar_sy
v1
v2
v_Ybar_SRS
v_Ybar_sy
v_Ybar_st

#q3
v_Ybar_SRS/v_Ybar_sy
v_Ybar_st/v_Ybar_sy
