# question no 1
R<- matrix(c(1,0.676,0.875,0.676,1,0.699,0.875,0.699,1),nrow=3,byrow=T)
R
install.packages('psych')
library(psych)
princomp(R)
eigen(R)
# y1 = 0.5911518*x1 + 0.5429954*x2 + 0.5964021*x3
# y2 = 0.4225138*x1 - 0.8383426*x2 + 0.3444762*x3

p1<- eigen(R)$values[1]/sum(eigen(R)$values)
p1 #0.8345676
p2<- eigen(R)$values[2]/sum(eigen(R)$values)
p2 #0.1240282
# fro m=1
A<- principal(R,nfactors = 1, rotate = 'None')
A
A$loadings
A$uniquenesses
# fro m=2
B<- principal(R,nfactors = 2, rotate = 'None')
B
B$loadings
B$uniquenesses

# question no 2
R<- matrix(c(1,0.505,0.569,0.602,0.621,0.603,0.505,1,0.422,0.467,0.482,0.45,0.569,
             0.422,1,0.926,0.877,0.878,0.602,0.467,0.926,1,0.874,0.894,0.621,0.482,
             0.877,0.874,1,0.937,0.603,0.45,0.878,0.894,0.937,1),nrow=6,byrow=T)
R
A<- principal(R,nfactors = 2, rotate = 'None')
A
B<- factanal(covmat = R,factors = 2,rotation = 'none')
B
#1
A$loadings
B$loadings
#ii
A$uniquenesses
B$uniquenesses
#iii
A$communality
1-B$uniquenesses # communality = 1- specific variance
# iv
C<- principal(r=R,nfactors = 6, rotate = 'none')
C
# 0.74 0.13 
#v
A$residual
A$residual-diag(A$residual)
#R-LL'-diag(specific var)
R-(A$loadings%*%t(A$loadings)) - diag(A$uniquenesses,6)
W<- R-(B$loadings%*%t(B$loadings)) - diag(B$uniquenesses,6)
W-diag(W)
#vi
A1<- principal(r=R,nfactors = 2, rotate = 'varimax')
A$loadings
B1<- factanal(covmat = R, rotation = 'varimax', factors=2)
B1$loadings

# question no 3
R<- matrix(c(1,0.63,0.45,0.63,1,0.35,0.45,0.35,1),nrow=3, byrow = T)
R
R<- cov2cor(R)
R
B<- principal(R,nfactors = 2, rotate = 'none')
B
# i
B$communality
# ii
B$loadings #corr L11 is highest

# question no 4
data<- read.csv(file.choose())
data[-1]
R<- cor(data[-1])
R
# i
B<- principal(r=R,nfactors = 6,rotate = 'none')
B
B$loadings
C<- factanal(covmat = R,factors = 3, rotation = 'none')
C$loadings
# ii
scree(data[-1]) # 2
# iii
B1<-principal(R,nfactors=6,rotate = 'varimax');B1
B1$loadings
C1<-factanal(covmat=R,factors = 3,rotation = 'varimax')
C1$loadings
# iv
?factor.scores
factor.scores(x=R,f=C$loadings)
