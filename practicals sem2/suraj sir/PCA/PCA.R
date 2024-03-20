# question no 1
R<- matrix(c(1,0.676,0.875,0.676,1,0.699,0.875,0.699,1), nrow = 3, ncol = 3,byrow = T)
R
A<- eigen(R) #to find eigen value and eigen vector
A
#principal components
# y1<- 0.5911518*X1+0.5429954*X2+0.5964021*X3
# y1<- 0.4225138*X1-0.8383426*X2+0.3444762*X3
# y1<- 0.6870382*X1+0.04835036*X2-0.72501078*X3
# praportion of variance
A$values[1]/sum(A$values) #0.8345676
A$values[2]/sum(A$values) #0.1240282
A$values[3]/sum(A$values) #0.04140415
# correlation between components and variables
y1x1<- sqrt(A$values[1])*A$vectors[1,1]
y1x1
y1x2<- sqrt(A$values[1])*A$vectors[2,1]
y1x2
y1x3<- sqrt(A$values[1])*A$vectors[3,1]
y1x3
y2x1<- sqrt(A$values[2])*A$vectors[1,2]
y2x1
y2x2<- sqrt(A$values[2])*A$vectors[2,2]
y2x2
y2x3<- sqrt(A$values[2])*A$vectors[3,2]
y2x3


# question no 2
R<-matrix(c(1,0.25,0.25,0.25,1,0.25,0.25,0.25,1),nrow = 3,ncol = 3,byrow=T)
R
A<- eigen(R)
A
A$values[1]/sum(A$values) #0.5
A$values[2]/sum(A$values) #0.25
A$values[3]/sum(A$values) # 0.25
y1x1<- sqrt(A$values[1])*A$vectors[1,1]
y1x1
y1x2<- sqrt(A$values[1])*A$vectors[2,1]
y1x2
y1x3<- sqrt(A$values[1])*A$vectors[3,1]
y1x3
y2x1<- sqrt(A$values[2])*A$vectors[1,2]
y2x1
y2x2<- sqrt(A$values[2])*A$vectors[2,2]
y2x2
y2x3<- sqrt(A$values[2])*A$vectors[3,2]
y2x3

#question no 3
R<-matrix(c(1,0.920,0.875,0.625,0.920,1,0.889,0.750,0.875,0.889,1,0.425,0.625,0.750,0.425,1),nrow = 4,byrow=TRUE);R
A<- eigen(R)
A
A$values
A$values[1]/sum(A$values)
A$values[2]/sum(A$values)
A$values[3]/sum(A$values)
A$values[4]/sum(A$values)


# question no 4
data<- read.csv(file.choose())
head(data)
data[-1]
df<- data[-1]
s<- data.matrix(df)
#i
R<- cor(s)
R
A<- eigen(R)
A
#ii
princomp(R)$scores
