# practical linear model 

# question no 1
# 1
install.packages("matlib")
library(matlib)
A<- matrix(c(2,-3,4,1,1,0,1,-1,3,-3,5,0,4,-3,6,-1),nrow=4,ncol=4, byrow = TRUE)
A
B<- matrix(c(0,0,0,0), nrow = 4, ncol=1) # rank 2
B
qr(A)$rank

A_B<- matrix(c(2,-3,4,1,0,1,0,1,-1,0,3,-3,5,0,0,4,-3,6,-1,0),nrow=4,ncol=5,byrow=T)
A_B
qr(A_B)$rank # rank 2
# rank(A) = rank(B) < no.of unknowns means infinitely many solution

# 2

P<- matrix(c(1,2,-1,3,-1,2,2,-2,3,1,-1,1),nrow=4,ncol=3, byrow=T)
P
Q<- matrix(c(3,1,2,-1), nrow = 4, ncol = 1, byrow = T)
Q
qr(P)$rank # rank of P 3
qr.solve(P,Q)
# unique solution -1,4,4

# 3

X<- matrix(c(2,-3,1,1,2,3,4,-1,2),nrow = 3,ncol=3, byrow = T)
X
Y<- matrix(c(0,0,0),nrow = 3)
Y
qr.solve(X,Y)
# unique trival solution


# question no 2 find rank
# 1
L<- matrix(c(1,2,3,2),nrow = 2, ncol=2,byrow=T)
L
qr(L)$rank
# rank of L 2

# 2
M<- matrix(c(6,2,3,1), nrow=2, ncol=2, byrow=T)
M
qr(M)$rank 
# rank of M 1

# 3
N<- matrix(c(1,3,3,1,4,3,1,3,4), nrow=3,byrow = T)
N
qr(N)$rank
# rank of N 3

# 4
O<- matrix(c(1,2,3,2,4,6,3,6,9),nrow=3, ncol=3,byrow=T)
O
qr(A)$rank
#rank of O 2

# 5
P<- matrix(c(2,3,-1,-1,1,-1,-2,-4,3,1,3,-2,6,3,0,-7),nrow=4, ncol=4,byrow = T)
P
qr(P)$rank
# rank of P 3

Q<- matrix(c(3,-2,0,-1,-7,0,2,2,1,-5,1,-2,-3,-2,1,0,1,2,1,-6),nrow=4,byrow = T)
Q
qr(Q)$rank
# rank of Q 4

R<- matrix(c(1,0,2,1,0,1,-2,1,-2,2,-8,0),nrow = 3,ncol=4,byrow=T)
R
qr(R)$rank
# rannk of R 2


# question no 3
# 1
A<- matrix(c(1,-1,2,2,3,-2,2,5,6),nrow=3,ncol=3,byrow=T)
A
B<- diag(c(2,3,4),nrow=3)
B
A%*%B
B%*%A
A%%B==B%%A 
#A%*%B is not = to B%*%A

# 2
A<- matrix(c(3,1,0,0,4,2,0,0,3),nrow=3,ncol=3,byrow=T)
A
B<- matrix(c(5,1,12,0,7,-1,0,0,2),nrow=3,ncol = 3, byrow=T)
B
A%*%B
B%*%A
A%*%B==B%*%A
#AB is not equal to BA i.e. matrix doesn't satisfy commutative property

# question no 4
A<- matrix(c(1,0,3,2,0,-1,-1,1),nrow = 2,byrow = T)
A
A%*%t(A)
install.packages("psych")
library(psych)
tr(A%*%t(A))
t(A)%*%A
tr(t(A)%*%A)

# question no 5
X<- matrix(c(1,2,3,0,-1,-2,-1,0,-7),nrow=3, ncol = 3, byrow = T)
X
Y<- matrix(c(6,0,0,-3,4,0,0,-5,2),nrow=3,ncol=3,byrow=T)
Y
x_sq<- X%*%X
y_sq<- Y%*%Y
xy<- X%*%Y
yx<- Y%*%X
a<- x_sq + y_sq + xy + yx
a
b<- X+Y
c<- b%*%b
a==c
# a & c are equal

# question no 6
A1<- matrix(c(3,8,4,8,7,-1,4,-1,2),nrow=3,byrow=T)
A1
A2<- matrix(c(1,-1,3,-1,2,4,3,4,6),nrow=3,ncol=3,byrow=T)
A2
A<- A1+A2
A[1,2] == A[2,1]
A[3,1] == A[1,3]
A[3,2] == A[2,3]
# matrix is symmetric

# question no 7
A<- matrix(c(1,2,2,3),nrow = 2, byrow = T)
A
B<- matrix(c(1,0,2,-1),nrow = 2, byrow = T)
B
C<- matrix(c(1,3,4,5),nrow = 2, byrow = T)
C
tr(A%*%B%*%C) # 6
tr(B%*%C%*%A) # 6
tr(C%*%A%*%B) # 6
tr(B%*%A%*%C) # 14
