install.packages('markovchain')
library(markovchain)
# question no 1
#i
A<- matrix(c(0,1,0,1),nrow=2,byrow = T)
A
rowSums(A) # tpm
#ii
B<- matrix(c(0.3,0.2,0.5,0.2,0.3,0.5,0.5,0.5,0),nrow = 3,byrow = T)
B
rowSums(B) # tpm
#iii
C<- matrix(c(0.5,0.1,0.5,0.6,0.5,0.1,0,1,1),nrow=3,byrow=T)
C
rowSums(C) # not tpm

# question no 2
P<- matrix(c(0.5,0,0.5,0,1,0,0.5,0,0.5),nrow=3,byrow=T)
P
tpm<- new('markovchain',transitionMatrix=P,states=c("0","1","2"))
tpm
plot(tpm)
summary(tpm)

# question no 3
P<- matrix(c(0.5,0.5,0,0,0.5,0.5,0,0,0,0,0,1,0,0,1,0),nrow=4,byrow = T)
P
tpm<- new('markovchain',transitionMatrix=P,states=c("0","1","2","3"))
tpm
summary(tpm)

# question no 4
P<- matrix(c(1,0,0,0,0,0.25,0.75,0,0,0,0,0.5,0.5,0,0,0.25,0.25,0,
             0.25,0.25,0,0,0,0.5,0.5),nrow=5,byrow=T)
P
tpm<- new('markovchain',transitionMatrix=P,states=c("0","1","2","3","4"))
tpm
summary(tpm)

# question no 5
P<- matrix(c(0.5,0.5,0,0,0,0,0.5,0.5,0,0,0.5,0,0.5,0,0,0,0,0,1,0,0.2,0.2,
             0.2,0.2, 0.2),nrow = 5,byrow=T)
P
tpm<- new('markovchain',transitionMatrix=P,states=c("0","1","2","3","4"))
tpm
summary(tpm)
communicatingClasses(tpm)
absorbingStates(tpm)
period(tpm)
steadyStates(tpm)
recurrentStates(tpm)
meanRecurrenceTime(tpm)

# question no 6
P<- matrix(c(0.5,0.5,0.25,0.75),nrow=2,byrow = T)
P
tpm<- new("markovchain",transitionMatrix=P,states=c("0","1"))
tpm
rownames(P)<- c("0","1")
colnames(P)<- c("0","1")
matrix.power(P,3)
matrix.power(P,4)

# question no 7
P<-matrix(c(0,1/2,1/2,0,1/3,0,0,2/3,1,0,0,0,0,0,0,1),nrow = 4,byrow = TRUE)
P
tpm<- new('markovchain',transitionMatrix=P,states=c("1","2","3","4"))
tpm
plot(tpm)
communicatingClasses(tpm)
is.irreducible(tpm)
absorbingStates(tpm)
summary(tpm)
meanRecurrenceTime(tpm)

# question no 8
P<-matrix(c(1/8,1/4,1/8,1/2,3/4,0,0,1/4,0,3/4,0,1/4,1/8,0,7/8,0),nrow = 4,byrow = TRUE)
P
tpm<- new('markovchain',transitionMatrix=P,states=c("0","1","2","3"))
tpm
steadyStates(tpm)

# question no 9
P<-matrix(c(0.5,0,0.5,0,1,0,0.5,0,0.5),nrow = 3,byrow = TRUE)
P
tpm<-new("markovchain",transitionMatrix=P,states=c("0","1","2"))
tpm
period(tpm)
summary(tpm)
steadyStates(tpm)
