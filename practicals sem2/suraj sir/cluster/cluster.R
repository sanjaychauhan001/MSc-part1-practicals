# question no 1
m<- matrix(c(0,0.76,2.97,4.88,3.86,0.76,0,0.8,4.17,1.92,2.97,0.8,0,0.21,1.51,4.88,
             4.17,0.21,0,0.51,3.86,1.92,1.51,0.51,0),nrow = 5,byrow = T)
d<- as.dist(m)
d
single_l<- hclust(d,method = 'single')
complete_l<- hclust(d,method = 'complete')
avg_l<- hclust(d,method = 'average')
par(mfrow=c (1,3))
plot(single_l)
plot(complete_l)
plot(avg_l)

# question no 2
m<- matrix(c(1,0.58,0.51,0.39,0.46,0.58,1,0.6,0.39,0.32,0.51,0.6,1,0.44,0.43,0.39,
             0.39,0.44,1,0.52,0.46,0.32,0.43,0.52,1),nrow=5,byrow=T)
d1<- as.dist(m)
d1
single_l<- hclust(d1,method = 'single')
complete_l<- hclust(d1,method = 'complete')
avg_l<- hclust(d1,method = 'average')
par(mfrow=c (1,3))
plot(single_l)
plot(complete_l)
plot(avg_l)

# question no 3
data<- read.csv(file.choose())
head(data)
data = data[-1]
d4<- dist(x=data,method = 'euclidean')
singlle<- hclust(d4,method = 'single')
complete<- hclust(d4, method = 'complete')
par(mfrow=c (1,2))
plot(singlle)
plot(complete)
singlle$height
singlle$order
