# question no 1
y1<- c(8.04,6.95,7.58 ,8.81,8.33,9.96,7.24,4.26,10.84 ,4.82,5.68)
x1<- c(10,8,13,9,11,14,6,4,12,7,5)
cor(y1,x1)
model1<- lm(y1~x1)
summary(model1)

y2<- c(9.14,8.14,8.74 ,8.77,9.26 ,8.10,6.13,3.10,9.13,7.26,4.74)
x2<- c(10,8,13,9,11,14,6,4,12,7,5)
cor(y2,x2)
model2<- lm(y2~x2)
summary(model2)

y3<- c(7.46,6.77,12.74 ,7.11 ,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
x3<- c(10,8,13,9,11,14,6,4,12,7,5)
cor(y3,x3)
model3<- lm(y3~x3)
summary(model3)

y4<- c(6.58,5.76,7.71,8.84,8.47,7.04,5.25 ,12.50,5.56,7.91,6.89)
x4<- c(8,8,8,8,8,8,8,19,8,8,8)
cor(y4,x4)
model4<- lm(y4~x4)
summary(model4)


# question no 2
df<- read.csv(file.choose())
View(df)
#a
library(ggplot2)
ggplot(df,aes(x=Daily,y=Sunday)) + geom_point()

#b
ggplot(df,aes(x=Daily,y=Sunday)) + geom_point() + 
  geom_smooth(method = "lm")

# c
model<- lm(df$Sunday~df$Daily)
model$coefficients

#e
summary(model)
