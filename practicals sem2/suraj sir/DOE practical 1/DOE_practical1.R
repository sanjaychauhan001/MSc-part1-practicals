# question no 1
data<- read.csv(file.choose())
head(data)
data$A<- as.factor(data$A)
data$B<- as.factor(data$B)
data$C<- as.factor(data$C)
data$block<- as.factor(data$block)
str(data)
#without block effect
model1<- lm(obs~A+B+C+A:B+B:C+A:C, data = data)
model1
anova(model1)
# p-value of factor A,B,C are less than 0.05 they are signifcant
plot(model1)
# homoginity of variance
install.packages('car')
install.packages('carData')
library(carData)
library(car)
# H0: var are equal VS H1: var are not equal
leveneTest(obs~A,data = data) # car of group A are equal
leveneTest(obs~B,data = data) # car of group B are equal
leveneTest(obs~C,data = data) # car of group C are equal
# normality test
# H0: residuals are normally distributes vs H1 : not H0
shapiro.test(model1$residuals)
# since p-value < 0.05 residuals are not normally distributed
# model with block effect
model2<- lm(obs~A+B+C+A:B+A:C+B:C+block,data = data)
model2
anova(model2)
plot(model2)
shapiro.test(model2$residuals)
# not normal

# question no 2
data<- read.csv(file.choose())
head(data)
install.packages('doebioresearch')
install.packages('htmltools')
library(htmltools)
library(doebioresearch)
?splitplot
splitplot(data = data[4],block=data$replication,main.plot=data$varity,sub.plot=data$nitrogen,1)

# question no 3
data<- read.csv(file.choose())
head(data)
model<- lm(data$height~data$forest/data$tree)
model
anova(model)
plot(model)
summary(model)
shapiro.test(model$residuals)#normal
leveneTest(height~tree,data = data)
leveneTest(height~forest,data = data)
leveneTest(height~forest/tree,data = data)

# question no 4
data<- read.csv(file.choose())
head(data)
model<- lm(data$obs~data$machine/data$operator)
model
anova(model)
plot(model)
summary(model)
shapiro.test(model$residuals)
