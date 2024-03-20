# question no 1
data<- read.csv(file.choose())
head(data)
str(data)
model<- lm(weights~fertilizers,data = data)
model
summary(model)
TukeyHSD(aov(weights~fertilizers,data = data))
plot(model)
shapiro.test(model$residuals)
install.packages('lmtest')
library(lmtest)
bptest(model)

#question no 2
data<- read.csv(file.choose())
head(data)
model<- lm(values~treatments,data)
model
summary(model)
TukeyHSD(aov(lm(values~treatments,data = data)))
plot(model)
shapiro.test(model$residuals)

# question no 3
data<- read.csv(file.choose())
head(data)
str(data)
model<- lm(values~items,data = data)
model
summary(model)
plot(model)
TukeyHSD(aov(lm(values~items,data = data)))
shapiro.test(model$residuals)
leveneTest(model)

#question no 4
data<- read.csv(file.choose())
head(data)
data$treatment<- as.factor(data$treatment)
data$varity<- as.factor(data$varity)
model<- lm(data$yeild~data$treatment+data$varity)
model
summary(model)
anova(model)
TukeyHSD(aov(lm(data$yeild~data$treatment+data$varity)))
plot(model)
shapiro.test(model$residuals)
bptest(model)
