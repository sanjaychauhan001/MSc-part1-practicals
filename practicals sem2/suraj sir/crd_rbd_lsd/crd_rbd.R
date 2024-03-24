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

# question no 5
#Hypothesis
#HoA;THERE IS NO SIGNIFICANT DIFF BET THE EFFECT OF DIFF BATCHES H1A:NOT H0A
#H0B:THERE IS NO SIGNIFICANT DIFF BET THE EFFECT OF DIFF OPERATORS H1B:NOT H0B
#H0C:THERE IS NO SIGNIFICANT DIFF BET THE EFFECT OF DIFF TREATMNETS  H1C:NOT H0C
data<- read.csv(file.choose())
head(data)
data$batches<- as.factor(data$batches)  
data$operators<- as.factor(data$operators)
data$formulation<- as.factor(data$formulation)
model<- lm(data$yeild~data$batches+data$operators+data$formulation)
model
summary(model)
anova(model)
plot(model)
TukeyHSD(aov(lm(data$yeild~data$batches+data$operators+data$formulation)))
shapiro.test(model$residuals)
bptest(model)
leveneTest(model)

# question no 6
data<- read.csv(file.choose())
head(data)
data$skin<- as.factor(data$skin)
data$rabbit<- as.factor(data$rabbit)
data$order<- as.factor(data$order)
model<- lm(data$yeild~data$skin+data$rabbit+data$order)
model
summary(model)
anova(model)
plot(model)
shapiro.test(model$residuals)
bptest(model)
leveneTest(model)
