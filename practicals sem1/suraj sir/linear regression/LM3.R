#Q1

#Loading the package
install.packages("MPV")
library("MPV")
#To load the dataset
data=table.b5
#OR
data=MPV::table.b5

#a)
?lm
model=lm(y~x6+x7,data=data)
#b)
summary(model) #ISke F ke value se hi significance bata sakte hai, ANoVA KA JARURAT HI NAHI, But idhar liya maine cause baadme MSE chahiye joh isme hai
#We can understand the required things ANOVA ki bhi jarurat nahi hai
#H0=B6=B7=0 vs H1:atleast one != 0
#p val <alpha so Rej Ho. Regression is Significant
#x7 bohot kam significant hai, baad mai we wil verify too
#R2 and AdjR2 isme diya hai

aov(model)
summary(aov(model)) 
#Isme partial F test hai to test sif of ind variable
#FOr overall we talk about SSR  - isme bhi x6 and x7 ka MS add karke MSR nikalke we can test


#c)T test summary(model) mai values hai
#Reject H0 as dono significant hai ie 0 nahi hai - x6 and x7
#T test summary mai hi hai
#Dono reject ho rahe hai

###d)
#Notes mai formula hai iska diya hua , For this we need Cii for that XTransposeX ka inverse chahiye
dataframe_1=data.frame(table.b5$x6,table.b5$x7)
matrix_1=as.matrix(dataframe_1)
matrix_1
XTX=t(matrix_1)%*%matrix_1
XTX_inv=solve(XTX) #TO find the inverse
XTX_inv

C66=XTX_inv[1,1]
C66
C77=XTX_inv[2,2]
C77

MSE=98 #Anova table se
?qt
coeff=c(0.018522,2.185733)#Coefficients i.e estimates of paramters-B6,B7
ci_lower_B6=coeff[1]-qt(0.975,24)*sqrt(MSE*C66)
ci_upper_B6=coeff[1]+qt(0.975,24)*sqrt(MSE*C66)
ci_B6=c(ci_lower_B6,ci_upper_B6)
ci_B6
#DF=n-p=Error ka df ANOVA table se lele
ci_lower_B7=coeff[2]-qt(0.975,24)*sqrt(MSE*C77)
ci_upper_B7=coeff[2]+qt(0.975,24)*sqrt(MSE*C77)
ci_B7=c(ci_lower_B7,ci_upper_B7)
ci_B7
#VErify using this

confint(model)
#Almost matches

#e))

model2=lm(y~x6,data=data)
summary(model2)
#pval<alpha - Reression is SIG:Fstatistic dekhna hota hai
#R2 and #Adj R2 hai isme
summary(aov(model2))

#THe model is satisfying because there is only 6% decrease in R2 due to deletion of x7 which is fine,
#ANyway we had seen that x7 is not that significant
#this we will verify in the next subparts too



#f)
MSE_2=114
ci_lower_B6_1=model2$coefficients[2]-qt(0.975,25)*sqrt(MSE_2*C66)
ci_upper_B6_1=model2$coefficients[2]+qt(0.975,25)*sqrt(MSE_2*C66)
CI_B6_1=c(ci_lower_B6_1,ci_upper_B6_1)
CI_B6_1
#OR
# SLR ka bhi formula we can use- Notes dekhlena

sxx=sum((data$x6-mean(data$x6))^2)
sxx
MSE_3=114 #Model se ANOVA ka
ci_lower_B6_1_2=model2$coefficients[2]-qt(0.975,25)*sqrt(MSE_3/sxx)
ci_upper_B6_1_2=model2$coefficients[2]+qt(0.975,25)*sqrt(MSE_3/sxx)
CI_3=c(ci_lower_B6_1_2,ci_upper_B6_1_2)
CI_3

confint(model2)
#THey are somewhat similar as compared to confint vala

#NOw to see the length
length1=ci_B6[2]-ci_B6[1]
length1
length2=CI_B6_1[2]-CI_B6_1[1]
length2
#The confidence intervals length just increases by 0.001, hence removing x7 from the model did not make too much diff
#HEnce x7 is not contributing too much to the model

#g)
MSE=98
MSE_2=114

#The MSE increased but not too much so x7 was not contributing very highly
#So we conclude that X7 is hardly significant

##Q2)
install.packages("readxl")
library("readxl")
?read_excel
p2q2=read_excel(file.choose())
View(p2q2)
##a)
#H0 female is non significant - Not needed
#H1 female is significant
model.2=lm(p2q2$Sales~p2q2$Age+p2q2$HS+p2q2$Income+p2q2$Black+p2q2$Female+p2q2$Price)
summary(model.2)
#Using the t test, we can we that Female is not significant, hence we do not reject H0, Hence Female is not needed

summary(aov(model.2))
#ISeey bhi upar ka hi conclusion mil raha hai

##b)
#H0:Female and HS are not needed that is both are insignificant
#H1: At least one of them is significant and is needed
summary(model.2) #USe t
summary(aov(model.2)) #use partial F
#Using t and F test We conclude that neither of them is needed in the model since it is insignificant

#c) We need to find the CI

#Jaise Q1 mai kiya tha vaise karna padega

#Notes mai formula hai iska diya hua , For this we need Cii for that XTransposeX ka inverse chahiye
dataframe_2=data.frame(p2q2$Age,p2q2$HS,p2q2$Income,p2q2$Black,p2q2$Female,p2q2$Price)
dataframe_2
matrix_2=as.matrix(dataframe_2)
matrix_2
dim(matrix_2)


XTX_1=t(matrix_2)%*%matrix_2
XTX_inv_1=solve(XTX_1) #TO find the inverse
XTX_inv_1

C33=XTX_inv_1[3,3]
C33

MSE_4=794 #Anova table se
?qt

ci_lower_Income=model.2$coefficients[4]-qt(0.975,44)*(sqrt(MSE_4*C33))
ci_upper_Income=model.2$coefficients[4]+qt(0.975,44)*(sqrt(MSE_4*C33))
ci_Income=c(ci_lower_Income,ci_upper_Income)
ci_Income
#DF=n-p=Error ka df ANOVA table se lele

#VErify using this

confint(model.2)
#Almost matches

#d)
model.21=lm(p2q2$Sales~p2q2$Age+p2q2$HS+p2q2$Black+p2q2$Female+p2q2$Price) #Income nikal diya
summary(model.21)
#26.78% variation is explained
#value of R2 decreased a little bit

#e)
model.22=lm(p2q2$Sales~p2q2$Age+p2q2$Income+p2q2$Price)
summary(model.22)
summary(aov(model.22))
summary(model.2)
summary(aov(model.2))
#30.32% is explained as they contain the most signifificant variables 

#f)
model.23=lm(p2q2$Sales~p2q2$Income)
summary(model.23)
#10% is explained, this is a low number since others were also contributing and we removed them
#Still 10% of the variation is explained

#a)
R2=0.9
n=25
k=2
p=k+1
F0=(R2*(n-p))/(k*(1-R2))
F0
?qf
Ftab=qf(0.95,k,n-p)
Ftab
#Highly significant regression
#b)
#We will create a loop to get the lowest value of R2 for which the Regression is significant i.e F0>Ftab


R2=seq(1,0,-0.001)
length(R2)
for (i in 1:length(R2)){
  F0[i]=(R2[i]*(n-p))/(k*(1-R2[i]))
  if (F0[i]>Ftab){
    print(R2[i])&print(F0[i])
  }
  else{
    print("0")
  }
}

#OR

a=c()
R2=seq(1,0,-0.001)
length(R2)
for (i in 1:length(R2)){
  F0[i]=(R2[i]*(n-p))/(k*(1-R2[i]))
  if (F0[i]>Ftab){
    a[i]=1
  }
  else{
    a[i]=0
  }
}
x=data.frame(F0,R2,a)
View(x)
#Joh last 1 hai uske corr mera R2 ka minimum value hai


#Hence the lowest value of R2 is 0.239 for which the regression is significant
R2_small=0.239

F0_at_SmallestR2=(R2_small*(n-p))/(k*(1-R2_small))
F0_at_SmallestR2

