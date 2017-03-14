#what we need to install and read
install.packages('')
library(corrplot)

#step1: reading the dataset
dataUSA=read.csv('USAdata.csv')
dataUSA=read.csv(file.choose())
#step2: let's see the variable names 
names(dataUSA)
#now let's produce some basic descriptives
summary(dataUSA)

#Main study variables (log transformation)
dataUSA$trans_Poverty=log(dataUSA$Poverty)
dataUSA$trans_Doctors=log(dataUSA$Doctors)
dataUSA$trans_University=log(dataUSA$University)
dataUSA$trans_White=log(dataUSA$White)

#Control variables variables (log transformation)
dataUSA$trans_Unemployed=log(dataUSA$Unemployed)

#Correlation
correl=cor(dataUSA[,11:16])
corrplot(correl, method="circle")

#Regression model (control variables)
modelControl=lm(dataUSA$trans_Poverty~dataUSA$trans_Unemployed)
summary(modelControl)

#Regression model (full model)
modelFull1=lm(dataUSA$trans_Poverty~dataUSA$trans_Unemployed+
               dataUSA$trans_Doctors)
summary(modelFull1)

#Regression model (full model)
modelFull2=lm(dataUSA$trans_Poverty~dataUSA$trans_Unemployed+
               dataUSA$trans_Doctors+dataUSA$trans_White)
summary(modelFull2)

#Regression model (full model)
modelFull3=lm(dataUSA$trans_Poverty~dataUSA$trans_Unemployed+
               dataUSA$trans_Doctors+dataUSA$trans_White+dataUSA$trans_University)
summary(modelFull3)

#Inspecting regression assumptions
plot(modelFull3)

