install.packages('')
library(multilevel)
library(lme4)
library(plyr)
library(multicon)
library(lattice)

#Uploading the Workplace Employment Relations Survey
dataWERS=read.csv('wers.csv')
colnames(dataWERS)
summary(dataWERS)
length(unique(dataWERS$serno)) # How many workplaces are in the dataset?

#Recoding the union membership variable
dataWERS$Union=mapvalues(dataWERS$qd1,from=c(1,2,3),
                         to=c(1,0,0))
#Specifying the union membership variable as categorical with specific categories attached
dataWERS$Union=factor(dataWERS$Union,levels=c(0,1),
                      labels=c('Non-members','Union members'))
summary(dataWERS[,c(65)])

#Frequencies
prop.table(table(dataWERS$Union))
plot(dataWERS$Union,main='Union membership',col=c("red","blue"))

# Creating a compostive variable for job satisfaction
dataWERS$Satisfaction=composite(dataWERS[,15:22],
                                rel=TRUE)

# Can workplace level make a difference?
xyplot(Union~Satisfaction|as.factor(serno),
       data=dataWERS[1:1000,])

# Tutorial - Fit logistic regression using
# You need to alter yesterday's syntax in two ways
# 1. The command changes from 'lmer' to 'glmer'
# 2. in the main body of the syntax you need to specify
# family="binomial"
modelUnionMulti=glmer(Union~qd4+(1|serno),
                     family="binomial",data=dataWERS)
summary(modelUnionMulti)

# How much of the variance is due to level two?
Var2=4.513/(4.513+3.29)
Var2

Var3=3.127/(3.127+3.29)
Var3

# Predicted probabilities
# First for the average workplace
Pa=exp(-1.252)/(1+exp(-1.252))
Pa

# Then for 1SD above an average workplace
P1=exp(-1.252+2.124)/(1+exp(-1.252+2.124))
P1

# Lastly, for 1SD below an average workplace
P2=exp(-1.252-2.124)/(1+exp(-1.252-2.124))
P2

# Tutorial: Add job satisfaction as an explanatory variable
# Estimate how much of the total variation is explained at level two