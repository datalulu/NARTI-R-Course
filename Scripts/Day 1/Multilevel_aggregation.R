install.packages('')
library(multilevel)

dataEmployment=read.csv('ScotlandEmployment.csv')
names(dataEmployment)
head(dataEmployment)
summary(dataEmployment)

#Exploring nested data
length(unique(dataEmployment$Person)) #number of unique cases (people)
length(unique(dataEmployment$ID)) #number of unique cases (districts)

#Is there a statistically significan difference between the groups?
variance=aov(Employed~as.factor(ID),data=dataEmployment)
summary(variance)

#Examining Intra-Class Correlations (reliability)
ICC1(variance) # Variance partitioning coefficient

#Visualising 
ICC2(variance) # Can groups be reliably differentiated?

#Within-Group agreement (rule of thumb >.7)
# Manifest variables
rwgEmployment=rwg(dataEmployment$Employed,dataEmployment$ID,ranvar=34)
summary(rwgEmployment) #ranvar - expected random variance = (A^2-1)/12

#Aggregating employment to the second level of analysis
#we can use syntax from Bliese (2013, p.46)
TEMP=aggregate(dataEmployment$Employed,list(dataEmployment$ID),mean) #Aggregates variable to its group mean
names(TEMP)=c("ID","G.Employed") #giving names
summary(TEMP)
dataEmploymentMerged<-merge(dataEmployment,TEMP,by="ID") #merge group and individual data
names(dataEmploymentMerged)

#Plotting individual and group level effects
plot(dataEmploymentMerged$Qualification,dataEmploymentMerged$Employed,
     xlab="Qualification",
     ylab="Employment",type="n",ylim = c(2,5))
abline(lm(Employed~Qualification,data=dataEmploymentMerged)) # plots the individual-level slope
abline(lm(G.Employed~Qualification,data=dataEmploymentMerged),col='red') #group-level slope
