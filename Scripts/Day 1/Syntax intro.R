install.packages('')
library(psych)

#Creating an object (vector) and printing it
a=c(1,2,3,4,5)
a
print(a)

#Creating and printing a matrix
B=matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=5,ncol=2)
B

#This function returns the sum of each row
apply(B,1,sum)
#A more sophisticated function that summates rows and adds 2 to the result
sum.plus=function(x){sum(x)+2}
apply(B,1,sum.plus)
#Writing matrix B into  the data frame
df1=data.frame(B)
df1

#Reading a dataset into the data frame
data=read.csv('WERS.csv')
data.frame=data

#Let's check variable names and produce basic descriptives
names(data)
summary(data)

#Decriptives using package 'psych'
describe(data)
describe(data[,3:10])
describeBy(data,group=data$qe1)

#Saving descriptives into an html file using package 'xtable'
install.packages('xtable')
library(xtable)
sum=describe(data)
sumtable=xtable(sum)
print.xtable(sumtable, type="html", file="summary.html") 
