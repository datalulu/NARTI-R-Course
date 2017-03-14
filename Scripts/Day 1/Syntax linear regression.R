#We need the following packages
install.packages('')
library(tidyr)
library(plotly)
library(corrplot)
library(car)
library(gvlma)

#Reading data into R
life=read.csv(file.choose())
names(life)
gdp=read.csv(file.choose())
names(gdp)

#Transforming data from wide into long
life_new=gather(life[,2:57], Year, Life_exp, X1960:X2014, factor_key=TRUE)
head(life_new)
summary(life_new)
gdp_new=gather(gdp[,2:57], Year, GDP_pc, X1960:X2014, factor_key=TRUE)
head(gdp_new)

#Merging datasets
data_final=merge(life_new,gdp_new,by=c('Code','Year'))
head(data_final)
names(data_final)

#Probability density function of the main variables
ggplot(data_final,aes(x=Life_exp)) + geom_density()
ggplot(data_final,aes(x=GDP_pc)) + geom_density()

#Let's log transform the variables
data_final$Life_expLog=log(data_final$Life_exp)
data_final$GDP_pcLog=log(data_final$GDP_pc)

#Perform Pearon's correlation test
cor.test(data_final$Life_exp,data_final$GDP_pc,method=c('pearson'))
#Or like that
corr=cor(na.omit(data_final[,5:6]))
corrplot(corr, method="circle")

#Regression model
modelLife=lm(Life_expLog~GDP_pcLog,data=data_final)
summary(modelLife)
#Regression assumptions
plot(modelLife)
#Check the mean of residuals
mean(modelLife$residuals)
#Collinearity
vif(modelLife)
#Checking assumptions automatically
gvlma(modelLife)

#We can extract various parameters from the regression model
coef=coefficients(model)       # coefficients
resid=residuals(model)          # residuals
pred=predict(model)            # fitted values
rsq=summary(model)$r.squared   # R-sq for the fit
se=summary(model)$sigma      # se of the fit

#Scatter plot of life expectancy and GDP per capita
plot_ly(data = data_final, x = ~GDP_pcLog, y = ~Life_expLog,
        marker = list(size = 10,
                      color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(152, 0, 0, .8)',
                                  width = 2))) %>%
  layout(title = 'Life expectancy and GDP per capita',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

#Subsetting
dataCHN=subset(data_final,Code=="CHN")
dataUSA=subset(data_final,Code=="USA")

# Tutorial 1: Please preform linear regression 
# for China and the US separately
# inspect model fit
# plot the correlation graphs using plotly package

# Tutorial 2: Using 'USAdata.csv' perform multiple
# regression analysis. Use Poverty as a dependent variable
# use the ratio of doctors, share of white population 
# and people with university degress as independent variables
# use unemployment as a control variable
# don't forget to log-transform the variables

