install.packages('')
library(lme4)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(dplyr)

# Creating a group-means data set
group <- dataEmployment %>% 
  group_by(ID) %>% 
  summarise(Employed = mean(Employed))

# We can visualise the data like this
ggplot(dataEmployment, aes(x=ID,y=Employed,colour = factor(ID))) +
  geom_point(position=position_jitter(w=0.00, h=0.5)) + guides(colour=FALSE) +
  geom_point(data = group)

# Or have a look at group means as follows
ggplot(group, aes(x = ID, y = Employed)) +
  geom_bar(stat = "identity")

# Fitting a random intercept model
multilevelIntercept=lmer(Employed~(1|ID),REML=TRUE,
                            data=dataEmployment)
summary(multilevelIntercept)

# We can compute ICC1 by hand
# Note: the result is similar to the one we obtained earlier through 'multilevel' package
2.211/(2.211+7.904)

# OLS linear model without random intercept
EmploymentLinear=lm(Employed~1,data=dataEmployment)
summary(EmploymentLinear)

# Is model with random intercept significantly better 
# than the one based on simple OLS estimator?
logLik(EmploymentLinear)*-2
logLik(multilevelIntercept)*-2
2060.164-2040.312 
pchisq(19.852,1,lower=FALSE) # returns p-value for chi-square test
# The difference between log-likelihood functions is statistically significant
# Hence, the random intercept model is superior to a single-level one

#Let's add an independent variable
multilevelInterceptQual=lmer(Employed~Qualification+(1|ID),REML=TRUE,
                         data=dataEmployment)
summary(multilevelInterceptQual)

3.169/(3.169+5.924) #ICC1

EmploymentLinearQual=lm(Employed~Qualification,data=dataEmployment)
summary(EmploymentLinearQual)

logLik(EmploymentLinearQual)*-2
logLik(multilevelInterceptQual)*-2
2014.001-1967.619 
pchisq(19.852,1,lower=FALSE)

# Predicted values grouped by qualification
sjp.lmer(multilevelInterceptQual, type = "pred", facet.grid = FALSE,
         vars = c("Qualification"))
# plot fixed effects depending on group levels
sjp.lmer(multilevelInterceptQual, 
         type = "ri.slope", 
         facet.grid = FALSE)
#Random effects by predictor
sjp.lmer(multilevelInterceptQual,
         facet.grid = FALSE,
         sort.est = "sort.all",
         y.offset = .4)

# Tutorial - fit a random intercept model for 
# the attitudes towards abortion
# use age as an independent variable