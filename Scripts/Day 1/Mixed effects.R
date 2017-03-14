library(lme4)
library(psych)
library(lattice)

#Let's continue working with the attitudes data and
# fit a random coefficients model
dataAbortion=read.csv(file.choose())
names(dataAbortion)
describe(dataAbortion)

#Plotting the means
group2 <- dataAbortion %>% 
  group_by(ID) %>% 
  summarise(Attitudes = mean(Attitudes))

ggplot(dataAbortion, aes(x=ID,y=Attitudes,colour = factor(ID))) +
  geom_point(position=position_jitter(w=0.00, h=0.5)) + guides(colour=FALSE) +
  geom_point(data = group2)

ggplot(group2, aes(x = ID, y = Attitudes)) +
  geom_bar(stat = "identity")

#Random intercept model first
AbortionIntercept=lmer(Attitudes~(1|ID),REML=TRUE,
                         data=dataAbortion)
summary(AbortionIntercept)

#ICC1
0.3876/(0.3876+3.0249)

#OLS linear model
AbortionLinear=lm(Attitudes~1,data=dataAbortion)
summary(AbortionLinear)

#Deviance statistics
logLik(AbortionLinear)*-2
logLik(AbortionIntercept)*-2
4275.614-4227.772 
pchisq(47.842,1,lower=FALSE)

# Visualising  the relationship between 
# Attitdues and age
ggplot(dataAbortion, aes(x=Age,y=Attitudes,colour = factor(ID))) +
  geom_point(position=position_jitter(w=0.00, h=0.5)) + guides(colour=FALSE) +
  geom_smooth(method=lm, se=FALSE, colour="black", size=2)


xyplot(Attitudes~Age|as.factor(ID),data=dataAbortion[1:200,], 
       type=c("p","g","r"),
       col="dark blue",col.line="black", 
       xlab="Age", ylab="Attitudes")

#Density plot by age
ggplot(dataAbortion, aes(x=Attitudes, colour=as.factor(Age))) + geom_density()

# Adding age as a predictor
AbortionIntercept2=lmer(Attitudes~Age+(1|ID),REML=TRUE,
                       data=dataAbortion)
summary(AbortionIntercept2)

#ICC1
0.4505/(0.4505+2.9725)

#Adding random slope
dataAbortion$AgeMC=scale(dataAbortion$Age,center=TRUE,scale=TRUE)
AbortionMixed=lmer(Attitudes~AgeMC+(AgeMC|ID),REML=TRUE,
                        data=dataAbortion)
summary(AbortionMixed)

logLik(AbortionMixed)*-2
logLik(AbortionIntercept2)*-2
4223.842-4186.753
pchisq(37.089,2,lower.tail=FALSE)

#Fixed effects
sjp.lmer(AbortionMixed, 
         type = "ri.slope", 
         facet.grid = FALSE)

# plot random-slope-intercept
sjp.lmer(AbortionMixed, type = "rs.ri", sample.n = 50)

# plot random-slope-intercept, plot subjects 1, 5 and 7.
sjp.lmer(AbortionMixed, type = "rs.ri", 
         sample.n = c(1, 5, 7),
         show.legend = TRUE)

#Model fit
sjp.lmer(AbortionMixed, type = "re.qq")
plot(AbortionMixed)

#How errors are clustered
diag2=fortify(AbortionMixed)
ggplot(diag2, aes(x=ID, y=.scresid)) + geom_point() +
  stat_summary(color="red")
