#Leeds Q-Step course
# Day 3 Morning lab: ML Growth modelling

#Libraries

library(lme4)
library(ggplot2)
library(faraway)
library(dplyr)
library(psych)
library(Hmisc)
library(Rmisc)
library(sjPlot)

#Data
alco<- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/alcohol1_pp.txt", 
                  header=T, sep=",")
attach(alco)

names(alco)  
head(alco)
describe(alco) 

length(unique(alco$id)) 

#Data plots

ggplot(alco, aes(x=age_14, y=alcuse, colour = factor(id))) + 
  geom_line(position=position_jitter(w=0.00, h=0.5)) + guides(colour=FALSE) +
  geom_smooth(method=lm, se=FALSE, colour="black", size=2)


ggplot(alco, aes(x=alcuse, colour=as.factor(age))) + geom_density() 


error <- summarySE(data=alco, measurevar="alcuse", groupvars="age", 
                   na.rm=FALSE, conf.interval=.95)
ggplot(error, aes(x=age, y=alcuse, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=alcuse-ci, ymax=alcuse+ci) , data=error) +
  geom_point(shape=21, size=3, fill="white") 

alco20 <- filter(alco, id<= 20) # select subsample with command filter
ggplot(alco20, aes (x=age_14, y=alcuse)) + 
  geom_line() + facet_wrap( ~ id) # facet_wrap produces a trellis of individual graphs by id

ggplot(alco20, aes (x=age_14, y=alcuse)) + geom_point () + 
  geom_smooth(method=lm, se=FALSE) + facet_wrap( ~ id)


ggplot(alco, aes(x=age_14, y=alcuse, colour = factor(id))) + 
  geom_smooth(method=lm, se=FALSE) + guides(colour=FALSE) +
  facet_wrap(~ coa) + geom_smooth(method=lm, se=FALSE, colour="black", size=2)

alco$peerbin <- cut(alco$peer,
                    breaks=c(-Inf, 1.01756 , Inf),
                    labels=c("low","high"))


ggplot(alco, aes(x=age_14, y=alcuse, colour = factor(id))) + 
  geom_smooth(method=lm, se=FALSE) + guides(colour=FALSE) +
  facet_wrap(~ peerbin) + stat_smooth(method=lm, se=FALSE, colour="black", size=2)

#Models:

#Model A: Unconditional Means Model

model.a <- lmer(alcuse ~ 1 + 1|id , REML=FALSE)
summary(model.a)
model.a.vc<-as.data.frame(VarCorr(model.a))
model.a.icc<-model.a.vc[1,4]/ (model.a.vc[1,4] + model.a.vc[2,4])            
model.a.icc

#Model B: Unconditional Growth Model
model.b <- lmer(alcuse ~ age_14 + (age_14|id) , REML=FALSE)
summary(model.b)

#Model C: Growth model with predictor COA
model.c <- lmer(alcuse ~ coa + age_14 + coa:age_14 + (age_14|id) , REML=FALSE)
summary(model.c)
sjp.lmer(model.c, type="fe")


##Model D: Growth model with predictors COA + PEER
model.d <- lmer(alcuse ~ coa + peer + age_14 + coa:age_14
                + peer:age_14 + (age_14|id) , REML=FALSE)
summary(model.d)
sjp.lmer(model.d, type="fe")


##Model E: Growth model with predictors COA + PEER
model.e <- lmer(alcuse ~ coa + peer + age_14 + peer:age_14 
                + (age_14|id) , REML=FALSE)
summary(model.e)
sjp.lmer(model.e, type="fe")

##Model E: "Final" model with centered predictors
model.f <- lmer(alcuse ~ coa + cpeer + age_14 + cpeer:age_14 
                + (age_14|id) , REML=FALSE)
summary(model.f)
sjp.lmer(model.e, type="fe")

#Model comparison
anova(model.a,model.b,model.c, model.d, model.e, model.f)

#Visualising models:

#Model B: Population average trajectories

model.b <- lmer(alcuse ~ age_14 + (age_14|id), REML=FALSE)

fixef.b <- fixef(model.b)

fit.b <- fixef.b[[1]] + alco$age_14[1:3]*fixef.b[[2]]

plot(alco$age[1:3], fit.b, ylim=c(0, 2), type="b", 
     ylab="predicted alcuse", xlab="age")

title("Model B \n Unconditional growth model")



###Model C: Trajectories by COA (Y/N)


fixef.c <- fixef(model.c)
fit.c0 <- fixef.c[[1]] + alco$age_14[1:3]*fixef.c[[3]]
fit.c1 <- fixef.c[[1]] + fixef.c[[2]] + 
  alco$age_14[1:3]*fixef.c[[3]] +
  alco$age_14[1:3]*fixef.c[[4]]
plot(alco$age[1:3], fit.c0, ylim=c(0, 3), type="b", 
     ylab="predicted alcuse", xlab="age")
lines(alco$age[1:3], fit.c1, type="b", pch=17)   
title("Model C \n Uncontrolled effects of COA") 
legend(14.5, 3, c("COA=0", "COA=1"), pch=c(1, 17))


sjp.lmer(model.c, type = "pred", facet.grid = FALSE,
vars = c("age_14", "coa"))



###Model E: Trajectories by COA (Y/N) & PEER (High 1.381 vs Low .655)

fixef.e <- fixef(model.e)
fit.ec0p0 <- fixef.e[[1]] + .655*fixef.e[[3]] +
alco$age_14[1:3]*fixef.e[[4]] +
.655*alco$age_14[1:3]*fixef.e[[5]]   
fit.ec0p1 <- fixef.e[[1]] + 1.381*fixef.e[[3]] +
alco$age_14[1:3]*fixef.e[[4]] +
1.381*alco$age_14[1:3]*fixef.e[[5]] 
fit.ec1p0 <- fixef.e[[1]] + fixef.e[[2]] + .655*fixef.e[[3]] +
alco$age_14[1:3]*fixef.e[[4]] +
.655*alco$age_14[1:3]*fixef.e[[5]] 
fit.ec1p1 <- fixef.e[[1]] + fixef.e[[2]] + 1.381*fixef.e[[3]] +
alco$age_14[1:3]*fixef.e[[4]] +
1.381*alco$age_14[1:3]*fixef.e[[5]]
plot(alco$age[1:3], fit.ec0p0, ylim=c(0, 3), type="b", 
ylab="predicted alcuse", xlab="age", pch=2)
lines(alco$age[1:3], fit.ec0p1, type="b", pch=0)   
lines(alco$age[1:3], fit.ec1p0, type="b", pch=17)   
lines(alco$age[1:3], fit.ec1p1, type="b", pch=15)   
title("Model E \n *Final* model for the controlled effects of COA") 
legend(14.5, 3, c("COA=0, low peer", "COA=0, high peer", 
"COA=1, low peer", "COA=1, high peer"), pch=c(2, 0, 17,15))


## Model diagnostics


modelf_diag <- fortify(model.f)
modelf_diag <- as.data.frame(modelf_diag)

re <- ranef(model.f)
str(re)
names(re$id)[1] <- "Intercept"


### Normality



ggplot(modelf_diag, aes(sample=.scresid)) +
stat_qq()

ggplot(re$id, aes(sample=Intercept)) +
stat_qq()

ggplot(re$id, aes(sample=age_14)) +
stat_qq()



### Homoskedasticity and linearity


ggplot(modelf_diag, aes(x=.fitted, y=.scresid)) +
geom_point() +
geom_smooth()


### Outliers


ggplot(modelf_diag, aes(x=.fitted,
y=.scresid, color=age_14)) +
geom_text(aes(label=id))


###Clustering of residuals


#std residuals by Subject - most means near 0
ggplot(modelf_diag, aes(x=id, y=.scresid)) +
geom_point() +
stat_summary(color="red")















