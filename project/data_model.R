#Data Model

#data sources ----
#http://deathtimeline.com/ -> for season, ep, name, time, killed by, and type
#http://regressing.deadspin.com/valar-morghulis-a-statistical-guide-to-deaths-in-game-1618282560  -> for allegiance/house
#http://gameofthrones.wikia.com/  -> for allegiance/house 
#http://www.r-statistics.com/2013/07/creating-good-looking-survival-curves-the-ggsurv-function/  -> for ggsurv function

#packages ----
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rvest)
library(survival)
library(nlme)
library(lubridate)
library(locfit)
library(survMisc)
library(muhaz)
library(flexsurv)
library(ggthemes)
library(MASS)

#several parametric fits for deaths (no covariates) and their plots and summaries ----
exp.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="exponential")
wei.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="weibull")
gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="gamma")
lognorm.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="lnorm")
gen.gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="gengamma")
loglog.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="llogis")

par(mfrow = c(3,3))
plot(exp.fit, conf.int=F, ci=F, mark.time=T, main="Exponential and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(wei.fit, conf.int=F, ci=F, mark.time=T,  main="Weibull and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(gamma.fit, conf.int=F, ci=F, mark.time=T,  main="Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(lognorm.fit, conf.int=F, ci=F, mark.time=T, main="Log-Normal and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(gen.gamma.fit, conf.int=F, ci=F, mark.time=T, main="Gen. Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(loglog.fit, conf.int=F, ci=F, mark.time=T, main="Log-Log and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")

#comparing AIC scores
aic <- as.data.frame(c(exp.fit$AIC, wei.fit$AIC,gamma.fit$AIC,lognorm.fit$AIC,gen.gamma.fit$AIC,loglog.fit$AIC))
dist <- c("Exponential", "Weibull", "Gamma", "Log Normal", "Gen. Gamma", "Log Log")
aic <- cbind(aic, dist)
names(aic)[1] <- "AIC"

(aic.plot <- ggplot(aic, aes(x=factor(reorder(dist, AIC)), y=AIC, fill=dist)) + geom_bar(stat="identity") + theme_hc() + 
  labs(x="", y="AIC", title="Comparing AIC Scores") + guides(fill=F))

#several parametric fits for deaths w/covariates and their plots and summaries ----

exp.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~season + episode + type + factor(house2), data=deaths, dist="exponential")
wei.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~season + episode + type + factor(house2), data=deaths, dist="weibull")
gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~season + episode + type + factor(house2), data=deaths, dist="gamma")
lognorm.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~season + episode + type + factor(house2), data=deaths, dist="lnorm")
gen.gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~season + episode + type + factor(house2), data=deaths, dist="gengamma")
loglog.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~season + episode + type + factor(house2), data=deaths, dist="llogis")

par(mfrow = c(3,3))
plot(exp.fit, conf.int=F, ci=F, mark.time=T, main="Exponential and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(wei.fit, conf.int=F, ci=F, mark.time=T,  main="Weibull and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(gamma.fit, conf.int=F, ci=F, mark.time=T,  main="Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(lognorm.fit, conf.int=F, ci=F, mark.time=T, main="Log-Normal and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(gen.gamma.fit, conf.int=F, ci=F, mark.time=T, main="Gen. Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(loglog.fit, conf.int=F, ci=F, mark.time=T, main="Log-Log and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")

#comparing AIC scores
aic <- as.data.frame(c(exp.fit$AIC, wei.fit$AIC,gamma.fit$AIC,lognorm.fit$AIC,gen.gamma.fit$AIC,loglog.fit$AIC))
dist <- c("Exponential", "Weibull", "Gamma", "Log Normal", "Gen. Gamma", "Log Log")
aic <- cbind(aic, dist)
names(aic)[1] <- "AIC"

(aic.plot <- ggplot(aic, aes(x=factor(reorder(dist, AIC)), y=AIC, fill=dist)) + geom_bar(stat="identity") + theme_hc() + 
  labs(x="", y="AIC", title="Comparing AIC Scores") + guides(fill=F))

#AFT Model ----
#generalized gamma AFT model based on lowest AIC in previous step
gengamma.fit1 <- flexsurvreg(Surv(deaths$min, deaths$murdered) ~ season + episode + type + factor(house2), data=deaths, dist="gengamma")
gengamma.fit #produces NA's, trgin next lowest AIC (Weibull)

#weibull AFT model
weibull.fit1 <- survreg(Surv(deaths$min, deaths$murdered) ~ season + episode + type + factor(house2), data=deaths, dist="weibull")
summary(weibull.fit1)

#model building
#StepAIC
stepAIC(weibull.fit1) #only "type" important

#best subsets - need to re-define type and house2 vars
deaths$type2 <- 1
deaths$type2[deaths$type == "Minor"] <- 2
deaths$type2[deaths$type == "Supporting"] <- 3

deaths$house3 <- 1
deaths$house3[deaths$house2 == "Night's Watch"] <- 2
deaths$house3[deaths$house2 == "House Stark"] <- 3
deaths$house3[deaths$house2 == "House Lannister"] <- 4
deaths$house3[deaths$house2 == "House Targaryen"] <- 5

IndMatrix <- expand.grid(0:1,0:1,0:1,0:1)
Covariates <- deaths[,c(2,3,12,13)]
AICvals <- rep(0,2^4)
AICvals[1] <- extractAIC(survreg(Surv(deaths$min, deaths$murdered) ~ 1, dist="weibull"))[2]

for(i in 2:2^4){
  Covs <- Covariates[,which(IndMatrix[i,]==1)]
  if(is.vector(Covs)==FALSE) Formula <- formula(paste("Surv(deaths$min, deaths$murdered) ~ ", paste(Covs, collapse=" + "))) else Formula <- formula(Surv(deaths$min, deaths$murdered) ~ Covs)
  AICvals[i] <- extractAIC(survreg(Formula, dist="weibull"))[2]
}

Covariates[1,which(IndMatrix[which(AICvals==min(AICvals)),]==1)] #best model is intercept only???

#fitting new, reduced model based on StepAIC
weibull.fit2 <- survreg(Surv(deaths$min, deaths$murdered) ~ type, data=deaths, dist="weibull")
summary(weibull.fit2)

#check model fit - seems weird,  is it plotting 1 line for each level of type??
fit.final <- flexsurvreg(Surv(deaths$min, deaths$murdered) ~ type2, data=deaths, dist="weibull")
km.fit <- survfit(Surv(deaths$min, deaths$murdered) ~ 1)
plot(km.fit, fun="cumhaz", conf.int = FALSE, xlab="Time (Min)", ylab="Cumulative Hazard", main="Comparing AFT and K-M Cumulative Hazard")
lines(fit.final, type="cumhaz", ci=FALSE)

#Cox PH Model

