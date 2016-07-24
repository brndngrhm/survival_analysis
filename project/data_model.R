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
exp.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~ episode + factor(house2) + type, data = deaths,dist="exponential")
wei.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~ episode + factor(house2) + type,  data = deaths,dist="weibull")
gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~ episode + factor(house2) + type,  data = deaths,dist="gamma")
lognorm.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~ episode + factor(house2) + type,  data = deaths,dist="lnorm")
gen.gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~ episode + factor(house2) + type,  data = deaths,dist="gengamma")
loglog.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~ episode + factor(house2) + type,  data = deaths,dist="llogis")

par(mfrow = c(3,3))
plot(exp.fit, conf.int=F, ci=F, mark.time=T, main="Exponential and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
legend(30, .9, paste("AIC: ", format(exp.fit$AIC, digits=6)))
plot(wei.fit, conf.int=F, ci=F, mark.time=T,  main="Weibull and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
legend(30, .9, paste("AIC: ", format(wei.fit$AIC, digits=6)))
plot(gamma.fit, conf.int=F, ci=F, mark.time=T,  main="Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
legend(30, .9, paste("AIC: ", format(gamma.fit$AIC, digits=6)))
plot(lognorm.fit, conf.int=F, ci=F, mark.time=T, main="Log-Normal and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
legend(30, .9, paste("AIC: ", format(lognorm.fit$AIC, digits=6)))
plot(gen.gamma.fit, conf.int=F, ci=F, mark.time=T, main="Gen. Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
legend(30, .9, paste("AIC: ", format(gen.gamma.fit$AIC, digits=6)))
plot(loglog.fit, conf.int=F, ci=F, mark.time=T, main="Log-Log and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
legend(30, .9, paste("AIC: ", format(loglog.fit$AIC, digits=6)))

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
#generalized gamma AFT model based on lowest AIC in previous step, no p-values though
gengamma.fit1 <- flexsurvreg(Surv(deaths$min, deaths$murdered) ~ episode + type + factor(house2), data=deaths, dist="gengamma")
gengamma.fit1 

#weibull AFT model
weibull.fit1 <- flexsurvreg(Surv(deaths$min, deaths$murdered) ~ episode + type + factor(house2), data=deaths, dist="weibull") # + season 
summary(weibull.fit1)

#model building
#StepAIC
stepAIC(weibull.fit1) #only "type" important

#best subsets - need to re-define type and house2 vars from character to number
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

Covariates[1,which(IndMatrix[which(AICvals==min(AICvals)),]==1)] #no variables selected - best model is intercept only???

#fitting new, reduced model based on StepAIC
weibull.fit2 <- survreg(Surv(deaths$min, deaths$murdered) ~ type, data=deaths, dist="weibull")
summary(weibull.fit2)

#adding episode back in to check for interaction episode*type interaction - no good, nothing significant
deaths$episode.center <- deaths$episode - mean(deaths$episode) #centering season
weibull.fit2 <- survreg(Surv(deaths$min, deaths$murdered) ~ type + episode.center*type, data=deaths, dist="weibull")
summary(weibull.fit2)


#compare AFT and K-M curves at each level of character type

weibull.fit3 <- flexsurvreg(Surv(deaths$min, deaths$murdered) ~ type, data=deaths, dist="weibull") #same as weibull2 but need to do flexsurv reg in order to plot

km.fit.supporting <-  survfit(Surv(subset(deaths$min, deaths$type == "supporting"), subset(deaths$murdered, deaths$type == "supporting")) ~ 1, conf.type = "log-log", data=deaths)
km.fit.minor <-  survfit(Surv(subset(deaths$min, deaths$type == "minor"), subset(deaths$murdered, deaths$type == "minor")) ~ 1, conf.type = "log-log", data=deaths)
km.fit.main <-  survfit(Surv(subset(deaths$min, deaths$type == "main"), subset(deaths$murdered, deaths$type == "main")) ~ 1, conf.type = "log-log", data=deaths)

par(mfrow = c(2,2))
plot(km.fit.supporting, conf.int=F, main="Comparing AFT and K-M Survival Curves for \n Supporting Characters", ylab="Estimated Survival", xlab="Time (min)", lwd=2)
lines(weibull.fit3, newdata = data.frame(type = "supporting"), ci=F, col='red', lty=2, type="survival")
legend(4, .2, c("AFT Surv for Supporting Characters"), lwd = 2, lty=2, col="red")

plot(km.fit.minor, conf.int=F, main="Comparing AFT and K-M Survival Curves for \n Minor Characters", ylab="Estimated Survival", xlab="Time (min)", lwd=2)
lines(weibull.fit3, newdata = data.frame(type = "minor"), ci=F, col='red', lty=2, type="survival")
legend(4, .2, c("AFT Surv for Minor Characters"), lwd = 2, lty=2, col="red")

plot(km.fit.main, conf.int=F, main="Comparing AFT and K-M Survival Curves for \n Main Characters", ylab="Estimated Survival", xlab="Time (min)", lwd=2)
lines(weibull.fit3, newdata = data.frame(type = "main"), ci=F, col='red', lty=2, type="survival")
legend(4, .2, c("AFT Surv for Main Characters"), lwd = 2, lty=2, col="red")

#search for influential points/outliers for AFT

#dfbetas
dfb <- residuals(prop.haz.reduced, type="dfbetas")
eff.samp <- ((141-5) + 141)/2 #effective sample size = 138.5

(intercept <- cbind(deaths[,c(7,9:10)], dfb[,1])[which(dfb[,1] > 2 / sqrt(eff.samp)),])
(type <- cbind(deaths[,c(7,9:10)], dfb[,2])[which(dfb[,2] > 2 / sqrt(eff.samp)),])

#LDCase
ld <-  residuals(prop.haz.reduced, type="ldcase")
cbind(deaths[order(ld, decreasing=T), c(7,9:10)], sort(ld, decreasing = T))[1:2,]

#Cox PH model ----
prop.haz.full <- coxph(Surv(deaths$min, deaths$murdered) ~ type + episode + factor(house2), data=deaths, ties="breslow")
summary(prop.haz.full)

prop.haz.reduced <- coxph(Surv(deaths$min, deaths$murdered) ~ type, data=deaths, ties="breslow")
summary(prop.haz.reduced)

prop.haz.full$loglik[2]
prop.haz.reduced$loglik[2]

1-pchisq(4.0578, 2) # 2 df

#adding interation term, not significant
deaths$mean.episode <- deaths$episode - mean(deaths$episode) #centering season

prop.haz.int <- coxph(Surv(deaths$min, deaths$murdered) ~ type + mean.episode*type, data=deaths, ties="breslow")
summary(prop.haz.int)


#checking martingale residuals
deaths$type2 <- 1
deaths$type2[deaths$type == "minor"] <- 2
deaths$type2[deaths$type == "supporting"] <- 3

resid <- cbind(residuals(prop.haz.reduced, type = "martingale"), type)
names(resid)[1] <- "resids"

resid$type2 <- 1
resid$type2[resid$type == "minor"] <- 2
resid$type2[resid$type == "supporting"] <- 3

ggplot(resid, aes(x=type2, y=resids)) + geom_point() + geom_smooth(method = loess, se=F) + labs(x="", y="Martingale Residuals")



