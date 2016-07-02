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

#load .rda file
deaths <- load(file = "~/R Working Directory/Villanova/survival_analysis/project/deaths.rda")

#several parametric fits for deaths (no covariates) and their plots and summaries ----
exp.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="exponential")
wei.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="weibull")
gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="gamma")
lognorm.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="lnorm")
gen.gamma.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="gengamma")
loglog.fit <- flexsurvreg(Surv(deaths$min, deaths$murdered)~1, dist="llogis")

plot(exp.fit, conf.int=F, ci=F, mark.time=T, main="Comparing Exponential and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(wei.fit, conf.int=F, ci=F, mark.time=T,  main="Comparing Weibull and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(gamma.fit, conf.int=F, ci=F, mark.time=T,  main="Comparing Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(lognorm.fit, conf.int=F, ci=F, mark.time=T, main="Comparing Log-Normal and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(gen.gamma.fit, conf.int=F, ci=F, mark.time=T, main="Comparing Gen. Gamma and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
plot(loglog.fit, conf.int=F, ci=F, mark.time=T, main="Comparing Log-Log and K-M Curves", xlab="Time (Min)", ylab="Survival Probability")
par(mfrow = c(3,3))

#comparing AIC scores
aic <- as.data.frame(c(exp.fit$AIC, wei.fit$AIC,gamma.fit$AIC,lognorm.fit$AIC,gen.gamma.fit$AIC,loglog.fit$AIC))
dist <- c("Exponential", "Weibull", "Gamma", "Log Normal", "Gen. Gamma", "Log Log")
aic <- cbind(aic, dist)
names(aic)[1] <- "AIC"

(aic.plot <- ggplot(aic, aes(x=factor(reorder(dist, AIC)), y=AIC, fill=dist)) + geom_bar(stat="identity") + theme_hc() + 
  labs(x="", y="AIC", title="Comparing AIC Scores") + guides(fill=F))
