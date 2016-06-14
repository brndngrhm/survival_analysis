#loading data and formatting

#packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rvest)
library(survival)
library(nlme)
library(lubridate)

#load & format data 
deaths <- read.csv("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/project/deaths.csv", 
                   stringsAsFactors = F)
deaths$time <- ms(deaths$time)
deaths$min <- minute(deaths$time)
deaths$sec <- second(deaths$time)

#organize into subsets
ep <- deaths %>% group_by(episode) %>% summarise(total = sum(murdered))
season <- deaths %>% group_by(season) %>% summarise(total = sum(murdered))
house <- deaths %>% group_by(house) %>% summarise(total = sum(murdered)) %>% ungroup() %>% arrange(desc(total))
type <-  deaths %>% group_by(type) %>% summarise(total = sum(murdered)) %>% ungroup() %>% arrange(desc(total))
censored <- deaths %>% group_by(murdered) %>% summarise(total = n()) %>% ungroup() %>% arrange(desc(total))

#summary plots
(season.plot <- ggplot(season, aes(x=as.factor(season), y=total)) + 
  geom_bar(stat="identity") + theme_hc()+ 
  labs(x="\n Season", y="", title="Total Murders by Season"))

(ep.plot <- ggplot(ep, aes(x=as.factor(episode), y=total)) + 
  geom_bar(stat="identity") + theme_hc() + 
  labs(x="\nEpisode", y="", title="Total Murders by Episode Number"))

(house.plot <- ggplot(house, aes(x=reorder(as.factor(house), total), y=total)) + 
  geom_bar(stat="identity") + coord_flip() + theme_hc() + 
  labs(x="\nHouse", y="", title="Total Murders by House"))

(type.plot <- ggplot(type, aes(x=reorder(as.factor(type), -total), y=total)) + 
  geom_bar(stat="identity")+ theme_hc() + 
  labs(x="\nCharacter Role", y="", title="Total Murders by Role"))

#plot simple survival curve
death.surv <- Surv(deaths$time, deaths$murdered)

fit <- survfit(death.surv~1)

plot(fit, conf.int = T, mark.time = T, 
     xlab = "Time (Miniutes)", ylab = "Survival Probability", 
     main = "Survivial Curve: Time Until Murdered \n (Seasons 1-5 of Game of Thrones)")

#add lines for different covariates
ep1.surv <- Surv(subset(deaths$time, deaths$episode == 1), subset(deaths$murdered, deaths$episode == 1))
fit.ep1 <- survfit(ep1.surv~1)

ep2.surv <- Surv(subset(deaths$time, deaths$episode == 2), subset(deaths$murdered, deaths$episode == 2))
fit.ep2 <- survfit(ep2.surv~1)

ep3.surv <- Surv(subset(deaths$time, deaths$episode == 3), subset(deaths$murdered, deaths$episode == 3))
fit.ep3 <- survfit(ep3.surv~1)

ep4.surv <- Surv(subset(deaths$time, deaths$episode == 4), subset(deaths$murdered, deaths$episode == 4))
fit.ep4 <- survfit(ep4.surv~1)

ep5.surv <- Surv(subset(deaths$time, deaths$episode == 5), subset(deaths$murdered, deaths$episode == 5))
fit.ep5 <- survfit(ep5.surv~1)

ep6.surv <- Surv(subset(deaths$time, deaths$episode == 6), subset(deaths$murdered, deaths$episode == 6))
fit.ep6 <- survfit(ep6.surv~1)

ep7.surv <- Surv(subset(deaths$time, deaths$episode == 7), subset(deaths$murdered, deaths$episode == 7))
fit.ep7 <- survfit(ep7.surv~1)

ep8.surv <- Surv(subset(deaths$time, deaths$episode == 8), subset(deaths$murdered, deaths$episode == 8))
fit.ep8 <- survfit(ep8.surv~1)

ep9.surv <- Surv(subset(deaths$time, deaths$episode == 9), subset(deaths$murdered, deaths$episode == 9))
fit.ep9 <- survfit(ep9.surv~1)

ep10.surv <- Surv(subset(deaths$time, deaths$episode == 10), subset(deaths$murdered, deaths$episode == 10))
fit.ep10 <- survfit(ep10.surv~1)

plot(fit.ep1, conf.int = F, mark.time = F, 
     xlab = "Time (Miniutes)", ylab = "Survival Probability", 
     main = "Survivial Curve: Time Until Murdered \n (Episodes 1-5 of Game of Thrones)")

lines(fit.ep2, col = "blue", conf.int = F, mark.time = F)
lines(fit.ep3, col = "red", conf.int = F, mark.time = F)
lines(fit.ep4, col = "green", conf.int = F, mark.time = F)
lines(fit.ep5, col = "purple", conf.int = F, mark.time = F)
lines(fit.ep6, col = "orange", conf.int = F, mark.time = F)
lines(fit.ep7, col = "brown", conf.int = F, mark.time = F)
lines(fit.ep8, col = "blue", conf.int = F, mark.time = F)
lines(fit.ep9, col = "red", conf.int = F, mark.time = F)
lines(fit.ep10, col = "green", conf.int = F, mark.time = F)



