#getting data

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
     xlab = "Time (Miniutes)", ylab = "Survivl Probability", 
     main = "Survivial Curve: Time Until Murdered \n (Seasons 1-5 of Game of Thrones)")
