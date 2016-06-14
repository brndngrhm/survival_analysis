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

#plot simple survival curve
death.surv <- Surv(deaths$time, deaths$murdered)

fit <- survfit(death.surv~1)

plot(fit, conf.int = T, mark.time = T, 
     xlab = "Time (Miniutes)", ylab = "Survivl Probability", 
     main = "Survivial Curve: Time Until Murdered \n (Seasons 1-5 of Game of Thrones)")
