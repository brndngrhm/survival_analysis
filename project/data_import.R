#getting data

library(dplyr)
library(ggplot2)
library(ggthemes)
library(rvest)
library(survival)
library(nlme)
library(lubridate)

url <- read_html("http://deathtimeline.com/")
selector_name <- ".name"
name <- as.data.frame(html_nodes(url, selector_name) %>% html_text())

selector_name <- ".time"
time <- as.data.frame(html_nodes(url, selector_name) %>% html_text())

selector_name <- ".death"
death <- as.data.frame(html_nodes(url, selector_name) %>% html_text())

deaths <- cbind(name, time)
names(deaths)[1] <- "name"
names(deaths)[2] <- "time"

write.csv(deaths, file = "C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/project/deaths.csv")

#load data and test idea
deaths <- read.csv("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/project/deaths.csv", 
                   stringsAsFactors = F)
deaths$time <- hms(deaths$time)
deaths$min <- minute(deaths$time)
deaths$sec <- second(deaths$time)
death.surv <- Surv(deaths$time, deaths$status)

fit <- survfit(death.surv~1)

plot(fit, conf.int = F, mark.time = T)
