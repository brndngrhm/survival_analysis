#loading data, formatting, plotting

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


#load & format data ----
deaths <- read.csv(file = "https://raw.githubusercontent.com/brndngrhm/survival_analysis/master/project/deaths.csv", header = T, strip.white = T)
deaths$time <- ms(deaths$time)
deaths$min <- minute(deaths$time)

#collapse "House" Variable
deaths$house2 <- "Other"
deaths$house2[deaths$house == "House Stark"] <- "House Stark"
deaths$house2[deaths$house == "Night's Watch"] <- "Night's Watch"
deaths$house2[deaths$house == "House Lannister"] <- "House Lannister"
deaths$house2[deaths$house == "House Targaryen"] <- "House Targaryen"

#Save .rda file
save(deaths, file = "~/R Working Directory/Villanova/survival_analysis/project/deaths.rda")
