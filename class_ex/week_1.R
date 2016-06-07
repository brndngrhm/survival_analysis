#week 1 class examples 

#packages
library(survival)
library(nlme)

#data
aids <- read.delim("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/data/aids.txt")
divorce <- read.delim("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/data/divorce.txt")
gbcs <- read.delim("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/data/gbcs.txt")
lung <- read.delim("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/data/lungcancer.txt")
uis <- read.delim("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/data/uis.txt")
whas500 <- read.delim("C:/Users/GRA/Desktop/Misc/R Working Directory/School/survival_analysis/data/whas500.txt")


#survival curve----

#create survival object
lungsurv <- Surv(lung$time, lung$status-1)

#fit a line using survival object
fit <- survfit(lungsurv~1)

#plot the survival curve
plot(fit,  conf.int= F, mark.time=T, 
     main = "Survival Curve for Lung Cancer Data", 
     xlab = "Time", ylab = "Survival Rate")
