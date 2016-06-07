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


#survival curves ----

#create survival object
lungsurv <- Surv(lung$time, lung$status-1) # originally coded as 1,2. This recodes as 0,1 where 1=event

#fit a line using survival object
fit <- survfit(lungsurv~1)

#plot the survival curve
plot(fit,  conf.int= T, mark.time=T, 
     main = "Survival Curve for Lung Cancer Data", 
     xlab = "Time", ylab = "Survival Rate")

#2nd example using divorce data
divsurv <- Surv(divorce$Time, divorce$Divorce)
fit2 <- survfit(divsurv~1)
plot(fit2, conf.int= T, mark.time=F, 
     main = "Survival Curve for Divorce Data", 
     xlab = "Time", ylab = "Survival Rate",
     ylim=c(.5,1))

#life table example ----

# for notes ----
data <- data.frame(cbind(c(4, 6, 14, 20, 22), c(1,1,1,1,1)))
names(data)[1] <- "data"
names(data)[2] <- "status"
data.surv <- Surv(data$data, data$status)
fit <- survfit(data.surv ~1)
plot(fit, conf.int= F,
     main = "Survival Curve for Class Example", 
     xlab = "Time", ylab = "Survival Rate")

data <- data.frame(cbind(c(4, 6, 14, 16, 20, 22, 24, 40), c(1,1,1,0,1,1,0,1)))
names(data)[1] <- "data"
names(data)[2] <- "status"
data.surv <- Surv(data$data, data$status)
fit <- survfit(data.surv ~1)
plot(fit, conf.int= F, mark.time = T, 
     main = "Survival Curve for Class Example", 
     xlab = "Time", ylab = "Survival Rate")

