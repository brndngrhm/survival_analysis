#Data Explore

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

#load .rda file
deaths <- load(file = "~/R Working Directory/Villanova/survival_analysis/project/deaths.rda")

#ggsurv function ----
ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}

#organize into subsets ----
ep <- deaths %>% group_by(episode) %>% summarise(total = sum(murdered))
season <- deaths %>% group_by(season) %>% summarise(total = sum(murdered))
house2 <- deaths %>% group_by(house2) %>% summarise(total = sum(murdered)) %>% ungroup() %>% arrange(desc(total))
type <-  deaths %>% group_by(type) %>% summarise(total = sum(murdered)) %>% ungroup() %>% arrange(desc(total))
censored <- deaths %>% group_by(murdered) %>% summarise(total = n()) %>% ungroup() %>% arrange(desc(total))
min <- deaths %>% group_by(min) %>% summarise(total = sum(murdered))

#summary plots ----
(season.plot <- ggplot(season, aes(x=as.factor(season), y=total)) + 
   geom_bar(stat="identity") + theme_hc()+ 
   labs(x="\n Season", y="", title="Total Murders by Season"))

(ep.plot <- ggplot(ep, aes(x=as.factor(episode), y=total)) + 
  geom_bar(stat="identity") + theme_hc() + 
  labs(x="\nEpisode", y="", title="Total Murders by Episode Number"))

(house.plot <- ggplot(house2, aes(x=reorder(as.factor(house2), total), y=total)) + 
  geom_bar(stat="identity") + coord_flip() + theme_hc() + 
  labs(x="\nHouse", y="", title="Total Murders by House"))

(type.plot <- ggplot(type, aes(x=reorder(as.factor(type), -total), y=total)) + 
  geom_bar(stat="identity")+ theme_hc() + 
  labs(x="\nCharacter Role", y="", title="Total Murders by Role"))

(min.plot <- ggplot(min, aes(x=min, y=total)) + 
  geom_bar(stat="identity")+ theme_hc() + 
  labs(x="\nMinute", y="", title="Total Murders by Minute"))

#estimated percentiles, mean, mean resid life for deaths ----
summary(fit) #k-m values in table form
quantile(fit, probs=c(.25, .5, .75)) #estimates and conf int for percentiles
print(fit, print.rmean=T) #Mean for restricetd survival curve

#mean resid life
mrl <- km.mrl(deaths$min[deaths$murdered == 1], abs(deaths$murdered[deaths$murdered == 1]))
mrl <- cbind(deaths$min[deaths$murdered == 1], mrl)
mrl <- data.frame(mrl) %>% arrange(V1)
plot(mrl[,1], mrl[,2], xlab="Minute", ylab="Mean Residual Life in Minutes", main = "Mean Residual Lifetime for GoT Characters")

#mrlife function for mean resid life at a specific time - DOESN'T SEEM TO MATCH PLOT OR MRL TABLE
mrlife <- function(t, event, censoring){
  kmfit <- survfit(Surv(event[event>t]-t, censoring[event>t])~1, conf.type="log-log")
  print(kmfit, print.rmean = T)
}

mrlife(2, deaths$min, deaths$murdered) #supposed to look at "restricted mean with upper limit" line????


#plot K-M survival curves ----
death.surv <- Surv(deaths$time, deaths$murdered)
fit <- survfit(death.surv~1, data=deaths)
plot(fit, main="Survival Curve for GoT Murders (Seasons 1-5)", xlab = "Minute", ylab = "Survival Probability")
conf.bands <- km.ci(fit, method = "loghall")
lines(conf.bands, lty=3)
legend(40, .8, c("K-M Curve"," Pointwise CI", "Simultaneous CB"), lty=c(1,2,3))

fit.season <- survfit(death.surv~season, data=deaths)
ggsurv(fit.season) + labs(x="Time (Minutes)", title = "Survival Curves for GoT Murders by Season")

fit.ep <- survfit(death.surv~episode, data=deaths)
ggsurv(fit.ep) + labs(x="Time (Minutes)", title = "Survival Curves for GoT Murders by Episode Number")

fit.type <- survfit(death.surv~type, data=deaths)
ggsurv(fit.type) + labs(x="Time (Minutes)", title = "Survival Curves for GoT Murders by Character Type")

fit.house <- survfit(death.surv~house2, data=deaths)
ggsurv(fit.house) + labs(x="Time (Minutes)", title = "Survival Curves for GoT Murders by House")

