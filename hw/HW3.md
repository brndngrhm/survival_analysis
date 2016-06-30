# HW3
Brendan Graham  
June 28, 2016  



### 3.
#### a) 
Fitted model for rectime using a Weibull AFT Model:  
_log(T)_ = 8.39983 - 0.00156 * _age_ + 0.2808 * _hormone_ - 0.01117 * _size_ - 0.31116 * _grade_

```r
weib.fit <- survreg(Surv(gbcs$rectime, gbcs$censrec) ~ age  + hormone + size + grade, data=gbcs, dist="weibull")
summary(weib.fit)
```

```
## 
## Call:
## survreg(formula = Surv(gbcs$rectime, gbcs$censrec) ~ age + hormone + 
##     size + grade, data = gbcs, dist = "weibull")
##                Value Std. Error      z         p
## (Intercept)  8.39983    0.32586 25.778 1.59e-146
## age         -0.00156    0.00456 -0.342  7.32e-01
## hormone      0.28084    0.09715  2.891  3.84e-03
## size        -0.01117    0.00274 -4.081  4.48e-05
## grade       -0.31116    0.07678 -4.053  5.06e-05
## Log(scale)  -0.27711    0.04932 -5.619  1.92e-08
## 
## Scale= 0.758 
## 
## Weibull distribution
## Loglik(model)= -2614.8   Loglik(intercept only)= -2637.3
## 	Chisq= 45.04 on 4 degrees of freedom, p= 3.9e-09 
## Number of Newton-Raphson Iterations: 5 
## n= 686
```

#### b)  
Interpretations:  

* __Age__: For every year increase in age, we estimate the expected survival time for women with breast cancer will decrease by a factor of e^- 0.00156^ = 0.9984, holding all else in the model constant.  

* __Hormone (AFT Interpretation)__: Holding all else in the model constant, we estimate that those receiving hormone therapy will survive e^0.2808^ = 1.324 times longer than those not receiving hormones therapy on average, holding all else in the model constant.  

* __Hormone (proportional hazards interpretation)__: We estimate that women with breast cancer who receive hormone therapy die at a rate that is e^(-0.2808/0.758)^ = 0.6904 times that of women with breast cancer who receive hormone therapy, holding all else in the model constant.


#### c)
The p-value associated with age is statistically significant, but the coefficient is so small that the exponentiation values is practically 1, which indicates age is not practically significant when predicting recurrence free survival time in days.  

Both interpretations of hormone are practically significant as well as statistically significant (based on p-values). The AFT interpretation tells us that women receiving hormone therapy live 32% long than those that do not, which is very useful to know. The proportional hazards interpretaiton tells us the same story form the other direction - that women who receive hormone therapy die at a slower rate than those that do not. Both of these interpretations would likely be very significant and meaningful to both patients and physicians.

#### d)
Based on AIC, the best distribution to use for the recurrence free survival time should be the generalized gamma distribution.  

```r
weib.fit <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="weibull")
exp.fit <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="exponential")
gamma.fit <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="gamma")
loglog.fit <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="llogis")
lognorm.fit <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="lnorm")
gengamma.fit <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="gengamma")

aic <- data.frame(c(weib.fit$AIC, exp.fit$AIC, gamma.fit$AIC, loglog.fit$AIC, lognorm.fit$AIC, gengamma.fit$AIC))
names(aic)[1] <- "AIC"
aic$dist <- c("weibull", "exponential", "gamma", "log-logistic", "log-normal", "gen. gamma")
aic <- aic %>% group_by(AIC) %>% arrange(aic) %>% ungroup()
knitr::kable(aic)
```

      AIC  dist         
---------  -------------
 5135.681  gen. gamma   
 5139.910  log-normal   
 5153.992  log-logistic 
 5170.157  gamma        
 5182.393  weibull      
 5220.056  exponential  

#### e)
* __Naive AIC backward Selection__:

```r
weib.fit <- survreg(Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + size  + grade + nodes + prog_recp + estrg_recp, data=gbcs, dist="weibull")
aic.fit <- stepAIC(weib.fit)
```

```
## Start:  AIC=5182.39
## Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + 
##     size + grade + nodes + prog_recp + estrg_recp
## 
##              Df    AIC
## - estrg_recp  1 5180.6
## - age         1 5181.4
## <none>          5182.4
## - menopause   1 5182.8
## - size        1 5184.3
## - grade       1 5188.1
## - hormone     1 5188.7
## - prog_recp   1 5201.6
## - nodes       1 5218.7
## 
## Step:  AIC=5180.61
## Surv(gbcs$rectime, gbcs$censrec) ~ age + menopause + hormone + 
##     size + grade + nodes + prog_recp
## 
##             Df    AIC
## - age        1 5179.5
## <none>         5180.6
## - menopause  1 5181.0
## - size       1 5182.4
## - grade      1 5186.3
## - hormone    1 5186.8
## - prog_recp  1 5200.8
## - nodes      1 5216.9
## 
## Step:  AIC=5179.53
## Surv(gbcs$rectime, gbcs$censrec) ~ menopause + hormone + size + 
##     grade + nodes + prog_recp
## 
##             Df    AIC
## - menopause  1 5179.1
## <none>         5179.5
## - size       1 5181.2
## - grade      1 5185.4
## - hormone    1 5186.1
## - prog_recp  1 5200.7
## - nodes      1 5215.7
## 
## Step:  AIC=5179.11
## Surv(gbcs$rectime, gbcs$censrec) ~ hormone + size + grade + nodes + 
##     prog_recp
## 
##             Df    AIC
## <none>         5179.1
## - size       1 5180.7
## - hormone    1 5184.5
## - grade      1 5184.9
## - prog_recp  1 5200.7
## - nodes      1 5215.7
```

```r
aic.fit$coefficients
```

```
##  (Intercept)      hormone         size        grade        nodes 
##  8.006325473  0.241708148 -0.005393685 -0.211973424 -0.039019327 
##    prog_recp 
##  0.001653820
```


* __Best Subset Regression__: hormone, size, grade, nodes, prog_recp


```r
IndMatrix <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1, 0:1)
Covariates <- gbcs[,3:10]
AICvals <- rep(0,2^8)
AICvals[1] <- extractAIC(survreg(Surv(gbcs$rectime, gbcs$censrec) ~ 1, dist="weibull"))[2]

for(i in 2:2^8){
	Covs <- Covariates[,which(IndMatrix[i,]==1)]
if(is.vector(Covs)==FALSE) Formula <- formula(paste("Surv(gbcs$rectime, gbcs$censrec) ~ ", paste(Covs, collapse=" + "))) else Formula <- formula(Surv(gbcs$rectime, gbcs$censrec) ~ Covs)
	AICvals[i] <- extractAIC(survreg(Formula, dist="weibull"))[2]
}

Covariates[1,which(IndMatrix[which(AICvals==min(AICvals)),]==1)]
```

```
##   hormone size grade nodes prog_recp
## 1       1   18     3     5       141
```

#### f)
Based on the high p-value, it does not make sense to add a quadratic size term.


```r
gbcs$size2 <- gbcs$size*gbcs$size
weib.fit.f<- survreg(Surv(gbcs$rectime, gbcs$censrec) ~ hormone + size  + grade + nodes + prog_recp + size2, data=gbcs, dist="weibull")
summary(weib.fit.f)
```

```
## 
## Call:
## survreg(formula = Surv(gbcs$rectime, gbcs$censrec) ~ hormone + 
##     size + grade + nodes + prog_recp + size2, data = gbcs, dist = "weibull")
##                 Value Std. Error      z         p
## (Intercept)  8.12e+00   0.278840 29.118 2.10e-186
## hormone      2.36e-01   0.090546  2.609  9.09e-03
## size        -1.21e-02   0.009142 -1.320  1.87e-01
## grade       -2.08e-01   0.075935 -2.735  6.25e-03
## nodes       -3.90e-02   0.005347 -7.301  2.86e-13
## prog_recp    1.64e-03   0.000404  4.066  4.78e-05
## size2        7.62e-05   0.000100  0.759  4.48e-01
## Log(scale)  -3.31e-01   0.048888 -6.766  1.33e-11
## 
## Scale= 0.718 
## 
## Weibull distribution
## Loglik(model)= -2582.3   Loglik(intercept only)= -2637.3
## 	Chisq= 110.05 on 6 degrees of freedom, p= 0 
## Number of Newton-Raphson Iterations: 7 
## n= 686
```

#### g)
Ho: grade should be a quantitative variable  
Ha: grade should be a categorical variable

LRT Stat = (-2)*(-2582.3 - (-2580.6)) = 3.4

Rejection Region: LRT Stat > Chi-Square(2) = 5.991

Conclusion: Since 3.4 is not > 5.991, we do not reject the null and do not conclude the alternative. Treating grade as a quantitative variable is reasonable.


```r
weib.fit.reg <- survreg(Surv(gbcs$rectime, gbcs$censrec) ~ hormone + size  + grade + nodes + prog_recp + size2, data=gbcs, dist="weibull")
weib.fit.factor <- survreg(Surv(gbcs$rectime, gbcs$censrec) ~ hormone + size  + as.factor(grade) + nodes + prog_recp + size2, data=gbcs, dist="weibull")

summary(weib.fit.reg)
```

```
## 
## Call:
## survreg(formula = Surv(gbcs$rectime, gbcs$censrec) ~ hormone + 
##     size + grade + nodes + prog_recp + size2, data = gbcs, dist = "weibull")
##                 Value Std. Error      z         p
## (Intercept)  8.12e+00   0.278840 29.118 2.10e-186
## hormone      2.36e-01   0.090546  2.609  9.09e-03
## size        -1.21e-02   0.009142 -1.320  1.87e-01
## grade       -2.08e-01   0.075935 -2.735  6.25e-03
## nodes       -3.90e-02   0.005347 -7.301  2.86e-13
## prog_recp    1.64e-03   0.000404  4.066  4.78e-05
## size2        7.62e-05   0.000100  0.759  4.48e-01
## Log(scale)  -3.31e-01   0.048888 -6.766  1.33e-11
## 
## Scale= 0.718 
## 
## Weibull distribution
## Loglik(model)= -2582.3   Loglik(intercept only)= -2637.3
## 	Chisq= 110.05 on 6 degrees of freedom, p= 0 
## Number of Newton-Raphson Iterations: 7 
## n= 686
```

```r
summary(weib.fit.factor)
```

```
## 
## Call:
## survreg(formula = Surv(gbcs$rectime, gbcs$censrec) ~ hormone + 
##     size + as.factor(grade) + nodes + prog_recp + size2, data = gbcs, 
##     dist = "weibull")
##                       Value Std. Error      z         p
## (Intercept)        8.14e+00   0.281238 28.950 2.78e-184
## hormone            2.44e-01   0.090746  2.692  7.11e-03
## size              -1.32e-02   0.009203 -1.431  1.52e-01
## as.factor(grade)2 -4.81e-01   0.180171 -2.667  7.65e-03
## as.factor(grade)3 -5.82e-01   0.193733 -3.005  2.66e-03
## nodes             -3.82e-02   0.005376 -7.097  1.27e-12
## prog_recp          1.62e-03   0.000402  4.044  5.25e-05
## size2              8.79e-05   0.000101  0.872  3.83e-01
## Log(scale)        -3.30e-01   0.048878 -6.745  1.53e-11
## 
## Scale= 0.719 
## 
## Weibull distribution
## Loglik(model)= -2580.6   Loglik(intercept only)= -2637.3
## 	Chisq= 113.36 on 7 degrees of freedom, p= 0 
## Number of Newton-Raphson Iterations: 7 
## n= 686
```

```r
#rejection region = qchisq(.95, df=2) = 5.991
```

#### h)
The plot below indicates the fit is good, but could likely be improved. It underestimates the cumulative hazard at first, then overestimates it. Fitting separate curves for hormone or grade might improve the fit.


```r
fit.final <- flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ hormone + size  + grade + nodes + prog_recp + size2, data=gbcs, dist="weibull")
```

```
## Warning in flexsurvreg(Surv(gbcs$rectime, gbcs$censrec) ~ hormone + size
## + : Optimisation has probably not converged to the maximum likelihood -
## Hessian is not positive definite.
```

```r
km.fit <- survfit(Surv(gbcs$rectime, gbcs$censrec) ~1)
plot(km.fit, fun="cumhaz", conf.int = F, xlab="Time in Days", ylab="Cumulative Hazard", main="Comparing AFT and K-M Cumulative Hazard")
lines(fit.final, type="cumhaz", ci=F)
```

![](HW3_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

