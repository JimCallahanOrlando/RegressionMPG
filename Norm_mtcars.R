# Norm_mtcars.R
# Creator: Jim Callahan
# Date:    october 19, 2015 (Friday)
# Cert:    Johns Hopkins, "Data Science" by Coursera
# Course:  Regression
# Purpose: Test out some ideas and code for RegressionMPG project
# Source:  Principal Components is impacted by absolute size of numbers
#          So, numbers in the hundreds (cubic inch displacement and hp)
#          dominate. So, idea is to normalize all variables frist and then
#          do principal components.

data(mtcars); 
attach(mtcars)
Nmpg    <- (mpg -  mean(mpg)  ) / sd(mpg)
Ncyl    <- (cyl -  mean(cyl)  ) / sd(cyl)
Ndisp   <- (disp - mean(disp) ) / sd(disp)
Nhp     <- (hp -   mean(hp)   ) / sd(hp)
Ndrat   <- (drat - mean(drat) ) / sd(drat)
Nwt     <- (wt -   mean(wt)   ) / sd(wt)
Nqsec   <- (qsec - mean(qsec) ) / sd(qsec)
Nvs     <- (vs -   mean(vs)   ) / sd(vs)
Nam     <- (am -   mean(am)   ) / sd(am)
Ngear   <- (gear - mean(gear) ) / sd(gear)
Ncarb   <- (carb - mean(carb) ) / sd(carb)

Norm_mtcars <- cbind(Nmpg, Ncyl, Ndisp, Nhp, Ndrat, Nwt, Nqsec, Nvs, Nam, Ngear, Ncarb)
Norm_mtcars
detach(mtcars)
prcomp(Norm_mtcars)
prcomp(mtcars, center = TRUE, scale = TRUE)
pr1fit <- lm(mpg ~ cyl+disp+wt+hp+vs, data=mtcars)
pr2fit <- lm(mpg ~ as.factor(cyl)+disp+wt+hp+as.factor(vs), data=mtcars)
pr3fit <- lm(mpg ~ wt+as.factor(cyl)+hp, data=mtcars)
pr4fit <- lm(mpg ~ I(wt^2)+wt+as.factor(cyl)+hp, data=mtcars)
pr5fit <- lm(mpg ~ I(wt^2)+ wt+I(hp^2)+hp+as.factor(cyl), data=mtcars)
pr6fit <- lm(mpg ~ I(wt^2)+ wt+I(hp^2)+hp, data=mtcars)
# pr7fit <- lm(mpg ~ I(wt^2)+wt+hp, data=mtcars)
# pr8fit <- lm(mpg ~ I(wt^2)+ wt+I(disp^2)+disp, data=mtcars)
pr9fit <- lm(mpg ~ log(wt), data=mtcars)
pr10fit <- lm(mpg ~ log(wt)+as.factor(cyl), data=mtcars)

summary(pr1fit)
summary(pr2fit)
summary(pr3fit)
summary(pr4fit)
summary(pr5fit)
summary(pr6fit)
# summary(pr7fit)
# summary(pr8fit)
summary(pr9fit)
summary(pr10fit)