# TestReg.R
# Creator: Jim Callahan
# Date:    october 9, 2015 (Friday)
# Cert:    Johns Hopkins, "Data Science" by Coursera
# Course:  Regression
# Purpose: Test out some ideas and code for RegressionMPG project
# Source:  Modified Swiss example in "Residuals and Diagnostics" lecture

data(mtcars); 

MPGmod000 <- lm(mpg ~ as.factor(am), data=mtcars)
summary(MPGmod000)

par(mfrow = c(2, 2))
MPGmod001 <- lm(mpg ~ as.factor(am)+wt, data=mtcars)
summary(MPGmod001)
plot(MPGmod001)

par(mfrow = c(2, 2))
MPGmod002 <- lm(mpg ~ as.factor(am)+wt+as.factor(cyl), data=mtcars)
summary(MPGmod002)
plot(MPGmod002)

par(mfrow = c(2, 2))
MPGModAll <- lm(mpg  ~ . , data = mtcars); 
summary(MPGModAll)
plot(MPGModAll)

# drop Transmission (am) *** BEST MODEL ***
par(mfrow = c(1,2))
MPGmod003 <- lm(mpg ~ +wt+as.factor(cyl), data=mtcars)
summary(MPGmod003)
plot(MPGmod003, which = 1)
plot(MPGmod003, which = 2)


# square of the weight
par(mfrow = c(2, 2))
MPGmod004 <- lm(mpg ~ as.factor(cyl) + wt + I(wt^2), data=mtcars)
summary(MPGmod004)
plot(MPGmod004)


# Weight and hp 
par(mfrow = c(2, 2))
MPGmod005 <- lm(mpg ~ as.factor(cyl) + wt + hp, data=mtcars)
summary(MPGmod005)
plot(MPGmod005)

# Plots
# Web search for ggplot2 facet examples found:
# http://www.statmethods.net/advgraphs/ggplot2.html

# Separate regressions of mpg on weight for each number of cylinders
# create factors with value labels 
library(ggplot2)
data(mtcars)
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4 cyl","6 cyl","8 cyl")) 

qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), 
      method="lm", formula=y~x, color=cyl, 
      main="Regression of MPG on Weight", 
      xlab="Weight (1,000 lbs)", 
      ylab="Miles per Gallon")

