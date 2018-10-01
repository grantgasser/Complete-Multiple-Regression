### Complete Multiple Regressions on Brand Preference and Grocery Retailer Datasets ### 

## Brand Preference Data ##
brand <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR05.txt", header=F)
names(brand) <- c("Y", "X1", "X2")

#View data
brand

attach(brand)

#Inspect data and analyze assumptions
pairs(brand)

#Thoughts from pairs plot: X1 and Y have obvious linear relationship
#X2 takes the only value of 2 or 4

#Obtain fit
fit <- lm(Y ~ X1 + X2, data=brand)

summary(fit)

#Thoughts: 
#Adding X2 when X1 is already in the model seems to help (p-val=2.01e-05)
#Another way of saying this is that Beta_2 is significantly different from 0
#F-stat much larger than 1. Good with only 2 variables (p-val=2.658e-09)
#Adj. R^2 is high (.9447)


#Check normality of residuals assumption
library(car)
par(mfrow=c(1,2))
qqPlot(resid(fit)) #looks good

#not enough evidence to say residuals are not normally distributed (pval=.92)
shapiro.test(resid(fit))


#Check uncorrelation of residuals assumption
library(lmtest)

#not enough evidence to say residuals do not have correlation of 0
dwtest(fit) #pval=.74
bgtest(fit) #pval=.52


#Check constant variance assumption
plot(fit, which=1)

#By visual inspection, residuals do not seem to have constant variance, test with bptest

#pval=.36, not enough evidence to say residuals do not have constant variance
bptest(fit)


#Choose values for prediction interval and confidence interval
#need to be careful about interpolating for valudes that don't appear in data (e.g. X2 = 5)
brand
newX1 = 6
newX2 = 4

newX = data.frame(X1=newX1, X2 = newX2)

#Confidence interval (95%)
predict(fit, newX, interval="confidence")

#Prediction Interval (95%)
predict(fit, newX, interval="prediction")

## Grocery Retailer Data ##
grocery <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR09.txt", header=F)
names(grocery) <- c("Y","X1", "X2", "X3")

#Note: X3 is a binary categorical variable
grocery

#Inspect data and analyze assumptions
pairs(grocery)

#X1 and Y seem to have a minor linear relationship, but can easily see variance of errors will be non-constant
#X2 doesn't seem to have much of a linear relationship with Y

#Use lm first
attach(grocery)
fit <- lm(Y ~ X1 + X2 + X3, data=grocery)
summary(fit)

#Thoughts: 
#Adding X2 to the model when X1 is already there does not seem to help (pval=.57)
#F-stat is much greater than 1. Good for only 3 variables. Pval=3.3=-12, implying this model
#better than no model
#Adding X3 when X1 and X2 are already in the model seems to be useful (pval=2.94e-13)
#Adj. R^2 is .6689, which seems decent but hard to interpret

#Check normality of residuals assumption
par(mfrow=c(1,2))
qqPlot(resid(fit))

#the larger residuals seem to deviate a bit more, still within bands 

#not enough evidence to say residuals are not nomally distributed (pval=.3644)
shapiro.test(resid(fit))


#Check uncorrelation of residuals assumption
#not enough evidence to say residuals do not have correlation of 0
dwtest(fit) #pval=.8283
bgtest(fit) #pval=.2597


#Check constant variance assumption
plot(fit, which=1)

#by visual inspection, residuals seem to vary a lot, but red line follows 0 fairly closely
#Note: hardly any fitted values in range (4400, 4800)

#Not enough evidence to suggest residuals don't have constant variance (pval=.1544)
bptest(fit)


#Choose values for prediction interval and confidence interval
#need to be careful about interpolating for valudes that don't appear in data (e.g. X2 = 5)
grocery
newX1 = 300000
newX2 = 6.55
newX3 = 0

newX = data.frame(X1=newX1, X2 = newX2, X3=newX3)

#Confidence interval (95%)
predict(fit, newX, interval="confidence")

#Prediction Interval (95%)
predict(fit, newX, interval="prediction")

