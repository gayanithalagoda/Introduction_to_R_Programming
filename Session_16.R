# To install the packages you need for the analysis, run this code (you only need to do this once):

install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("datarium")

# load the packages into your R environment by running this code (you need to do this every time you restart R)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(datarium)


####### EXAMPLE 01 ####### 
mice <- read.csv(file.choose(), header = T)

# Let us look at the summary 

summary(mice)
cor(mice$Response_Time,mice$Dose)
#There is a strong positive ocrrelation. Hence it is advisable to proceed to linear regression modelling.

# Checking assumptions
# 1. independence : Because we only have one independent variable and one dependent variable, we don’t need to test for any hidden relationships among variables.

# 2. Normality : Plot histogram for Y variable. (Response_Time)
hist(mice$Response_Time)

#3 Linearity: The relationship between the independent and dependent variable must be linear.
plot(Response_Time ~ Dose, data = mice)

# 4.Homoscedasticity (aka homogeneity of variance) ; prediction error doesn’t change significantly over the range of prediction. We can check this after fitting model

# Fitting the Mode

response.time.lm <- lm(mice$Response_Time ~ mice$Dose)
summary(response.time.lm)
# The p-value ( Pr(>| t | ) ), aka the probability of finding the supporting evidence to not reject null hypothesis given t-statistic.
# For 'Dose variable, p-value is less than 2e-16. Thus, evidence to NOT REJECT null hypothesis is very low. Hence, we reject the null hypothesis and say
# Dose is a significant variable that contributes to prediction of response time. 
# there is a significant positive relationship between Dose and Response_Time (p-value < 0.001), with a 0.71383 -unit  increase in Response for every unit increase in Dose.

# Checking assumptions after fitting the model

par(mfrow=c(2,2)) # Divide the graph window in to 2 by 2 pane. We can see 4 plots at the same time
plot(response.time.lm)
par(mfrow=c(1,1)) # reverse the graph window back to 1 by 1 pane. 1 plot at a time. 

# 3. Normality: Normal Q-Q plot shows real residuals from our model form an almost perfectly one-to-one line with the theoretical residuals from a perfect model. Normality assumption fulfilled.(Normal Q-Q plot)
shapiro.test( residuals(response.time.lm))

# 4. Homogeneity of variance (homoscedasticity): the size of the error in our prediction doesn’t change significantly across the values of the independent variable.Hence, constant variance assumption is satisfied. (Residuals vs. Fitted Values )
bptest(response.time.lm)

####### EXAMPLE 02 ####### 

# We wish to see if we ca predict Sepal length by Sepal Width.
# Y variabe: 
# X variable:

data("iris")
iris

# Checking assumptions
# 1. independence : Because we only have one independent variable and one dependent variable, we don’t need to test for any hidden relationships among variables.

# 2. Linearity: The relationship between the independent and dependent variable must be linear.
plot(iris$Sepal.Length ~ iris$Sepal.Width)

# 3. Normality : Plot histogram for Y variable. (Response_Time). We can also check this after fitting model
hist(iris$Sepal.Length)

# 4.Homoscedasticity (aka homogeneity of variance) ; prediction error doesn’t change significantly over the range of prediction. We can check this after fitting model

# Fitting the Mode
cor(iris$Sepal.Length,iris$Sepal.Width)

#No strong correlation. Hence, no point in following through with the linear regression 

####### EXAMPLE 03 ####### 
#install.packages("DAAG")
library(DAAG) 
ais

#building a simple regression model that can be use to predict hematocrit (hc) by establishing a statistically significant linear relationship with hemaglobin (hg).

summary(ais)

cor(ais$hc,ais$hg)
#There is a strong positive ocrrelation. Hence it is advisable to proceed to linear regression modelling.

# Checking assumptions
# 1. independence : Because we only have one independent variable and one dependent variable, we don’t need to test for any hidden relationships among variables.

# 2. Linearity: The relationship between the independent and dependent variable must be linear.
plot(ais$hc ~ais$hg)

# 3. Normality : Plot histogram for Y variable. (Response_Time)
hist(ais$hc)

# 4.Homoscedasticity (aka homogeneity of variance) ; prediction error doesn’t change significantly over the range of prediction. We can check this after fitting model

# Fitting the Mode

hc.prediction.lm <- lm(ais$hc ~ais$hg)
summary(hc.prediction.lm)
# The p-value ( Pr(>| t | ) ), aka the probability of finding the supporting evidence to not reject null hypothesis given t-statistic.
# For 'hg' variable, p-value is less than 2e-16. Thus, evidence to NOT REJECT null hypothesis is very low. Hence, we reject the null hypothesis and say
# hg' is a significant variable that contributes to prediction of 'hc' variable. 
# there is a significant positive relationship between 'hc' and 'hg' (p-value < 0.001), with a 2.55614 -unit  increase in 'hc' for every unit increase in 'hg'.

# Checking assumptions after fitting the model

par(mfrow=c(2,2)) # Divide the graph window in to 2 by 2 pane. We can see 4 plots at the same time
plot(hc.prediction.lm)
par(mfrow=c(1,1)) # reverse the graph window back to 1 by 1 pane. 1 plot at a time. 

# 3. Normality : Plot histogram for Y variable. (Response_Time)
#Normality Q-Q plot shows real residuals from our model form an moderate fit one on one-to-one line with the theoretical residuals from a perfect model.

hist(ais$hc)
shapiro.test( residuals(hc.prediction.lm))

# 4.Homoscedasticity (aka homogeneity of variance) ; prediction error doesn’t change significantly over the range of prediction. We can check this after fitting model
bptest(hc.prediction.lm)

