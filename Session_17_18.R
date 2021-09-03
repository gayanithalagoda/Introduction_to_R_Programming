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
heart.data <- read.csv(file.choose(), header = T)
summary(heart.data)

#There is a strong positive ocrrelation. Hence it is advisable to proceed to linear regression modelling.

# Checking Model assumptions

# 1. independence : observations of response variable are independent.
# 2. Linearity: The relationship between the independent and dependent variable must be linear.
par(mfrow=c(1,1))
plot(heart.data$heart.disease ~ heart.data$smoking)
plot(heart.data$heart.disease ~ heart.data$biking)


# Fitting the Mode
heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.disease.lm)

heart.data1 = data.frame(heart.data$biking,heart.data$smoking,heart.data$heart.disease)
names(heart.data1)
heart.disease.lm1<-lm(heart.data.heart.disease ~ ., data = heart.data1) # short cut method
summary(heart.disease.lm1)


# Testing for OVERALL SIGNIFCANCE of the model
# F-statistic: 1.19e+04 on 2 and 495 DF,  p-value: < 2.2e-16

# The Pr( > | t | ) column shows the p-value. This shows how likely the calculated t-value would have occurred by chance if the null hypothesis of no effect of the parameter were true.
# From Pr( > | t | ) value of both biking and smoking variables we can see that both variables are significant to the model at 5%.
# hence, we can write the model as follows.
# heart disease = 14.984658 + (-0.200133*biking) + (0.178334*smoking) ± e

# We use Adjusted R-squared value to understand how well the model fits. it says that 97.95% of heart disease variation can explained using the above model. (sum.sq.regression/ sum.sq.total)
# Residual standard error: 0.654  also shows the sqaureroot of unexplained variance is very small. 


# Checking Model assumptions
par(mfrow=c(2,2)) # Divide the graph window in to 2 by 2 pane. We can see 4 plots at the same time
plot(heart.disease.lm)
par(mfrow=c(1,1)) # reverse the graph window back to 1 by 1 pane. 1 plot at a time. 


# Checking Assumptions
# 3. Normality: Normal Q-Q plot shows real residuals from our model form an almost perfectly one-to-one line with the theoretical residuals from a perfect model. Normality assumption fulfilled.(Normal Q-Q plot)
shapiro.test(heart.disease.lm)

# 4. Homogeneity of variance (homoscedasticity): the size of the error in our prediction doesn’t change significantly across the values of the independent variable.Hence, constant variance assumption is satisfied. (Residuals vs. Fitted Values )
bptest(heart.disease.lm)

anova(heart.disease.lm)
# following theory learnt in the class

## Multiple Regression t-test is required to determine if particular predictor variables are useful in making prediction

summary(heart.disease.lm)

# biking :  coeffcient = -0.200133 , p-value = <2e-16 *** ; We have sufficient evidence to say  beta(biking) =! 0, and biking is useful to predict heart disease 
# smoking :  coeffcient = 0.178334 , p-value = <2e-16 *** ; We have sufficient evidence to say  beta(smoking) =! 0 , and smoking is useful to predict heart disease 

confint(heart.disease.lm)
# confidence intervals for each predictor varaible coefficient
# none of the CI s include 'zero'. 


## Parital F tests to comprare full model to reduced models.

#if beta_1  = biking, beta_2= smoking, beta_3 = biking*smoking
full_model = lm(heart.disease ~ biking + smoking + biking*smoking, data = heart.data) # all coefficients are not zero
reduced_model = lm(heart.disease ~ biking + smoking , data = heart.data) # smoking (beta_3) coefficient = 0
anova(reduced_model,full_model)

# p_value = 0.477 > 0.05. Hence, we fail to reject null hypothesis. we keep reduced model 

#We can also see from 
print(anova(reduced_model)) # residual = 211.7
print(anova(full_model)) # residual = 211.5
# only a marigical decrease in residual value when adding the interaction term to the model

summary(full_model)

# p-value for biking:smoking = 0.477 > 0.05 
# this also shows that the interaction model is not significant to the model . reduced model is enough