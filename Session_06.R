##############################################################################################
#####################################  HYPOTHESIS TESTING FOR ONE SAMPLE MEAN  ################################
##############################################################################################
# Task 09 : using summarized data

install.packages("BSDA")
library("BSDA")

# example 01 in the note
x_bar= 1.75
mu = 1.3
n= 4
alpha = 0.01
sd = 0.13

t_calc= (x_bar-mu)/ (sd/sqrt(n))
t_calc

pt(t_calc, df= n-1, lower.tail = F)


tsum.test(mean.x=1.75, s.x = 0.13, n.x = 4, mean.y = NULL, 
          s.y = NULL, n.y = NULL, alternative = "greater", mu = 1.3,
          conf.level = 0.99)



# example 02 in the note
x_bar = 62.6
sd= 9.7
mu= 60
n=  26
alpha= 0.01

t_calc= (x_bar-mu)/ (sd/sqrt(n))
t_calc

2*pt(abs(t_calc), df=n-1, lower.tail = F)


tsum.test(mean.x=62.6, s.x = 9.7, n.x = 26, mean.y = NULL, 
          s.y = NULL, n.y = NULL, alternative = "two.sided", mu = 60,
          conf.level = 0.99)


# less than example

x_bar = 62.6
sd= 9.7
mu= 60
n=  26
alpha= 0.01

t_calc= (x_bar-mu)/ (sd/sqrt(n))
t_calc

pt(t_calc, df=n-1, lower.tail = T)

tsum.test(mean.x=62.6, s.x = 9.7, n.x = 26, mean.y = NULL, 
          s.y = NULL, n.y = NULL, alternative = "less", mu = 60,
          conf.level = 0.99)


### Practice Exercises

# A practitioner wants to check whether average blood sugar level of a group of teenagers under a study is greater than 100.  
# The average sugar levels of a sample of 50 are $101.9. 
# Sample standard deviation is 10. Can we reject the null hypothesis at 5% significance level?

# Is the population SD known : No , Therefore we can use a t test
# Extract Information

x_bar= 101.9
mu = 100
sd = 10
n = 50
alpha = .05 # significance level

t_calc= (x_bar-mu)/ (sd/sqrt(n))
t_calc
pt(t_calc, df=n-1, lower.tail = F)

tsum.test(mean.x=101.9, s.x = 10, n.x = 50, mean.y = NULL, 
          s.y = NULL, n.y = NULL, alternative = "greater", mu = 100,
          conf.level = 0.95)


# A practitioner wants to check whether average blood sugar level of a group of teenagers under a study is less than 100.  
# The average sugar levels of a sample of 50 are $95.9. 
# Sample standard deviation is 10. Can we reject the null hypothesis at 5% significance level?

# Is the population SD known : No , Therefore we can use a t test
# Extract Information

x_bar= 95.9
mu = 100
sd = 10
n = 50
alpha = .05 #significance level

t_calc= (x_bar-mu)/ (sd/sqrt(n))
t_calc
pt(t_calc, df=n-1, lower.tail = T)



tsum.test(mean.x=95.9, s.x = 10, n.x = 50, mean.y = NULL, 
          s.y = NULL, n.y = NULL, alternative = "less", mu = 100,
          conf.level = 0.95)


# A practitioner wants to check whether average blood sugar level of a group of teenagers under a study is equal to 100.  
# The average sugar levels of a sample of 50 are $101.9. 
# Sample standard deviation is 10. Can we reject the null hypothesis at 5% significance level?

# Is the population SD known : No , Therefore we can use a t test
# Extract Information

x_bar= 101.9
mu = 100
sd = 10
n = 50
alpha = .05 # significance level

t_calc= (x_bar-mu)/ (sd/sqrt(n))
t_calc

2*pt(abs(t_calc), df=n-1, lower.tail = F)

tsum.test(mean.x=101.9, s.x = 10, n.x = 50, mean.y = NULL, 
          s.y = NULL, n.y = NULL, alternative = "two.sided", mu = 100,
          conf.level = 0.95)


# Task 10 :Using Whole Data Sets

# import data Happiness data set in D2L

xx = Happiness$SALARY

mean(xx)
plot(density(xx))
shapiro.test(Happiness$SALARY)

t.test(xx, y = NULL, alternative = "greater", mu = 45000, conf.level = 0.95) # alpha = 1-0.95 = 0.05
t.test(xx, y = NULL, alternative = "less", mu = 45000, conf.level = 0.95)
t.test(xx, y = NULL, alternative = "two.sided", mu = 45000, conf.level = 0.95) # alpha = 0.05


##############################################################################################
#####################################  CONFIDENCE INTERVALS   ################################
##############################################################################################


# Task 11 : BUilding Confidence INtervals
#Build 95% Confidence Intervals for the average blood sugar levels for a group of teenagers 
#when the sample size is 50, average of the sample is 101.9 and 
#sample standard deviation is 10.

# Is the population SD known : No , Therefore we can use a t test
# Extract Information

xbar= 101.9 # mean(data)
sig = 10 # sd(data)
n = 50 # length(data)
alpha = .05   #significance level # 95% confidence

Standard_Error = sig/ sqrt(n)
Critical_Value = qt(1-alpha/2, df=n-1)
Margin_Error = Standard_Error*Critical_Value

CI = c(xbar-Margin_Error, xbar+ Margin_Error)
CI



