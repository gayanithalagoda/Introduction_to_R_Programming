# Task 08: Measures 


data("CO2")
head(CO2) # check first few lines of the data set
x = CO2$uptake


#is the data uni modal,bi modal or multi modal
plot(density(x))
hist(x)
# CENTER Measures : MEAN/MEDIAN

mean(x)
median(x)

## SPREAD : RANGE, IQR , SD

range(x)
IQR(x)
sd(x)

# What can we say about the Skewness 
install.packages("e1071")
library("e1071")
skewness(x)

#distribution is skewed towards the left.

### Five Number Summary

fivenum(x)

# Five number summary ( min, 1st Q , median , 3rd Q, max)

max(x) # highest value
min(x) # lowest value
quantile(x, 0.25) # first quartile
quantile(x,0.5) # = median(x)  # 2nd quartile
quantile(x,0.75) # third quartile

#The five-number summary gives information about the location (from the median), 
#spread (from the quartiles)  and range (from the sample minimum and maximum) of the observations.

# are there any outliers (OUTLIERS)

boxplot(x)

# what is the upper bound
UB = quantile(x,0.75) + 1.5 * IQR(x)
UB

# we can see max(x) < UB(x) , indicates there are no upper bound outliers


# what is the lower bound
LB = quantile(x,0.25) - 1.5 * IQR(x)
LB

# # we can see  min(x) > LB(x), indicates there are no lower bound outliers

################################################################################
############################## PRACTICE EXERCISE ##############################
################################################################################


salary =  Happiness$SALARY

# is the data unimodal , bimodal or multi modal ?
plot(density(salary))
# seems like uni modal

# does the data show skewness ?
skewness(salary)
#distribution is skewed towards the left.

# what is the mean and median ?
mean(salary)
median(salary)

# what is the range, IQR , sd ?
range(salary)
IQR(salary)
sd(salary)

# what is the value for five number theory ?
fivenum(salary)

# are there upper bound outliers ?
boxplot(salary)

# what is the upper bound
UB = quantile(salary,0.75) + 1.5 * IQR(salary)
UB

# we can see max(x) > UB(x) , indicates there are upper bound outliers


# what is the lower bound
LB = quantile(salary,0.25) - 1.5 * IQR(salary)
LB

# we can see min(x) < LB(x) , indicates there are upper bound outliers