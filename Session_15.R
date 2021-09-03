#Chiq square test for goodness of fit

# 01: Are orchid clours equally distributed in the sample selected

observed_colour <- c(15,35,20)
expected_colour <- c(1/3,1/3,1/3)
chisq.test(observed_colour,p = expected_colour)
# p value < 0.05, we can see colours are not equally distributed.

output <- chisq.test(observed_colour,p = expected_colour)
output$expected
# had the colours been equally distributed, this would have been the fequency.

# 02: Are makes and females equally present in a study
observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions
chisq.test(x = observed, p = expected)
# p value < 0.05, there is not enough evidence to reject the hypohtesis that males and females are not equally present in the study.


# 03: past data suggest that the distribution of grapesharvest for black, red and green grapes in a particular vineyard is as follows.
# the owner wishes to check whether this years yield is significantly different from past data. 

observed       = c(1203,  2919,  1678)
expected.prop  = c(0.211, 0.497, 0.292)
chisq.test(x = observed, p = expected.prop)

#manually
expected.count = sum(observed)*expected.prop
chi2 = sum((observed- expected.count)^2/ expected.count)
chi2
pchisq(chi2,df=2,lower.tail=FALSE)

# 04 
expected = c(0.3,0.2,0.2,0.1,0.1,0.1)# observed frequencies
observed = c(50,32,20,18,22,25)      # expected proportions
chisq.test(x = observed, p = expected)
# chisquare can never be negative. because it is a squared term

# chi-square for Homogeneity between groups regarding their distribution among categorical variables.

library(dplyr)
library(tidyr)
library(knitr)

A <- sample(1:4,  #Levels
            200,  #Number of Observations
            p=c(1/4,1/4,1/4,1/4), #Probabilities for Each Level
            replace=TRUE) #"Replace" if sample larger than population.
B = sample(1:4,  #Levels
           200,  #Number of Observations
           p=c(1/8,1/16,3/16,2.5/4), #Probabilities for Each Level
           replace=TRUE) #"Replace" if sample larger than population.
AB<- rbind(table(A),table(B))
chisq.test(AB)
# groups show statistically significant differences in their distributions between the 4 categories. The samples are heterogeneous

Male <- c(70,80,100)
Female <- c(34,40,76)
data_table <- rbind(Male,Female)
chisq.test(data_table)

age_1 <- c(24,289)
age_2 <- c(9,100)
age_3 <- c(13,565)
artharitis_table <- rbind(age_1,age_2,age_3)
chisq.test(artharitis_table)
