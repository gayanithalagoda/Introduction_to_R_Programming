##############################################################################################
#################    HYPOTHESIS TESTING FOR TWO SAMPLE PROPORTIONS  ##########################
############################################################################################### 

# Example : Suppose that we wanted to compare two treatments for skin cancer to see if one resulted in more patients going into
# remission (partial or complete).
# TASK 01 : Example (continued) : Conduct a two-sided hypothesis test to see if there is a difference in the proportion of patients who went into remission from those who received Treatment 1 in comparison to Treatment 2

# step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : treatment_1 = treatment_2  
# this sign is "equal". therefore, alternative = two.sided

# step 02: Extract Information
p_expected = 0 ; n_A= 100; n_B= 80; x_A=90; x_B=70; p_A = x_A/n_A; p_B= x_B/n_B  ; alpha =0.05 ; 
p_star = ( x_A+x_B)/(n_A+n_B) # significance level

# step 03: Calculate test statistic
z = ((p_A - p_B)-0)/sqrt(p_star*(1-p_star)*(1/n_A+ 1/n_B))
z

# step 04: Calculate the table value
2* pnorm(abs(z), lower.tail = F) 
prop.test(x = c(90, 70), n = c(100, 80), alternative = "two.sided", correct = F)$conf.int


##############################################################################################
#################       CONFIDENCE INTERVALS      ############################################
##############################################################################################

# Task 02 : Find 95% confidence interval for the difference of proportions of people who went into remission from Treatment 1 and Treatmen 2

# step 01: extract information
p_expected = 0 ; n_A= 100; n_B= 80; x_A=90; x_B=70; p_A = x_A/n_A; p_B= x_B/n_B  ; alpha =0.05 ; 
p_star = ( x_A+x_B)/(n_A+n_B) 

# step 02 : calculate 
Standard_Error = sqrt((p_A *(1-p_A)/n_A) + (p_B *(1-p_B)/n_B))
Critical_Value = qnorm(1-alpha/2)
Margin_Error = Critical_Value*Standard_Error 

CI = c( (p_A - p_B) -Margin_Error, (p_A - p_B) + Margin_Error)
CI
prop.test(x = c(x_A, x_B), n = c(n_A, n_B), alternative = "two.sided", correct = F)$conf.int

##############################################################################################
#################                 EXTRA : IN CLASS ACTIVITY        ##########################
############################################################################################### 


# Suppose I want to see if there are more female children have asthma than male children. 
# I randomly sampled 54 elementary school students and found that 21/25 female students have asthmaand 26/29 male students have asthma. 
# Test whether there are more female students who have asthma than male student

# step 01: establish hypothesis : less/greater/two.sided
# p_Female > p_males --> (p_A - p_B) > 0  : greater than

# step 02: Extract Information

p_expected =0 ; n_A= 25; n_B= 29; x_A=21; x_B=26; p_A = x_A/n_A; p_B= x_B/n_B  ; alpha =0.05 ; 
p_star = (x_A+x_B)/(n_A+n_B) 

# step 03: Calculate test statistic
z = ((p_A - p_B)-0)/sqrt(p_star*(1-p_star)*(1/n_A+ 1/n_B))
z

# step 04: Calculate table Value
pnorm(z, lower.tail = F) # since normal-distribution is symmetric
prop.test(x = c(x_A, x_B), n = c(n_A, n_B),alternative = "greater", correct = F)



# We have two groups of individuals:
# Group A  consists of people with lung cancer,  n = 500
# Group B : consists of people with no lung cancer  : n = 500

# The number of smokers in each group is as follow:
# Group A : n = 500, 490 smokers, pA=490/500=98
# Group B : n = 500, 400 smokers, pB=400/500=80
# The overall proportion of smokers is p=(490+400)/(500+500)=89
# The overall proportion of non-smokers is q=1−p=0.11

# We wish to analyze whether the observed proportion of smokers in group A (pA) is greater than the observed proportion of smokers in group (pB)?

# step 01: establish hypothesis : less/greater/two.sided
# p_A > p_B --> (p_A - p_B) > 0  : greater than

# step 02: Extract Information
p_expected =0 ;n_A= 500; n_B= 500;x_A=490; x_B=400; p_A = x_A/n_A; p_B= x_B/n_B  ; alpha =0.05 ; 
p_star = ( x_A+x_B)/(n_A+n_B) 

# step 03: Calculate test statistic
z = ((p_A - p_B)-0)/sqrt(p_star*(1-p_star)*(1/n_A+ 1/n_B))
z


# step 04: Calculate table Value

# Question 01:
pnorm(abs(z), lower.tail = F) # since normal-distribution is symmetric
prop.test(x = c(x_A, x_B), n = c(n_A, n_B),alternative = "two.sided", correct = F)

z = ((p_A - p_B)-0)/sqrt(p_star*(1-p_star)*(1/n_A+ 1/n_B))
z



install.packages("BHH2")
library(BHH2)
data(poison.data)

aov1 = aov(poison.data$y~ poison.data$treat)
summary(aov1)

rez= residuals(aov1)
shapiro.test(rez)
leveneTest(aov1)


x=Bp
attach(x)

xxx=table(x$Gender,x$Change)
chisq.test(xxx)
