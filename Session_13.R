##############################################################################################
#################    HYPOTHESIS TESTING FOR ONE SAMPLE PROPORTION  ##########################
############################################################################################### 

#### POOLED SAMPLE : WHEN ratio of standard deviations are less than 2

library("BSDA")

# Suppose that AHS claims that 40% of Albertans received the flu shot this year. You conduct a survey and find that 38 out of 112 Albertans received the flu shot. 
# At the 5% significance level, are these findings consistent with AHS’s claim

# step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : P(received)= 40%  
# this sign is "equal". therefore, alternative = two.sided

# step 02: Extract Information
p_expected = 0.4 ; n_observed= 38 ; n= 112; p_observed = n_observed/n ; alpha =0.05 ;  # significance level

# step 03: Calculate test statistic
z = (p_observed - p_expected )/sqrt(p_expected*(1 - p_expected )/ n)
z

# step 04: Calculate the table value
2* pnorm(abs(z), lower.tail = F) 
prop.test(x=n_observed, n=n, p = p_expected, alternative = "two.sided",correct = FALSE)

# Example: Suppose that the proportion of people who purchase a certain brand of headache relief medication is 0.8 one year ago.
# After a year of heavily marketing the product, the company wants to find out if this proportion is now higher.
# Assume you take a random sample of n = 100 and 85 people reported to purchase this brand of medication. 
# Carry out a hypothesis test at the level of significance of 0.01 to test your claim

# step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : P(this year) > 0.8  
# this sign is "greater". therefore, alternative = greater

# step 02: Extract Information
p_expected = 0.8 ; n_observed= 85 ; n= 100; p_observed = n_observed/n ; alpha =0.05 ;  # significance level

# step 03: Calculate test statistic
z = (p_observed - p_expected )/sqrt(p_expected*(1 - p_expected )/ n)
z

# step 04: Calculate the table value
pnorm(z, lower.tail = F) 
prop.test(x=n_observed, n=n, p = p_expected, alternative = "greater",correct = FALSE) # Yates : abs(85-80*100) = 5, so "correction = F"

# Example : In Canada, it is reported that 6% of people have the A-blood type. Suppose that you randomly select 
# 300 Canadians and 10 have A-blood. Test whether less than 6% of Canadians have the A-blood type at the level of significance of 0.05.

# step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : P(A-blood) < 0.06  
# this sign is "greater". therefore, alternative = greater

# step 02: Extract Information
p_expected = 0.06 ; n_observed= 10 ; n= 300; p_observed = n_observed/n ; alpha =0.05 ;  # significance level

# step 03: Calculate test statistic
z = (p_observed - p_expected )/sqrt(p_expected*(1 - p_expected )/ n)
z

# step 04: Calculate the table value
pnorm(z, lower.tail = T) 
prop.test(x=n_observed, n=n, p = p_expected, alternative = "less",correct = FALSE) # # Yates : abs(10-6*300) = 8, so "correction = F"


##############################################################################################
#################       CONFIDENCE INTERVALS      ############################################
##############################################################################################


# Example:Suppose we surveyed a class and found that 45 out of 92 reported to struggle with their mental health.
# Find a 95% confidence interval for proportion

# step 01: extract information
n_observed= 45 ; n=92  ; p_observed = n_observed/n ; alpha = 0.05 ;  # significance level

# step 02 : calculate 
Standard_Error = sqrt(p_observed * (1- p_observed)/n)
Critical_Value = qnorm(1-alpha/2)
Margin_Error = Critical_Value*Standard_Error 

CI = c(p_observed -Margin_Error, p_observed + Margin_Error)


########################################################################################
################################ IN CLASS ACTIVITY ####################################
########################################################################################


# Example : Suppose it was found from a random sample of dogs that 44 of 63 dogs who visited their local veterinary clinic have fleas.

# Task 01: Perform an appropriate hypothesis test to determine whether the majority ofdogs in this area have fleas at the level of significance of 0.05

# step 01 : greater/less/two.sided?

# step 02 : extract information
p_expected =  ; n_observed=  ; n=  ; p_observed = n_observed/n ; alpha =  ;  # significance level

# step 03 :

# step 04 :

# Task 02 : Develop Confidence intervals for the above example. 
n_observed=  ; n=  ; p_observed = n_observed/n ; alpha =  ;