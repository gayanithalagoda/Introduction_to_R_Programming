##############################################################################################
#################    HYPOTHESIS TESTING FOR TWO SAMPLE MEANS  ##########################
############################################################################################### 

#### POOLED SAMPLE : WHEN ratio of standard deviations are less than 2

library("BSDA")

# Anne is skeptical that Zara's cakes contain less sugar that Mary's Cakes.  
# So Anne takes a random sample of cakes from both shops, and measures the amount of sugar in the cakes.
# Here is a summary of the results:
# Sara's : Mean	:164 , sd = 5.1 , n = 35 	
# Mary's : Mean	:170 , sd = 3.1 , n = 37

# step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(Zara) < mu(Mary)  -->   mu(Zara)-mu(Mary) < 0
# this sign is "less than". therefore, alternative = less than

#step 02: Extract Information
xbar1 = 164 ; xbar2 = 170; mu=0
sd1 = 5.1 ; sd2=3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

# step 03: check whether it is pooled or non-pooled
# is large(sd)/small(sd) < 2?  YES. --> pooled

# Step 04: calculate S-pooled
Spooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))

# step 05: Calculate test statistic
t_calc = ((xbar1-xbar2)-mu)/(Spooled*sqrt(1/n1+1/n2))
t_calc

# step 06 : calculate probability to left
pt(t_calc, df= n1+n2-1, lower.tail = TRUE)

tsum.test(mean.x= 164, s.x = 5.1, n.x = 35, mean.y = 170, 
          s.y = 3.1, n.y = 37, alternative = "less", mu = 0,
          conf.level = 0.95, var.equal = TRUE)


# Anne is skeptical that Zara's cakes contain more sugar that Mary's Cakes.  So Anne takes a random sample of cakes from both shops, and measures the amount of sugar in the cakes.
# Here is a summary of the results:
# Sara's : Mean	:172 , sd = 5.1 , n= 35 	
# Mary's : Mean	:170 , sd = 3.1 , n= 37

#step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(Zara) > mu(Mary)  -->   mu(Zara)-mu(Mary) > 0
# this sign is "greater than". therefore, alternative = greater than

#step 02: Extract Information
xbar1 = 172 ; xbar2 = 170; mu=0
sd1 = 5.1 ; sd2=3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

#step 03: check whether it is pooled or non-pooled
#is large(sd)/ small(sd) < 2?  YES. --> pooled

#Step 04: calculate S-pooled
Spooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))

#step 05: Calculate test statistic
t_calc = ((xbar1-xbar2)-mu)/(Spooled*sqrt(1/n1+1/n2))
t_calc

# step 06 : calculate probability to right
pt(t_calc, df=n1+n2-1, lower.tail = FALSE)

tsum.test(mean.x=xbar1, s.x = xbar2, n.x = n1, mean.y = xbar2, 
          s.y = sd2, n.y = n2, alternative = "greater", mu = 0,
          conf.level = 0.95, var.equal = TRUE)


# Anne is wants to confirm that Zara's cakes are as equally sweet as Mary's Cakes.  So Anne takes a random sample of cakes from both shops, and measures the amount of sugar in the cakes.
# Here is a summary of the results:
# Sara's : Mean	:172 , sd = 5.1 , n= 35 	
# Mary's : Mean	:170 , sd = 3.1 , n= 37

#step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(Zara) = mu(Mary)  -->   mu(Zara)-mu(Mary) = 0
# this sign is "equal to". therefore, alternative = two.sided

#step 02: Extract Information
xbar1 = 172 ; xbar2 = 170; mu=0
sd1 = 5.1 ; sd2= 3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

#step 03: check whether it is pooled or non-pooled
# is large(sd)/ small(sd) < 2?  YES. --> pooled
sd1/sd2

#Step 04: calculate S-pooled
Spooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))

#step 05: Calculate test statistic
t_calc = ((xbar1-xbar2)-mu)/(Spooled*sqrt(1/n1+1/n2))
t_calc

# step 06 : calculate probability to right and left
2*pt(abs(t_calc), df=n1+n2-1, lower.tail = FALSE)

tsum.test(mean.x=172, s.x = 5.1, n.x = 35, mean.y = 170, 
          s.y = 3.1, n.y = 37, alternative = "two.sided", mu = 0,
          conf.level = 0.95, var.equal = TRUE)


#### NON-POOLED :  WHEN ratio of standard deviations are greater than 2

# Anne is skeptical that Zara's cakes contain less sugar that Mary's Cakes.  So Anne takes a random sample of cakes from both shops, and measures the amount of sugar in the cakes.
# Here is a summary of the results:
# Sara's : Mean	:166 , sd = 10.1 , n= 35 	
# Mary's : Mean	:170 , sd = 3.1 , n= 37


#step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(Zara) < mu(Mary)  -->   mu(Zara)-mu(Mary) < 0
# this sign is "less than". therefore, alternative = less than


# Step 02: Extract Information

xbar1 = 166 ; xbar2 = 170; mu=0
sd1 = 10.1 ; sd2=3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

#step 03: check whether it is pooled or non-pooled
#is large(sd)/ small(sd) < 2?  NO --> non- pooled
10.1/3.1

# Step -04 : no need to create pooled SD

# Step 05: Calculate test statistic

t_calc = ((xbar1-xbar2)-mu)/(sqrt(sd1^2/n1+sd2^2/n2))
t_calc

# step 06 : calculate probability to the left 
pt(t_calc, df= min(n1-1,n2-1), lower.tail = TRUE)

tsum.test(mean.x=166, s.x = 10.1, n.x = 35, mean.y = 170, 
          s.y = 3.1, n.y = 37, alternative = "less", mu = 0,
          conf.level = 0.95, var.equal = F)

# Anne is skeptical that Zara's cakes contain more sugar that Mary's Cakes.  So Anne takes a random sample of cakes from both shops, and measures the amount of sugar in the cakes.
# Here is a summary of the results:
# Sara's : Mean	:172 , sd = 10.1 , n= 35 	
# Mary's : Mean	:170 , sd = 3.1 , n= 37


#step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(Zara) > mu(Mary)  -->   mu(Zara)-mu(Mary) > 0
# this sign is "greater than". therefore, alternative = greater than


# Step 02: Extract Information
xbar1 = 172 ; xbar2 = 170; mu=0
sd1 = 10.1 ; sd2=3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

#step 03: check whether it is pooled or non-pooled
#is large(sd)/ small(sd) < 2?  NO --> non- pooled

#Step -04 : no need to create pooled variance.

# Step 05: Calculate test statistic
t_calc = ((xbar1-xbar2)-mu)/(sqrt(sd1^2/n1+sd2^2/n2))
t_calc

# step 06 : calculate probability to right 
pt(t_calc, df=min(n1-1,n2-1), lower.tail = FALSE)


tsum.test(mean.x=172, s.x = 10.1, n.x = 35, mean.y = 170, 
          s.y = 3.1, n.y = 37, alternative = "greater", mu = 0,
          conf.level = 0.95, var.equal = FALSE)

# Anne is wants to confirm that Zara's cakes are as equally sweet as Mary's Cakes.  So Anne takes a random sample of cakes from both shops, and measures the amount of sugar in the cakes.
# Here is a summary of the results:
# Sara's : Mean	:172 , sd = 10.1 , n= 35 	
# Mary's : Mean	:170 , sd = 3.1 , n= 37


#step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(Zara) = mu(Mary)  -->   mu(Zara)-mu(Mary) = 0
# this sign is "equal to". therefore, alternative = two.sided


# Step 02: Extract Information
xbar1 = 172 ; xbar2 = 170; mu=0
sd1 = 10.1 ; sd2=3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

#step 03: check whether it is pooled or non-pooled
#is large(sd)/ small(sd) < 2?  NO --> non- pooled

#Step -04 : no need to create pooled variance.

# Step 05: Calculate test statistic
t_calc = ((xbar1-xbar2)-mu)/(sqrt(sd1^2/n1+sd2^2/n2))
t_calc

# step 06 : calculate probability to right 
2*pt(abs(t_calc), df=min(n1-1,n2-1), lower.tail = FALSE)

tsum.test(mean.x=172, s.x = 10.1, n.x = 35, mean.y = 170, 
          s.y = 3.1, n.y = 37, alternative = "two.sided", mu = 0,
          conf.level = 0.95, var.equal = FALSE)

##############################################################################################
#################    CONFIDENCE INTERVALS  ##########
##############################################################################################

# confidence interval for t test - Pooled

# step 01: Extract Information
xbar1 = 164 ; xbar2 = 170; mu=0
sd1 = 5.1 ; sd2 = 3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level

# calculate Spooled, Standard Error, Critical Value and Margin of Error
Spooled = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
Standard_Error = Spooled*sqrt(1/n1+1/n2)
Critical_Value = qt(1-alpha/2, df= n1+n2-2)
Margin_Error = Critical_Value*Standard_Error 

CI = c((xbar1-xbar2) -Margin_Error, (xbar1-xbar2) + Margin_Error)
CI # CI does not contain zero ; 


# confidence interval for t test - Non-Pooled

#step 01: Extract Information
xbar1 = 172 ; xbar2 = 170; mu=0
sd1 = 10.1 ; sd2=3.1 ; 
n1 = 35; n2=37; alpha = .05; # significance level


Standard_Error  = sqrt(sd1^2/n1+sd2^2/n2)
Critical_Value = qt(1-alpha/2, df= min(n1-1,n2-1))
Margin_Error  = Critical_Value*Standard_Error

CI = c(xbar1-xbar2-Margin_Error , xbar1-xbar2+Margin_Error )
CI # CI contains zero 

#########################################################################################

# Using Whole Data Sets

nothern_giraffe = c(517,495,503,491,503,493,505,495, 498, 481,499, 494)
southern_giraffe = c(493,508,531,521,569,572,500,515,536,490,519,515)

# Mean of nothers Giraffe = mean of southern giraffe?

# step 01: establish hypothesis : less/greater/two.sided
# we want to check the claim : mu(northern) = mu(southern)  -->   mu(Zara)-mu(Mary) = 0
# this sign is "equal to". therefore, alternative = two.sided


#step 02: Extract Information 
alpha=0.01
xbar1=mean(nothern_giraffe)
xbar2=mean(southern_giraffe)
sd1=sd(nothern_giraffe) 
sd2=sd(southern_giraffe) 
n1=length(nothern_giraffe)
n2=length(southern_giraffe)


##step 03: check whether it is pooled or non-pooled
sd1
sd2
sd2/sd1
#is large(sd)/ small(sd) < 2?  NO --> non- pooled

#Step -04 : no need to create pooled SD

# Step 05: Calculate test statistic
t_calc = ((xbar1-xbar2)-0)/ sqrt(sd1^2/n1+sd2^2/n2)
t_calc

# step 06 : calculate probability to right 
2*pt(abs(t_calc), df=min(n1-1,n2-1), lower.tail = FALSE)

tsum.test(mean.x=xbar1, s.x = sd1, n.x = n1, mean.y = xbar2, 
          s.y = sd2, n.y = n2, alternative = "two.sided", mu = 0,
          conf.level = 0.99, var.equal = F)


# Confidence Intervals

alpha=0.01
xbar1=mean(nothern_giraffe)
xbar2=mean(southern_giraffe)
sd1=sd(nothern_giraffe) 
sd2=sd(southern_giraffe) 
n1=length(nothern_giraffe)
n2=length(southern_giraffe)

alpha=0.01


Standard_Error = sqrt(sd1^2/n1+sd2^2/n2)
Critical_Value = qt(1-alpha/2, df= min(n1,n2)-1)
Margin_Error =  Critical_Value*Standard_Error

CI = c((xbar1-xbar2)-Margin_Error,(xbar1-xbar2)+Margin_Error)
CI


# Using the whole data set 
t.test(x=nothern_giraffe, y = southern_giraffe, alternative = "two.sided", mu = 0, paired =F, conf.level = 0.99, var.equal = FALSE)
t.test(x=nothern_giraffe, y = southern_giraffe, alternative = "less", mu = 0, paired =F, conf.level = 0.99, var.equal = FALSE)
t.test(x=nothern_giraffe, y = southern_giraffe, alternative = "greater", mu = 0, paired =F, conf.level = 0.99, var.equal = FALSE)


# NOTE: When variances of the two independent samples are significantly different, the t-test uses a modification called Welch test. The answers from 'pt' calculation and t.test calculation therefore may be slightly different when, "var.equal =F"  is used

?help(package="BSDA")
