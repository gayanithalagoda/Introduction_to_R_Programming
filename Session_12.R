library(lmtest)
## RANDOMIZED BLOCK DESIGN

# A randomized block design eliminates the effect of the block factor so that it does not affect the factor(s) of interest. 
# Blocking eliminates the effect of extraneous variables 
# Note: In each block, each treatment is represented once

# Randomized Block Design : In addition to the other assumptions of two-way ANOVA, we assume that there is no significant interactionbetween the main factor of interest (treatment) and the block factor (which is not of interest)
# There is no significant difference between population means of the main factor of interest

# step 01 : import the data : "Health_Score.csv" file in d2l
Health_Score <- read.csv(file.choose(),header = T)
View(Health_Score)

# step 02 : construct a two way table and see whether it is balanced
table(Health_Score$Patient,Health_Score$treatment)

# step 03 :check for the structure
str(Health_Score)
# we see that patient number is "num" and not "factor". We need to correct this before we move further
Health_Score$Patient = as.factor(Health_Score$Patient)
str(Health_Score) # now patient is a "factor"

# step 04: Carrying out RCBD analysis
score_aov <- aov(Health_Score ~ treatment + Patient, data=Health_Score)
summary(score_aov)
#From the output, the test statistic is F = 6.0065. p-value= 0.004. 
#there is sufficient evidence to reject H0
#Therefore, there is significant difference in means in the 'Health_Score' means of the four 'Treatment' methods

# step 05 : Post-Hoc Tests
TukeyHSD(score_aov, which = "treatment")
TukeyHSD(score_aov, which = "Patient")

# step 06 : Check for Normality Assumption
plot(score_aov,2)
score_aov_residuals <- residuals(score_aov)
shapiro.test(score_aov_residuals) # p value > 0.05 or not?

# step 07: The Levene's test for homogeneity of variances
plot(score_aov,3)
leveneTest(Health_Score ~ treatment, data=Health_Score) # p value > 0.05 or not?
bartlett.test(Health_Score~treatment,data=Health_Score)


#############################################################################################

# step 01 : import the data : "Health_Score.csv" file in d2l
Surgery_Cost <- read.csv(file.choose(),header = T)
View(Surgery_Cost)

# step 02 : construct a two way table and see whether it is balanced
table(Surgery_Cost$Skill_Level, Surgery_Cost$Type_Surgery) # balanced

# step 03: check structure of data 
str(Surgery_Cost)

# step 04: Carrying out RCBD analysis
cost_aov <- aov(Fees ~ Type_Surgery + Skill_Level  ,data= Surgery_Cost)
summary(cost_aov)

# step 05 : Post-Hoc Tests
TukeyHSD(cost_aov, which = "Type_Surgery")
TukeyHSD(cost_aov, which = "Skill_Level")

# step 06 : Check for Normality Assumption
plot(cost_aov,2)
cost_aov_residuals <- residuals(cost_aov)
shapiro.test(cost_aov_residuals) # p value > 0.05 or not?

# step 07: The Levene's test for homogeneity of variances
plot(cost_aov,3)
leveneTest(Fees ~ Type_Surgery,data= Surgery_Cost) # p value > 0.05 or not?
bartlett.test(Fees ~ Type_Surgery,data= Surgery_Cost)

#########################################################################################
##########################      EXTRA        ############################################
#########################################################################################

# Annotated notes: 12

mean_weights <- matrix(c(726.3,733.9,729.1,731.2,739.7,739.5,740.7,745.5,778.9,777.3,775.1,772.4),ncol=4,byrow=TRUE)
colnames(mean_weights) <- c("May","June","July","August")
rownames(mean_weights) <- c("Drug_A","Drug_B","Drug_C")
table_weights <- as.table(mean_weights)
table_weights

df_weights= data.frame(table_weights)
names(df_weights)= c("treatments","month", "weight")
weight_aov <- aov(df_weights$weight ~ df_weights$treatments+ factor(df_weights$month))
anova(weight_aov)



### dealing with NA values
XX = c(NA, 100,101,102,103,104,103,108,NA)
mean(XX[is.na(XX)==F])

name = c("000A1","000A2","000A3","000B1","000B2", "000B3","000B4","000C1", "000C2","000C3")
age = c(15,15,15,14,14,14,11,12,13,13)
height = c(155,160,NA,152,155,153,149,146,150,NA)
gender = c("F","M","F","F","M","M","M","F","F","F")
data_created = data.frame(name, age, height, gender)
data_created

mean(data_created$height)
mean(data_created$height[is.na(data_created$height)==F])
