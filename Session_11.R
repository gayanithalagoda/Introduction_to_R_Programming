library(lmtest)
# “ToothGrowth” data set in R. The description is given in Annotated Notes 11
# Reproduce Results according to Annotated Notes 11

# step 01 : Import data
data(ToothGrowth)
View(ToothGrowth)


# step 02: Produce a cross table to check whether the data are 'balanced'
table(ToothGrowth$supp, ToothGrowth$dose) # 10 objects in each cell. BALANCED


# step 03: check for structure of data. sometimes, factors can be recognized by Rstudio as "int" or "num". We need to correct it before we proceed further
str(ToothGrowth) # if you check the structure of the data set, "dose" is considered as a number. 
#option 1 : use "as.factor() option"
ToothGrowth$dose = as.factor(ToothGrowth$dose)
#option 2 : You ca change the label to a string. then , it automatically becomes "factor"
ToothGrowth$dose <- as.factor(ToothGrowth$dose, levels = c(0.5, 1, 2),labels = c("D0.5", "D1", "D2"))
# let us check the structure again
str(ToothGrowth) # now "dose" is a factor


# step 04 : draw boxplots and see how the variables behave
boxplot( len ~ supp * dose, data= ToothGrowth)

interaction.plot(x.factor = ToothGrowth$dose, trace.factor = ToothGrowth$supp, response = ToothGrowth$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))


# step 05 : Carrying out Two-Way ANOVA only for main effects 
tooth_aov <- aov(len ~ supp*dose, data = ToothGrowth)
summary(tooth_aov)
# From the ANOVA table we can conclude that both supp and dose are statistically significant at 95% confidence level (alpha= 0.05)
# Results shows that changing delivery methods (supp) or the dose of vitamin C, will impact the mean tooth length significantly.

# step 06 : Post-Hoc Tests
TukeyHSD(tooth_aov, which = "dose")
TukeyHSD(tooth_aov, which = "supp")

# step 07 : Check for Normality Assumption
plot(tooth_aov, 2)
tooth_aov_residuals <- residuals(tooth_aov)
shapiro.test(tooth_aov_residuals)

# step 08 : The Levene's test for homogeneity of variances
plot(tooth_aov, 3)
leveneTest(len~as.factor(dose)*as.factor(supp),data=ToothGrowth) # The variances of the response variable are the same for all combinations of groups
# p value > 0.05 , levene's test shows no strong evidence against null hypothesis. Hence, equal variances assumption is not violated.


# Spruce Moth Traps
# Response: number of spruce moths found in trap after 48 hours
# Factor 1: Location of trap in tree (top branches, middle branches, lower branches, ground)
# Factor 2: Type of lure in trap (scent, sugar, chemical)

# step 01 : import the data : "moth.csv" file in d2l
moth_data =  read.csv(file.choose(), header = T)
View(moth_data)

# step 02: Check whether the design in balanced : constant a two way table
table()

# step 03: check for structure of data. sometimes, factors can be recognized by Rstudio as "int" or "num"
str()

# step 04 : Plot boxplots to get an idea about the number
boxplot()

# step 05: Carrying out Two-Way ANOVA 
moth_aov <- aov()
summary(moth_aov)
# interpretation of output
# 0.416 : There is no main effect of Type. (The means for all 'types' are same at 5% significance level) 
# 2.09e-05 : There is is a main effect of 'Place'. (The means for all 'types' are not same at 5% significance level. Atleast one mean is significantly different from the other)
# 0.932 : : There is no interaction effect of Type*Place. (The means that there is no significant interaction effect between "Type" and "Place" at 5% significance level) 

# step 06 : Post-Hoc Tests
TukeyHSD()
TukeyHSD()

# step 07 : Check for Normality Assumption
plot(moth_aov, 2)
moth_aov_residuals <- residuals()
shapiro.test() #p value > 0.05  or not

# step 08: The Levene's test for homogeneity of variances
plot(moth_aov, 3)
leveneTest() # p value > 0.05 or not





