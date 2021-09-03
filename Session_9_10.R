library(car)

# effectiveness of two new diet plans were being tested . 
# det 1 : paleo , diet 2: keto . effectiveness of weight loss for a group of 25 females between 30-40 were assesed. 
# weight loss over 3 month is given.

DIET_DATA = read.csv(file.choose(), header = T)
DIET_DATA

levels(DIET_DATA$DIET)

# draw a boxplot to graphically analyze
boxplot(DIET_DATA$LOSS~DIET_DATA$DIET)

# test for equality of variance
leveneTest(DIET_DATA$LOSS~DIET_DATA$DIET)

#another test for equality of variance
bartlett.test(DIET_DATA$LOSS~DIET_DATA$DIET)

# Compute the analysis of variance
diet.aov <- aov(DIET_DATA$LOSS~ DIET_DATA$DIET)
# Summary of the analysis
summary(diet.aov)

#As the p-value is less than the significance level 0.05, we can conclude that there a re significant differences 
#between the groups highlighted with “*" in the model summary.

###################################################################################
#Multiple comparison tests
#TUKEY TEST
TukeyHSD(diet.aov)


# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.
# It can be seen from the output, that only PALEO and CONTROL are two groups whose performance is not significantly different from one another

pairwise.t.test(DIET_DATA$LOSS,DIET_DATA$DIET,p.adjust.method = "bonferroni")
pairwise.t.test(DIET_DATA$LOSS,DIET_DATA$DIET,p.adjust.method = "holm")
pairwise.t.test(DIET_DATA$LOSS,DIET_DATA$DIET,p.adjust.method = "none")

#An alternative procedure (i.e.: Welch one-way test), 
#that does not require that assumption have been implemented in the function oneway.test().

#ANOVA test with no assumption of equal variances
oneway.test(DIET_DATA$LOSS~ DIET_DATA$DIET)


#ANOVA test with no assumption of equal variances : Using Summary Statistics 
library(rpsychi)
mu<-c(0.75,0.77,0.72,0.85)
mu.sd<-c(0.16,0.11,0.12,0.10)
mu.n<-c(13,22,42,28)
(mu.ind.anova <- ind.oneway.second(mu,mu.sd,mu.n))


# Video Lecture Example : Annotated notes 10

View(PlantGrowth)

# draw a boxplot to graphically analyze
boxplot(PlantGrowth$weight~PlantGrowth$group)

# Test for Homogeneity of variances
leveneTest(PlantGrowth$weight~PlantGrowth$group)

# Test for Homogeneity of variances
bartlett.test(PlantGrowth$weight~PlantGrowth$group)

# Carry out ANOVA test
plant.result <- aov(PlantGrowth$weight~PlantGrowth$group)

# Summary Results
summary(plant.result)

#As the p-value is less than the significance level 0.05, we can conclude that there a re significant differences 
#between the groups highlighted with “*" in the model summary

# Multiple comparison Tests
TukeyHSD(plant.result)
