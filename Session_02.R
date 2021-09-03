#################################################################################
#########################       ADDITIONAL    ###################################
#################################################################################
# Creating GENDER variable : METHOD 01
set.seed(111)
GENDER= rbinom(100,1,0.2)
GENDER[GENDER==0] = "F"
GENDER[GENDER==1] = "M"
GENDER
#Creating GENDER variable : METHOD 02
# Using sample function
GENDER_01 = sample(c("M","F"), size = 100, replace = T, prob = c(0.2,0.8))


#Creating AGE variable : METHOD 01
AGE= rnorm(100,55,2.5)
AGE=round(AGE,0)
#Creating AGE variable : METHOD 02
#Using sample function
AGE_01 = sample(c(55:65), size = 100, replace = T, prob = 1/11*c(1,1,1,1,1,1,1,1,1,1,1))




#Creating FAMILY variable : METHOD 01
FAMILY = rpois(100,3) + 1
#Creating FAMILY variable : METHOD 02
#Using sample function
FAMILY_01 = sample(c(2:7), size = 100, replace = T, prob = c(0.05,0.2,0.25,.25,.2,.05))


#Creating SALARY Variable : METHOD 01
SALARY = rnorm(100,50000,20000)

#Creating HAPINESS variable : METHOD 01
HAPPYNESS_SCORE = sample(c(1,2,3,4), size = 100, replace = T, prob = c(0.2,0.3,0.3,0.2))

# Creating the Data Frame
Happiness = data.frame(GENDER_01, AGE_01, FAMILY_01, SALARY, HAPPYNESS_SCORE)

#################################################################################
#################################################################################
#################################################################################


# Task 06 : Categorical Data ANalysis

# Frequency Tables , Pie Charts , Barplots
# 1.  table function, prop.table function , chisq.test function
# 2.  pie function
# 3.  barplot function



# import dataset
attach(Happiness)
View(Happiness)

# categorical : nominal variable : GENDER

gender_table = table(Happiness$GENDER_01)
gender_table

prop.table(gender_table)
round(prop.table(gender_table),1) 

pie(gender_table , main = "Pie chart for gender distribution", col = c("pink","blue"), labels = c("F-76%", "M-24%"))
barplot(gender_table,main = "bar plot for gender distribution", col = c("green","yellow") )

x = c(83,17)
labels = c("FEMALE","MALE")
pie(x,labels,main = "Pie chart for gender distribution", col = c("green","yellow"))
barplot(x,main = "bar plot for gender distribution", col = c("green","blue") )

# categorical : ordinal variable : HAPPINESS SCORE
happiness_table = table(Happiness$HAPPYNESS_SCORE)
prop.table(happiness_table)
chisq.test(happiness_table)

pie(happiness_table,main = "Pie chart for happiness distribution", col = c("green","yellow"))
barplot(happiness_table,main = "bar plot for happiness distribution", col = c("green","yellow") )

x1 = c(17,29, 36, 18)
labels1 = c("Unhappy","Slighlytly_Unhappy","Slighlytly_Happy","Happy")
pie(x1,labels1,main = "Pie chart for happiness distribution", col = c("green","yellow"))
barplot(x1,main = "bar plot for happiness distribution", col = c("green","blue") )


# 2-Way Tables 
gender_hapiness = table(Happiness$GENDER_01,Happiness$HAPPYNESS_SCORE)
prop.table(gender_hapiness,1)
prop.table(gender_hapiness,2)

barplot(gender_hapiness, col = c("pink","purple"))

# task 07 : Numerical Data Analysis

# Numerical : Discrete variables: Number of members in the Family
#1.  stem()function
#2.  hist()function
#3.  boxplot() function

CO2$

Happiness$FAMILY_01
hist(Happiness$FAMILY_01)
stem(Happiness$FAMILY_01)
hist(Happiness$FAMILY_01, col="pink",main="distribution of age of participants
in the study",xlab = "age distribution" ,)




# Numerical : Continous Variable : AGE
stem(Happiness$AGE_01,2)

hist(Happiness$AGE_01, col="pink", freq = T)
hist(Happiness$AGE_01, col="pink", freq = F)
hist(Happiness$AGE_01, col="pink", freq = F, 
     main="distribution of age of participants in the study", xlab = "age distribution")
hist(Happiness$AGE_01, col="pink", freq = F, 
     main="distribution of age of participants in the study", xlab = "age distribution", breaks=10)
boxplot(Happiness$AGE_01, col="pink", freq = F,
        main="distribution of age of participants in the study", xlab = "age distribution")

# Numerical : Continous Variable : SALARY

hist(Happiness$SALARY, col="pink", freq = T)
hist(Happiness$SALARY, col="pink", freq = F)
hist(Happiness$SALARY, col="green", freq = F, 
     main="distribution of salary of participants in the study", xlab = "salaries distribution")
boxplot(Happiness$SALARY, col="pink", freq = F, 
        main="distribution of salary of participants in the study", xlab = "salaries distribution")


# TWO-WAY Analysis
boxplot(Happiness$SALARY~Happiness$GENDER_01, 
        main="distribution of salary of participants in the study", xlab = "salaries distribution", col= c("green", "yellow"))


# numerical ~ categorical 


