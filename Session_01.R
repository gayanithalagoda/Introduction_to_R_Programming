# TASK 01 : Familiarising with R syntax


n <- 5 # assignment operator
n
n = 5


a = 22
b = 35
c = a+b
c


string <- "R can save strings"
string

myname <- "my name is Gayani"
myname


# vector : store details 


numbers <- c(1,2,3,4,5,6,7,8,9,10)
numbers <- c(1,2,3,4,5,6,7,8,9,10)
numbers 

height = c(150,160,166,170,180)
height


numbers[4]
numbers[4] # 4th element
numbers[4]
numbers[8]

height[3]

oddnumbers <- c(1,3,5,7,9,11,13,15)
oddnumbers[5]

odd_number
nums <- c(1:250) # extra
nums <- c(1:100)
nums
nums[14]  #14th element

letters <- c("A","B","C","D","E")
letters
letters[4] #4th element

states <- c("AB","ON","BC","QC","MB")
states[5]

fruits = c( "apple", "oranges", "strawberry" , "mangoes") 
fruits

#length of vectors
length(numbers)
length(numbers)
length(nums)

length(height)

settings<-c("High","Medium","Low")
factor(settings)

length(letters)
length(states)

#sum of the values in a vec

sum(numbers)
sum(numbers)
sum(nums)

sum(letters) # Error because 'letters' is a string vector

# maximum and minimum 

max(numbers)
min(numbers)

max(height)
min(height)

max(nums)
min(nums)

ages <- c(16,11,10)
sum(ages)
max(ages)
min(ages)


# mean of a numeric vector
sum(numbers)/length(numbers)

mean(height)
sum(height)/length(height)

mean(numbers)

sum(nums)/length(nums)
mean(nums) 


age = c(10,23,25,11,26,34,55,45,43,51,10,20)
age[4] #fourth element chosen
age[-4] # all elements but 4th 
age[2:5]  # elements 2 to 5 chosen. 2,3,4,5
age[c(2,6)] # only the elements 2 and 6 chosen
age[age==50] # select values from 'age' vector where 'age'= 50. 

#Since there is no value equal to 50 in 'age vector', answer= 0
age[age==23] # 1 value 
age[age==10] # two values of 10
age[age < 20] # select values from 'age' vector where 'age' less than 20.


# Task 02 : Using Rstudio as a calculator

2 + 4
2 - 4
2 * 4
2 / 4
sqrt(9)
sqrt(144)
3^2 # square
3**2

log(9) #2.197225
?log

pi

p <- 2 + 4 # store the value in a variable
p

round(2.197225) #2 rounding off
ceiling(2.197225) #3 rounding up
floor(2.197225) #2 rounding down 
round(2.197225,3) #2.197 , 3 denotes the number of decimal places in the answer 


x = c(1,4,3,7,6,3)
y = c(2.3, 4.5, 4.5, 8.9, 9.1, 2.6)

length(x)==length(y)
z = 2*x # multiply vector by x
p = x + y # add vector x and vector y
q = x * y # multiple vector x by y , element multiplication
r = x-y #  vector x minus vector y
s = x^2 # square of vector x 
t = sqrt(x) #squareroot of vector x
u = log(x) # log of vector x



# TASK 03 :  Importing Data Files in to RStudio

data_01 <- read.csv("/Users/gayanithalagoda/Downloads/Airlines.csv", header = TRUE) # using location of the file
data_02 <- read.csv(file.choose(), header = T)  #using file.choose option
#data_03 <- read.csv(url("http://.../acs_or.csv")) # you can directly give the URL 

View(data_02)



# Task 04 : Installing Packages and Loading packages in to Rstudio

install.packages("ggplot2") # installing the package name . 
# note: installing the package needs internet connection. installing it one time is enough
library(ggplot2) # calling the package to R studio script. 
#note: does not need internet connection. 
#library() function has to run everytime you open the Rstudio after closing it. 

?ggplot2  # if you need help with the package
help(package ='ggplot2') # if you need help with the package

install.packages(lmtest)
library(lmtest)


# TASK 05 : Creating a dataframe in R

name = c("000A1","000A2","000A3","000B1","000B2", "000B3","000B4","000C1", "000C2","000C3")
age = c(15,15,15,14,14,14,11,12,13,13)
height = c(155,160,NA,152,155,153,149,146,150,NA)
gender = c("F","M","F","F","M","M","M","F","F","F")
data_created = data.frame(name, age, height, gender)
data_created

is.na(age)
is.na(height)

mean(age)
mean(height)

mean(height[is.na(height)==F])
mean(na.omit(height))


#################################################################################
#########################       ADDITIONAL     ##################################
#################################################################################

install.packages("tidyr")
library(tidyr)

data_created %>% drop_na() # compare with original 'data_created'
# all rows containing 'NA' is dropped now. If you have large datasets, this is good. 
install.packages("datasets.load") # intsalling the package containing "iris" data set
library(datasets) # koading the library

data(iris)#loading the data
iris
summary(iris) #obtaining summary statistics for the data set

install.packages("dplyr") #installing package for data wrangling
library(dplyr) # loading the package

virginica <- filter(iris, iris$Species == "virginica") # filetring data
head(virginica) # This dispalys the first six rows

# filetring data for versicolor

SepalLength <- filter(iris, iris$Species == "virginica", iris$Sepal.Length > 5)
tail(SepalLength)

add_new_column <- mutate(iris, greater.half = Sepal.Width > 0.5 * Sepal.Length)
tail(add_new_column)

# add another column where petal area = Petal.Length * Petal.Width)
newdataset = mutate(iris, petal.area = Petal.Length * Petal.Width)
head(newdataset)

add_new_column_arranged <- arrange(add_new_column, Petal.Width)
head(add_new_column_arranged)


#################################################################################
#################################################################################
#################################################################################