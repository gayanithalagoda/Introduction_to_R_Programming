# session_08 video example:

#Suppose that a dietary supplement claims to reduce an individual’s weight after one month 
#of taking the supplement. Seven adults were sampledand their weights (in lbs) were recorded 
#before and after taking the supplement

before <- c(155.8,136.9,221.6,188.8,209.3,220.7,178.9)
after <- c(158.5,137.0,217.2,189.4,195.6,216.4,173.3) # before - after > 0 , after - before < 0
t.test()
t.test(before, after, paired = T, alternative = "greater")
t.test(after, before, paired = T, alternative = "less")
t.test(before, after, paired = T, alternative = "two.sided")

t.test(before, after, paired = T, conf.level=0.9)$conf.int


# 90% Confidence Intervals for the same example

#step 01: Extract Information

n = 7 # length(after) = length(after)= 7
X_d = before-after
xbar = mean(X_d)
alpha=0.10

shapiro.test(X_d)

Standard_Error = sd(X_d)/sqrt(n)
Critical_Value = qt(1 - alpha/2, df= n-1)
Margin_Error =  Critical_Value*Standard_Error
CI = c(xbar- Margin_Error ,xbar + Margin_Error)
CI



# Practice exercise
# A group of students wants to analyze whether a treating a plant with fertilizer 'PQR' effectively reduces the disease affected area in that plant.
# following are the details of area of desease before and after the treatment

before <- c(4.2, 4.7, 6.6, 7.0, 6.7, 4.5, 5.7, 6.0, 7.4, 4.9, 6.1, 5.2)
after <- c(4.1, 4.9, 6.2, 6.9, 6.8, 4.4, 5.7, 5.8, 6.9, 4.7, 6.0, 4.9)
t.test(before, after, paired = T, alternative = "greater")
t.test(before, after, paired = T, conf.level=0.95)$conf.int

# Develop 90% Confidence Intervals for the same example
t.test(before, after, paired = T, alternative = "two.sided",conf.level=0.9)$conf.int


