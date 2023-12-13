setwd("C:/Users/katan/OneDrive/Desktop/STAT7510")
library(VGAM)
library(dplyr)
library(MASS)
library(gee)
set.seed(1)
tax = read.table("7150_final_tax_number.txt", header = TRUE, stringsAsFactors = TRUE)
tax$Degree = as.factor(tax$Degree)
tax$Belief = as.factor(tax$Belief)
depression = read.csv("depression.csv")

##creates a version of the tax data frame to be used for logistic regression
tax2 = data.frame(matrix(data = NA, nrow = 6, ncol = 5))
tax2 = tax2 %>% mutate(gender = c("Female","Female","Female","Male","Male","Male"))
tax2 = tax2 %>% mutate(degree = c(1,2,3,1,2,3))
tax2 = tax2 %>% mutate(belief1 = c(100, 257, 89, 78, 313, 49))
tax2 = tax2 %>% mutate(belief2 = c(65, 320, 140, 73, 328, 112))
tax2 = tax2 %>% mutate(belief3 = c(49, 227, 163, 59, 215, 89))
tax2 = tax2[,6:10]
tax2$gender=as.factor(tax2$gender)
tax2$degree=as.numeric(tax2$degree)


##logistic regression
logist = vglm(cbind(belief1, belief2, belief3) ~ gender + degree, 
          family = cumulative(parallel = TRUE), data = tax2)
summary(logist)
logist0 = vglm(cbind(belief1, belief2, belief3) ~ 1, 
            family = cumulative(parallel = TRUE), data = tax2)
lrtest(logist, logist0)

##loglinear model
D.B = glm(count ~ Degree + Belief, family = poisson, data = tax)
anova(D.B)
summary(D.B)
data.frame(tax, fitted(D.B))

##GEE
depression$time = as.numeric(depression$time)
depression$drug = as.numeric(depression$drug)
depression$outcome = as.numeric(depression$outcome)
depression$severity = as.numeric(depression$severity)

gee = gee(outcome ~ drug + severity + time + drug*time, id = case,
          family = binomial, corstr = "exchangeable", data = depression)
summary(gee)
