#_____________________________----


#Coursebook chapter 13
##Efficient ways to carry out comparisons and use functions in R to perform
##linear model analysis


#_____________________________----

# PACKAGES ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)


#_____________________________----



# LINEAR MODEL ANALYSIS FOR COMPARING GROUPS ----

#make a the least squared model 0 (lsmodel0) for linear analysis
lsmodel0 <- lm(formula = height ~ 1, data = darwin)



#_____________________________----



# SUMMARIES FOR MODEL ----


# Broom -----

#summarise information about model components
broom::tidy()

#report information about the whole model
broom::glance()

#add information about individual observations in a dataset and it can be used to model
#predictions onto a new dataset
broom::augment() 




# Model summary -----

summary(lsmodel0)
# or
broom::tidy()

#The first row in any R model is always labeled the 'Intercept', and the challenge
#usually is to work out what that represents. In this case we can prove that it is
#the same as the overal mean as follows:

mean(darwin$height)




# Compare means -----

#use the lm() function to fit the difference in average plant height as a function
#of pollination type
lsmodel1 <- lm(height ~ type, data = darwin)

# note that the following is identical:
# lsmodel1 <- lm(height ~ 1 + type, data = darwin)



broom::tidy(lsmodel1)
#now the model contain the pollination types in addition to the intercept

#the intercept value has changed, representing the mean height of the Crossed plants

#the typeSelf refers to the difference in mean height between 2 groups, not the
#height of Self plants (since it is a negative number)

#Confirm this by
darwin %>%
  group_by(type) %>%
  summarise(mean = mean(height))



#Fuller summary of the model
summary(lsmodel1)



#Superimpose the calculated mean onto a plot
darwin %>%
  ggplot(aes(x = type, 
             y = height, 
             colour = type))+
  geom_jitter(alpha = 0.5, 
              width = 0.1) +
  stat_summary(fun = mean, 
               size = 1.2) + 
  theme_bw()




# Standard error of the difference -----

#Confidence interval
confint(lsmodel1)
#or
broom::tidy(lsmodel1, conf.int=T)





