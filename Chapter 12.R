
#_____________________________-----


#Coursebook chapter 12
## Explore data analysis, groups' differences and communcation
## Determine whether or not inbreeding reduced the fitness of the selfed plants


#_____________________________-----

# PACKAGES ----
library(tidyverse)
library(here)


#_____________________________-----



# put this at top of script
library(kableExtra)

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")




# IMPORT AND CHECK DATA ----

darwin <- read_csv(here("data","darwin.csv"))

#check the structure of data
glimpse(darwin)

#check if the data is in a tidy format
head(darwin)

#check variable names
colnames(darwin)

#clean up column names
data <- janitor::clean_names(darwin)

#check for duplication
darwin %>%
  duplicated() %>%
  sum()

#check for typos by looking at impossible values
darwin %>%
  summarise(min=min(height, na.rm=TRUE),
            max=max(height, na.rm=TRUE))

#check for typos by looking at distinct characters/values
darwin %>%
  distinct(pair)

darwin %>%
  distinct(type)

#missing values
darwin %>%
  is.na() %>%
  sum()

#quick summary
summary(darwin)


#_____________________________-----


# DATA VISUALISATION ----

# ggplot
darwin %>%
  ggplot(aes(x = type,
             y = height)) +
  geom_point(aes(color = type)) +
  geom_violin(aes(color = type),
              alpha = 0.2,
              )+
  theme_void()


#comparing groups
darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))


#make a new object
darwin_summary <- darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))


#make a summary plot
darwin_summary %>%
  ggplot(aes(x=type,
             y=mean)) +
  
  geom_pointrange(aes(ymin=mean-sd,
                      ymax=mean+sd))+
  theme_bw()



#_____________________________-----


# ESTIMATION -----
# estimate the mean height of each group, the mean of different in hight between each group and quantify the confidence in the differences

darwin_wide <- darwin %>%
  pivot_wider(names_from = type, values_from = height) %>%
  mutate(difference = Cross - Self)


difference_summary <- darwin_wide %>%
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary


#standard error of the difference
difference_summary %>%
  mutate(se=sd/sqrt(n))


#_____________________________-----


# COMMUNICATE -----
#The average difference in height = 2.62 +/- 1.22 inches (mean +/- SE)


#_____________________________-----


# UNCERTAINTY -----

#Normal distribution

#create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length = 100)


#create a vector of values that shows the height of probability distribution
#for each value in x
y <- dnorm(x)


#plot x and y as a scatter plot with connected lines(type="l") and add
#an x-axis with custom labels
plot(x, y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s","-2s","-1s","mean","1s","2s","3s"))


#Confidence intervals






