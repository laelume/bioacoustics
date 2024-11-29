# focuses on f0
# consider the multivariate parameters: 
# x=[h,a,s], y=[fn]
# consider the effect of f0 on formants 
# x=f0, y=[fn] 

# imports and file mgmt
#library(wavethresh)
library(ggplot2)
library(gridExtra)
library(dplyr)
#library(effects)

data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')

########### Age Weights

# clean; need more data above age 40, age 20-30 is disproportionately represented 
cutoff_age <- 40
data <- data[data$age <= cutoff_age,]

# age brackets
data$AgeGroup <- cut(data$age,
                     breaks = c(20, 30, 40),
                     labels = c("2030", "3040"),
                     right = FALSE)

# number of data points in each age group
age_counts <- table(data$AgeGroup)
age_tot=sum(age_counts)
age_tot #195
count_2030 <- sum(data$age >= 20 & data$age < 30)
count_2030 #156
count_3040 <- sum(data$age >= 30 & data$age < 40)
count_3040 #39
 
# weight ratios
desired_proportions <- c(count_2030/age_tot, count_3040/age_tot)
weights <- desired_proportions
data$Weight <- ifelse(data$AgeGroup == "2030", weights[1], 
                      ifelse(data$AgeGroup == "3040", weights[2], NA))

data

# LOESS MODELS
plot1 <- ggplot(data, aes(x = age, y = f0)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "loess") +
  facet_wrap(~ condition) +
  labs(title = "F0 vs. Age (LOESS)")

plot2 <- ggplot(data, aes(x = height, y = f0)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "loess") +
  facet_wrap(~ condition) +
  labs(title = "F0 vs. Height (LOESS)")

plot3 <- ggplot(data, aes(x = age, y = spacing)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "loess") +
  facet_wrap(~ condition) +
  labs(title = "Formant Spacing vs. Age (LOESS)")

plot4 <- ggplot(data, aes(x = height, y = spacing)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "loess") +
  facet_wrap(~ condition) +
  labs(title = "Formant Spacing vs. Height (LOESS)")

grid.arrange(plot1, plot3, plot2, plot4, ncol = 2)

data


# LOESS models with separate fit for sex
plot1 <- ggplot(data, aes(x = age, y = f0, color = sex)) +
  geom_point() + 
  geom_smooth(method = "loess", aes(group = sex)) + 
  facet_wrap(~ condition) +
  labs(title = "F0 vs. Age (LOESS)")

plot2 <- ggplot(data, aes(x = height, y = f0, color = sex)) +
  geom_point() + 
  geom_smooth(method = "loess", aes(group = sex)) + 
  facet_wrap(~ condition) +
  labs(title = "F0 vs. Height (LOESS)")

plot3 <- ggplot(data, aes(x = age, y = spacing, color = sex)) +
  geom_point() + 
  geom_smooth(method = "loess", aes(group = sex)) + 
  facet_wrap(~ condition) +
  labs(title = "Formant Spacing vs. Age (LOESS)")

plot4 <- ggplot(data, aes(x = height, y = spacing, color = sex)) +
  geom_point() + 
  geom_smooth(method = "loess", aes(group = sex)) + 
  facet_wrap(~ condition) +
  labs(title = "Formant Spacing vs. Height (LOESS)")

grid.arrange(plot1, plot3, plot2, plot4, ncol = 2)



