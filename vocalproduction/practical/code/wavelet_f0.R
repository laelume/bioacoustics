# focuses on f0
# consider the multivariate parameters: 
# x=[h,a,s], y=[fn]
# consider the effect of f0 on formants 
# x=f0, y=[fn] 

# imports and file mgmt
library(wavethresh)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(effects)

install.packages("gridExtra")




data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')

#clean, need more data above age 40, age 20-30 is disproportionately represented 


cutoff_age <- 40
data <- data[data$age <= cutoff_age,]

#include weighted age brackets

#data$Weight <- ifelse(data$AgeGroup == "20-30", count_2030, ifelse(data$AgeGroup == "30-40", count_3040,NA))

#age brackets
data$AgeGroup <- cut(data$age,
                     breaks = c(20, 30, 40),
                     labels = c("2030", "3040"),
                     right = FALSE)

# Count number of data points in each age group
age_counts <- table(data$AgeGroup)

age_tot=sum(age_counts)
age_tot #195
count_2030 <- sum(data$age >= 20 & data$age < 30)
count_2030 #156

count_3040 <- sum(data$age >= 30 & data$age < 40)
count_3040 #39
 

desired_proportions <- c(count_2030/age_tot, count_3040/age_tot)

# Calculate weights based on observed vs desired proportions
weights <- desired_proportions

# Assign weights back to the dataset
data$Weight <- ifelse(data$AgeGroup == "2030", weights[1], 
                      ifelse(data$AgeGroup == "3040", weights[2], NA))
data



model <- lm(f0 ~ age+sex+height+condition, data = data)
m1 <- lm(f0 ~ age+sex, data = data)
plot(m1)
m2 <- lm(f0 ~ height, data = data)
plot(m2)
m3 <- lm(f0 ~ sex, data = data)
plot(m3)
m4 <- lm(f0 ~ condition, data = data)
plot(m4)

# LINEAR MODELS
ggplot(data, aes(x = age, y = f0)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ condition) +
  labs(title = "Effect of Age on f0 by Sex and Condition, Linear Fit")

ggplot(data, aes(x = height, y = f0)) +
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ condition) +
  labs(title = "Effect of Height on f0 by Sex and Condition, Linear Fit")


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


#WAVELET TRANSFORMATIONS
## clean
# enumerate non-numerical values 
data$sex <- replace(data$sex, data$sex == 'F', 1)
data$sex <- replace(data$sex, data$sex == 'M', 0)
data$condition <- replace(data$condition, data$condition == 'small', 0)
data$condition <- replace(data$condition, data$condition == 'normal', 1)
data$condition <- replace(data$condition, data$condition == 'big', 2)

# ensure data is num type
numeric_vector <- as.numeric(unlist(data))
data <- as.numeric(numeric_vector)

## construct wavelet-based density estimate. 
  
# use discrete wavelet transformation of order O(n). 
# wd uses mallat pyramid algorithm or cohen interval transformation
# filter type: DaubExPhase or DaubLeAsymm

# params
filt_num=10
filt_type='DaubLeAsymm'

# squared pad for input to wavelet algorithm
next_power_of_two <- 2^ceiling(log2(length(data)))
padded_data <- c(data, rep(0, next_power_of_two - length(data)))

# filter 
pad_wavelet_decomp <- wd(padded_data, filter.number = filt_num, family = filt_type)

# denoise
thresholded <- threshold(pad_wavelet_decomp,policy='universal') 

# reconstruct
density_estimate <- wr(thresholded) 



model <- lm(f0 ~ age + height + sex + condition, data = data)




## plot density estimation of f0 etc

# set resolution
x <- seq(min(data), max(data), length.out = length(data))
y <- approx(seq_along(density_estimate), density_estimate, x)$y

# params
bw=diff(range(x))/60 # adjust bins

# smooth kernel with normal distro
smooth_density <- ksmooth(x, y, kernel = 'normal', bandwidth = bw)

# refine plot variables
ggplot(data.frame(x = smooth_density$x, y = smooth_density$y), aes(x, y)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Smoothed Wavelet-based Density Estimation",
       x = "Data Columns", y = "Density")





