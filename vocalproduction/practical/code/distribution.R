library(ggplot2)
library(reshape2)
#install.packages('reshape2')

data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')

#Weighting the data
sex_counts <- table(data$sex)
ratio <- sex_counts["F"] / sex_counts["M"]
#female_weight=m_tot/f_tot
#male_weight=f_tot/m_tot


#SCATTERPLOTS

#sIMPLE SCATTERPLOT OF ALL DATAPONITS
plot(data$f0, data$height, 
     xlab = "f0", 
     ylab = "height", 
     main = "height as a function of f0")
abline(lm(data$height ~ data$f0), col = 'red')

#Linear fit LINES scatterplotting male and female Height vs. f0
ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point() +  # Add points for each observation
  geom_smooth(method = "lm", aes(group = sex), se = FALSE) +  # Fit lines for each group without confidence interval
  labs(title = "Scatter Plot with Fit Lines by Sex",
       x = "f0",
       y = "height",
       color = "Sex") +  # Customize labels
  theme_minimal()  # Use a minimal theme for better aesthetics


ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point() +  # Add points for each observation
  geom_smooth(method = "loess", aes(group = sex), se = FALSE) +  # Fit LOESS lines for each group without confidence interval
  labs(title = "Scatter Plot with LOESS Fit Lines by Sex",
       x = "f0",
       y = "Height",
       color = "Sex") +  # Customize labels
  theme_minimal()  # Use a minimal theme for better aesthetics


#This one looks nice! is it skewed??
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0), color = sex),
             linetype = "dotted", size = 0.5,color="black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Density")




#VARIOUS EXLPORATORY PLOTS
ggplot(data, aes(x = f0, y = height, color = sex, shape = sex)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Scatter plot by sex")

ggplot(data, aes(x = f0)) +
  geom_histogram() +
  facet_wrap(~sex) +
  labs(title = "Histograms by sex")

ggplot(data, aes(x = sex, y = f0, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Box plot by sex")

ggplot(data, aes(x = height, y = f0, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Grouped bar plot by sex")

# Combined plot with distribution curve
ggplot(data, aes(x = height, y = f0, fill = condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_jitter(aes(color = condition), width = 0.2, alpha = 0.5) +  # Add jittered points for visibility
  geom_smooth(aes(group = condition), method = "loess", se = FALSE, color="black") +  # LOESS fit line
  labs(title = "Grouped Bar Plot with Distribution Curve",
       x = "Group",
       y = "Mean Value",
       fill = "Condition") +
  theme_minimal()


# Combined plot with distribution curve
ggplot(data, aes(x = height, y = f0, fill = sex)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_jitter(aes(color = condition), width = 0.2, alpha = 0.5) +  # Add jittered points for visibility
  geom_smooth(aes(group = sex), method = "loess", se = FALSE, color="black") +  # LOESS fit line
  labs(title = "Grouped Bar Plot with Distribution Curve",
       x = "Group",
       y = "Mean Value",
       fill = "Condition") +
  theme_minimal()



# MULTIVARIABLE DATA

ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~condition) +
  scale_color_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Faceted scatter plots with trend lines by sex")

ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = MASS::rlm, se = FALSE) +
  facet_wrap(~condition) +
  scale_color_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Faceted scatter plots with trend lines by sex")

ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = mgcv::gam, se = FALSE) +
  facet_wrap(~condition) +
  scale_color_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Faceted scatter plots with trend lines by sex")

#LOESS LOOKS GOOD - use for less than 1000 datapoinnts - Quadratic polynomial curve
#LOESS (Locally Estimated Scatterplot Smoothing) is a nonparametric regression 
#span (or bandwidth) parameter controls the degree of smoothing.

ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = stats::loess, se = FALSE) +
  facet_wrap(~condition) +
  scale_color_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Faceted scatter plots with trend lines by sex")




# Histograms
#hist(data)

ggplot(data, aes(x = f0, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Count")

ggplot(data, aes(x = spacing, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Count")

ggplot(data, aes(x = f0, fill = sex)) +
  geom_histogram(bins = 30) +
  facet_wrap(~sex, ncol = 1) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Count")

#This one looks nice! is it skewed??
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Density")

ggplot(data, aes(x = f0, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(aes(xintercept = mean(f0), color = sex),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  scale_color_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender with Mean Lines", x = "Variable", y = "Count")

#This one looks nice! is it skewed??
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0), color = sex),
             linetype = "dotted", size = 0.5,color="black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Density")




#hmmmmmmm
# Count the number of datapoints for each gender
male_count <- sum(data$sex == "M")
female_count <- sum(data$sex == "F")

# Assuming 'p' is your existing ggplot histogram
# and 'data' is your dataframe with a 'sex' column

# Count the number of datapoints for each sex
sex_counts <- table(data$sex)

# Modify the existing plot from above
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0), color = sex),
             linetype = "dotted", size = 0.5,color="black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "f0", y = "Density") + 
  scale_fill_discrete(labels = paste(names(sex_counts), " (", sex_counts, ")", sep = "")
)


# Normalizes male data to female data before plotting: 
library(ggplot2)
library(dplyr)

# Calculate the ratio of females to males
sex_counts <- table(data$sex)
ratio <- sex_counts["F"] / sex_counts["M"]

# Create the modified plot with counts on the y-axis
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(aes(y = after_stat(count)), alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0)), 
             linetype = "dotted", size = 0.5, color = "black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  scale_y_continuous(
    # Secondary axis for normalized density
    sec.axis = sec_axis(~./ratio, name = "Normalized Density")
  ) +
  labs(title = "Normalized Distribution by Gender", 
       x = "F0", 
       y = "Count") +
  scale_fill_discrete(labels = paste(names(sex_counts), " (", sex_counts, ")", sep = ""))




# Calculate the weighted counts
weighted_counts <- data %>%
  group_by(sex) %>%
  summarize(count = n()) %>%
  mutate(weighted_count = ifelse(sex == "M", count * ratio, count))

# Modify the existing plot
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0)), 
             linetype = "dotted", size = 0.5, color = "black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Normalized Distribution by Sex", 
       x = "F0", 
       y = "Density") +
  scale_fill_discrete(
    labels = paste(
      weighted_counts$sex, 
      " (", 
      round(weighted_counts$weighted_count, 0), 
      ")", 
      sep = ""
    )
  )

ggplot(data, aes(x = spacing, fill = sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0)), 
             linetype = "dotted", size = 0.5, color = "black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Formant Spacing", 
       x = "spacing", 
       y = "Density") +
  scale_fill_discrete(
    labels = paste(
      weighted_counts$sex, 
      " (", 
      round(weighted_counts$weighted_count, 0), 
      ")", 
      sep = ""
    )
  )

