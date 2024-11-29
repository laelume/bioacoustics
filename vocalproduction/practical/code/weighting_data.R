library(ggplot2)

data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')


#Weighting the data
sex_counts <- table(data$sex)
ratio <- sex_counts["F"] / sex_counts["M"]

# Calculate weights
total_females <- sum(data$sex == "F")
total_males <- sum(data$sex == "M")

weight_female <- total_males / total_females
weight_male <- total_females / total_males

# Assign weights to the dataset
data$weight <- ifelse(data$sex == "F", weight_female, weight_male)


ggplot(data, aes(x = sex, weight = weight)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  labs(title = "Weighted Density Plot of Sex Distribution")


#This one looks nice! is it skewed??
ggplot(data, aes(x = f0, fill = sex)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(f0), color = sex),
             linetype = "dotted", size = 0.5,color="black") +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(title = "Distribution by Gender", x = "Variable", y = "Density")
