
data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')

# Assuming 'data' is your dataframe with a 'gender' column
weights <- ifelse(data$sex == "M", 120, 78) #because there are 120 females and 78 males

model <- glm(outcome ~ predictors, data = data, weights = weights, family = "binomial")

#combo under over sample
library(caret)
install.packages('caret')
balanced_data <- upSample(x = data[, -which(names(data) == "sex")], 
                          y = data$sex, 
                          yname = "sex")
