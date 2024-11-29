library(ggplot2)

data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')

# Plot random stuff to see what happens 

ggplot(data, aes(x = f0, y = height, color = sex)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = f0, y = height, color = age)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = f0, y = height, color = condition)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = f0, y = height, color = spacing)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = f0, y = spacing, color = sex)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = f0, y = spacing, color = age)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = f0, y = spacing, color = height)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()

ggplot(data, aes(x = condition, y = f0, color = height)) +
  geom_point() +
  labs(title = "Three-Column Plot", x = "Column 1", y = "Column 2", color = "Column 3") +
  theme_minimal()



ggplot(data, aes(x = f0)) +
  geom_line(aes(y = height, color = "Column 2")) +
  geom_line(aes(y = spacing, color = "Column 3")) +
  geom_line(aes(y = age, color = "Column 4")) +
  labs(title = "Multi-Line Plot", x = "Column 1", y = "Values", color = "Legend") +
  theme_minimal()

ggplot(data, aes(x = height)) +
  geom_line(aes(y = f0, color = "Column 2")) +
  geom_line(aes(y = spacing, color = "Column 3")) +
  geom_line(aes(y = age, color = "Column 4")) +
  labs(title = "Multi-Line Plot", x = "Column 1", y = "Values", color = "Legend") +
  theme_minimal()


ggplot(data, aes(x = spacing)) +
  geom_line(aes(y = height, color = "Column 2")) +
  geom_line(aes(y = f0, color = "Column 3")) +
  geom_line(aes(y = age, color = "Column 4")) +
  labs(title = "Multi-Line Plot", x = "Column 1", y = "Values", color = "Legend") +
  theme_minimal()


ggplot(data, aes(x = age)) +
  geom_line(aes(y = height, color = "Column 2")) +
  geom_line(aes(y = spacing, color = "Column 3")) +
  geom_line(aes(y = f0, color = "Column 4")) +
  labs(title = "Multi-Line Plot", x = "Column 1", y = "Values", color = "Legend") +
  theme_minimal()


ggplot(data, aes(x = f0)) +
  geom_line(aes(y = height, color = "Column 2")) +
  geom_line(aes(y = spacing, color = "Column 3")) +
  geom_line(aes(y = age, color = "Column 4")) +
  labs(title = "Multi-Line Plot", x = "Column 1", y = "Values", color = "Legend") +
  theme_minimal()

