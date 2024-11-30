# Set working directory and read the data
setwd("C:/Users/sheil/Desktop")
data <- read.csv("WP Acoustic Features.csv")

# Check the general structure of the data
str(data)

# Create a mapping of WPT to altitude
altitude_mapping <- c(
  "1" = "1235_meters",
  "2" = "1180_meters",
  "3" = "1134_meters",
  "4" = "1134_meters",
  "5" = "1180_meters",
  "6" = "1274_meters",
  "7" = "1235_meters",
  "8" = "1180_meters",
  "9" = "1134_meters",
  "10" = "1274_meters",
  "11" = "1235_meters",
  "12" = "1274_meters"
)

# Add altitude column to the data
data$altitude <- factor(altitude_mapping[as.character(data$WPT)], 
                        levels = c("1134_meters", "1180_meters", "1235_meters", "1274_meters"))

# Check levels of the altitude factor
levels(data$altitude)

# Summary of the data
summary(data)

# Load necessary libraries
library(ggplot2)

# Function to create boxplot with linear trend and save it
create_and_save_plot <- function(y_var, y_label) {
  plot <- ggplot(data, aes(x = altitude, y = !!sym(y_var))) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red") +
    labs(x = "Altitude", y = y_label, 
         title = paste(y_label, "by Altitude")) +
    theme_minimal()
  
  # Save the plot
  filename <- paste0("plot_", gsub("\\.", "_", y_var), ".png")
  ggsave(filename, plot, width = 10, height = 6, dpi = 300)
  
  # Return the plot object for display
  return(plot)
}

# Function to run linear model and print summary
run_lm_and_summarize <- function(y_var) {
  model <- lm(as.formula(paste(y_var, "~ altitude")), data = data)
  print(summary(model))
}

# Analyze and save plots for each variable
variables <- c("Low.Freq", "High.Freq", "Delta.Freq", "Avg.Power.Density", "Delta.Time")
y_labels <- c("Low Frequency (Hz)", "High Frequency (Hz)", "Delta Frequency (Hz)", 
              "Average Power Density (dB/Hz)", "Delta Time (s)")

for (i in 1:length(variables)) {
  print(paste("Analysis of", y_labels[i]))
  plot <- create_and_save_plot(variables[i], y_labels[i])
  print(plot)  # Display the plot
  run_lm_and_summarize(variables[i])
}

# Calculate mean of Low.Freq at each altitude
print(aggregate(Low.Freq ~ altitude, data = data, mean))

# Calculate mean of Delta.Time at each altitude
print(aggregate(Delta.Time ~ altitude, data = data, mean))

