Code for analysing any possible relationship between bird distribution and altitude

# Set working directory
setwd("C:/Users/sheil/Desktop/WP_75")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(readr)

# Define the altitude groups
altitude_groups <- list(
  "1134_metres" = c("WP3", "WP4", "WP9"),
  "1180_metres" = c("WP2", "WP5", "WP8"),
  "1235_metres" = c("WP1", "WP7", "WP11"),
  "1274_metres" = c("WP6", "WP10", "WP12")
)

# Function to read and process a single file
process_file <- function(file_name) {
  data <- read_csv(paste0(file_name, "_75.csv"), col_types = cols())
  data %>%
    count(label, name = "frequency") %>%
    filter(label != "none") %>%
    mutate(WP = file_name)
}

# Process all files and combine results
all_results <- map_df(unlist(altitude_groups), process_file)

# Group results by altitude and label
grouped_results <- all_results %>%
  mutate(altitude_group = case_when(
    WP %in% altitude_groups[["1134_metres"]] ~ "1134_metres",
    WP %in% altitude_groups[["1180_metres"]] ~ "1180_metres",
    WP %in% altitude_groups[["1235_metres"]] ~ "1235_metres",
    WP %in% altitude_groups[["1274_metres"]] ~ "1274_metres"
  )) %>%
  group_by(altitude_group, label) %>%
  summarise(total_frequency = sum(frequency), .groups = "drop") %>%
  arrange(altitude_group, desc(total_frequency))

# Print results and save to CSV
print(grouped_results)
write.csv(grouped_results, "grouped_label_counts_by_altitude.csv", row.names = FALSE)

# Create a summary table
summary_table <- grouped_results %>%
  pivot_wider(names_from = altitude_group, values_from = total_frequency, values_fill = 0)

# Print and save summary table
print(summary_table)
write.csv(summary_table, "summary_label_counts_by_altitude.csv", row.names = FALSE)


# Create the plot
plot <- ggplot(grouped_results, aes(x = label, y = total_frequency, fill = altitude_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Label Frequencies by Altitude Group", x = "Label", y = "Frequency", fill = "Altitude Group")

# Save the plot
ggsave("label_frequencies_by_altitude.png", plot, width = 12, height = 8)


### Altitude Filtered Analysis

# Filter labels with at least two non-zero counts across altitudes
filtered_summary_table <- summary_table %>%
  mutate(non_zero_count = rowSums(dplyr::select(., -label) > 0)) %>%
  filter(non_zero_count >= 2) %>%
  dplyr::select(-non_zero_count)

# Calculate trend for each species
bird_trends <- filtered_summary_table %>%
  mutate(trend_value = `1274_metres` - `1134_metres`,
         trend = case_when(
           trend_value > 0 ~ "+",
           trend_value < 0 ~ "-",
           TRUE ~ "="
         ))

# Reshape data for plotting
plot_data <- bird_trends %>%
  dplyr::select(-trend_value) %>%
  pivot_longer(cols = -c(label, trend), names_to = "altitude_group", values_to = "frequency") %>%
  mutate(label_with_trend = paste(label, trend))

# Create the plot with trend indicators
plot <- ggplot(plot_data, aes(x = label_with_trend, y = frequency, fill = altitude_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Bird Call Frequencies by Altitude Group",
       subtitle = "With trend indicator (+ more calls at higher altitude, - fewer calls at higher altitude)",
       x = "Bird Species", y = "Frequency", fill = "Altitude Group") +
  scale_fill_brewer(palette = "Set2")

# Display the plot and save it
print(plot)
ggsave("bird_frequencies_with_trend.png", plot, width = 14, height = 8)

# Save filtered trends to CSV
write.csv(bird_trends, "bird_frequencies_with_trends.csv", row.names = FALSE)


## Additional Analyses

### Scatter Plot for Trend Values of Bird Species

### Scatter Plot for Trend Values of Bird Species

scatter_plot <- ggplot(bird_trends, aes(x = trend_value, y = label)) +
  geom_point(aes(color = trend_value), size = 4) +  # Points colored by trend_value
  geom_text(aes(label = label), hjust = 0.4, vjust = -0.9) +  # Adjust text position slightly above points
  scale_color_gradientn(colors = c("red", "orange", "yellow", "green"), 
                        values = scales::rescale(c(-18, -5, 0, 8)), 
                        limits = c(-18, 8)) +  # Define color gradient and limits
  theme_minimal() +
  labs(title = "Trend Values of Bird Species",
       x = "Trend Value",
       y = NULL) +  # Remove y-axis label
  theme(axis.ticks.y = element_blank(),   # Hide y-axis ticks
        axis.text.y = element_blank(),    # Hide y-axis text
        axis.title.y = element_blank(),   # Hide y-axis title
        panel.grid.major.y = element_blank(), # Remove major grid lines for y-axis
        panel.grid.minor.y = element_blank()) # Remove minor grid lines for y-axis

print(scatter_plot)
ggsave("scatter_plot_bird_species_trends.png", scatter_plot, width=14, height=8)


### Stacked Bar Plot for Species Distribution by Altitude

stacked_bar_plot <- ggplot(grouped_results, aes(x = altitude_group, y = total_frequency, fill = label)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Species Distribution by Altitude",
       x = "Altitude Group",
       y = "Total Frequency",
       fill = "Bird Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Rotate x-axis labels for better readability

print(stacked_bar_plot)
ggsave("stacked_bar_species_distribution.png", stacked_bar_plot, width=14, height=8)

### Heatmap of Species Distribution

# Step 1: Convert summary_table to data frame if it's a tibble
heatmap_data <- as.data.frame(summary_table)

# Step 2: Count the number of altitudes for each species and filter
species_counts <- heatmap_data %>%
  mutate(non_zero_count = rowSums(.[-1] > 0)) %>%  # Count non-zero frequencies, excluding the first column (label)
  filter(non_zero_count > 1)  # Keep only species present in more than one altitude

# Step 3: Filter the original heatmap_data to keep only those species
filtered_heatmap_data <- heatmap_data %>%
  filter(label %in% species_counts$label)

# Step 4: Reshape the filtered data for heatmap
melted_heatmap_data <- reshape2::melt(filtered_heatmap_data, id.vars = "label", 
                                      variable.name = "Altitude Group", 
                                      value.name = "Frequency")

# Step 5: Create the heatmap plot
heatmap_plot <- ggplot(melted_heatmap_data, aes(x = `Altitude Group`, y = label)) + 
  geom_tile(aes(fill = Frequency), color = 'white') + 
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(title = "Heatmap of Species Distribution by Altitude",
       x= "Altitude", 
       y = "Bird Species") + 
  theme_minimal()

print(heatmap_plot)

# Save the heatmap plot
ggsave("heatmap_species_distribution_filtered.png", heatmap_plot, width = 14, height = 8)


### Diversity Analysis: Calculate Shannon Diversity Index for Each Altitude

shannon_diversity_index <- grouped_results %>%
  group_by(altitude_group) %>%
  summarise(shannon_index=-sum((total_frequency/sum(total_frequency)) * log(total_frequency/sum(total_frequency))), .groups='drop')

# Create a bar plot for the Shannon Diversity Index

diversity_plot <- ggplot(shannon_diversity_index, aes(x=altitude_group, y=shannon_index)) +
  geom_bar(stat='identity', fill='lightblue') +
  theme_minimal() +
  labs(title='Shannon Diversity Index by Altitude',
       x='Altitude Group',
       y='Shannon Diversity Index')

print(diversity_plot)
ggsave("shannon_diversity_index.png", diversity_plot, width=14,height=8)



# Load necessary libraries
library(dplyr)
library(tidyr)

# Assuming filtered_summary_table is already created and contains total_frequency for each altitude group

# Step 1: Check column names
print(colnames(filtered_summary_table))

# Step 2: Prepare long_data from filtered_summary_table
# Here we assume that the first column is "label" and we want to pivot all other columns
long_data <- filtered_summary_table %>%
  pivot_longer(cols = -label, names_to = "Altitude", values_to = "Frequency")

# Step 3: Convert Altitude to numeric (removing "_metres")
long_data$Altitude <- as.numeric(gsub("_metres", "", long_data$Altitude))

# Print the long_data to verify its structure
print(head(long_data))

# Initialize an empty data frame to store results
results <- data.frame(Species = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Step 4: Loop through each unique species in long_data
for (species in unique(long_data$label)) {
  # Subset data for the current species
  species_data <- long_data[long_data$label == species, ]
  
  # Check if there are enough data points to fit the model
  if (nrow(species_data) > 1) {
    # Fit linear model: Frequency ~ Altitude
    model <- lm(Frequency ~ Altitude, data = species_data)
    
    # Get summary of the model
    summary_model <- summary(model)
    
    # Extract p-value for Altitude (second row corresponds to Altitude)
    p_value <- summary_model$coefficients[2, 4]  
    
    # Store results
    results <- rbind(results, data.frame(Species = species, P_Value = p_value))
  } else {
    # If not enough data points, store NA or a message
    results <- rbind(results, data.frame(Species = species, P_Value = NA))
  }
}

# Print results
print(results)


# Load necessary libraries
library(ggplot2)

# Filter long_data for significant species
significant_species <- results %>%
  filter(P_Value <= 0.05) %>%
  pull(Species)

# Plotting frequencies against altitude for significant species
ggplot(long_data[long_data$label %in% significant_species, ], aes(x = Altitude, y = Frequency)) +
  geom_point(aes(color = label)) +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear model fit line
  labs(title = "Frequency of Significant Bird Species by Altitude",
       x = "Altitude (metres)",
       y = "Frequency") +
  theme_minimal() +
  scale_color_discrete(name = "Species")

# Save the plot if needed
ggsave("significant_species_frequency_by_altitude.png", width = 14, height = 8)


Code for analysing any possible relationship between hour of singing and altitude
# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(lubridate)  # For date-time manipulation

# Define altitude groups with correct file numbers
altitude_groups <- list(
  "1134_meters" = c(3, 4, 9),   # Corresponds to WP3, WP4, WP9
  "1180_meters" = c(2, 5, 8),   # Corresponds to WP2, WP5, WP8
  "1235_meters" = c(1, 7, 11),  # Corresponds to WP1, WP7, WP11
  "1274_meters" = c(6, 10, 12)  # Corresponds to WP6, WP10, WP12
)

# Function to process each file
process_file <- function(file_number) {
  # Construct the correct file name
  file_name <- paste0("WP", file_number, "_75.csv")  # Use the correct naming convention
  
  # Read the CSV file
  data <- read.csv(file_name)
  
  # Determine altitude group
  altitude <- names(altitude_groups)[sapply(altitude_groups, function(x) file_number %in% x)]
  
  # Check if altitude is found
  if (length(altitude) == 0) {
    warning(paste("No altitude group found for file number:", file_number))
    altitude <- NA  # Assign NA if no group found
  } else {
    altitude <- altitude[1]  # Get the first matching group
  }
  
  # Filter out "none" labels and extract time from filename
  vocalizations <- data %>%
    filter(label != "none") %>%
    mutate(
      datetime = ymd_hms(filename),
      hour = hour(datetime),
      minute = minute(datetime),
      altitude_group = altitude
    )
  
  return(vocalizations)
}

# Process all files and combine results
all_vocalizations <- map_dfr(1:12, process_file)

# Count vocalizations by hour and altitude group
hourly_counts <- all_vocalizations %>%
  group_by(altitude_group, hour) %>%
  summarise(count = n(), .groups = "drop")

# Create overall cumulative hourly plot
overall_plot <- ggplot(hourly_counts, aes(x = hour, y = count, fill = altitude_group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Overall Hourly Vocalizations by Altitude Group",
       x = "Hour of Day", y = "Number of Vocalizations") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_brewer(palette = "Set3")

ggsave("Overall_hourly_vocalizations_by_altitude.png", overall_plot, width = 12, height = 8)

# Analyze peak vocalization times by altitude at the hour level
peak_times <- all_vocalizations %>%
  group_by(altitude_group, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(altitude_group) %>%
  slice_max(order_by = count, n = 1)

# Write results to CSV files
write.csv(hourly_counts, "hourly_vocalizations_by_altitude.csv", row.names = FALSE)
write.csv(peak_times, "peak_vocalization_times_by_altitude.csv", row.names = FALSE)


# Analyze peak vocalization times by species at the hour level
peak_times_species <- all_vocalizations %>%
  group_by(label, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(label) %>%
  slice_max(order_by = count, n = 1)

# Filter out species with peak counts less than 2
peak_times_species_filtered <- peak_times_species %>%
  filter(count >= 2)

# Write results to CSV files
write.csv(peak_times_species_filtered, "peak_vocalization_times_by_species.csv", row.names = FALSE)

# Print peak times for each species
cat("\nPeak Vocalization Times by Species:\n")
print(peak_times_species_filtered)






# Load necessary libraries
library(dplyr)
library(ggplot2)

# Count vocalizations by hour and species
hourly_counts_species <- all_vocalizations %>%
  group_by(label, hour) %>%
  summarise(count = n(), .groups = "drop")

# Count total vocalizations for each species
total_counts_species <- hourly_counts_species %>%
  group_by(label) %>%
  summarise(total_calls = sum(count), .groups = "drop")

# Print total counts to check
print(total_counts_species)

# Filter out species with fewer than 4 total vocalizations
species_with_min_calls <- total_counts_species %>%
  filter(total_calls >= 4)  # Keep only species with at least 4 calls

# Print filtered species to check
print(species_with_min_calls)

# Filter the hourly counts to include only those species with at least 4 calls
filtered_hourly_counts <- hourly_counts_species %>%
  filter(label %in% species_with_min_calls$label)

# Print filtered hourly counts to verify filtering
print(filtered_hourly_counts)

# Identify peak hour for each species from filtered hourly counts
peak_hours_species <- filtered_hourly_counts %>%
  group_by(label) %>%
  slice_max(order_by = count, n = 1)

# Manually remove entries with counts of 3 and 2
peak_hours_species <- peak_hours_species %>%
  filter(count > 3) # Keep only those with counts greater than 3

# Print the peak hours data frame to check
print(peak_hours_species)

# Create a dot plot for peak hours by species without a legend
plot <- ggplot(peak_hours_species, aes(x = hour, y = count, color = label)) +
  geom_point(size = 3) +  # Use dots to represent peak counts
  labs(title = "Peak Vocalization Counts by Hour of Day and Species",
       x = "Hour of Day",
       y = "Peak Vocalization Count") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +  # Ensure all hours are shown
  geom_text(aes(label = label), angle = 0, vjust = -1.5, hjust = -0.1) +  
  theme(axis.text.x = element_text(hjust = 1), 
        legend.position = "none")  # Remove the legend

# Save the plot
ggsave("peak_vocalization_counts_by_hour_and_species_dot_plot_no_legend.png", plot, width = 10, height = 6)




