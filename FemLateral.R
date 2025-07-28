library(readxl)
library(ggplot2)
library(reshape2)
library(tidyr)

# Load data using 
data <- read_excel("C:/Users/elizg/Desktop/JDC Data/Anap_mate-choice_dataset.xlsx",
                   sheet = "all_data_cleaned")
mc_data <- data[, c("fem_id", "prop_frames_fem_bicolor", "prop_frames_fem_solid", "male_order")]

# Extrapolate time on the left versus time on the right based on male order
mc_data$time_left <- ifelse(mc_data$male_order == "BS",
                              mc_data$prop_frames_fem_bicolor,
                              mc_data$prop_frames_fem_solid)

mc_data$time_right <- ifelse(mc_data$male_order == "BS",
                               mc_data$prop_frames_fem_solid,
                               mc_data$prop_frames_fem_bicolor)


mc_data$bias_score <- mc_data$time_left - mc_data$time_right

# Just to double check it's doing the thing right!
View(mc_data[, c("male_order",
                 "prop_frames_fem_bicolor",
                 "prop_frames_fem_solid",
                 "time_left",
                 "time_right")])

# Check normality
shapiro.test(mc_data$bias_score)

# Paired t test
lat_ttest <- t.test(mc_data$time_left, mc_data$time_right, paired = TRUE)
print(lat_ttest)


# Sorta visualized it?
library(ggplot2)

# Plot with boxplot + dots
ggplot(long_data, aes(x = side, y = time, fill = side)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # Hide default outliers to avoid double plotting
  geom_jitter(width = 0.2, size = 2, shape = 21, fill = "black", color = "black") +
  scale_fill_manual(values = c("time_left" = "#E69F00", "time_right" = "#56B4E9")) +
  theme_minimal() +
  labs(title = "Time Spent Per Side by Female",
       x = "Side of Arena",
       y = "Proportion of Time (frames)")

