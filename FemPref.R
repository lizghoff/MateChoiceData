# The purpose of this workspace is to test whether females spent a higher proportion of their time during a trial a bicolor or solid morph male
library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)

data <- read_excel("C:/Users/elizg/Desktop/JDC Data/Anap_mate-choice_dataset.xlsx",
                   sheet = "all_data_cleaned")

# Extract columns
propf_data <- data[, c("fem_id", "prop_frames_fem_bicolor", "prop_frames_fem_solid", "total_frames")]

# Wilcoxon signed-rank test
# Paired to keep choice within trials consistent
wilcox.test(
  x = propf_data$prop_frames_fem_bicolor, # proportion of frames a female spent with a bicolor male
  y = propf_data$prop_frames_fem_solid,   # proportion of frames a female spent with a solid male
  paired = TRUE
)



# These graphs are meh, blame copilot for now!
# Prepare data from Excel-derived proportions
plot_data <- propf_data |>
  select(fem_id, prop_frames_fem_bicolor, prop_frames_fem_solid) |>
  pivot_longer(cols = starts_with("prop_frames_fem_"),
               names_to = "morph",
               values_to = "prop") |>
  mutate(morph = gsub("prop_frames_fem_", "", morph))

# Boxplot + Jitter
ggplot(plot_data, aes(x = morph, y = prop, fill = morph)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2.2) +
  scale_fill_manual(values = c("bicolor" = "#ff7f0e", "solid" = "#1f77b4")) +
  labs(
    title = "Female Time Allocation by Male Morph",
    x = "Morph Type",
    y = "Proportion of Trial Time",
    fill = "Morph"
  ) +
  theme_minimal()


