# The purpose of this workspace is to test whether bicolor or solid males dewlaped more often or for a longer proportion of time per trial. 
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)

data <- read_excel("C:/Users/elizg/Desktop/JDC Data/Anap_mate-choice_dataset.xlsx",
                   sheet = "all_data_cleaned")

male_data <- data[, c("bicolor_male_id", "solid_male_id", "bicolor_disp", "bicolor_disp_frames", "prop_bicolor_disp_frames", "solid_disp", "solid_disp_frames", "prop_solid_disp_frames", "light_treatment")]

# POISSON TEST
# Total dewlap displays for each morph
bicolor_total <- sum(bicolor_disp) 
solid_total <- sum(solid_disp)      

#just to double check the counts
print(bicolor_total)
print(solid_total)

# Poisson test, assuming equal display times
poisson.test(c(bicolor_total, solid_total))

# PLOT PLOT PLOT
library(ggplot2)

# Total counts 
bicolor_total <- 176
solid_total <- 139

# Standard error for Poisson = sqrt(count)
morph_totals <- data.frame(
  morph = c("Bicolor", "Solid"),
  displays = c(bicolor_total, solid_total),
  se = sqrt(c(bicolor_total, solid_total))  # Poisson SE
)

ggplot(morph_totals, aes(x = morph, y = displays, fill = morph)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_errorbar(aes(ymin = displays - se, ymax = displays + se), width = 0.2) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(title = "Total Dewlap Displays by Morph",
       y = "Display Count", x = "") +
  theme_minimal() +
  annotate("text", x = 1.5, y = max(morph_totals$displays) + 15,
           label = "*", size = 8, fontface = "bold")

# Wilcoxon signed-rank test for within trial differences of male displays
wilcox.test(male_data$bicolor_disp, male_data$solid_disp, paired = TRUE)

## Normality Test of Duration of Displays
diff_duration <- bicolor_disp_frames - solid_disp_frames
shapiro.test(diff_duration)

wilcox.test(male_data$bicolor_disp_frames, male_data$solid_disp_frames, paired=FALSE)

