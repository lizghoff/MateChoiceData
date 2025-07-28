library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(lme4)
library(tidyr)
library(ggpubr)

fem_treat <- read_excel("C:/Users/elizg/Desktop/JDC Data/Anap_mate-choice_dataset.xlsx",
                   sheet = "all_data_cleaned")

# Revamp format
fem_long <- fem_treat %>%
  pivot_longer(
    cols = c(prop_frames_fem_bicolor, prop_frames_fem_solid),
    names_to = "morph_type",
    values_to = "prop_frames"
  ) %>%
  mutate(morph_type = ifelse(morph_type == "prop_frames_fem_bicolor", "bicolor", "solid"))

# Run Fixed Effect Model
lm_model <- lm(prop_frames ~ light_treatment * morph_type, data = fem_long)
summary(lm_model)


# Graph sorta, thanks copilot
# First, summarize mean + SE per group
summary_df <- fem_long %>%
  group_by(morph_type, light_treatment) %>%
  summarize(mean_prop = mean(prop_frames),
            se_prop = sd(prop_frames)/sqrt(n()), .groups = "drop")


ggplot(fem_long, aes(x = morph_type, y = prop_frames, fill = light_treatment)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Mean Proportion of Time by Morph and Light",
       x = "Morph Type", y = "Mean Proportion of Frames") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()




# Make a subset for deep shade condition
fem_deep <- fem_long %>% 
  filter(light_treatment == "deep_shade")


fem_deeps <- fem_deep %>%
  dplyr::select(fem_id, morph_type, prop_frames) %>%
  pivot_wider(names_from = morph_type, values_from = prop_frames)

# Run Paired T Test
t.test(fem_deeps$bicolor, fem_deeps$solid, paired = TRUE)

