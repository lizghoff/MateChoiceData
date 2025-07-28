# The purpose of this workspace is to test if females that were tested more than once chose the same morph each time. Additionally, I included tests
# to see if females selected the same morph that was dominant in their source zone.
library(dplyr)
library(readxl)

df <- read_excel("C:/Users/elizg/Desktop/JDC Data/Anap_mate-choice_dataset.xlsx",
                   sheet = "all_data_cleaned")

# Sort females tested more than once
multi_fem_df <- df %>%
  group_by(fem_id) %>%
  filter(n() > 1)

# Assign morph choice based on female higher proportion of time spent
multi_fem_df <- multi_fem_df %>%
  mutate(preferred_morph = case_when(
    prop_frames_fem_solid > prop_frames_fem_bicolor ~ "solid",   # spent more time with the soild morph, define "preferred" as solid
    prop_frames_fem_solid < prop_frames_fem_bicolor ~ "bicolor", # spent more time with the bicolro morph, define "preferred" as bicolor
    TRUE ~ "tie" 
  ))

# Consistency across trials
consistency_summary <- multi_fem_df %>%
  group_by(fem_id) %>%
  summarise(
    trials_tested = n(),
    unique_choices = n_distinct(preferred_morph),
    consistent = unique_choices == 1,
    morph_chosen = if_else(unique_choices == 1, first(preferred_morph), "mixed")
  )

# View summary
print(consistency_summary, n=21)


# Count consistency of females
n_same <- sum(consistency_summary$consistent)
n_total <- nrow(consistency_summary)

# Binomial Test
binom.test(n_same, n_total, p = 0.5, alternative = "greater")


# Test if females preferred a morph based on source zone
# Filter data
pref_by_zone <- multi_fem_df %>%
  filter(preferred_morph != "tie") %>%  # Optional: remove ties for clean test
  group_by(fem_source_zone, preferred_morph) %>%
  summarise(n = n(), .groups = "drop")

# Contingency table
pref_table <- xtabs(n ~ fem_source_zone + preferred_morph, data = pref_by_zone)

# Chi-Square Test
chisq.test(pref_table)


