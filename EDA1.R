###############################################################################
# Full R Code: Separating Data Levels, Descriptive Analysis, and PCA
###############################################################################

# 1. Load libraries
###############################################################################
library(tidyverse)   # Includes readr, dplyr, ggplot2, etc.
library(readr)       # For CSV reading (also in tidyverse)
library(ggplot2)     # For plotting
library(broom)       # For tidying PCA results (optional)

# 2. Importation of clean_data_2.csv
###############################################################################
# Adjust the file path if needed
df <- read_csv("./clean_data/clean_data_2.csv")

cat("=== STRUCTURE OF THE ORIGINAL DATA ===\n\n")
glimpse(df)
cat("\n")

# 3. Separate poet-level vs. verse-level data
###############################################################################
# poet-level: 1 row per poet
df_poet <- df %>%
  distinct(poet, .keep_all = TRUE) %>%
  select(
    poet,
    suicidal,
    sex,
    heterosexual,
    date_of_birth,
    date_of_death,
    country_of_birth
  )

# verse-level: 1 row per verse
df_verse <- df %>%
  select(
    pair_id,
    poet,
    poem_title,
    period,
    anger,
    anticipation,
    disgust,
    fear,
    joy,
    sadness,
    surprise,
    trust,
    negative,
    positive
  )

cat("=== STRUCTURE OF df_poet (One row per poet) ===\n\n")
glimpse(df_poet)
cat("\n")

cat("=== STRUCTURE OF df_verse (One row per verse) ===\n\n")
glimpse(df_verse)
cat("\n")

# 4. Missing values & distributions
###############################################################################
# 4.1. Missing values in df_poet
missing_poet <- colSums(is.na(df_poet))

# 4.2. Missing values in df_verse
missing_verse <- colSums(is.na(df_verse))

cat("=== MISSING VALUES (df_poet) ===\n")
print(missing_poet)
cat("\n")

cat("=== MISSING VALUES (df_verse) ===\n")
print(missing_verse)
cat("\n")

# 4.3. Distribution of 'period' in df_verse
table_period <- table(df_verse$period, useNA = "ifany")
cat("=== DISTRIBUTION OF 'period' IN df_verse ===\n")
print(table_period)
cat("\n")

# 4.4. Distributions at poet level (e.g., suicidal, sex, heterosexual)
table_suicidal <- table(df_poet$suicidal, useNA = "ifany")
table_sex <- table(df_poet$sex, useNA = "ifany")
table_hetero <- table(df_poet$heterosexual, useNA = "ifany")

cat("=== DISTRIBUTION 'suicidal' (POET LEVEL) ===\n")
print(table_suicidal)
cat("\n")

cat("=== DISTRIBUTION 'sex' (POET LEVEL) ===\n")
print(table_sex)
cat("\n")

cat("=== DISTRIBUTION 'heterosexual' (POET LEVEL) ===\n")
print(table_hetero)
cat("\n")

# 5. Descriptive statistics of emotions (verse-level)
###############################################################################
emotion_vars <- c(
  "anger", "anticipation", "disgust", "fear",
  "joy", "sadness", "surprise", "trust",
  "negative", "positive"
)

desc_emotions_verse <- df_verse %>%
  select(all_of(emotion_vars)) %>%
  summary()

cat("=== DESCRIPTIVE STATS (VERSE-LEVEL EMOTIONS) ===\n")
print(desc_emotions_verse)
cat("\n")

# 6. Compare emotions by suicidal or not (verse-level)
###############################################################################
# Join verse-level with poet-level to get 'suicidal' info
df_joined <- df_verse %>%
  inner_join(df_poet, by = "poet")

cat("=== HEAD OF df_joined (VERSE + POET DATA) ===\n")
print(head(df_joined))
cat("\n")

# Summarize average emotions by suicidal group
desc_suicidal <- df_joined %>%
  group_by(suicidal) %>%
  summarise(
    across(
      all_of(emotion_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    )
  )

cat("=== AVERAGE EMOTIONS BY SUICIDAL (VERSE-LEVEL) ===\n")
print(desc_suicidal)
cat("\n")

# 7. PCA (Principal Component Analysis) at poet-level
###############################################################################
# Aggregate verse-level emotions by poet (mean)
df_agg <- df_joined %>%
  group_by(poet) %>%
  summarise(
    across(all_of(emotion_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  left_join(df_poet, by = "poet")

# PCA on aggregated emotions
pca_res <- prcomp(df_agg %>% select(all_of(emotion_vars)), scale. = TRUE)
pca_summary <- summary(pca_res)
pca_var_explained <- pca_summary$importance[2, ]

cat("=== PCA: Proportion of Variance Explained (%) ===\n")
print(round(pca_var_explained * 100, 2))
cat("\n")

# Prepare data frame of PCA scores (for plotting, etc.)
pca_scores <- as.data.frame(pca_res$x) %>%
  mutate(
    poet = df_agg$poet,
    suicidal = df_agg$suicidal
  )

cat("=== HEAD OF PCA SCORES ===\n")
print(head(pca_scores))
cat("\n")

# 8. (Optional) Plot of PC1 vs PC2
###############################################################################
# ggplot(pca_scores, aes(x = PC1, y = PC2, color = suicidal)) +
#   geom_point(size = 3, alpha = 0.7) +
#   theme_minimal() +
#   labs(
#     title = "PCA on Mean Emotions per Poet",
#     x = paste0("PC1 (", round(pca_var_explained[1] * 100, 1), "%)"),
#     y = paste0("PC2 (", round(pca_var_explained[2] * 100, 1), "%)")
#   )

cat("=== END OF SCRIPT ===\n")
