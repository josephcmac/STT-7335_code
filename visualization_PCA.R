library(dplyr)
library(readr)
library(ggplot2)

#-----------------------------
# 1. Data Aggregation
#-----------------------------
df <- read_csv("./clean_data/clean_data_2.csv", show_col_types = FALSE)

# Specify the emotion columns
emotions <- c("anger", "anticipation", "disgust", "fear", 
              "joy", "sadness", "surprise", "trust", 
              "negative", "positive")

# Aggregate by poet & suicidal status, calculating mean of each emotion
df_poet <- df %>%
  group_by(poet, suicidal) %>%
  summarise(across(all_of(emotions), mean, na.rm = TRUE), .groups = "drop")

# Convert suicidal to factor with clear labels
df_poet$suicidal <- factor(
  df_poet$suicidal,
  levels = c(FALSE, TRUE),
  labels = c("nonsuicidal", "suicidal")
)

#-----------------------------
# 2. PCA Computation
#-----------------------------
pca_res <- prcomp(df_poet[, emotions], scale. = TRUE)

# Calculate the proportion of variance for PC1 and PC2
variance_explained <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
pc1_var <- round(variance_explained[1], 1)
pc2_var <- round(variance_explained[2], 1)

# Extract PC1 and PC2 into a data frame for plotting
pca_data <- data.frame(
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  suicidal = df_poet$suicidal
)

#-----------------------------
# 3. Plot using ggplot2
#-----------------------------

g <- ggplot(pca_data, aes(x = PC1, y = PC2, color = suicidal)) +
  geom_point(size = 3, alpha = 0.5) +   # points semi-transparents
  scale_color_manual(values = c("green", "red")) +
  labs(
    title = "ACP des émotions par poète",
    subtitle = "Deux composantes principales illustrant les valeurs\nmoyennes agrégées des émotions",
    x = paste0("CP1 (", pc1_var, "% de variance)"),
    y = paste0("CP2 (", pc2_var, "% de variance)"),
    color = "Statut suicidaire"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )



png("pca_plot_example.png")
print(g)
dev.off()
