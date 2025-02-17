#-------------------------------------------------------------
# 0. Chargement des librairies
#-------------------------------------------------------------
# install.packages("factoextra") # à faire une fois si pas encore installé
library(dplyr)
library(readr)
library(ggplot2)
library(factoextra)

#-------------------------------------------------------------
# 1. Importation et agrégation des données
#-------------------------------------------------------------
df <- read_csv("./clean_data/clean_data_2.csv", show_col_types = FALSE)

emotions <- c("anger", "anticipation", "disgust", "fear", 
              "joy", "sadness", "surprise", "trust", 
              "negative", "positive")

df_poet <- df %>%
  group_by(poet, suicidal) %>%
  summarise(across(all_of(emotions), mean, na.rm = TRUE), .groups = "drop")

df_poet$suicidal <- factor(
  df_poet$suicidal,
  levels = c(FALSE, TRUE),
  labels = c("nonsuicidal", "suicidal")
)

# Matrice d'émotions uniquement
df_emotions <- df_poet[, emotions]

#-------------------------------------------------------------
# 2. ACP : calcul et objets principaux
#-------------------------------------------------------------
pca_res <- prcomp(df_emotions, scale. = TRUE)

# Proportion de variance expliquée par chaque composante
variances <- (pca_res$sdev^2) / sum(pca_res$sdev^2)
variances_pc <- round(100 * variances, 2)  # en pourcentage

# Coordonnées des individus (poètes) sur les composantes principales
pc_df <- data.frame(
  poet     = df_poet$poet,
  suicidal = df_poet$suicidal,
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  PC3 = pca_res$x[, 3]  # si on veut aller plus loin
)

#-------------------------------------------------------------
# 3. Scree Plot (Éboulis des valeurs propres)
#-------------------------------------------------------------
# Préparer les données pour le scree plot
scree_data <- data.frame(
  PC = factor(paste0("PC", 1:length(variances)), 
              levels = paste0("PC", 1:length(variances))),
  Variance = variances_pc
)

g_scree <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Variance, "%")), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Scree Plot - Variance expliquée par composante",
    x = "Composantes principales",
    y = "Pourcentage de variance expliquée"
  ) +
  theme_minimal(base_size = 14)

# Enregistrer le scree plot
ggsave("01_scree_plot.png", plot = g_scree, width = 6, height = 5)

#-------------------------------------------------------------
# 4. Visualisation Individus (PC1 vs PC2) en ggplot2
#-------------------------------------------------------------
g_ind <- ggplot(pc_df, aes(x = PC1, y = PC2, color = suicidal, label = poet)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(
    nudge_y = 0.15, size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("forestgreen", "firebrick")) +
  labs(
    title = "Projection des poètes dans le plan (PC1, PC2)",
    x = paste0("PC1 (", variances_pc[1], "%)"),
    y = paste0("PC2 (", variances_pc[2], "%)"),
    color = "Statut suicidaire"
  ) +
  theme_minimal(base_size = 14)

ggsave("02_individuals_PC1_PC2.png", plot = g_ind, width = 7, height = 5)

#-------------------------------------------------------------
# 5. Biplot et Cercle des Corrélations avec factoextra
#-------------------------------------------------------------
# Biplot : individus + variables
g_biplot <- fviz_pca_biplot(
  pca_res,
  repel = TRUE,
  label = "var",                # n'afficher que le label des variables (émotions)
  col.var = "steelblue",        # couleur des flèches
  habillage = df_poet$suicidal, # colorer les points selon suicidal
  title = "Biplot ACP (Individus + Variables)"
)
ggsave("03_pca_biplot.png", plot = g_biplot, width = 7, height = 6)

# Cercle de corrélations (variables uniquement)
g_vars <- fviz_pca_var(
  pca_res,
  col.var = "contrib",   # colorer selon la contribution à l'inertie
  gradient.cols = c("navyblue", "orange", "red"),
  repel = TRUE,
  title = "Cercle des corrélations - Emotions"
)
ggsave("04_correlations_circle.png", plot = g_vars, width = 7, height = 6)

#-------------------------------------------------------------
# 6. Comparaisons PC1 / PC2 entre groupes suicidaires
#-------------------------------------------------------------
# Exemple : boxplot pour PC1
g_box1 <- ggplot(pc_df, aes(x = suicidal, y = PC1, fill = suicidal)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_manual(values = c("forestgreen", "firebrick")) +
  labs(
    title = "Comparaison de PC1 selon le statut suicidaire",
    x = "Statut suicidaire",
    y = "Score sur PC1"
  ) +
  theme_minimal(base_size = 14)

ggsave("05_boxplot_PC1.png", plot = g_box1, width = 6, height = 5)

# Exemple : boxplot pour PC2
g_box2 <- ggplot(pc_df, aes(x = suicidal, y = PC2, fill = suicidal)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_manual(values = c("forestgreen", "firebrick")) +
  labs(
    title = "Comparaison de PC2 selon le statut suicidaire",
    x = "Statut suicidaire",
    y = "Score sur PC2"
  ) +
  theme_minimal(base_size = 14)

ggsave("06_boxplot_PC2.png", plot = g_box2, width = 6, height = 5)

#-------------------------------------------------------------
# 7. Tests statistiques (facultatif)
#-------------------------------------------------------------
# Pour un test de comparaison, on peut faire un t.test simple sur PC1 et PC2 :
test_PC1 <- t.test(PC1 ~ suicidal, data = pc_df)
test_PC2 <- t.test(PC2 ~ suicidal, data = pc_df)

# Afficher dans la console
test_PC1
test_PC2

# Vous pouvez aussi vouloir exporter ces résultats dans un fichier texte
capture.output(
  list(PC1_t_test = test_PC1, PC2_t_test = test_PC2),
  file = "07_tests_statistiques.txt"
)

#-------------------------------------------------------------
# Fin de l'analyse
#-------------------------------------------------------------
