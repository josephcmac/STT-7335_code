#-----------------------------------------------------
# 0. Chargement des librairies
#-----------------------------------------------------
library(tidyverse)

#-----------------------------------------------------
# 1. Création du dossier 'images' s'il n'existe pas déjà
#-----------------------------------------------------
if (!dir.exists("images")) {
  dir.create("images")
}

#-----------------------------------------------------
# 2. Fonction de lecture des données
#-----------------------------------------------------
readData <- function(filename = "clean_data_2.csv") {
  read.csv(filename, stringsAsFactors = FALSE) %>%
    mutate(
      poet         = factor(poet),
      poem_title   = factor(poem_title),
      sex          = factor(sex),
      suicidal     = as.logical(suicidal),
      heterosexual = as.logical(heterosexual)
    )
}

#-----------------------------------------------------
# 3. Fonction de bootstrapping
#-----------------------------------------------------
bootstrapping <- function(df, n_boot = 1000) {
  # Échantillonnage bootstrap (avec remise) et calcul des moyennes colMeans()
  # pour les groupes suicidaires (s_plus) et non-suicidaires (s_minus).
  
  df_boot <- df[sample(1:nrow(df), size = nrow(df), replace = TRUE),]
  
  s_plus <- df_boot %>%
    filter(suicidal) %>%
    select(-suicidal, -poet) %>%
    colMeans()
  
  s_minus <- df_boot %>%
    filter(!suicidal) %>%
    select(-suicidal, -poet) %>%
    colMeans()
  
  for (i in 2:n_boot) {
    df_boot <- df[sample(1:nrow(df), size = nrow(df), replace = TRUE),]
    
    s_plus <- rbind(
      s_plus,
      df_boot %>%
        filter(suicidal) %>%
        select(-suicidal, -poet) %>%
        colMeans()
    )
    
    s_minus <- rbind(
      s_minus,
      df_boot %>%
        filter(!suicidal) %>%
        select(-suicidal, -poet) %>%
        colMeans()
    )
  }
  
  # Combine les résultats dans un data frame final
  s <- rbind(s_plus, s_minus)
  row.names(s) <- 1:nrow(s)
  s <- as.data.frame(s)
  s$suicidal <- as.logical(c(rep(TRUE, nrow(s_plus)), rep(FALSE, nrow(s_minus))))
  
  return(s)
}

#-----------------------------------------------------
# 4. Lecture et préparation des données
#-----------------------------------------------------
df <- readData("clean_data_2.csv") %>%
  select(
    poet, suicidal, 
    anticipation, trust, anger, disgust, joy, 
    positive, surprise, sadness
  )

#-----------------------------------------------------
# 5. Application du bootstrapping
#-----------------------------------------------------
df <- bootstrapping(df, n_boot = 10000)

#-----------------------------------------------------
# 6. Sélection des variables numériques et ACP
#-----------------------------------------------------
df_num <- df %>% select(where(is.double))

pca_result <- prcomp(df_num, center = TRUE, scale. = TRUE)
summary(pca_result)
pca_result$rotation  # Chargements (loadings) des variables sur les axes

# Extraction des coordonnées (scores) sur les axes principaux
pca_scores <- as.data.frame(pca_result$x)

#-----------------------------------------------------
# 7. Construction d'un data frame final pour la visualisation
#-----------------------------------------------------
# Conserver la variable suicidaire (logique) + PC1, PC2, ...
df_pca <- df %>%
  select(where(is.logical)) %>%
  bind_cols(pca_scores)

#-----------------------------------------------------
# 8. Calcul de la variance expliquée
#-----------------------------------------------------
variance_expliquee <- pca_result$sdev^2 / sum(pca_result$sdev^2)
pc1_var <- round(variance_expliquee[1] * 100, 1)
pc2_var <- round(variance_expliquee[2] * 100, 1)

#-----------------------------------------------------
# 9. Visualisation : Graphique PCA
#-----------------------------------------------------
pca_plot <- ggplot(df_pca, aes(x = PC1, y = PC2, color = suicidal)) +
  geom_point(alpha = 0.3) +
  theme_classic(base_size = 14) +
  labs(
    title = "Projection des données sur les deux premiers axes (ACP)",
    subtitle = "Analyse en composantes principales sur les variables émotionnelles",
    x = paste0("PC1 (", pc1_var, "% de variance expliquée)"),
    y = paste0("PC2 (", pc2_var, "% de variance expliquée)"),
    color = "Suicidaire"
  ) +
  scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
    labels = c("FALSE" = "Non", "TRUE" = "Oui")
  ) +
  coord_equal()

#-----------------------------------------------------
# 10. Sauvegarde du graphique dans le dossier 'images'
#-----------------------------------------------------
ggsave(
  filename = "images/pca_plot.png",
  plot     = pca_plot,
  width    = 8,   # Largeur en pouces
  height   = 6,   # Hauteur en pouces
  dpi      = 300  # Résolution en ppp
)
