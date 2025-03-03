################################################################################
# SCRIPT R : Génération de figures (histogrammes, matrices de corrélation, etc.)
#            avec noms de variables en français
################################################################################

# 1. Charger les bibliothèques nécessaires
# install.packages("tidyverse")   # Décommentez si nécessaire
# install.packages("ggcorrplot")  # Décommentez si nécessaire
# install.packages("reshape2")    # Décommentez si nécessaire

library(tidyverse)   # Pour la manipulation de données et ggplot2 (inclus dans tidyverse)
library(readr)       # Pour lire des fichiers CSV
library(ggcorrplot)  # Pour la visualisation des corrélations (Option A)
library(reshape2)    # Pour la conversion de données (Option B)

################################################################################
# 2. Lecture des données et conversion de types
################################################################################
df_verses <- read_csv("clean_data/clean_data_2.csv", show_col_types = FALSE) %>%
  mutate(
    suicidal         = as.factor(suicidal),
    sex              = as.factor(sex),
    period           = as.factor(period),
    date_of_birth    = as.Date(date_of_birth, format = "%B %d, %Y"),
    date_of_death    = as.Date(date_of_death, format = "%B %d, %Y"),
    country_of_birth = as.factor(country_of_birth)
  )

################################################################################
# 3. Renommer les variables en français
################################################################################
df_verses_fr <- df_verses %>%
  rename(
    # Par exemple, si vous souhaitez aussi renommer d'autres variables :
    "Suicidaire"   = suicidal,
    "Sexe"         = sex,
    "Période"      = period,
    "Colère"       = anger,
    "Anticipation" = anticipation,
    "Dégoût"       = disgust,
    "Peur"         = fear,
    "Joie"         = joy,
    "Tristesse"    = sadness,
    "Surprise"     = surprise,
    "Confiance"    = trust,
    "Négatif"      = negative,
    "Positif"      = positive
    # Ajoutez d'autres renommages si nécessaire (n_words, country_of_birth, etc.)
  )

################################################################################
# 4. Créer le répertoire "figures" s'il n'existe pas
################################################################################
if (!dir.exists("figures")) dir.create("figures")

################################################################################
# 5. Histogramme de la variable "Anticipation"
################################################################################
hist_anticipation <- ggplot(df_verses_fr, aes(x = Anticipation)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal() +
  scale_y_log10() +
  labs(
    title = "Histogramme de l'Anticipation",
    x     = "Anticipation",
    y     = "Fréquence (échelle logarithmique)"
  )

ggsave(
  filename = "figures/hist_anticipation.png",
  plot     = hist_anticipation,
  width    = 6,
  height   = 4,
  dpi      = 300
)

################################################################################
# 6. Corrélation entre les émotions (avec noms en français)
################################################################################

# 6a. Sélectionner uniquement les colonnes d'émotions en français
df_emotions_fr <- df_verses_fr %>%
  select(
    Colère, Anticipation, Dégoût, Peur, 
    Joie, Tristesse, Surprise, Confiance,
    Négatif, Positif
  )

# 6b. Calculer la matrice de corrélation (exclut les lignes avec valeurs manquantes)
cor_matrix_fr <- cor(df_emotions_fr, use = "pairwise.complete.obs")

################################################################################
# 7. Visualiser et sauvegarder la matrice de corrélation
################################################################################

#===============================================================================
# OPTION A : Utiliser ggcorrplot
#===============================================================================
corr_plot_fr <- ggcorrplot(
  cor_matrix_fr,
  hc.order = TRUE,        # Réordonner la matrice par clustering hiérarchique
  type     = "lower",     # Afficher uniquement la partie inférieure du triangle
  lab      = TRUE,        # Afficher les coefficients de corrélation sur la matrice
  lab_size = 3,
  method   = "square",    # Forme des cases
  colors   = c("#6D9EC1", "white", "#E46726"), # Couleurs en dégradé (faible, moyen, fort)
  title    = "Corrélation entre les émotions (en français)",
  ggtheme  = theme_minimal()
)

# Sauvegarde du graphique ggcorrplot
ggsave(
  filename = "figures/correlation_ggcorrplot.png",
  plot     = corr_plot_fr,
  width    = 6,
  height   = 6,
  dpi      = 300
)

#===============================================================================
# OPTION B : Utiliser reshape2 + ggplot2 pour créer une heatmap
#===============================================================================
cor_data_fr <- as.data.frame(cor_matrix_fr) %>%
  rownames_to_column("Emotion1") %>%
  melt(id.vars = "Emotion1", variable.name = "Emotion2", value.name = "Corrélation")

heatmap_cor_fr <- ggplot(cor_data_fr, aes(x = Emotion1, y = Emotion2, fill = Corrélation)) +
  geom_tile() +
  scale_fill_gradient2(
    low       = "blue",
    mid       = "white",
    high      = "red",
    midpoint  = 0
  ) +
  labs(
    title = "Carte de chaleur des corrélations entre émotions (en français)",
    x     = "",
    y     = ""
  ) +
  theme_minimal() +
  # Option : faire pivoter les étiquettes en abscisse pour plus de lisibilité
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sauvegarde du graphique heatmap
ggsave(
  filename = "figures/correlation_heatmap.png",
  plot     = heatmap_cor_fr,
  width    = 6,
  height   = 6,
  dpi      = 300
)

message("Les figures ont été générées avec des noms de variables en français dans le dossier 'figures'.")
