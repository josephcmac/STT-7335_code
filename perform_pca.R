################################################################################
# Chargement des bibliothèques
################################################################################
library(tidyverse)   # Pour la manipulation de données (inclut readr)
library(FactoMineR)  # Pour l'ACP
library(factoextra)  # Pour les fonctions pratiques de visualisation d'ACP

################################################################################
# Lecture et préparation des données
################################################################################
df <- read_csv("clean_data/clean_data_2.csv", show_col_types = FALSE) %>%
  mutate(
    suicidal         = as.factor(suicidal),
    sex              = as.factor(sex),
    period           = as.factor(period),
    date_of_birth    = as.Date(date_of_birth, format = "%B %d, %Y"),
    date_of_death    = as.Date(date_of_death, format = "%B %d, %Y"),
    country_of_birth = as.factor(country_of_birth)
  )

# Création d'un dossier pour les figures si nécessaire
if (!dir.exists("figures")) dir.create("figures")

################################################################################
# Renommer les variables d'émotion en français
################################################################################
# Variables originales en anglais
pca_vars <- c("anger", "disgust", "fear", "joy", "sadness", 
              "surprise", "trust", "negative", "positive")

# Sélection des colonnes, suppression des valeurs manquantes,
# et renommage des variables en français
df_pca_fr <- df %>%
  select(anticipation, any_of(pca_vars)) %>%
  drop_na() %>%
  rename(
    Colère     = anger,
    Dégout     = disgust,
    Peur       = fear,
    Joie       = joy,
    Tristesse  = sadness,
    Surprise   = surprise,
    Confiance  = trust,
    Négatif    = negative,
    Positif    = positive
  )

################################################################################
# Effectuer l'ACP sur les variables renommées
################################################################################
# Exclure la colonne "anticipation" de l'ACP, mais la garder pour la coloration
res.pca_fr <- PCA(
  df_pca_fr %>% select(-anticipation),  # toutes les colonnes sauf anticipation
  scale.unit = TRUE, 
  graph      = FALSE
)

################################################################################
# Création d'un biplot coloré par la variable anticipation
################################################################################
# Récupération de la variance expliquée (en pourcentage) par chaque dimension
var_expl <- res.pca_fr$eig[,2]  # pourcentage de variance expliquée
dim1_var <- round(var_expl[1], 1)
dim2_var <- round(var_expl[2], 1)

# Biplot de l'ACP
pca_biplot_anticipation <- fviz_pca_biplot(
  res.pca_fr,
  label    = "var",                              # Étiquettes seulement pour les variables (flèches)
  col.ind  = as.factor(df_pca_fr$anticipation),  # Couleur des points selon anticipation
  palette  = c("black", "green", "orange", "red"), 
  pointsize = 2,
  alpha.ind = 0.3,
  repel    = TRUE                                # Évite le chevauchement du texte
) +
  theme_minimal() +
  labs(
    title = "Biplot ACP coloré par l'anticipation",
    color = "Anticipation",
    x     = paste0("Dimension 1 (", dim1_var, "% de variance expliquée)"),
    y     = paste0("Dimension 2 (", dim2_var, "% de variance expliquée)")
  )

# Sauvegarde du biplot
if (!dir.exists("figures_PCA")) dir.create("figures_PCA")
ggsave(
  filename = "figures_PCA/pca_biplot_anticipation.png",
  plot     = pca_biplot_anticipation,
  width    = 6,
  height   = 4,
  dpi      = 300
)

################################################################################
# Graphique d'éboulis : visualisation de la variance expliquée
################################################################################
pca_scree <- fviz_screeplot(res.pca_fr) +
  theme_minimal() +
  labs(
    title = "Graphique d'éboulis : ACP sur les variables émotionnelles",
    x     = "Dimensions de l'ACP",
    y     = "Pourcentage de variance expliquée"
  )

# Sauvegarde du graphique d'éboulis
ggsave(
  filename = "figures_PCA/pca_screeplot.png",
  plot     = pca_scree,
  width    = 6,
  height   = 4,
  dpi      = 300
)
