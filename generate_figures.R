################################################################################
# SCRIPT R : Génération de figures (histogrammes, matrices de corrélation, etc.)
#            avec noms de variables en français
################################################################################

library(tidyverse)   # Pour la manipulation de données et ggplot2 (inclus dans tidyverse)
library(readr)       # Pour lire des fichiers CSV
library(ggcorrplot)  # Pour la visualisation des corrélations (Option A)

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
cor_matrix_fr <- cor(df_emotions_fr, use = "pairwise.complete.obs", method="kendall")

################################################################################
# 7. Visualiser et sauvegarder la matrice de corrélation
################################################################################

#===============================================================================
# OPTION A : Utiliser ggcorrplot
#===============================================================================
corr_plot_fr <- ggcorrplot(
  cor_matrix_fr,
  hc.order = TRUE,        # Réordonner la matrice par clustering hiérarchique
  type     = "full",     # Afficher uniquement la partie inférieure du triangle
  lab      = TRUE,        # Afficher les coefficients de corrélation sur la matrice
  lab_size = 3,
  method   = "circle",    # Forme des cases
  legend.title = "Corrélation",
  colors   = c("#6D9EC1", "white", "#E46726"), # Couleurs en dégradé (faible, moyen, fort)
  title    = "Corrélation entre les émotions",
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


################################################################################
# 8. Petit multiple d’histogrammes pour toutes les variables explicatives numériques
#    (en excluant "Anticipation")
################################################################################

# 8a. Identifier les colonnes numériques, sauf Anticipation
numeric_vars <- df_verses_fr %>%
  select(where(is.numeric)) %>%
  select(-Anticipation) %>% 
  names()

numeric_vars <- setdiff(numeric_vars, c("verse", "poet_id", "n_words"))

# 8b. Passer en format "long" pour ggplot
df_numeric_long <- df_verses_fr %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "Variable",
    values_to = "Valeur"
  )

# 8c. Créer le graphique
hist_all_explicatives <- ggplot(df_numeric_long, aes(x = Valeur)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Histogrammes des variables explicatives (émotions)",
    x = "Valeur",
    y = "Effectif"
  )

# 8d. Sauvegarder la figure
ggsave(
  filename = "figures/hist_all_explicatives.png",
  plot     = hist_all_explicatives,
  width    = 9,
  height   = 6,
  dpi      = 300
)

################################################################################
# Code R pour générer des tables de fréquences d'Anticipation (0,1,2,3) 
# par variables catégorielles
################################################################################

library(dplyr)
library(knitr)

# Supposez que df_verses_fr est déjà chargé et contient les colonnes :
#   - Anticipation (numérique)
#   - Sexe, Période, Suicidaire, heterosexual

# 0. Créer le dossier "tables" s'il n'existe pas
if (!dir.exists("tables")) dir.create("tables")

# 1. Filtrer les données pour ne garder que Anticipation ∈ {0,1,2,3}
df_sub <- df_verses_fr %>%
  filter(Anticipation %in% c(0,1,2,3))

################################################################################
# Table par Sexe
################################################################################
table_sexe_counts <- df_sub %>%
  group_by(Sexe) %>%
  summarise(
    `Valeur_0` = sum(Anticipation == 0),
    `Valeur_1` = sum(Anticipation == 1),
    `Valeur_2` = sum(Anticipation == 2),
    `Valeur_3` = sum(Anticipation == 3)
  )

kable_sexe_counts <- kable(
  table_sexe_counts,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Nombre de vers ayant Anticipation = 0,1,2,3 selon le Sexe."
)

cat(kable_sexe_counts, file = "tables/table_sexe_counts.tex")


################################################################################
# Table par Période
################################################################################
table_periode_counts <- df_sub %>%
  group_by(Période) %>%
  summarise(
    `Valeur_0` = sum(Anticipation == 0),
    `Valeur_1` = sum(Anticipation == 1),
    `Valeur_2` = sum(Anticipation == 2),
    `Valeur_3` = sum(Anticipation == 3)
  )

kable_periode_counts <- kable(
  table_periode_counts,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Nombre de vers ayant Anticipation = 0,1,2,3 selon la Période."
)

cat(kable_periode_counts, file = "tables/table_periode_counts.tex")


################################################################################
# Table par Suicidaire
################################################################################
table_suicidaire_counts <- df_sub %>%
  group_by(Suicidaire) %>%
  summarise(
    `Valeur_0` = sum(Anticipation == 0),
    `Valeur_1` = sum(Anticipation == 1),
    `Valeur_2` = sum(Anticipation == 2),
    `Valeur_3` = sum(Anticipation == 3)
  )

kable_suicidaire_counts <- kable(
  table_suicidaire_counts,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Nombre de vers ayant Anticipation = 0,1,2,3 selon l'indicateur Suicidaire."
)

cat(kable_suicidaire_counts, file = "tables/table_suicidaire_counts.tex")


################################################################################
# Table par heterosexual
################################################################################
table_heterosexual_counts <- df_sub %>%
  group_by(heterosexual) %>%
  summarise(
    `Valeur_0` = sum(Anticipation == 0),
    `Valeur_1` = sum(Anticipation == 1),
    `Valeur_2` = sum(Anticipation == 2),
    `Valeur_3` = sum(Anticipation == 3)
  )

kable_heterosexual_counts <- kable(
  table_heterosexual_counts,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Nombre de vers ayant Anticipation = 0,1,2,3 selon l'orientation sexuelle."
)

cat(kable_heterosexual_counts, file = "tables/table_heterosexual_counts.tex")
