################################################################################
# SCRIPT R : Générer des tableaux récapitulatifs (variables en français)
################################################################################

# 1. Charger les bibliothèques nécessaires
library(tidyverse)   # Pour la manipulation de données (inclut tidyr, dplyr)
library(readr)       # Pour lire des fichiers CSV
library(knitr)       # Pour la création de tableaux (kable)
library(kableExtra)  # Pour enrichir la mise en forme des tableaux

################################################################################
# 2. Lecture des données
#    Ajustez le chemin si nécessaire, ou placez ce script dans le même dossier
#    où se trouve 'clean_data_2.csv'.
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
    "Suicidaire"         = suicidal,
    "Sexe"               = sex,
    "Période"            = period,
    "Date de naissance"  = date_of_birth,
    "Date de décès"      = date_of_death,
    "Pays de naissance"  = country_of_birth,
    "Nombre de mots"     = n_words,
    "Colère"             = anger,
    "Anticipation"       = anticipation,
    "Dégoût"             = disgust,
    "Peur"               = fear,
    "Joie"               = joy,
    "Tristesse"          = sadness,
    "Surprise"           = surprise,
    "Confiance"          = trust,
    "Négatif"            = negative,
    "Positif"            = positive,
    "Vers"               = verse
  )

################################################################################
# 4. Créer le répertoire "tables" s'il n'existe pas
################################################################################
if (!dir.exists("tables")) dir.create("tables")

################################################################################
# 5. Générer un résumé des variables clés et sauvegarder sous forme de fichiers texte
################################################################################
# Vous pouvez adapter la liste ci-dessous en fonction des variables d'intérêt
vars_of_interest <- c(
  "Sexe", "Période", "Nombre de mots",
  "Colère", "Anticipation", "Dégoût", "Peur",
  "Joie", "Tristesse", "Surprise", "Confiance",
  "Négatif", "Positif", "Vers"
)

# Résumé numérique
df_summary_numeric_fr <- df_verses_fr %>%
  select(any_of(vars_of_interest)) %>%
  select(where(is.numeric)) %>%
  summary()

# Résumé pour les variables qualitatives (facteurs)
df_summary_factor_fr <- df_verses_fr %>%
  select(any_of(vars_of_interest)) %>%
  select(where(is.factor)) %>%
  summary()

# Écrire les résumés dans des fichiers texte
sink("tables/summary_numeric.txt")
cat("=== Résumé des variables numériques ===\n")
print(df_summary_numeric_fr)
sink()

sink("tables/summary_factor.txt")
cat("=== Résumé des variables qualitatives ===\n")
print(df_summary_factor_fr)
sink()

################################################################################
# 6. Créer un tableau récapitulatif personnalisé pour les variables numériques (en LaTeX)
################################################################################
numeric_vars_fr <- df_verses_fr %>%
  select(any_of(vars_of_interest)) %>%
  select(where(is.numeric))

# Passer du format "large" au format "long" avec gather() / pivot_longer()
summary_table_fr <- numeric_vars_fr %>%
  gather("Variable", "Valeur") %>%
  group_by(Variable) %>%
  summarise(
    `Min`           = min(Valeur, na.rm = TRUE),
    `1er quartile`  = quantile(Valeur, probs = 0.25, na.rm = TRUE),
    `Médiane`       = median(Valeur, na.rm = TRUE),
    `Moyenne`       = mean(Valeur, na.rm = TRUE),
    `3e quartile`   = quantile(Valeur, probs = 0.75, na.rm = TRUE),
    `Max`           = max(Valeur, na.rm = TRUE)
  ) %>%
  arrange(Variable)

summary_table_latex_fr <- kable(
  summary_table_fr,
  format   = "latex",
  booktabs = TRUE,
  digits   = 2,  # Arrondir à 2 décimales
  caption  = "Variables numériques : statistiques récapitulatives",
  label    = "tab:summaryStatsFR"
) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

# Exporter le tableau en LaTeX
cat(summary_table_latex_fr, file = "tables/summary_table.tex")

message("Les tableaux ont été générés dans le dossier 'tables' avec des noms de variables.")
