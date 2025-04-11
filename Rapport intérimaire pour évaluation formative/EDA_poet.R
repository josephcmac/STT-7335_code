library(tidyverse)

#-----------------------------------------------------
# 1. Fonction de lecture et de préparation des données
#-----------------------------------------------------
readData <- function(filename = "clean_data_2.csv") {
  # Lit un fichier CSV et convertit les types de colonnes
  df <- readr::read_csv(filename, show_col_types = FALSE) %>%
    mutate(
      poet         = factor(poet),
      poem_title   = factor(poem_title),
      sex          = factor(sex),
      suicidal     = as.logical(suicidal),
      heterosexual = as.logical(heterosexual)
    )
  return(df)
}

#-----------------------------------------------------
# 2. Lecture des données depuis le fichier
#-----------------------------------------------------
df_raw <- readData("clean_data_2.csv")

#-----------------------------------------------------
# 3. Agrégation par poet pour obtenir une table résumée
#-----------------------------------------------------
df_summary <- df_raw %>%
  group_by(poet) %>%
  summarise(
    sex          = first(sex),
    suicidal     = first(suicidal),
    heterosexual = first(heterosexual),
    positive     = mean(positive, na.rm = TRUE)
  ) %>%
  ungroup()

# Création du dossier "images" s'il n'existe pas déjà
if (!dir.exists("images")) {
  dir.create("images")
}

#-----------------------------------------------------
# 4. Visualisation initiale
#-----------------------------------------------------
plot1 <- df_summary %>%
  filter(!is.na(heterosexual)) %>%
  ggplot(aes(x = heterosexual, y = positive)) +
  geom_boxplot() +
  geom_hline(
    yintercept = df_summary %>%
      filter(is.na(heterosexual)) %>%
      pull(positive),
    linetype = "dashed",
    color = "gray"
  ) +
  theme_minimal() +
  labs(
    title = "Box plot du score d'émotion positive selon l'hétérosexualité",
    subtitle = "Valeur manquante indiquée par une ligne pointillée",
    x = "Hétérosexuel",
    y = "Score d’émotions positives"
  )

# Affichage à l'écran
plot1

# Sauvegarde dans le dossier images
ggsave(
  filename = "images/boxplot-positive-heterosexual-missing-values.png",
  plot     = plot1,
  width    = 8,    # Ajuster les dimensions selon vos besoins
  height   = 6,
  dpi      = 300
)

#-----------------------------------------------------
# 5. Modèle de régression logistique pour valeurs manquantes
#-----------------------------------------------------
model_missing_data <- glm(
  formula = heterosexual ~ positive,
  data    = df_summary %>% filter(!is.na(heterosexual)),
  family  = binomial(link = "logit")
)

#-----------------------------------------------------
# 6. Imputation des valeurs manquantes de heterosexual
#-----------------------------------------------------
df_imputed <- df_summary %>%
  mutate(
    heterosexual = if_else(
      is.na(heterosexual),
      predict(model_missing_data,
              newdata = df_summary %>% filter(is.na(heterosexual)),
              type = "response") > 0.5,
      heterosexual
    )
  )
df_summary
#-----------------------------------------------------
# 7. Sélection, renommage et ajout de labels
#-----------------------------------------------------
df_final <- df_imputed %>%
  select(-positive) %>%
  mutate(
    Sexe         = if_else(sex == "Male", "Homme", "Femme"),
    Suicidaire   = if_else(suicidal == TRUE, "Oui", "Non"),
    Hétérosexuel = if_else(heterosexual == TRUE, "Oui", "Non")
  )
df_final
#-----------------------------------------------------
# 8. Tables de fréquences
#-----------------------------------------------------
table_sexe_suicidaire <- table(df_final$Sexe, df_final$Suicidaire)
table_sexe_suicidaire

table_hetero_suicidaire <- table(df_final$Hétérosexuel, df_final$Suicidaire)
table_hetero_suicidaire

# Pour les hommes
mean_suicidal_male <- df_final %>%
  filter(sex == "Male") %>%
  summarise(moyenne_suicidal = mean(suicidal)) %>%
  pull(moyenne_suicidal)
mean_suicidal_male

# Pour les femmes
mean_suicidal_female <- df_final %>%
  filter(sex == "Female") %>%
  summarise(moyenne_suicidal = mean(suicidal)) %>%
  pull(moyenne_suicidal)
mean_suicidal_female

# Pour les hétérosexuels
mean_suicidal_hetero <- df_final %>%
  filter(heterosexual) %>%
  summarise(moyenne_suicidal = mean(suicidal)) %>%
  pull(moyenne_suicidal)
mean_suicidal_hetero

# Pour les non-hétérosexuels
mean_suicidal_non_hetero <- df_final %>%
  filter(!heterosexual) %>%
  summarise(moyenne_suicidal = mean(suicidal)) %>%
  pull(moyenne_suicidal)
mean_suicidal_non_hetero

