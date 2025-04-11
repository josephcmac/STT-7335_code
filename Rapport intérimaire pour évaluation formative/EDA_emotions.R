library(tidyverse)
library(lme4)
library(lmerTest)

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
bootstrapping <- function(df, n_boot) {
  df_boot <- df[sample(1:nrow(df), size = nrow(df), replace = TRUE),]
  
  s_plus <- df_boot %>%
    filter(suicidal) %>%
    select(anger, anticipation, disgust, fear, joy, 
           sadness, surprise, trust, negative, positive) %>%
    colMeans()
  
  s_minus <- df_boot %>%
    filter(!suicidal) %>%
    select(anger, anticipation, disgust, fear, joy, 
           sadness, surprise, trust, negative, positive) %>%
    colMeans()
  
  for (i in 2:n_boot) {
    df_boot <- df[sample(1:nrow(df), size = nrow(df), replace = TRUE),]
    
    s_plus <- rbind(
      s_plus,
      df_boot %>%
        filter(suicidal) %>%
        select(anger, anticipation, disgust, fear, joy, 
               sadness, surprise, trust, negative, positive) %>%
        colMeans()
    )
    
    s_minus <- rbind(
      s_minus,
      df_boot %>%
        filter(!suicidal) %>%
        select(anger, anticipation, disgust, fear, joy, 
               sadness, surprise, trust, negative, positive) %>%
        colMeans()
    )
  }
  
  s <- rbind(s_plus, s_minus)
  row.names(s) <- 1:nrow(s)
  s <- as.data.frame(s)
  s$suicidal <- as.logical(c(rep(TRUE, nrow(s_plus)), rep(FALSE, nrow(s_minus))))
  return(s)
}

#-----------------------------------------------------
# Lecture du fichier et préparation
#-----------------------------------------------------
df <- readData()

#-----------------------------------------------------
# Mise en format long (émotions) pour le 1er graphique
#-----------------------------------------------------
df_long <- df %>%
  pivot_longer(
    cols = c(anger, anticipation, disgust, fear, joy,
             sadness, surprise, trust, negative, positive),
    names_to = "emotion_en",
    values_to = "valeur"
  ) %>%
  mutate(
    emotion_fr = case_when(
      emotion_en == "anger"        ~ "Colère",
      emotion_en == "anticipation" ~ "Anticipation",
      emotion_en == "disgust"      ~ "Dégoût",
      emotion_en == "fear"         ~ "Peur",
      emotion_en == "joy"          ~ "Joie",
      emotion_en == "sadness"      ~ "Tristesse",
      emotion_en == "surprise"     ~ "Surprise",
      emotion_en == "trust"        ~ "Confiance",
      emotion_en == "negative"     ~ "Négatif",
      emotion_en == "positive"     ~ "Positif",
      TRUE ~ emotion_en
    )
  )

#-----------------------------------------------------
# 1er Graphique : distribution des émotions (données originales)
#-----------------------------------------------------
plot1 <- ggplot(
  data = df_long,
  aes(
    x    = factor(suicidal, labels = c("Non suicidaire", "Suicidaire")),
    y    = valeur,
    fill = factor(suicidal)
  )
) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  # --------- Changement principal : ncol = 2 ---------
facet_wrap(~ emotion_fr, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c("#00AFBB", "#FC4E07"),
    name   = "Groupe",
    labels = c("Non suicidaire", "Suicidaire")
  ) +
  labs(
    title    = "Distribution des émotions selon le caractère suicidaire",
    subtitle = "Données Originales",
    x        = "Groupe",
    y        = "Valeur de l'émotion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

# Affichage du graphique
print(plot1)

# --------- Format plus vertical (width < height) ---------
ggsave(
  filename = "images/boxplot-emotions-suicidal-raw.png",
  plot     = plot1,
  width    = 7,
  height   = 10,
  dpi      = 300
)

#-----------------------------------------------------
# Bootstrap et préparation du 2e graphique
#-----------------------------------------------------
df_boot <- bootstrapping(df, n_boot = 10000)

# Optionnel : libérer la mémoire
rm(df)

df_long_boot <- df_boot %>%
  pivot_longer(
    cols = c(anger, anticipation, disgust, fear, joy,
             sadness, surprise, trust, negative, positive),
    names_to = "emotion_en",
    values_to = "valeur"
  ) %>%
  mutate(
    emotion_fr = case_when(
      emotion_en == "anger"        ~ "Colère",
      emotion_en == "anticipation" ~ "Anticipation",
      emotion_en == "disgust"      ~ "Dégoût",
      emotion_en == "fear"         ~ "Peur",
      emotion_en == "joy"          ~ "Joie",
      emotion_en == "sadness"      ~ "Tristesse",
      emotion_en == "surprise"     ~ "Surprise",
      emotion_en == "trust"        ~ "Confiance",
      emotion_en == "negative"     ~ "Négatif",
      emotion_en == "positive"     ~ "Positif",
      TRUE ~ emotion_en
    )
  )

#-----------------------------------------------------
# 2e Graphique : distribution des émotions (bootstrap)
#-----------------------------------------------------
plot2 <- ggplot(
  data = df_long_boot,
  aes(
    x    = factor(suicidal, labels = c("Non suicidaire", "Suicidaire")),
    y    = valeur,
    fill = factor(suicidal)
  )
) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  # --------- Changement principal : ncol = 2 ---------
facet_wrap(~ emotion_fr, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c("#00AFBB", "#FC4E07"),
    name   = "Groupe",
    labels = c("Non suicidaire", "Suicidaire")
  ) +
  labs(
    title    = "Distribution des émotions selon le caractère suicidaire",
    subtitle = "Moyenne Bootstrap (10 000 rééchantillonnages)",
    x        = "Groupe",
    y        = "Valeur de l'émotion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

# Affichage du graphique
print(plot2)

# --------- Format plus vertical (width < height) ---------
ggsave(
  filename = "images/boxplot-emotions-suicidal-bootstrap-10000.png",
  plot     = plot2,
  width    = 7,
  height   = 10,
  dpi      = 300
)

