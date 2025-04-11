#-----------------------------------------------------
# 0. Chargement des librairies
#-----------------------------------------------------
library(tidyverse)
library(mclust)
library(caret)     # Pour confusionMatrix()

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
  
  s <- rbind(s_plus, s_minus)
  row.names(s) <- 1:nrow(s)
  s <- as.data.frame(s)
  s$suicidal <- as.logical(c(rep(TRUE, nrow(s_plus)), rep(FALSE, nrow(s_minus))))
  return(s)
}

#-----------------------------------------------------
# 4. Paramètres
#-----------------------------------------------------
n_boot <- 10000
#-----------------------------------------------------
# 5. Lecture des données
#-----------------------------------------------------
df <- readData("clean_data_2.csv") %>%
  select(
    poet, suicidal, 
    anticipation, trust, anger, disgust, joy, 
    positive, surprise, sadness
  )

poets <- unique(df$poet)

#-----------------------------------------------------
# 6. Validation leave-one-poet-out
#-----------------------------------------------------
predicted <- c()
observed  <- c()

for (poet_test in poets) {
  # Division des données
  df_train <- df %>% filter(poet != poet_test)
  df_test  <- df %>% filter(poet == poet_test)
  
  # Bootstrapping du jeu d'entraînement
  df_train_bs <- bootstrapping(df_train, n_boot = n_boot)
  
  # Sélection et ACP
  df_num <- df_train_bs %>% select(where(is.double))
  
  pca_result <- prcomp(df_num, center = TRUE, scale. = TRUE)
  pca_scores <- as.data.frame(pca_result$x)
  
  # Assemble jeux PCA + variable cible
  df_pca <- df_train_bs %>%
    select(suicidal) %>%
    bind_cols(pca_scores)
  
  X_train <- df_pca %>% select(-suicidal)
  y_train <- df_pca$suicidal
  
  # Ajustement du modèle MclustDA
  model_mda <- MclustDA(X_train, class = y_train)
  
  # Préparation du test (projection PCA)
  df_test_num <- df_test %>%
    select(-poet, -suicidal) %>%
    colMeans() %>%
    t() %>%
    as.data.frame()
  
  df_test_pca <- predict(pca_result, newdata = df_test_num) %>%
    as.data.frame()
  
  # Prédiction
  pred_test <- predict(model_mda, newdata = df_test_pca)
  class_pred <- pred_test$class
  
  predicted <- c(predicted, as.logical(class_pred))
  observed  <- c(observed, dplyr::first(df_test$suicidal))
}

#-----------------------------------------------------
# 7. Récapitulation des résultats
#-----------------------------------------------------
results <- data.frame(
  poet      = as.character(poets),
  predicted = predicted,
  observed  = observed
)

cat("\nRésultats de la validation leave-one-poet-out :\n")
print(results)

#-----------------------------------------------------
# 8. Analyse via confusionMatrix() de caret
#-----------------------------------------------------
# La fonction confusionMatrix() nécessite des facteurs ayant les mêmes niveaux.
# On spécifie explicitement l'ordre des niveaux (FALSE, TRUE) pour être cohérent.
res_conf <- confusionMatrix(
  data      = factor(results$predicted, levels = c("FALSE","TRUE")),
  reference = factor(results$observed,  levels = c("FALSE","TRUE")),
  positive  = "TRUE"
)

cat("\n--- Matrice de confusion (caret) ---\n")
print(res_conf$table)

cat("\n--- Statistiques de performance ---\n")
print(res_conf)  # affiche un résumé complet : accuracy, kappa, sensibilité, spécificité, etc.

