# -------------------------------------------------------------------------
# compare_models.R
# -------------------------------------------------------------------------
# Ce script lit les données, entraîne trois modèles distincts (XGBoost, 
# régression logistique, modèle gaussien) et compare leurs performances 
# via la matrice de confusion.
# -------------------------------------------------------------------------

library(tidyverse)
library(xgboost)
library(lme4)
library(lmerTest)
library(caret)
library(mvtnorm)

# -------------------------------------------------------------------------
# 1. Lecture et préparation des données
# -------------------------------------------------------------------------
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

# -------------------------------------------------------------------------
# 2. Bootstrapping : on simule n_boot échantillons, et on empile les 
#    moyennes par classe (suicidal ou non).
# -------------------------------------------------------------------------
bootstrapping <- function(df, n_boot) {
  
  # Première itération
  df_boot <- df[sample(1:nrow(df), size = nrow(df), replace = TRUE),]
  
  s_plus <- df_boot %>%
    filter(suicidal) %>%
    select(-suicidal, -poet) %>%
    colMeans()
  
  s_minus <- df_boot %>%
    filter(!suicidal) %>%
    select(-suicidal, -poet) %>%
    colMeans()
  
  # Répétitions
  for (i in seq_len(n_boot - 1)) {
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
  
  # On combine les moyennes suicidaires / non-suicidaires
  s <- rbind(s_plus, s_minus)
  row.names(s) <- NULL  # juste pour clean
  s <- as.data.frame(s)
  s$suicidal <- c(rep(TRUE, nrow(s_plus)), rep(FALSE, nrow(s_minus)))
  
  return(s)
}

# -------------------------------------------------------------------------
# 3. Fonctions internes pour l'entraînement et la prédiction
#    en fonction du modèle choisi.
# -------------------------------------------------------------------------

# 3a. Entraînement XGBoost
train_xgboost <- function(df_boot_train) {
  # Prépare les données d'entraînement
  X_train <- as.matrix(df_boot_train %>% select(-suicidal))
  y_train <- as.numeric(df_boot_train$suicidal)  # TRUE=1, FALSE=0
  
  # Entraîne le modèle XGBoost
  model <- xgboost(
    data = X_train,
    label = y_train,
    objective = "binary:logistic",
    nrounds = 100,
    verbose = 0
  )
  return(model)
}

predict_xgboost <- function(model, new_data) {
  X_test <- as.matrix(new_data)
  pred_prob <- predict(model, X_test)
  return(pred_prob > 0.5)
}

# 3b. Entraînement régression logistique
train_glm <- function(df_boot_train) {
  # On crée une formule simple suicidial ~ toutes les colonnes
  # (sauf suicidal lui-même)
  model <- glm(
    suicidal ~ ., 
    data = df_boot_train,
    family = binomial(link = "logit")  # plus explicite
  )
  return(model)
}

predict_glm <- function(model, new_data) {
  # Retourne une valeur booléenne
  return(predict(model, new_data, type = "response") > 0.5)
}

# 3c. Entraînement modèle gaussien (Naive Bayes généralisé)
train_gaussian <- function(df_boot_train) {
  # On sépare les classes
  df_s_plus <- df_boot_train %>% filter(suicidal)
  df_s_minus <- df_boot_train %>% filter(!suicidal)
  
  # Moyennes et covariances
  mu_pos <- colMeans(df_s_plus %>% select(-suicidal))
  cov_pos <- cov(df_s_plus %>% select(-suicidal))
  
  mu_neg <- colMeans(df_s_minus %>% select(-suicidal))
  cov_neg <- cov(df_s_minus %>% select(-suicidal))
  
  # Priors
  prior_pos <- nrow(df_s_plus) / nrow(df_boot_train)
  prior_neg <- nrow(df_s_minus) / nrow(df_boot_train)
  
  # Pour éviter un éventuel problème de matrice singulière, on peut
  # ajouter une petite valeur sur la diagonale. (Optionnel)
  # cov_pos <- cov_pos + diag(1e-8, ncol(cov_pos))
  # cov_neg <- cov_neg + diag(1e-8, ncol(cov_neg))
  
  # On retourne tous les paramètres dans une liste
  list(
    mu_pos    = mu_pos,
    cov_pos   = cov_pos,
    mu_neg    = mu_neg,
    cov_neg   = cov_neg,
    prior_pos = prior_pos,
    prior_neg = prior_neg
  )
}

predict_gaussian <- function(model_params, new_data) {
  # new_data doit être un data.frame 1x10 
  x <- as.numeric(new_data[1, ])  # vecteur
  
  # log densités
  log_p_x_given_pos <- dmvnorm(x, mean = model_params$mu_pos, 
                               sigma = model_params$cov_pos, log = TRUE)
  log_p_x_given_neg <- dmvnorm(x, mean = model_params$mu_neg, 
                               sigma = model_params$cov_neg, log = TRUE)
  
  # log prior
  log_prior_pos <- log(model_params$prior_pos)
  log_prior_neg <- log(model_params$prior_neg)
  
  # Posterior = exp(log_likelihood + log_prior) / (somme pour toutes les classes)
  num <- exp(log_p_x_given_pos + log_prior_pos)
  den <- num + exp(log_p_x_given_neg + log_prior_neg)
  
  p_suicidal <- num / den
  
  return(p_suicidal > 0.5)
}

# -------------------------------------------------------------------------
# 4. La fonction principale : on entraîne selon le modèle choisi,
#    puis on prédit sur un poète tenu à part (poet_test).
# -------------------------------------------------------------------------
run_model_once <- function(df, n_boot, model_type = c("xgboost","glm","gaussian")) {
  # Choix du poète test
  poet_test <- sample(unique(df$poet), size = 1)
  
  # Split train / test
  df_train <- df %>% filter(poet != poet_test)
  df_test  <- df %>% filter(poet == poet_test)
  
  # Bootstrapping
  df_boot_train <- bootstrapping(df_train, n_boot = n_boot)
  
  # Sélection du modèle
  model_type <- match.arg(model_type)
  
  if (model_type == "xgboost") {
    model <- train_xgboost(df_boot_train)
  } else if (model_type == "glm") {
    model <- train_glm(df_boot_train)
  } else {
    model <- train_gaussian(df_boot_train)
  }
  
  # On construit la ligne de test en prenant la moyenne des colonnes 
  # (comme dans votre exemple)
  new_data <- df_test %>%
    select(-poet, -suicidal) %>%
    colMeans() %>%
    t() %>%
    as.data.frame()
  
  # Prédiction
  if (model_type == "xgboost") {
    pred <- predict_xgboost(model, new_data)
  } else if (model_type == "glm") {
    pred <- predict_glm(model, new_data)
  } else {
    pred <- predict_gaussian(model, new_data)
  }
  
  return(data.frame(
    actual = df_test$suicidal[1], # Tous suicidaux ou pas, supposant qu'ils sont homogènes 
    pred   = pred
  ))
}

# -------------------------------------------------------------------------
# 5. Fonction d'évaluation : répète N fois le run_model_once, puis
#    calcule la matrice de confusion via caret.
# -------------------------------------------------------------------------
evaluate_model <- function(df, model_type, n_sample = 100, n_boot = 500) {
  
  all_preds <- run_model_once(df, n_boot, model_type)
  for (i in seq_len(n_sample - 1)) {
    all_preds <- rbind(all_preds, run_model_once(df, n_boot, model_type))
  }
  row.names(all_preds) <- NULL
  
  # Conversion en facteur
  all_preds$actual <- factor(all_preds$actual, levels = c(FALSE, TRUE))
  all_preds$pred   <- factor(all_preds$pred,   levels = c(FALSE, TRUE))
  
  # On continue à créer la table, puis confusionMatrix
  cm <- table(Actual = all_preds$actual, Predicted = all_preds$pred)
  cm_caret <- caret::confusionMatrix(cm, positive = "TRUE")
  
  return(cm_caret)
}

# -------------------------------------------------------------------------
# 6. Script principal pour comparer les trois modèles
# -------------------------------------------------------------------------
compare_three_models <- function(filename = "clean_data_2.csv", 
                                 n_sample = 10, n_boot = 50) {

  # Lecture des données                                 
  df <- readData(filename = filename) %>%
      select(poet, suicidal, anticipation, trust, anger, disgust, joy, 
      positive, surprise, sadness)
  
  # Évaluation XGBoost
  cat("=== XGBoost ===\n")
  cm_xgb <- evaluate_model(df, model_type = "xgboost", n_sample, n_boot)
  print(cm_xgb)
  
  # Évaluation GLM (régression logistique)
  cat("\n=== Régression Logistique (GLM) ===\n")
  cm_glm <- evaluate_model(df, model_type = "glm", n_sample, n_boot)
  print(cm_glm)
  
  # Évaluation Gaussienne
  cat("\n=== Modèle Gaussien ===\n")
  cm_gaussian <- evaluate_model(df, model_type = "gaussian", n_sample, n_boot)
  print(cm_gaussian)
  
  # Si on veut comparer "vite fait" les Accuracy
  # (ou d'autres métriques) on peut extraire depuis cm_caret :
  cat("\n=== Résumé des Accuracy ===\n")
  cat(sprintf("XGBoost         : %.2f%%\n", cm_xgb$overall["Accuracy"]*100))
  cat(sprintf("GLM (Logistique): %.2f%%\n", cm_glm$overall["Accuracy"]*100))
  cat(sprintf("Gaussien        : %.2f%%\n", cm_gaussian$overall["Accuracy"]*100))
  
  # On peut retourner les trois matrices de confusion pour un usage ultérieur
  list(
    xgboost   = cm_xgb,
    glm       = cm_glm,
    gaussian  = cm_gaussian
  )
}

# -------------------------------------------------------------------------
# 7. Exécution (si vous lancez ce script directement)
# -------------------------------------------------------------------------
set.seed(734)
result <- compare_three_models(filename="clean_data_2.csv", n_sample = 100, n_boot = 1000)
# -------------------------------------------------------------------------
