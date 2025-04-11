########################################
# 1) Load Packages & Prepare Data
########################################
library(dplyr)
library(readr)
library(caret)
library(xgboost)
library(DMwR2)   # For SMOTE (instead of DMwR)
library(pROC)   # For ROC curve

# 1.1 Read and preprocess data
dataset <- read_csv("./clean_data/clean_data_2.csv", show_col_types = FALSE) %>%
  mutate(
    suicidal   = as.factor(suicidal),
    sex        = as.factor(sex),
    poet       = as.factor(poet),
    poem_title = as.factor(poem_title),
    period     = as.factor(period),
    # Binarize emotions
    anger        = ifelse(anger > 0, 1, 0),
    disgust      = ifelse(disgust > 0, 1, 0),
    fear         = ifelse(fear > 0, 1, 0),
    joy          = ifelse(joy > 0, 1, 0),
    sadness      = ifelse(sadness > 0, 1, 0),
    surprise     = ifelse(surprise > 0, 1, 0),
    trust        = ifelse(trust > 0, 1, 0),
    negative     = ifelse(negative > 0, 1, 0),
    positive     = ifelse(positive > 0, 1, 0),
    anticipation = ifelse(anticipation > 0, 1, 0)
  ) %>%
  # Convert to factors
  mutate(
    anger         = as.factor(anger),
    disgust       = as.factor(disgust),
    fear          = as.factor(fear),
    joy           = as.factor(joy),
    sadness       = as.factor(sadness),
    surprise      = as.factor(surprise),
    trust         = as.factor(trust),
    negative      = as.factor(negative),
    positive      = as.factor(positive),
    anticipation  = as.factor(anticipation)
  )

# Rename factor levels for 'anticipation' to "No" / "Yes"
levels(dataset$anticipation) <- c("No", "Yes")

########################################
# 2) Train/Test Split
########################################
set.seed(123)  # For reproducibility
train_index <- createDataPartition(dataset$anticipation, p = 0.8, list = FALSE)
train_data  <- dataset[train_index, ]
test_data   <- dataset[-train_index, ]

########################################
# 3) Train Control (with SMOTE)
########################################
# Note: 'sampling = "smote"' uses DMwR2::SMOTE internally
train_control <- trainControl(
  method          = "cv", 
  number          = 5,
  sampling        = "smote",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary
)

########################################
# 4) Hyperparameter Grid for XGBoost
########################################
xgb_grid <- expand.grid(
  nrounds          = c(100, 150),
  max_depth        = c(2, 4),
  eta              = c(0.3, 0.1),
  gamma            = c(0, 1),
  colsample_bytree = c(0.8, 1),
  min_child_weight = c(1, 2),
  subsample        = c(0.8, 1)
)

########################################
# 5) Train XGBoost Model
########################################
colnames(dataset)

set.seed(123)
xgb_model <- train(
  anticipation ~ suicidal + joy + surprise + trust + fear + sadness + negative + positive + anger + disgust,
  data       = train_data,
  method     = "xgbTree",
  trControl  = train_control,
  tuneGrid   = xgb_grid,
  metric     = "ROC"
)

# View model results
print(xgb_model)

# Variable importance
varImp(xgb_model)

########################################
# 6) Evaluate on Test Data
########################################
# 6.1 Predict class labels
test_preds <- predict(xgb_model, newdata = test_data)

# 6.2 Confusion Matrix
conf_mat <- confusionMatrix(
  data      = test_preds,
  reference = test_data$anticipation,
  positive  = "Yes"
)
print(conf_mat)

# 6.3 ROC Curve Visualization
# Obtain predicted probabilities for "Yes"
test_probs <- predict(xgb_model, newdata = test_data, type = "prob")[, "Yes"]

# Create ROC object
roc_obj <- roc(
  response  = test_data$anticipation,
  predictor = test_probs,
  levels    = c("No", "Yes")
)

# Print AUC
cat("AUC:", auc(roc_obj), "\n")

# Plot the ROC curve
plot(
  roc_obj,
  col     = "blue",
  main    = "ROC Curve - Anticipation",
  legacy.axes = TRUE,
  xlab    = "False Positive Rate",
  ylab    = "True Positive Rate"
)
abline(a = 0, b = 1, lty = 2, col = "gray")
