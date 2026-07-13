

Pasted text(8).txt
Document
Organise this code please
Its a bit messy

Worked for 52s
I cleaned and reorganised your original script 

.

COMM3501_organised_model_comparison.R

Key improvements:
  
  Removed duplicated predictions, evaluation functions and comparison tables.

Placed all four models in one workflow: Logistic Regression, Decision Tree, Random Forest and XGBoost.

Added reusable helper functions for binary conversion and probability classification.

Corrected logistic regression so its output represents the probability of NEOS = Yes.

Created one final model_comparison table.

Kept separate confusion matrices and variable-importance outputs.

Removed Recall because it is identical to Sensitivity.

Added clearly numbered sections and consistent object names.

I could not test the complete script against A3_Dataset_2023.xlsx because the Excel dataset was not attached, but the existing variables and modelling choices were preserved. 


Library
/
  COMM3501_organised_model_comparison.R


# ============================================================
# COMM3501 - NEOS Classification Model Comparison
# Logistic Regression, Decision Tree, Random Forest and XGBoost
# ============================================================


# ------------------------------------------------------------
# 1. Libraries and settings
# ------------------------------------------------------------
library(readxl)
library(tidyverse)
library(janitor)
library(stringr)
library(forcats)
library(caret)
library(pROC)
library(rpart)
library(randomForest)
library(xgboost)

SEED <- 123
TRAIN_PROPORTION <- 0.70
CLASSIFICATION_THRESHOLD <- 0.50

set.seed(SEED)


# ------------------------------------------------------------
# 2. Import data
# ------------------------------------------------------------
# Select A3_Dataset_2023.xlsx when the file window opens.
data_raw <- read_excel(file.choose())


# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

# Convert Yes/No variables to 1/0.
yes_no_to_binary <- function(x) {
  case_when(
    x == "Yes" ~ 1,
    x == "No"  ~ 0,
    TRUE       ~ NA_real_
  )
}

# Convert predicted probabilities into Yes/No classifications.
probability_to_class <- function(
    probability,
    threshold = CLASSIFICATION_THRESHOLD
) {
  factor(
    if_else(probability >= threshold, "Yes", "No"),
    levels = c("Yes", "No")
  )
}


# ------------------------------------------------------------
# 4. Clean and prepare data
# ------------------------------------------------------------
data_clean <- data_raw %>%
  clean_names() %>%
  
  # Clean character variables.
  mutate(
    across(
      where(is.character),
      ~ str_squish(.x)
    ),
    across(
      where(is.character),
      ~ na_if(.x, "")
    ),
    across(
      where(is.character),
      ~ na_if(.x, "[Blank]")
    )
  ) %>%
  
  # Remove identifier variables that should not be predictors.
  select(
    -any_of(
      c(
        "recommendation_id",
        "request_id",
        "life_id",
        "external_ref"
      )
    )
  ) %>%
  
  # Create the target variable.
  mutate(
    neos_flag = if_else(
      underwriter == "NEOS Life",
      "Yes",
      "No"
    ),
    neos_flag = factor(
      neos_flag,
      levels = c("Yes", "No")
    )
  ) %>%
  
  # Convert product indicators to binary variables.
  mutate(
    life_bin   = yes_no_to_binary(life),
    tpd_bin    = yes_no_to_binary(tpd),
    trauma_bin = yes_no_to_binary(trauma),
    ip_bin     = yes_no_to_binary(ip)
  ) %>%
  
  # Replace invalid negative values with missing values.
  mutate(
    annual_income = if_else(
      annual_income < 0,
      NA_real_,
      annual_income
    ),
    annualised_premium = if_else(
      annualised_premium < 0,
      NA_real_,
      annualised_premium
    )
  ) %>%
  
  # Engineer additional predictors.
  mutate(
    has_alternative = if_else(
      is.na(alternative),
      0,
      1
    ),
    
    total_cover = rowSums(
      pick(
        life_cover_amount,
        tpd_cover_amount,
        trauma_cover_amount,
        ip_cover_amount
      ),
      na.rm = TRUE
    ),
    
    log_annual_income = log1p(annual_income),
    log_annualised_premium = log1p(annualised_premium),
    log_total_cover = log1p(total_cover),
    
    occupation_group = fct_lump_n(
      as.factor(occupation),
      n = 30,
      other_level = "Other"
    ),
    
    commission_type = case_when(
      is.na(commission_structure) ~ "Unknown",
      
      str_detect(
        str_to_lower(commission_structure),
        "nil|nill|no commission"
      ) ~ "No commission",
      
      str_detect(
        str_to_lower(commission_structure),
        "level"
      ) ~ "Level",
      
      str_detect(
        str_to_lower(commission_structure),
        "hybrid"
      ) ~ "Hybrid",
      
      str_detect(
        str_to_lower(commission_structure),
        "upfront|higher initial"
      ) ~ "Upfront / Higher initial",
      
      TRUE ~ "Other specified"
    )
  ) %>%
  
  # Convert categorical predictors to factors.
  mutate(
    across(
      any_of(
        c(
          "gender",
          "smoker_status",
          "home_state",
          "occupation_group",
          "self_employed",
          "super",
          "rollover_tax_rebate",
          "premium_frequency",
          "commission_type"
        )
      ),
      as.factor
    ),
    has_alternative = factor(
      has_alternative,
      levels = c(0, 1)
    )
  )


# ------------------------------------------------------------
# 5. Select variables used by every model
# ------------------------------------------------------------
model_data <- data_clean %>%
  select(
    neos_flag,
    
    # Customer characteristics
    age_next,
    gender,
    smoker_status,
    home_state,
    occupation_group,
    self_employed,
    log_annual_income,
    
    # Product characteristics
    super,
    rollover_tax_rebate,
    life_bin,
    tpd_bin,
    trauma_bin,
    ip_bin,
    log_annualised_premium,
    log_total_cover,
    premium_frequency,
    
    # Adviser and recommendation characteristics
    commission_type,
    has_alternative
  ) %>%
  drop_na()


# Check the number of observations and class balance.
dim(model_data)
table(model_data$neos_flag)
prop.table(table(model_data$neos_flag))


# ------------------------------------------------------------
# 6. Split into training and test data
# ------------------------------------------------------------
set.seed(SEED)

train_index <- createDataPartition(
  y = model_data$neos_flag,
  p = TRAIN_PROPORTION,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]


# ------------------------------------------------------------
# 7. Logistic Regression
# ------------------------------------------------------------
# A separate numeric response ensures that the model predicts
# the probability of "Yes" rather than the probability of "No".

logistic_train_data <- train_data %>%
  mutate(
    neos_numeric = if_else(neos_flag == "Yes", 1, 0)
  ) %>%
  select(-neos_flag)

logistic_test_x <- test_data %>%
  select(-neos_flag)

logistic_regression_model <- glm(
  neos_numeric ~ .,
  data = logistic_train_data,
  family = binomial(link = "logit")
)

logistic_regression_probability <- predict(
  logistic_regression_model,
  newdata = logistic_test_x,
  type = "response"
)

logistic_regression_prediction <- probability_to_class(
  logistic_regression_probability
)


# ------------------------------------------------------------
# 8. Decision Tree
# ------------------------------------------------------------
set.seed(SEED)

decision_tree_model <- rpart(
  neos_flag ~ .,
  data = train_data,
  method = "class",
  parms = list(
    prior = c(
      Yes = 0.50,
      No  = 0.50
    ),
    split = "gini"
  ),
  control = rpart.control(
    cp = 0.0005,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 10,
    xval = 10
  )
)

decision_tree_probability <- predict(
  decision_tree_model,
  newdata = test_data,
  type = "prob"
)[, "Yes"]

decision_tree_prediction <- probability_to_class(
  decision_tree_probability
)


# ------------------------------------------------------------
# 9. Random Forest
# ------------------------------------------------------------
set.seed(SEED)

random_forest_model <- randomForest(
  neos_flag ~ .,
  data = train_data,
  ntree = 300,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

random_forest_probability <- predict(
  random_forest_model,
  newdata = test_data,
  type = "prob"
)[, "Yes"]

random_forest_prediction <- probability_to_class(
  random_forest_probability
)


# ------------------------------------------------------------
# 10. XGBoost data preparation
# ------------------------------------------------------------
# XGBoost requires numeric predictors, so categorical variables
# are converted into dummy variables.

xgb_dummy_model <- dummyVars(
  neos_flag ~ .,
  data = train_data,
  fullRank = FALSE
)

xgb_train_x <- predict(
  xgb_dummy_model,
  newdata = train_data
) %>%
  as.matrix()

xgb_test_x <- predict(
  xgb_dummy_model,
  newdata = test_data
) %>%
  as.matrix()

xgb_train_y <- if_else(
  train_data$neos_flag == "Yes",
  1,
  0
)


# ------------------------------------------------------------
# 11. XGBoost
# ------------------------------------------------------------
set.seed(SEED)

xgboost_model <- xgboost(
  data = xgb_train_x,
  label = xgb_train_y,
  objective = "binary:logistic",
  eval_metric = "logloss",
  nrounds = 150,
  max_depth = 5,
  eta = 0.10,
  subsample = 0.80,
  colsample_bytree = 0.80,
  verbose = 0
)

xgboost_probability <- predict(
  xgboost_model,
  newdata = xgb_test_x
)

xgboost_prediction <- probability_to_class(
  xgboost_probability
)


# ------------------------------------------------------------
# 12. Model evaluation function
# ------------------------------------------------------------
evaluate_model <- function(
    model_name,
    actual,
    predicted,
    probability
) {
  
  actual <- factor(
    actual,
    levels = c("Yes", "No")
  )
  
  predicted <- factor(
    predicted,
    levels = c("Yes", "No")
  )
  
  confusion <- confusionMatrix(
    data = predicted,
    reference = actual,
    positive = "Yes"
  )
  
  roc_result <- roc(
    response = actual,
    predictor = probability,
    levels = c("No", "Yes"),
    direction = "<",
    quiet = TRUE
  )
  
  tibble(
    Model = model_name,
    Accuracy = as.numeric(
      confusion$overall["Accuracy"]
    ),
    Sensitivity = as.numeric(
      confusion$byClass["Sensitivity"]
    ),
    Specificity = as.numeric(
      confusion$byClass["Specificity"]
    ),
    Precision = as.numeric(
      confusion$byClass["Pos Pred Value"]
    ),
    F1_Score = as.numeric(
      confusion$byClass["F1"]
    ),
    Balanced_Accuracy = as.numeric(
      confusion$byClass["Balanced Accuracy"]
    ),
    AUC = as.numeric(
      auc(roc_result)
    )
  )
}


# ------------------------------------------------------------
# 13. Compare all four models
# ------------------------------------------------------------
model_comparison <- bind_rows(
  evaluate_model(
    model_name = "Logistic Regression",
    actual = test_data$neos_flag,
    predicted = logistic_regression_prediction,
    probability = logistic_regression_probability
  ),
  
  evaluate_model(
    model_name = "Decision Tree",
    actual = test_data$neos_flag,
    predicted = decision_tree_prediction,
    probability = decision_tree_probability
  ),
  
  evaluate_model(
    model_name = "Random Forest",
    actual = test_data$neos_flag,
    predicted = random_forest_prediction,
    probability = random_forest_probability
  ),
  
  evaluate_model(
    model_name = "XGBoost",
    actual = test_data$neos_flag,
    predicted = xgboost_prediction,
    probability = xgboost_probability
  )
) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  ) %>%
  arrange(desc(AUC))


# Display the final comparison table.
model_comparison
View(model_comparison)


# ------------------------------------------------------------
# 14. Individual confusion matrices
# ------------------------------------------------------------
logistic_confusion_matrix <- confusionMatrix(
  data = logistic_regression_prediction,
  reference = test_data$neos_flag,
  positive = "Yes"
)

decision_tree_confusion_matrix <- confusionMatrix(
  data = decision_tree_prediction,
  reference = test_data$neos_flag,
  positive = "Yes"
)

random_forest_confusion_matrix <- confusionMatrix(
  data = random_forest_prediction,
  reference = test_data$neos_flag,
  positive = "Yes"
)

xgboost_confusion_matrix <- confusionMatrix(
  data = xgboost_prediction,
  reference = test_data$neos_flag,
  positive = "Yes"
)

logistic_confusion_matrix
decision_tree_confusion_matrix
random_forest_confusion_matrix
xgboost_confusion_matrix


# ------------------------------------------------------------
# 15. Model interpretation and variable importance
# ------------------------------------------------------------

# Logistic Regression coefficients
summary(logistic_regression_model)

# Decision Tree variable importance
decision_tree_model$variable.importance

# Random Forest variable importance
importance(random_forest_model)

# XGBoost variable importance
xgboost_importance <- xgb.importance(
  model = xgboost_model
)

xgboost_importance
