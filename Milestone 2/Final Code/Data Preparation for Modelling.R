#=========================================================
# 1. DATA CLEANING (BEFORE SPLIT)
#=========================================================

library(dplyr)

insurance_clean <- A3_Dataset_2023 %>%
  
  # Remove identifiers (no predictive value)
  select(
    -RecommendationId,
    -RequestId,
    -LifeId,
    -ExternalRef
  ) %>%
  
  # Feature engineering
  mutate(
    HasAlternative = ifelse(is.na(Alternative), 0, 1)
  ) %>%
  select(-Alternative) %>%
  
  # Convert categorical variables to factors
  mutate(
    across(
      c(
        Underwriter, Package, Gender, SmokerStatus,
        HomeState, PremiumFrequency, Super,
        RolloverTaxRebate, Life, TPD, Trauma,
        IP, BE, Severity, SelfEmployed,
        Occupation, CommissionStructure
      ),
      as.factor
    )
  )

#=========================================================
# 2. TRAIN / TEST SPLIT
#=========================================================

set.seed(123)

train_index <- sample(nrow(insurance_clean), 0.8 * nrow(insurance_clean))

train_data <- insurance_clean[train_index, ]
test_data  <- insurance_clean[-train_index, ]

#=========================================================
# 3. MISSING VALUE IMPUTATION (TRAIN-BASED)
#=========================================================

#-----------------------------
# Numeric variables (median imputation)
#-----------------------------

num_cols <- sapply(train_data, is.numeric)

for (col in names(train_data)[num_cols]) {
  median_val <- median(train_data[[col]], na.rm = TRUE)
  
  train_data[[col]][is.na(train_data[[col]])] <- median_val
  test_data[[col]][is.na(test_data[[col]])] <- median_val
}

#-----------------------------
# Categorical variables (mode imputation)
#-----------------------------

mode_impute <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  mode_val <- names(sort(table(x), decreasing = TRUE))[1]
  return(mode_val)
}

cat_cols <- sapply(train_data, is.factor)

for (col in names(train_data)[cat_cols]) {
  
  # compute mode from TRAIN only
  mode_val <- mode_impute(train_data[[col]])
  
  # impute TRAIN
  train_data[[col]] <- as.character(train_data[[col]])
  train_data[[col]][is.na(train_data[[col]])] <- mode_val
  
  # impute TEST
  test_data[[col]] <- as.character(test_data[[col]])
  test_data[[col]][is.na(test_data[[col]])] <- mode_val
  
  # convert back to factor
  train_data[[col]] <- as.factor(train_data[[col]])
  test_data[[col]] <- as.factor(test_data[[col]])
}

#=========================================================
# 4. REMOVE LEAKAGE VARIABLES (FINAL CHECK)
#=========================================================

train_data <- train_data %>% select(-AdviserID)
test_data  <- test_data %>% select(-AdviserID)

#=========================================================
# 5. FACTOR CONSISTENCY
#=========================================================

train_data <- droplevels(train_data)
test_data  <- droplevels(test_data)

#=========================================================
# 6. OPTIONAL FEATURE ENGINEERING
#=========================================================

train_data <- train_data %>%
  mutate(
    log_Premium = log1p(Premium),
    log_AnnualIncome = log1p(AnnualIncome)
  )

test_data <- test_data %>%
  mutate(
    log_Premium = log1p(Premium),
    log_AnnualIncome = log1p(AnnualIncome)
  )

#=========================================================
# 7. FINAL CHECK (MUST BE ALL ZEROS)
#=========================================================

colSums(is.na(train_data))
colSums(is.na(test_data))

str(train_data)
str(test_data)