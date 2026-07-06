#=========================================================
# LINEAR REGRESSION MODELS
# Model 1: Predict AnnualisedPremium
# Model 2: Predict log_AnnualisedPremium
#=========================================================

library(dplyr)
library(ggplot2)

#---------------------------------------------------------
# 0. Check cleaned data exists
#---------------------------------------------------------

# Make sure you have already run Data Cleaning and Quality Check.R
# The cleaned dataset should be called insurance_clean

if (!exists("insurance_clean")) {
  stop("insurance_clean does not exist. Please run the data cleaning script first.")
}


#---------------------------------------------------------
# 1. Create regression dataset
#---------------------------------------------------------

regression_data <- insurance_clean %>%
  select(
    AnnualisedPremium,
    AgeNext,
    AnnualIncome,
    Gender,
    SmokerStatus,
    HomeState,
    SelfEmployed,
    Super,
    PremiumFrequency,
    Life,
    TPD,
    Trauma,
    IP,
    HasAlternative
  ) %>%
  filter(
    !is.na(AnnualisedPremium),
    AnnualisedPremium >= 0
  ) %>%
  mutate(
    log_AnnualisedPremium = log1p(AnnualisedPremium)
  )

# Check missing values before modelling
colSums(is.na(regression_data))

# Check dataset size
dim(regression_data)


#---------------------------------------------------------
# 2. Train / test split
#---------------------------------------------------------

set.seed(3501)

train_index <- sample(
  1:nrow(regression_data),
  size = 0.8 * nrow(regression_data)
)

train_reg <- regression_data[train_index, ]
test_reg  <- regression_data[-train_index, ]


#---------------------------------------------------------
# 3. Missing value imputation
# Numeric variables: median from training data
# Categorical variables: mode from training data
#---------------------------------------------------------

# Function for mode imputation
get_mode <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  names(sort(table(x), decreasing = TRUE))[1]
}

# Numeric imputation
num_cols <- names(train_reg)[sapply(train_reg, is.numeric)]

# Do not impute target variables
num_cols <- setdiff(
  num_cols,
  c("AnnualisedPremium", "log_AnnualisedPremium")
)

for (col in num_cols) {
  median_val <- median(train_reg[[col]], na.rm = TRUE)
  
  train_reg[[col]][is.na(train_reg[[col]])] <- median_val
  test_reg[[col]][is.na(test_reg[[col]])] <- median_val
}

# Categorical imputation
cat_cols <- names(train_reg)[sapply(train_reg, is.factor) | sapply(train_reg, is.character)]

for (col in cat_cols) {
  
  mode_val <- get_mode(train_reg[[col]])
  
  train_reg[[col]] <- as.character(train_reg[[col]])
  test_reg[[col]]  <- as.character(test_reg[[col]])
  
  # Use training levels only
  train_levels <- unique(train_reg[[col]][!is.na(train_reg[[col]])])
  train_levels <- unique(c(train_levels, mode_val))
  
  # Replace missing values in train
  train_reg[[col]][is.na(train_reg[[col]])] <- mode_val
  
  # Replace missing and unseen values in test
  test_reg[[col]][
    is.na(test_reg[[col]]) | !(test_reg[[col]] %in% train_levels)
  ] <- mode_val
  
  # Convert back to factor
  train_reg[[col]] <- factor(train_reg[[col]], levels = train_levels)
  test_reg[[col]]  <- factor(test_reg[[col]], levels = train_levels)
}

# Final missing value check
colSums(is.na(train_reg))
colSums(is.na(test_reg))


#=========================================================
# MODEL 1: Linear regression for AnnualisedPremium
#=========================================================

model_annualised <- lm(
  AnnualisedPremium ~ AgeNext + AnnualIncome + Gender + SmokerStatus +
    HomeState + SelfEmployed + Super + PremiumFrequency +
    Life + TPD + Trauma + IP + HasAlternative,
  data = train_reg
)

# View Model 1 result
summary(model_annualised)


#---------------------------------------------------------
# Model 1 prediction and performance
#---------------------------------------------------------

test_reg$pred_annualised <- predict(
  model_annualised,
  newdata = test_reg
)

# Model 1 performance
rmse_annualised <- sqrt(mean((test_reg$AnnualisedPremium - test_reg$pred_annualised)^2))
mae_annualised <- mean(abs(test_reg$AnnualisedPremium - test_reg$pred_annualised))

rmse_annualised
mae_annualised


#=========================================================
# MODEL 2: Linear regression for log_AnnualisedPremium
#=========================================================

model_log_annualised <- lm(
  log_AnnualisedPremium ~ AgeNext + AnnualIncome + Gender + SmokerStatus +
    HomeState + SelfEmployed + Super + PremiumFrequency +
    Life + TPD + Trauma + IP + HasAlternative,
  data = train_reg
)

# View Model 2 result
summary(model_log_annualised)


#---------------------------------------------------------
# Model 2 prediction and performance
#---------------------------------------------------------

# Predict log premium
test_reg$pred_log_annualised <- predict(
  model_log_annualised,
  newdata = test_reg
)

# Convert predicted log premium back to annualised premium scale
test_reg$pred_annualised_from_log <- expm1(test_reg$pred_log_annualised)

# Avoid negative predicted premium values if any
test_reg$pred_annualised_from_log <- pmax(0, test_reg$pred_annualised_from_log)

# Model 2 performance on original AnnualisedPremium scale
rmse_log_model <- sqrt(mean((test_reg$AnnualisedPremium - test_reg$pred_annualised_from_log)^2))
mae_log_model <- mean(abs(test_reg$AnnualisedPremium - test_reg$pred_annualised_from_log))

rmse_log_model
mae_log_model


#=========================================================
# 4. Compare the two models
#=========================================================

model_comparison <- data.frame(
  Model = c(
    "Model 1: AnnualisedPremium",
    "Model 2: log_AnnualisedPremium"
  ),
  RMSE = c(
    rmse_annualised,
    rmse_log_model
  ),
  MAE = c(
    mae_annualised,
    mae_log_model
  ),
  Adjusted_R_Squared = c(
    summary(model_annualised)$adj.r.squared,
    summary(model_log_annualised)$adj.r.squared
  )
)

model_comparison


#=========================================================
# 5. Visualisation: Actual vs Predicted
#=========================================================

# Model 1 actual vs predicted
ggplot(test_reg, aes(x = AnnualisedPremium, y = pred_annualised)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  labs(
    title = "Model 1: Actual vs Predicted Annualised Premium",
    x = "Actual Annualised Premium",
    y = "Predicted Annualised Premium"
  )


# Model 2 actual vs predicted
ggplot(test_reg, aes(x = AnnualisedPremium, y = pred_annualised_from_log)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  labs(
    title = "Model 2: Actual vs Predicted Annualised Premium from Log Model",
    x = "Actual Annualised Premium",
    y = "Predicted Annualised Premium"
  )


#=========================================================
# 6. Extract important coefficient results
#=========================================================

# Coefficients for Model 1
coef_model_1 <- as.data.frame(summary(model_annualised)$coefficients)
coef_model_1$Variable <- rownames(coef_model_1)
rownames(coef_model_1) <- NULL

coef_model_1 <- coef_model_1 %>%
  arrange(`Pr(>|t|)`)

head(coef_model_1, 15)


# Coefficients for Model 2
coef_model_2 <- as.data.frame(summary(model_log_annualised)$coefficients)
coef_model_2$Variable <- rownames(coef_model_2)
rownames(coef_model_2) <- NULL

coef_model_2 <- coef_model_2 %>%
  arrange(`Pr(>|t|)`)

head(coef_model_2, 15)


#=========================================================
# 7. Optional: Save model comparison result
#=========================================================

write.csv(
  model_comparison,
  "linear_regression_model_comparison.csv",
  row.names = FALSE
)
