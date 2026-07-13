source("Final_Clean_Code.R")

# ============================================================
# Linear Regression Data
# ============================================================

linear_data <- data_clean
dim(linear_data)

# ============================================================
# Check Variables
# Purpose:
# Display all variable names in linear_data
# ============================================================

names(linear_data)

# ============================================================
# Create Regression Dataset
# Purpose:
# Keep only variables needed for linear regression
# ============================================================

regression_data <- linear_data %>%
  select(
    log_annualised_premium,
    
    age_next,
    gender,
    smoker_status,
    log_annual_income,
    self_employed,
    occupation_group,
    
    life_bin,
    tpd_bin,
    trauma_bin,
    ip_bin,
    product_count,
    log_total_cover,
    
    underwriter,
    super,
    commission_type
  )

dim(regression_data)
summary(regression_data)

# ============================================================
# Remove Missing Values
# Purpose:
# Create a complete dataset for regression modelling
# ============================================================

regression_data <- na.omit(regression_data)

dim(regression_data)

# ============================================================
# Model 1: Customer Characteristics
# Purpose:
# Examine how customer characteristics affect premiums
# ============================================================

model1 <- lm(
  log_annualised_premium ~
    age_next +
    gender +
    smoker_status +
    log_annual_income +
    self_employed +
    occupation_group,
  
  data = regression_data
)

summary(model1)

# ============================================================
# Model 2: Customer + Product Characteristics
# Purpose:
# Add product types and total cover to explain premium levels
# Product count is excluded because it duplicates the four
# product indicators and causes perfect multicollinearity
# ============================================================

model2 <- lm(
  log_annualised_premium ~
    age_next +
    gender +
    smoker_status +
    log_annual_income +
    self_employed +
    occupation_group +
    
    life_bin +
    tpd_bin +
    trauma_bin +
    ip_bin +
    log_total_cover,
  
  data = regression_data
)

summary(model2)

# ============================================================
# Model 3: Customer + Product + Insurer Characteristics
# Purpose:
# Examine whether insurer and policy structure affect premiums
# after controlling for customer and product characteristics
# ============================================================

model3 <- lm(
  log_annualised_premium ~
    age_next +
    gender +
    smoker_status +
    log_annual_income +
    self_employed +
    occupation_group +
    
    life_bin +
    tpd_bin +
    trauma_bin +
    ip_bin +
    log_total_cover +
    
    underwriter +
    super +
    commission_type,
  
  data = regression_data
)

summary(model3)

# ============================================================
# Compare Regression Models
# Purpose:
# Summarise model performance and extract the NEOS coefficient
# ============================================================

model_comparison <- data.frame(
  Model = c(
    "Model 1: Customer",
    "Model 2: Customer + Product",
    "Model 3: Customer + Product + Insurer"
  ),
  R_Squared = c(
    summary(model1)$r.squared,
    summary(model2)$r.squared,
    summary(model3)$r.squared
  ),
  Adjusted_R_Squared = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared,
    summary(model3)$adj.r.squared
  ),
  Residual_Standard_Error = c(
    summary(model1)$sigma,
    summary(model2)$sigma,
    summary(model3)$sigma
  )
)

print(model_comparison)

# Extract only the NEOS Life result from Model 3
neos_result <- coef(summary(model3))["underwriterNEOS Life", ]

print(neos_result)