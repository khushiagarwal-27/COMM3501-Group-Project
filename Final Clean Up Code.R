# Upload Data
library(readxl)
data_raw <- read_excel("COMM3501/A3_Dataset_2023.xlsx")
View(data_raw)

# 1. Load Packages
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(stringr)

# 2. Summarise Data
summary(data_raw)

# 3. Clean Column Names and Text
data <- data_raw %>%
  clean_names() %>%
  mutate(
    across(where(is.character), ~ str_squish(.)),
    across(where(is.character), ~ na_if(., "")),
    across(where(is.character), ~ na_if(., "[Blank]"))
  )

names(data)

# 4. Remove Unnecessary Columns
data_clean <- data %>%
  select(
    -recommendation_id,
    -request_id,
    -life_id,
    -external_ref
  )

# 5. Create NEOS target variable
data_clean <- data_clean %>%
  mutate(
    neos_flag = ifelse(underwriter == "NEOS Life", "Yes", "No"),
    neos_flag = factor(neos_flag, levels = c("No", "Yes"))
  )


# 6. Convert Product Based Yes/No variables to binary
data_clean <- data_clean %>%
  mutate(
    life_bin = case_when(life == "Yes" ~ 1, life == "No" ~ 0, TRUE ~ NA_real_),
    tpd_bin = case_when(tpd == "Yes" ~ 1, tpd == "No" ~ 0, TRUE ~ NA_real_),
    trauma_bin = case_when(trauma == "Yes" ~ 1, trauma == "No" ~ 0, TRUE ~ NA_real_),
    ip_bin = case_when(ip == "Yes" ~ 1, ip == "No" ~ 0, TRUE ~ NA_real_),
    be_bin = case_when(be == "Yes" ~ 1, be == "No" ~ 0, TRUE ~ NA_real_),
    severity_bin = case_when(severity == "Yes" ~ 1, severity == "No" ~ 0, TRUE ~ NA_real_)
  )

# 7. Focus on Life, TPD, Trauma and IP
data_clean <- data_clean %>%
  mutate(
    product_count = life_bin + tpd_bin + trauma_bin + ip_bin
  ) %>%
  filter(product_count > 0)

# 8. Clean Numeric Values
data_clean <- data_clean %>%
  mutate(
    annual_income = ifelse(annual_income < 0, NA, annual_income),
    premium = ifelse(premium < 0, NA, premium),
    annualised_premium = ifelse(annualised_premium < 0, NA, annualised_premium),
    inside_super_premium = ifelse(inside_super_premium < 0, NA, inside_super_premium),
    outside_super_premium = ifelse(outside_super_premium < 0, NA, outside_super_premium)
  )

# 9. Create alternative variable
data_clean <- data_clean %>%
  mutate(
    has_alternative = ifelse(is.na(alternative), 0, 1)
  )

# 10. Create cover and premium variables

data_clean <- data_clean %>%
  mutate(
    total_cover = life_cover_amount + tpd_cover_amount +
      trauma_cover_amount + ip_cover_amount,
    
    log_annual_income = log1p(annual_income),
    log_premium = log1p(premium),
    log_annualised_premium = log1p(annualised_premium),
    log_total_cover = log1p(total_cover),
    
    premium_to_income = ifelse(
      annual_income > 0,
      annualised_premium / annual_income,
      NA
    )
  )

# 11. Create product bundle variable

data_clean <- data_clean %>%
  mutate(
    product_bundle = case_when(
      life_bin == 1 & tpd_bin == 0 & trauma_bin == 0 & ip_bin == 0 ~ "Life only",
      life_bin == 0 & tpd_bin == 1 & trauma_bin == 0 & ip_bin == 0 ~ "TPD only",
      life_bin == 0 & tpd_bin == 0 & trauma_bin == 1 & ip_bin == 0 ~ "Trauma only",
      life_bin == 0 & tpd_bin == 0 & trauma_bin == 0 & ip_bin == 1 ~ "IP only",
      life_bin == 1 & tpd_bin == 1 & trauma_bin == 0 & ip_bin == 0 ~ "Life + TPD",
      life_bin == 1 & tpd_bin == 1 & trauma_bin == 1 & ip_bin == 0 ~ "Life + TPD + Trauma",
      life_bin == 1 & tpd_bin == 1 & trauma_bin == 1 & ip_bin == 1 ~ "Life + TPD + Trauma + IP",
      product_count >= 2 ~ "Other bundle",
      TRUE ~ "Other"
    )
  )

# 12. Engineer commission variables
commission_slash <- str_match(
  data_clean$commission_structure,
  "(\\d+(?:\\.\\d+)?)\\s*%?\\s*/\\s*(\\d+(?:\\.\\d+)?)\\s*%?"
)

commission_percent <- str_match(
  data_clean$commission_structure,
  "(\\d+(?:\\.\\d+)?)\\s*%.*?(\\d+(?:\\.\\d+)?)\\s*%"
)

data_clean <- data_clean %>%
  mutate(
    commission_type = case_when(
      is.na(commission_structure) ~ NA_character_,
      str_detect(str_to_lower(commission_structure), "nil") ~ "Nil",
      str_detect(str_to_lower(commission_structure), "upfront") ~ "Upfront",
      str_detect(str_to_lower(commission_structure), "hybrid") ~ "Hybrid",
      str_detect(str_to_lower(commission_structure), "level") ~ "Level",
      TRUE ~ "Other"
    ),
    
    upfront_commission = coalesce(
      as.numeric(commission_slash[, 2]),
      as.numeric(commission_percent[, 2])
    ),
    
    renewal_commission = coalesce(
      as.numeric(commission_slash[, 3]),
      as.numeric(commission_percent[, 3])
    )
  )

# 13. Create age and date variables
fix_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  as.Date(as.numeric(x), origin = "1899-12-30")
}

data_clean <- data_clean %>%
  mutate(
    date = fix_excel_date(date),
    year = year(date),
    month = month(date, label = TRUE),
    quarter = quarter(date),
    weekday = wday(date, label = TRUE),
    
    age_band = case_when(
      age_next < 30 ~ "Under 30",
      age_next < 40 ~ "30-39",
      age_next < 50 ~ "40-49",
      age_next < 60 ~ "50-59",
      age_next >= 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

# 14. Collapse high-cardinality variables
data_clean <- data_clean %>%
  mutate(
    occupation_group = fct_lump_n(as.factor(occupation), n = 30, other_level = "Other"),
    package_group = fct_lump_n(as.factor(package), n = 30, other_level = "Other"),
    adviser_group = fct_lump_n(as.factor(adviser_id), n = 50, other_level = "Other")
  )

# 15. Convert categorical variables to factors
data_clean <- data_clean %>%
  mutate(
    across(
      c(
        neos_flag, underwriter, package, package_group,
        super, rollover_tax_rebate, gender, smoker_status,
        home_state, occupation, occupation_group, self_employed,
        premium_frequency, product_bundle, commission_type,
        age_band, month, weekday, adviser_group
      ),
      as.factor
    )
  )

 # Missing Variables
# Check size after cleaning
nrow(data_clean)

# Missing percentage table
missing_table <- data_clean %>%
  summarise(
    missing_age = sum(is.na(age_next)),
    missing_gender = sum(is.na(gender)),
    missing_income = sum(is.na(annual_income)),
    missing_premium = sum(is.na(premium)),
    missing_commission = sum(is.na(commission_structure)),
    missing_adviser = sum(is.na(adviser_id))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Missing_Percent = round(Missing_Count / nrow(data_clean) * 100, 4)
  )

missing_table

model_data <- data_clean %>%
  drop_na(
    annual_income,
    commission_structure,
    adviser_id
  )

model_data <- model_data %>%
  mutate(
    log_annual_income = log1p(annual_income),
    premium_to_income = annualised_premium / annual_income
  )

# =========================
# 1. Load packages
# =========================

library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(ranger)
library(xgboost)

set.seed(123)

# =========================
# 2. Create final modelling data
# =========================

model_data_final <- model_data %>%
  select(
    neos_flag,
    age_next, age_band, gender, smoker_status, home_state,
    occupation_group, self_employed,
    log_annual_income,
    super, rollover_tax_rebate,
    life_bin, tpd_bin, trauma_bin, ip_bin,
    product_count, product_bundle,
    log_premium, log_annualised_premium,
    log_total_cover,
    premium_frequency,
    commission_type, upfront_commission, renewal_commission,
    has_alternative,
    adviser_group,
    month, quarter, weekday
  ) %>%
  drop_na() %>%
  mutate(
    neos_flag = factor(neos_flag, levels = c("No", "Yes"))
  )

# Check class balance
table(model_data_final$neos_flag)
prop.table(table(model_data_final$neos_flag))
