#=========================================================
# DATA CLEANING
#=========================================================

library(dplyr)

#---------------------------------------------------------
# 1. Remove Irrelevant / Identifier Variables
#---------------------------------------------------------

insurance_clean <- A3_Dataset_2023 %>%
  select(
    -RecommendationId,
    -RequestId,
    -LifeId,
    -ExternalRef
  )

#---------------------------------------------------------
# 2. Handle Invalid Values (Basic Data Cleaning)
#---------------------------------------------------------

# Replace invalid income values (-1) with NA
insurance_clean$AnnualIncome[insurance_clean$AnnualIncome < 0] <- NA

#---------------------------------------------------------
# 3. Feature Engineering
#---------------------------------------------------------

insurance_clean <- insurance_clean %>%
  mutate(
    HasAlternative = ifelse(is.na(Alternative), 0, 1)
  ) %>%
  select(-Alternative)

#---------------------------------------------------------
# 4. Convert Categorical Variables to Factors
#---------------------------------------------------------

insurance_clean <- insurance_clean %>%
  mutate(
    across(
      c(
        Underwriter, Package, Gender, SmokerStatus,
        HomeState, PremiumFrequency, Super,
        RolloverTaxRebate, Life, TPD, Trauma, IP, BE,
        Severity, SelfEmployed
      ),
      as.factor
    )
  )

#=========================================================
# DATA QUALITY
#=========================================================

library(skimr)

#---------------------------------------------------------
# 1. Dataset Overview (Raw Data)
#---------------------------------------------------------

summary(A3_Dataset_2023)

#---------------------------------------------------------
# 2. Missing Value Analysis (Raw Data)
#---------------------------------------------------------

sort(
  colSums(is.na(A3_Dataset_2023)),
  decreasing = TRUE
)

#---------------------------------------------------------
# 3. Duplicate Records Check
#---------------------------------------------------------

sum(duplicated(A3_Dataset_2023))

#---------------------------------------------------------
# 4. Factor Level Consistency Check (Cleaned Data)
#---------------------------------------------------------

lapply(
  insurance_clean %>% select(where(is.factor)),
  levels
)

#---------------------------------------------------------
# 5. Numerical Variable Summary (Raw Data)
#---------------------------------------------------------

summary(
  A3_Dataset_2023 %>%
    select(
      AgeNext, AnnualIncome, Premium, AnnualisedPremium,
      LifeCoverAmount, TPDCoverAmount, TraumaCoverAmount,
      IPCoverAmount, BECoverAmount, SeverityCoverAmount
    )
)

#=========================================================
# 6. Post-Cleaning Quality Check (NEW - IMPORTANT)
#=========================================================

# Check missing values AFTER cleaning
colSums(is.na(insurance_clean))

#=========================================================
# OPTIONAL NOTE (for report / slides)
#=========================================================

# AnnualIncome contained invalid values (-1) which were converted to NA
# No duplicate records were found in the dataset
# Missing values remain and will be addressed in modelling stage if required