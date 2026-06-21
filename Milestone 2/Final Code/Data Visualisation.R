#=========================================================
# DATA VISUALISATION
#=========================================================

library(dplyr)
library(ggplot2)

#---------------------------------------------------------
# 1. Age Distribution
#---------------------------------------------------------

ggplot(insurance_clean, aes(x = AgeNext)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Age",
    x = "Age",
    y = "Count"
  )

#---------------------------------------------------------
# 2. Annual Income Distribution
#---------------------------------------------------------

ggplot(insurance_clean %>% filter(AnnualIncome < 300000),
       aes(x = AnnualIncome)) +
  geom_histogram(binwidth = 10000, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Annual Income (Filtered)",
    x = "Annual Income",
    y = "Count"
  )

#---------------------------------------------------------
# 3. Premium vs Age (Relationship)
#---------------------------------------------------------

ggplot(insurance_clean, aes(x = AgeNext, y = Premium)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Premium vs Age",
    x = "Age Next",
    y = "Premium"
  )

#---------------------------------------------------------
# 4. Smoker vs Non-Smoker Premium Comparison
#---------------------------------------------------------

ggplot(insurance_clean, aes(x = SmokerStatus, y = AnnualisedPremium)) +
  geom_boxplot(fill = "red") +
  labs(
    title = "Annualised Premium by Smoker Status",
    x = "Smoker Status",
    y = "Annualised Premium"
  )

#---------------------------------------------------------
# 5. Cover Types Comparison (Average Coverage)
#---------------------------------------------------------

cover_means <- insurance_clean %>%
  summarise(
    Life = mean(LifeCoverAmount, na.rm = TRUE),
    TPD = mean(TPDCoverAmount, na.rm = TRUE),
    Trauma = mean(TraumaCoverAmount, na.rm = TRUE),
    IP = mean(IPCoverAmount, na.rm = TRUE),
    BE = mean(BECoverAmount, na.rm = TRUE),
    Severity = mean(SeverityCoverAmount, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "CoverType",
                      values_to = "AverageCover")

ggplot(cover_means, aes(x = CoverType, y = AverageCover)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Insurance Cover by Type",
    x = "Cover Type",
    y = "Average Cover Amount"
  )

ggplot(cover_means, aes(x = CoverType, y = AverageCover)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_log10() +
  labs(
    title = "Average Insurance Cover by Type (Log Scale)",
    x = "Cover Type",
    y = "Average Cover (log scale)"
  )

#---------------------------------------------------------
# 6. Gender Vs Premium
#---------------------------------------------------------
ggplot(insurance_clean, aes(x = Gender, y = AnnualisedPremium)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "Annualised Premium by Gender",
    x = "Gender",
    y = "Annualised Premium"
  )

#---------------------------------------------------------
# 7. Recommendations by Product Type
#---------------------------------------------------------
product_counts <- insurance_clean %>%
  summarise(
    Life = sum(Life == "Yes", na.rm = TRUE),
    TPD = sum(TPD == "Yes", na.rm = TRUE),
    Trauma = sum(Trauma == "Yes", na.rm = TRUE),
    IP = sum(IP == "Yes", na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Product",
    values_to = "Count"
  )

ggplot(product_counts, aes(x = Product, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Recommendations by Product Type",
    x = "Product",
    y = "Number of Recommendations"
  )
