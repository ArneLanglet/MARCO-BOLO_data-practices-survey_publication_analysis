## ============================================================
## Multinomial regression on LCA class membership
## ============================================================

## ------------------------------------------------------------
## Clean workspace and load data
## ------------------------------------------------------------
rm(list = ls())
gc()

df_lca <- read.csv("df_lca.csv")
load("lca_model.RData")

## ------------------------------------------------------------
## Load required packages
## ------------------------------------------------------------
library(dplyr)
library(car)
library(nnet)



#### check distribution of affiliations between classes



df_lca$class_membership <- lca_model$predclass

df_lca$class_membership <- factor(df_lca$class_membership, levels = c(1, 2, 3))

table(df_lca$class_membership)



# Calculate the percentage of affiliations per class
affiliation_percentage <- data.frame(prop.table(affiliation_table, margin = 1) * 100)

## ============================================================
## Attach LCA class membership
## ============================================================

# Predicted class membership from LCA model
df_lca$class_membership <- lca_model$predclass

# Convert to factor with fixed class ordering
df_lca$class_membership <- factor(df_lca$class_membership, levels = c(1, 2, 3))

table(df_lca$class_membership)



## ------------------------------------------------------------
## Distribution of affiliations across classes
## ------------------------------------------------------------

affiliation_table <- table(df_lca$class_membership, df_lca$affiliation)

# Percentage distribution by class
affiliation_percentage <- data.frame(
  prop.table(affiliation_table, margin = 1) * 100
)
## ============================================================
## Correlation diagnostics
## ============================================================

reg_dat <- df_lca %>%
  select(
    data_literacy_binary,
    policy_bin,
    scientific_bin,
    business_bin,
    ngo_igo_bin,
    academia_research,
    public_engagement,
    product_development,
    conservation_management,
    data_management,
    management_administration,
    law_policy_decision,
    media_communication,
    marine_ecosystem,
    coastal_ecosystem,
    freshwater_ecosystem,
    terrestrial_ecosystem,
    southern_europe,
    western_europe,
    northern_europe,
    northern_america,
    latin_america_caribbean,
    other
  )

# Correlation matrix (rounded)
cor_mat <- round(cor(reg_dat), 2)

image(
  cor_mat,
  main = "Correlation Matrix",
  col = colorRampPalette(c("blue", "white", "red"))(20)
)

cor_mat <- data.frame(cor_mat)
### based on correlation information, select, de-select certain variables



## ============================================================
## Grouped categorical predictors
## ============================================================

## ------------------------------------------------------------
## Affiliation (grouped)
## ------------------------------------------------------------

df_lca <- df_lca %>%
  mutate(
    affiliation_grouped = case_when(
      affiliation %in% c(
        "Local or regional governmental institution",
        "National governmental institution",
        "European Union and its agencies"
      ) ~ "Policy-making",
      affiliation %in% c(
        "Other academic research institution",
        "University"
      ) ~ "Scientific",
      affiliation %in% c(
        "Nongovernmental organization (NGO)",
        "International organization"
      ) ~ "NGO-IGO",
      affiliation == "Business and Industry" ~ "Business",
      affiliation %in% c("Other", "Local association") ~ "Other",
      TRUE ~ NA_character_
    )
  )

df_lca$affiliation_grouped <- relevel(
  factor(df_lca$affiliation_grouped),
  ref = "Other"
)

## ------------------------------------------------------------
## Area of work (grouped)
## ------------------------------------------------------------

df_lca <- df_lca %>%
  mutate(
    area_of_work_grouped = case_when(
      area_of_work %in% c(
        "Media and communication",
        "Product development and construction",
        "Public engagement",
        "Other"
      ) ~ "Other",
      TRUE ~ area_of_work
    )
  )

df_lca$area_of_work_grouped <- relevel(
  factor(df_lca$area_of_work_grouped),
  ref = "Other"
)

## ------------------------------------------------------------
## Region (grouped)
## ------------------------------------------------------------

df_lca <- df_lca %>%
  mutate(
    region_grouped = case_when(
      region %in% c("Latin America & Caribbean", "Africa") ~
        "Global South (Africa + LAC)",
      region %in% c("Asia", "Western Asia", "Oceania") ~
        "Asia-Pacific",
      region %in% c("Eastern Europe", "Southern Europe") ~
        "Eastern & Southern Europe",
      TRUE ~ region
    )
  )

df_lca$region_grouped <- relevel(
  factor(df_lca$region_grouped),
  ref = "Other"
)


# Convert to factor and set "Other" as the reference level
df_lca$region_grouped <- relevel(
  factor(df_lca$region_grouped),
  ref = "Other"
)





## ============================================================
## Multinomial logistic regression
## ============================================================

## ------------------------------------------------------------
## Model with grouped predictors
## ------------------------------------------------------------


  
model <- multinom(
  class_membership ~
    affiliation_grouped +
    area_of_work_grouped +
    # region_grouped +
    marine_ecosystem +
    freshwater_ecosystem +
    coastal_ecosystem +
    encourage_data_use +
    data_literacy_binary,
  data = df_lca
)


## ------------------------------------------------------------
## Alternative model with ungrouped predictors
## ------------------------------------------------------------

# model <- multinom(
#   class_membership ~
#     affiliation +
#     area_of_work +
#     region +
#     data_literacy,
#   data = df_lca
# )



## ============================================================
## Model output and significance testing
## ============================================================

summary_model <- summary(model)

coeffs <- summary_model$coefficients
ses    <- summary_model$standard.errors

# Wald z-tests
z_values <- coeffs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Significance stars
significance_stars <- function(p) {
  ifelse(
    p < 0.001, "***",
    ifelse(p < 0.01, "**",
           ifelse(p < 0.05, "*", ""))
  )
}

stars <- apply(p_values, c(1, 2), significance_stars)


## ------------------------------------------------------------
## Long-format results table
## ------------------------------------------------------------

results <- data.frame(
  Coefficient   = as.vector(coeffs),
  p_value       = as.vector(p_values),
  Significance  = as.vector(stars),
  Variable      = rep(colnames(coeffs), each = nrow(coeffs)),
  Class         = rep(rownames(coeffs), times = ncol(coeffs))
)

print(results)



## ------------------------------------------------------------
## Wide-format results table
## ------------------------------------------------------------

results_wide <- results %>%
  select(Variable, Class, Coefficient, p_value) %>%
  pivot_wider(
    names_from  = Class,
    values_from = c(Coefficient, p_value),
    names_glue  = "Class{Class}_{.value}"
  ) %>%
  arrange(Variable)

print(results_wide)

write_xlsx(results_wide, "results.xlsx")

