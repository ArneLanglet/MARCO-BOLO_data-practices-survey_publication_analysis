################ regression

rm(list = ls())
gc()
df_lca <- read.csv("//share.univie.ac.at/envpol/13_Horizon Europe_MARCO-BOLO/8_Publication/df_lca.csv")
load("//share.univie.ac.at/envpol/13_Horizon Europe_MARCO-BOLO/8_Publication/lca_model.RData")

library(car)
library(nnet)
library(dplyr)

#### check distribution of affiliations between classes



df_lca$class_membership <- lca_model$predclass

df_lca$class_membership <- factor(df_lca$class_membership, levels = c(1, 2, 3))

table(df_lca$class_membership)

# Create the contingency table
affiliation_table <- table(df_lca$class_membership, df_lca$affiliation)


# Calculate the percentage of affiliations per class
affiliation_percentage <- data.frame(prop.table(affiliation_table, margin = 1) * 100)




############## check correlation table

reg_dat <- df_lca %>% dplyr::select(data_literacy_binary,
  policy_bin, 
  scientific_bin, 
  business_bin,
  ngo_igo_bin,
  affiliation_local,
  affiliation_national,
  affiliation_international,
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
  other,
  encourage_repo_use,
  strong_encourage_repo_use)



# correlation for all variables
cor <- round(cor(reg_dat),
      digits = 2 # rounded to 2 decimals
)


image(cor, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20))


cor <- data.frame(round(cor(reg_dat),
             digits = 2 # rounded to 2 decimals
))


### based on correlation information, select, de-select certain variables


######## check variance inflation factor test


# Fit a multiple linear regression model
model <- multinom(class_membership ~ 
              + affiliation_grouped
            + area_of_work_grouped
            + region_grouped
            # + ecosystem
            + marine_ecosystem
            + freshwater_ecosystem
            + coastal_ecosystem
            + encourage_data_use
            + data_literacy_binary, data = df_lca)  



# Compute VIF for each predictor variable
vif_values <- vif(model)

# Display results
print(vif_values)


vif_df <- data.frame(
  Variable = rownames(vif_values),
  GVIF = vif_values[, "GVIF"],
  Df = vif_values[, "Df"],
  Adjusted_VIF = vif_values[, "GVIF^(1/(2*Df))"]
)

# Add interpretation
vif_df$Interpretation <- cut(vif_df$Adjusted_VIF,
                             breaks = c(-Inf, 2, 5, 10, Inf),
                             labels = c("Low", "Moderate", "High", "Very High")
)

print(vif_df)

## low to moderate multicollinearity only - good


# Conduct multinomial logistic regression


########### with categorical variables 



df_lca <- df_lca %>%
  mutate(affiliation_grouped = case_when(
    affiliation %in% c("Local or regional governmental institution", 
                       "National governmental institution", 
                       "European Union and its agencies") ~ "Policy-making",
    
    affiliation %in% c("Other academic research institution", 
                       "University") ~ "Scientific",
    
    affiliation %in% c("Nongovernmental organization (NGO)", 
                       "International organization") ~ "NGO-IGO",
    
    affiliation == "Business and Industry" ~ "Business",
    
    affiliation %in% c("Other", "Local association") ~ "Other",
    
    TRUE ~ NA_character_  # For any unmatched values
  ))

# Convert to factor and set "Other" as the reference level
df_lca$affiliation_grouped <- relevel(
  factor(df_lca$affiliation_grouped),
  ref = "Other"
)

df_lca$affiliation_grouped <- relevel(factor(df_lca$affiliation_grouped), ref = "Other")

# Recode area_of_work into a new grouped variable
df_lca$area_of_work_grouped <- case_when(
  df_lca$area_of_work %in% c(
    "Media and communication",
    "Product development and construction",
    "Public engagement",
    "Other"
  ) ~ "Other",
  TRUE ~ df_lca$area_of_work
)

# Convert to factor and set "Other" as the reference level
df_lca$area_of_work_grouped <- relevel(
  factor(df_lca$area_of_work_grouped),
  ref = "Other"
)

df_lca$region_grouped <- case_when(
  df_lca$region %in% c("Latin America & Caribbean", "Africa") ~ "Global South (Africa + LAC)",
  df_lca$region %in% c("Asia", "Western Asia", "Oceania") ~ "Asia-Pacific",
  df_lca$region %in% c("Eastern Europe", "Southern Europe") ~ "Eastern & Southern Europe",
  TRUE ~ df_lca$region  # Keep all other categories as-is
)


# Convert to factor and set "Other" as the reference level
df_lca$region_grouped <- relevel(
  factor(df_lca$region_grouped),
  ref = "Other"
)


model <- multinom(class_membership ~ 
                  + affiliation_grouped
                  + area_of_work_grouped
                  + region_grouped
                 # + ecosystem
                 + marine_ecosystem
                 + freshwater_ecosystem
                 + coastal_ecosystem
                  + encourage_data_use
                  + data_literacy_binary , data = df_lca)





# Summarize model
summary_model <- summary(model)

# Extract coefficients and standard errors
coeffs <- summary_model$coefficients
ses <- summary_model$standard.errors

# Calculate p-values
z_values <- coeffs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Function to add significance stars
significance_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# Apply significance stars
stars <- apply(p_values, c(1, 2), significance_stars)

# Create a summary table
results <- data.frame(
  Coefficient = as.vector(coeffs),
  `p-value` = as.vector(p_values),
  Significance = as.vector(stars),
  Class = rep(colnames(coeffs), each = nrow(coeffs)),
  Variable = rep(rownames(coeffs), times = ncol(coeffs))
)

results


#### redo the model with class 3 as baseline


# Relevel the outcome variable so that "3" is the reference
df_lca$class_membership <- relevel(factor(df_lca$class_membership), ref = "3")


model <- multinom(class_membership ~ 
                    + affiliation_grouped
                  + area_of_work_grouped
                  + region_grouped
                  # + ecosystem
                  + marine_ecosystem
                  + freshwater_ecosystem
                  + coastal_ecosystem
                  + encourage_data_use
                  + data_literacy_binary , data = df_lca)
# Summarize model
summary_model <- summary(model)

# Extract coefficients and standard errors
coeffs <- summary_model$coefficients
ses <- summary_model$standard.errors

# Calculate p-values
z_values <- coeffs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Function to add significance stars
significance_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# Apply significance stars
stars <- apply(p_values, c(1, 2), significance_stars)

# Create a summary table
results2 <- data.frame(
  Coefficient = as.vector(coeffs),
  `p-value` = as.vector(p_values),
  Significance = as.vector(stars),
  Class = rep(colnames(coeffs), each = nrow(coeffs)),
  Variable = rep(rownames(coeffs), times = ncol(coeffs))
)

results2


#### redo the model with class 2 as baseline


# Relevel the outcome variable so that "2" is the reference
df_lca$class_membership <- relevel(factor(df_lca$class_membership), ref = "2")


model <- multinom(class_membership ~ 
                    + affiliation_grouped
                  + area_of_work_grouped
                  + region_grouped
                  # + ecosystem
                  + marine_ecosystem
                  + freshwater_ecosystem
                  + coastal_ecosystem
                  + encourage_data_use
                  + data_literacy_binary , data = df_lca)
# Summarize model
summary_model <- summary(model)

# Extract coefficients and standard errors
coeffs <- summary_model$coefficients
ses <- summary_model$standard.errors

# Calculate p-values
z_values <- coeffs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Function to add significance stars
significance_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# Apply significance stars
stars <- apply(p_values, c(1, 2), significance_stars)

# Create a summary table
results3 <- data.frame(
  Coefficient = as.vector(coeffs),
  `p-value` = as.vector(p_values),
  Significance = as.vector(stars),
  Class = rep(colnames(coeffs), each = nrow(coeffs)),
  Variable = rep(rownames(coeffs), times = ncol(coeffs))
)

results3




  # Conduct multinomial logistic regression
model <- multinom(class_membership ~ 
                    data_literacy_binary
                  + strong_encourage_repo_use
                  + policy_bin 
                  + scientific_bin 
                  + business_bin
                  + ngo_igo_bin
                  # + affiliation_local
                  # + affiliation_national
                  # + affiliation_international
                  + academia_research
                  + public_engagement 
                  + product_development
                  + conservation_management 
                  + data_management 
                  + management_administration 
                  + law_policy_decision
                  + media_communication
                  + marine_ecosystem
                  + coastal_ecosystem
                  + freshwater_ecosystem
                  + terrestrial_ecosystem
                  + southern_europe
                  + western_europe
                  + northern_europe
                  + northern_america
                  + latin_america_caribbean
                  # + other
                  #+ encourage_repo_use
                  , data = df_lca)

# Conduct multinomial logistic regression
model <- multinom(class_membership ~ 
affiliation 
+ area_of_work
+ region
+ data_literacy
                  , data = df_lca)


# Summarize model
summary_model <- summary(model)

# Extract coefficients and standard errors
coeffs <- summary_model$coefficients
ses <- summary_model$standard.errors

# Calculate p-values
z_values <- coeffs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Function to add significance stars
significance_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# Apply significance stars
stars <- apply(p_values, c(1, 2), significance_stars)

# Create a summary table
results <- data.frame(
  Coefficient = as.vector(coeffs),
  `p-value` = as.vector(p_values),
  Significance = as.vector(stars),
  Variable = rep(colnames(coeffs), each = nrow(coeffs)),
  Class = rep(rownames(coeffs), times = ncol(coeffs))
)

# Print results in similar structure to Table 4



print(results)


# Restructure the results
results_wide <- data.frame(results %>%
  dplyr::select(Variable, Class, Coefficient, p.value) %>%
  pivot_wider(
    names_from = Class,
    values_from = c(Coefficient, p.value),
    names_glue = "Class{Class}_{.value}"
  )) %>%
  arrange(Variable)

# View the result
print(results_wide)


write_xlsx(results_wide, "results.xlsx")




#####################



df_lca$class_1 <- ifelse(df_lca$class_membership == 1, 1, 0)
df_lca$class_2 <- ifelse(df_lca$class_membership == 2, 1, 0)
df_lca$class_3 <- ifelse(df_lca$class_membership == 3, 1, 0)



logit_model1 <- lm(class_1 ~ 
                     data_literacy_binary
                   + policy_bin 
                   + scientific_bin 
                   + business_bin
                   + ngo_igo_bin
                   # + affiliation_local
                   # + affiliation_national
                   # + affiliation_international
                   + academia_research
                   + public_engagement 
                   + product_development
                   + conservation_management 
                   + data_management 
                   + management_administration 
                   + law_policy_decision
                   + media_communication
                   # + marine_ecosystem
                   # + coastal_ecosystem
                   # + freshwater_ecosystem
                   # + terrestrial_ecosystem
                   # + southern_europe
                   # + western_europe
                   # + northern_europe
                   # + northern_america
                   # + latin_america_caribbean
                   # + other
                   #+ encourage_repo_use
                   + strong_encourage_repo_use, data = df_lca)

summary(logit_model1)

logit_model2 <- lm(class_2 ~ 
                     data_literacy_binary
                   + policy_bin 
                   + scientific_bin 
                   + business_bin
                   + ngo_igo_bin
                   # + affiliation_local
                   # + affiliation_national
                   # + affiliation_international
                   + academia_research
                   + public_engagement 
                   + product_development
                   + conservation_management 
                   + data_management 
                   + management_administration 
                   + law_policy_decision
                   + media_communication
                   # + marine_ecosystem 
                   # + coastal_ecosystem
                   # + freshwater_ecosystem 
                   # + terrestrial_ecosystem
                   # + southern_europe
                   # + western_europe
                   # + northern_europe
                   # + northern_america
                   # + latin_america_caribbean
                   # + other
                   #+ encourage_repo_use
                   + strong_encourage_repo_use, data = df_lca)

summary(logit_model2)

logit_model3 <- lm(class_3 ~ 
                     data_literacy_binary
                   + policy_bin 
                   + scientific_bin 
                   + business_bin
                   + ngo_igo_bin
                   # + affiliation_local
                   # + affiliation_national
                   # + affiliation_international
                   + academia_research
                   + public_engagement 
                   + product_development
                   + conservation_management 
                   + data_management 
                   + management_administration 
                   + law_policy_decision
                   + media_communication
                   # + marine_ecosystem 
                   # + coastal_ecosystem
                   # + freshwater_ecosystem 
                   # + terrestrial_ecosystem
                   # + southern_europe
                   # + western_europe
                   # + northern_europe
                   # + northern_america
                   # + latin_america_caribbean
                   # + other
                  # + encourage_repo_use
                   + strong_encourage_repo_use, data = df_lca)




# View the results
summary(logit_model3)



# Load the stargazer package
library(stargazer)

# Create a combined table with all three models
stargazer(logit_model1, logit_model2, logit_model3, 
          type = "text",
          title = "Regression Results for Latent Class Membership",
          align = TRUE,
          dep.var.labels = c("Class 1 - Scientific data providers", "Class 2 - Applied users", "Class 3 - Data managers and engagers"),
          # covariate.labels = c("Data Literacy", "Policy Bin", "Scientific Bin", "Business Bin", 
          #                      "NGO/IGO Bin", "Affiliation local",
          #                      "Affiliation national",
          #                      "Affiliation international",
          #                      "Academia/Research", "Public Engagement", 
          #                      "Product Development", "Conservation Management", 
          #                      "Data Management", "Management/Administration", 
          #                      "Law/Policy/Decision", "Media/Communication"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)

