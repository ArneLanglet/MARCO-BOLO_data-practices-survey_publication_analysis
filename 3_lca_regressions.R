################ regression

lca_model$predclass

df_lca$class_membership <- lca_model$predclass

df_lca$class_membership

df_lca$class_membership <- factor(df_lca$class_membership, levels = c(1, 2, 3, 4))



# Conduct multinomial logistic regression
model <- multinom(class_membership ~ data_literacy 
                  + affiliation
                  + region
                  + ecosystem, data = df_lca)

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



# Conduct multinomial logistic regression
model <- multinom(class_membership ~ policy_bin 
                  + scientific_bin 
                  + data_literacy_binary
                  + marine_ecosystem 
                  + coastal_ecosystem
                  + freshwater_ecosystem 
                  + terrestrial_ecosystem
                  + southern_europe
                  + western_europe 
                  + northern_europe 
                  + northern_america, data = df_lca)

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

# Print results in similar structure to Table 4
print(results)

df_lca$class_1 <- ifelse(df_lca$class_membership == 1, 1, 0)
df_lca$class_2 <- ifelse(df_lca$class_membership == 2, 1, 0)
df_lca$class_3 <- ifelse(df_lca$class_membership == 3, 1, 0)



logit_model1 <- lm(class_1 ~ 
                     data_literacy_binary
                   + policy_bin 
                   + scientific_bin 
                   + business_bin
                   + ngo_igo_bin
                   + affiliation_local
                   + affiliation_national
                   + affiliation_international
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
                   + encourage_repo_use
                   + strong_encourage_repo_use, data = df_lca)

summary(logit_model1)

logit_model2 <- lm(class_2 ~ 
                     data_literacy_binary
                   + policy_bin 
                   + scientific_bin 
                   + business_bin
                   + ngo_igo_bin
                   + affiliation_local
                   + affiliation_national
                   + affiliation_international
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
                   + encourage_repo_use
                   + strong_encourage_repo_use, data = df_lca)

summary(logit_model2)

logit_model3 <- lm(class_3 ~ 
                     data_literacy_binary
                   + policy_bin 
                   + scientific_bin 
                   + business_bin
                   + ngo_igo_bin
                   + affiliation_local
                   + affiliation_national
                   + affiliation_international
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
                   + encourage_repo_use
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
          covariate.labels = c("Data Literacy", "Policy Bin", "Scientific Bin", "Business Bin", 
                               "NGO/IGO Bin", "Affiliation local",
                               "Affiliation national",
                               "Affiliation international",
                               "Academia/Research", "Public Engagement", 
                               "Product Development", "Conservation Management", 
                               "Data Management", "Management/Administration", 
                               "Law/Policy/Decision", "Media/Communication", 
                               "Marine Ecosystem", "Coastal Ecosystem", 
                               "Freshwater Ecosystem", "Terrestrial Ecosystem", 
                               "Southern Europe", "Western Europe", 
                               "Northern Europe", "Northern America"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)

