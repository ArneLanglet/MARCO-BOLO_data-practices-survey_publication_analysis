### LCA using binary-coded variables

library(tidyverse)
library(ggplot2)
require(ggthemes)
library(kableExtra)
library(knitr)
require(gridExtra)
require(scales)
require(reshape2)
require(countrycode)
library(knitr)
library(dplyr)

library(tidytext)
# Load the package
library(poLCA)

library(nnet)


library(flextable)
library(officer)


df_lca <- read.csv("//share.univie.ac.at/envpol/13_Horizon Europe_MARCO-BOLO/6_Research/Survey/CSV_TABLE/R analysis/df_lca.csv")




f <- cbind(
  data_time_binary, 
  #data_literacy_binary,   
  #uses_biodiversity_data, 
  produces_biodiversity_data,
  manages_biodiversity_data,
  
  geological_data, 
  #biological_data, 
  socioeconomic_data,
  acoustic_data, 
  fishery_data, 
  satellite_data, 
  pollution_data, 
  physical_data, 
  chemical_data, 
  numeric_data, 
  visual_data,
  
  reporting, 
  spatial_planning, 
  eia, 
  conservation, 
  policy_eval,
  policymaking,
  scientific_research, 
  communication, 
  education,
  decision_making, 
  protected_area_mgmt,
  indicator_dev, 
  product_dev,
  
  eov_binary, 
  emodnet_binary, 
  obis_binary, 
  gbif_binary, 
  nationalrepo_binary,
  projectrepo_binary,
  
  maps, 
  scenarios,
  models, 
  graphs, 
  tools_integrate_data,
  
  challenge_access, 
  challenge_data_availability, 
  #challenge_data_existence, 
  #challenge_data_knowledge, 
  challenge_technical,
  challenge_data_timing,
  challenge_data_cost,
  challenge_data_consistency
  ) ~ 1


lca <- df_lca %>% dplyr::select(
  data_time_binary, 
  #data_literacy_binary,   
  #uses_biodiversity_data, 
  produces_biodiversity_data,
  manages_biodiversity_data,
  
  geological_data, 
  #biological_data, 
  socioeconomic_data,
  acoustic_data, 
  fishery_data, 
  satellite_data, 
  pollution_data, 
  physical_data, 
  chemical_data, 
  numeric_data, 
  visual_data,
  
  reporting, 
  spatial_planning, 
  eia, 
  conservation, 
  policy_eval,
  policymaking,
  scientific_research, 
  communication, 
  education,
  decision_making, 
  protected_area_mgmt,
  indicator_dev, 
  product_dev,
  
  eov_binary, 
  emodnet_binary, 
  obis_binary, 
  gbif_binary, 
  nationalrepo_binary,
  projectrepo_binary,
  
  maps, 
  scenarios,
  models, 
  graphs, 
  tools_integrate_data,
  
  challenge_access, 
  challenge_data_availability, 
  #challenge_data_existence, 
  #challenge_data_knowledge, 
  challenge_technical,
  challenge_data_timing,
  challenge_data_cost,
  challenge_data_consistency,
  
  # explanatory variables
  data_literacy_binary, 
                                affiliation,
                                  scientific_bin,
                                  policy_bin,
  business_bin,
  ngo_igo_bin,
                                  area_of_work,
  academia_research,
  public_engagement,
  product_development,
  other_work,
  conservation_management,
  data_management,
  management_administration,
  law_policy_decision,
  media_communication,
                                  region,
  western_europe,
  southern_europe,
  northern_america,
  latin_america_caribbean,
  northern_europe,
  eastern_europe,
  western_asia,
  asia,
  africa,
  oceania,
                                  marine_ecosystem,
                                  coastal_ecosystem,
                                  freshwater_ecosystem,
                                  terrestrial_ecosystem,
                                  southern_europe,
                                  western_europe,
                                  northern_europe, 
                                  northern_america)


# Prepare an empty data frame to store model fit results
model_results <- data.frame(
  Classes = integer(),
  LogLikelihood = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Entropy = numeric()
)


# Loop through 2 to 6 latent classes
for (n in 2:6) {
  # Fit the LCA model
  lca_model <- poLCA(f, data = df_lca, nclass = n, maxiter = 5000, graphs = FALSE)
  
  # Compute entropy as a measure of model certainty
  posterior_probs <- lca_model$posterior
  entropy <- -sum(posterior_probs * log(posterior_probs)) / nrow(lca)
  
  # Extract class prevalence
  class_prevalence <- lca_model$P
  prevalence_cols <- setNames(as.list(class_prevalence), paste0("Class", 1:length(class_prevalence)))
  
  # Create a data frame for the current iteration and bind with model_results
  current_result <- data.frame(
    Classes = n,
    LogLikelihood = lca_model$llik,
    AIC = lca_model$aic,
    BIC = lca_model$bic,
    Entropy = entropy,
    prevalence_cols
  )
  
  # Bind the results
  model_results <- bind_rows(model_results, current_result)
}


# Create the flextable from model_results
ft <- flextable(model_results) %>%
  set_header_labels(
    Classes = "Number of Classes",
    LogLikelihood = "Log Likelihood",
    AIC = "AIC",
    BIC = "BIC",
    Entropy = "Entropy"
  ) %>%
  add_header_row(values = c("Latent Class Analysis Results"), colwidths = ncol(model_results)) %>%
  theme_vanilla() %>%  # Apply a clean theme
  autofit()  # Auto fit columns to content

# Save the flextable as a Word document
doc <- read_docx() %>% 
  body_add_flextable(ft) %>% 
  body_add_par(" ", style = "Normal")  # Add space between tables
print(doc, target = "fitted_binary_LCA_Model_Results.docx")



####################### 3 class model seems best


df_lca_clean <- na.omit(lca)

lca_model <- poLCA(f, data = df_lca, nclass = 4, maxiter = 5000, graphs = FALSE)


# Extract item response probabilities
item_probs <- lca_model$probs

# Initialize an empty list to store data frames for each item
item_probs_list <- lapply(seq_along(item_probs), function(i) {
  item_df <- as.data.frame(item_probs[[i]])
  item_df$Item <- names(item_probs)[i]
  item_df$Category <- rownames(item_df)
  return(item_df)
})

# Use bind_rows to combine the data frames, filling in missing columns with NA
item_probs_df <- bind_rows(item_probs_list, .id = "ItemID")

# Check the resulting data frame structure
str(item_probs_df)


# Load required library
library(ggplot2)

# Assuming your data frame is named item_probs_df
# Convert 'Category' to a factor for better control over colors
item_probs_df$Category <- factor(item_probs_df$Category)
item_probs_df$Probability <- item_probs_df$'Pr(2)'





# Create the line plot
ggplot(item_probs_df, aes(x = Item, y = Probability, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Optional: Add points for better readability
  labs(title = "Item Response Probabilities (Pr(2)) by Class",
       x = "Item",
       y = "Probability (Pr(2))",
       color = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))





###### group and reorder

# Define the desired order of items on the x-axis
item_probs_df$Item <- factor(item_probs_df$Item, 
                             levels = c("data_time_binary", "data_literacy_binary", 
                                        "manages_biodiversity_data", "produces_biodiversity_data",
                                        "eov_binary", "emodnet_binary",
                                        "obis_binary", "gbif_binary",
                                        "nationalrepo_binary", "projectrepo_binary",
                                        "geological_data","acoustic_data", "fishery_data", 
                                        "satellite_data", "pollution_data", "physical_data", "chemical_data", "numeric_data", "visual_data",
                                        "reporting", "spatial_planning", "eia", "conservation", "policy_eval",
                                        "scientific_research", "communication", "education", "decision_making", "protected_area_mgmt",
                                        "indicator_dev", "product_dev",
                                        "maps", "scenarios", "models", "graphs", "tools_integrate_data",
                                        "challenge_access", "challenge_data_availability",  "challenge_technical",
                                        "challenge_data_timing", "challenge_data_cost", "challenge_data_consistency"))




# Add a new column to specify groups
item_probs_df$Group <- ifelse(item_probs_df$Item %in% c("data_time_binary", "data_literacy_binary","manages_biodiversity_data", "produces_biodiversity_data"), "Data work", 
                                     ifelse(item_probs_df$Item %in% c( "eov_binary", "emodnet_binary",
                                                                       "obis_binary", "gbif_binary",
                                                                       "nationalrepo_binary", "projectrepo_binary"), "Use of ess.var. and repo.", 
                                            ifelse(item_probs_df$Item %in% c("geological_data","acoustic_data", "fishery_data", 
                                                                             "satellite_data", "pollution_data", "physical_data", "chemical_data", "numeric_data", "visual_data"), "Types of data",       
                                                   ifelse(item_probs_df$Item %in% c( "reporting", "spatial_planning", "eia", "conservation", "policy_eval", "indicator_dev", "product_dev",
                                                                                     "scientific_research", "communication", "education", "decision_making", "protected_area_mgmt"), "Uses of data",       
                                                          ifelse(item_probs_df$Item %in% c("maps", "scenarios", "models", "graphs", "tools_integrate_data"), "Tools needed",       
                                                                 ifelse(item_probs_df$Item %in% c("challenge_access", "challenge_data_availability",  "challenge_technical",
                                                                   "challenge_data_timing", "challenge_data_cost", "challenge_data_consistency"), "Challenges experienced", 
                                                                                        "Other"))))))


item_probs_df$Group <- factor(item_probs_df$Group, levels = c("Data work",
                                                              "Types of data","Uses of data", "Use of ess.var. and repo.", 
                                                              "Tools needed", "Challenges experienced"))


item_probs_df <- item_probs_df %>% 
  filter(!is.na(Group))

# Plot with ggplot2, using faceting to differentiate groups and ordered items
ggplot(item_probs_df, aes(x = Item, y = Probability, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Item Response Probabilities (Pr(2)) by Class",
       x = "Item",
       y = "Probability (Pr(2))",
       color = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  facet_wrap(~Group, scales = "free_x", nrow = 1)





