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


df_lca <- read.csv("//share.univie.ac.at/envpol/13_Horizon Europe_MARCO-BOLO/8_Publication/df_lca.csv")

## select variables for LCA

f <- cbind(
  #data work block
  half_of_time_with_data, 
  #data_literacy_binary,  - taken out because this is an explanatory factor
  uses_biodiversity_data, 
  produces_biodiversity_data,
  manages_biodiversity_data,
  
  # type of data block
  geological, 
  #biological_data - taken out because all have responded this. brings no differentiation
  socioeconomic,
  acoustic, 
  fishery, 
  satellite, 
  pollution, 
  physical, 
  chemical, 
  numeric, 
  visual,
  
  # uses of data block
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
  
  # essential variables and repositories block
  EOVs,
  EBVs,
  emodnet, 
  obis, 
  gbif, 
  national_repos,
  project_repos,
  
  # tools needed block
  maps, 
  scenarios,
  models, 
  graphs, 
  tools_integrate_data,
  
  # challenges block
  access, 
  data_availability, 
  #challenge_data_existence, 
  #challenge_data_knowledge, - both taken out because almost no-one answered these
  technical,
  data_timing,
  data_cost,
  data_consistency,
  lack_metadata,
  data_interoperable  
  ) ~ 1



############### run LCA. First 2-6 classes to compare model fit

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
  posterior_probs <- lca_model$posterior  # Matrix of posterior probabilities (rows = observations, cols = classes)
  
  # Apply entropy formula
  entropy <- -sum(posterior_probs * log(posterior_probs), na.rm = TRUE) / nrow(posterior_probs)
  
  # Print the entropy
  print(entropy)
  
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

lca_model <- poLCA(f, data = df_lca, nclass = 3, maxiter = 5000, graphs = FALSE)



save(lca_model, file = "lca_model.RData")
load("lca_model.RData")


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


# item_probs_df is the resulting dataframe that contains the item response probabilities for each class to be plotted
# Check the resulting data frame structure
str(item_probs_df)


# Load required library
library(ggplot2)

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
                             levels = c("half_of_time_with_data", "uses_biodiversity_data", 
                                        "manages_biodiversity_data", "produces_biodiversity_data",
                                        "EBVs","EOVs", "emodnet",
                                        "obis", "gbif", 
                                        "national_repos", "project_repos",
                                        "geological","acoustic", "fishery", 
                                        "satellite", "pollution", "physical", "chemical", "numeric", "visual",
                                        "reporting", "spatial_planning", "eia", "conservation", "policy_eval",
                                        "scientific_research", "communication", "education", "decision_making", "protected_area_mgmt",
                                        "indicator_dev", "product_dev",
                                        "maps", "scenarios", "models", "graphs", "tools_integrate_data",
                                        "access", "data_availability",  "technical", "lack_metadata", "data_interoperable",
                                        "data_timing", "data_cost", "data_consistency"))



# Add a new column to specify groups
item_probs_df$Group <- ifelse(item_probs_df$Item %in% c("half_of_time_with_data", "uses_biodiversity_data",
                                                        "manages_biodiversity_data", "produces_biodiversity_data"), "Data work", 
                                     ifelse(item_probs_df$Item %in% c( "EOVs", "EBVs", "emodnet",
                                                                       "obis", "gbif",
                                                                       "national_repos", "project_repos"), 
                                            "Use of ess.var. and repo.", 
                                            ifelse(item_probs_df$Item %in% c("geological","acoustic", "fishery", 
                                                                             "satellite", "pollution", "physical", "chemical", "numeric", "visual"), "Types of data",       
                                                   ifelse(item_probs_df$Item %in% c( "reporting", "spatial_planning", "eia", "conservation", "policy_eval", "indicator_dev", "product_dev",
                                                                                     "scientific_research", "communication", "education", "decision_making", "protected_area_mgmt"), "Uses of data",       
                                                          ifelse(item_probs_df$Item %in% c("maps", "scenarios", "models", "graphs", "tools_integrate_data"), "Products needed",       
                                                                 ifelse(item_probs_df$Item %in% c("access", "data_availability",  "technical",
                                                                   "data_timing", "data_cost", "data_consistency",  "lack_metadata", "data_interoperable"), "Challenges experienced", 
                                                                                        "Other"))))))


item_probs_df$Group <- factor(item_probs_df$Group, levels = c("Data work",
                                                              "Types of data","Uses of data", "Use of ess.var. and repo.", 
                                                              "Products needed", "Challenges experienced"))


item_probs_df <- item_probs_df %>% 
  filter(!is.na(Group))

# Rename the classes in the Category variable
# Convert to character, recode, then back to factor
item_probs_df$Category <- as.character(item_probs_df$Category)
item_probs_df$Category <- recode(item_probs_df$Category, 
                                 "class 1: " = "Class 1: Scientific data providers", 
                                 "class 2: " = "Class 2: Applied data users", 
                                 "class 3: " = "Class 3: Data managers and engagers")

item_probs_df$Category <- factor(item_probs_df$Category)


# Load ggforce for improved faceting options
library(ggforce)

# Plot with ggplot2, using faceting to differentiate groups and ordered items
ggplot(item_probs_df, aes(x = Item, y = Probability, color = Category, group = Category)) +
  geom_line(size = 1.2) +  # Slightly thicker lines for better visibility
  geom_point(size = 3) +   # Larger points for readability
  labs(
    title = "Item Response Probabilities by Class",
    x = "Item",
    y = "Probability",
    color = "Class"
  ) +
  theme_minimal() +
  theme(
    # Axis text adjustments
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),  # Larger and angled x-axis text
    axis.text.y = element_text(size = 14, face = "bold"),  # Larger y-axis text
    # Title adjustments
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Centered and bold title
    # Legend adjustments
    legend.title = element_text(size = 16, face = "bold"),  # Larger and bold legend title
    legend.text = element_text(size = 14),  # Larger legend text
    # Facet spacing and layout adjustments
    strip.text = element_text(size = 14, face = "bold"),  # Larger facet labels
    panel.spacing = unit(0.5, "lines"),  # Increase spacing between facets
    # Add space for axis labels and avoid clipping
    plot.margin = margin(20, 20, 20, 20)  # Add margin around the plot
  ) +
  facet_wrap(
    ~Group, 
    scales = "free_x", 
    nrow = 1
  )





#################

item_counts <- c()

# Count the number of items per group (facet) and ensure ItemCount is present
item_counts <- item_probs_df %>%
  group_by(Group) %>%
  summarise(ItemCount = n_distinct(Item), .groups = "drop")  # Count distinct items per group

# Join the item counts to the original dataframe
item_probs_df <- item_probs_df %>%
  left_join(item_counts, by = "Group") %>%
  mutate(FacetWidth = pmax(ItemCount / max(ItemCount), 0.12))  # Normalize widths and ensure a minimum width of 0.12



# Create individual plots for each group
plots <- item_probs_df %>%
  split(.$Group) %>%
  lapply(function(group_data) {
    ggplot(group_data, aes(x = Item, y = Probability, color = Category, group = Category)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        x = "Item",
        y = "Probability",
        color = "Class"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_blank(),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom"  # Position the legend at the bottom
      )
  })

# Determine layout parameters for two rows
facet_widths <- item_counts$ItemCount / sum(item_counts$ItemCount) # Proportional widths
facet_layout <- length(plots) %/% 2 + length(plots) %% 2  # Calculate rows for two rows

# Combine plots with a shared legend
library(cowplot)
final_plot <- plot_grid(
  plotlist = plots,
  nrow = 2,  # Two rows for increased readability
  rel_widths = facet_widths,
  align = "hv"  # Ensure equal axis scales
)

# Add legend back to the plot
legend <- get_legend(plots[[1]] + theme(legend.position = "bottom"))
final_with_legend <- plot_grid(final_plot, legend, ncol = 1, rel_heights = c(0.9, 0.1))

# Display the final plot
print(final_with_legend)


# Create the plot with all IGC facets in one row
ggplot(item_probs_df, aes(x = Item, y = Probability, color = Category, group = Category)) +
  geom_line(size = 1.2) +  # Slightly thicker lines for better visibility
  geom_point(size = 3) +   # Larger points for readability
  labs(
    title = "Item Response Probabilities by Class",
    x = "",
    y = "",
    color = "Class"  # Only one legend for color
  ) +
  theme_minimal() +
  theme(
    # Axis text adjustments
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),  # Larger and angled x-axis text
    axis.text.y = element_text(size = 14, face = "bold"),  # Larger y-axis text
    # Title adjustments
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Centered and bold title
    # Legend adjustments
    legend.title = element_text(size = 16, face = "bold"),  # Larger and bold legend title
    legend.text = element_text(size = 14),  # Larger legend text
    # Facet spacing and layout adjustments
    strip.text = element_text(size = 14, face = "bold"),  # Larger facet labels
    panel.spacing = unit(0.5, "lines"),  # Increase spacing between facets
    # Add space for axis labels and avoid clipping
    plot.margin = margin(20, 20, 20, 20)  # Add margin around the plot
  ) +
  facet_wrap(
    ~Group, 
    scales = "free_x", 
    nrow = 2  # Split into two rows for better readability
  )



