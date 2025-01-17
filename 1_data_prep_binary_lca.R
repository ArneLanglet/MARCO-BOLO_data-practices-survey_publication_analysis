## manipulate variables to fit binary LCA

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
library(writexl)
library(tidytext)
# Load the package
library(poLCA)



## variables to be used in LCA: 
###### Q4.2 what applies to you? produce, manage, use data? (7 different categories)
###### Q6.10 On a scale from 1 to 5, how would you rate your data literacy? - good: one category only
###### Q6.3 how much time spent with data? - good: one category only
#   Q45 What type of biodiversity-related data do you use?- too many combinations
######   Q6.1 For what purpose do you use biodiversity data? - binary: including policy or not
######   Q6.9 To what extent do you use Essential Variables in your work? ### ebv & eov 
######   Q7.3 How often do you use the following data repositories? ### could be indexed with essential variables
######   Q6.6 What data products and tools do you need most urgently? # 4 categories  Needs maps, not tools to integrate data Needs tools to integrate data, not maps
# Needs both maps and tools to integrate data , Needs neither
#   Q6.7 What challenges do you experience when using biodiversity data?
#   


dataset_raw <- read.csv("//share.univie.ac.at/envpol/13_Horizon Europe_MARCO-BOLO/6_Research/Survey/CSV_TABLE/CoP+Survey_011223.csv")
dataset_raw$created <- as.POSIXct(dataset_raw$StartDate, format = '%Y-%m-%d %H:%M:%S')
dataset_raw$date<- as.Date(dataset_raw$EndDate, format = '%Y-%m-%d %H:%M:%S')
dataset_raw$Progress <- as.numeric(dataset_raw$Progress)
df <- dataset_raw %>% filter(Progress >= 70)


## first step: find and delete NAs

df <- df %>%
  mutate_at(vars(Q4.2, Q6.3, Q45, Q7.3_1, Q6.9_1, Q6.10, Q2.3), ~replace(., . == "", NA))

df_lca <- df %>%
  drop_na(Q4.2, Q6.3, Q45, Q7.3_1, Q6.9_1, Q6.10, Q2.3)

	
df_lca$affiliation <- df_lca$Q2.3
df_lca$area_of_work <- df_lca$Q2.4

df_lca$policy_bin <- ifelse(df_lca$affiliation == "National governmental institution" | 
                              df_lca$affiliation == "Local or regional governmental institution" |
                              df_lca$affiliation == "European Union and its agencies", 
                            1,0)

df_lca$scientific_bin <- ifelse(df_lca$affiliation == "University" |
                                  df_lca$affiliation == "Other academic research institution",
                                1, 0)

df_lca$business_bin <- ifelse(df_lca$affiliation == "Business and Industry",
                     1, 0)

df_lca$ngo_igo_bin <- ifelse(df_lca$affiliation == "Nongovernmental organization (NGO)" |
                               df_lca$affiliation == "International organization",
                              1, 0)

# create variable for local, national, international 

df_lca$affiliation_international <- ifelse(df_lca$affiliation == "Nongovernmental organization (NGO)" |
                               df_lca$affiliation == "International organization" |
                                 df_lca$affiliation == "European Union and its agencies", 
                             1, 0)

df_lca$affiliation_local <- ifelse(df_lca$affiliation == "Local association" |
                                 df_lca$affiliation == "Local or regional governmental institution", 
                               1, 0)

df_lca$affiliation_national <- ifelse(df_lca$affiliation == "National governmental institution", 
                       1, 0)



df_lca$encourage_repo_use <- ifelse(df_lca$Q6.2 == "Strongly agree" |
                                      df_lca$Q6.2 == "Somewhat agree", 
                                      1, 0)
df_lca$strong_encourage_repo_use <- ifelse(df_lca$Q6.2 == "Strongly agree",
                                    1, 0)

# Create binary variables for each unique area of work
df_lca <- df_lca %>%
  mutate(
    academia_research = ifelse(grepl("Academia, research and education", area_of_work), 1, 0),
    public_engagement = ifelse(grepl("Public engagement", area_of_work), 1, 0),
    product_development = ifelse(grepl("Product development and construction", area_of_work), 1, 0),
    other_work = ifelse(grepl("Other", area_of_work), 1, 0),
    conservation_management = ifelse(grepl("Conservation and environmental management", area_of_work), 1, 0),
    data_management = ifelse(grepl("Data management", area_of_work), 1, 0),
    management_administration = ifelse(grepl("Management and administration", area_of_work), 1, 0),
    law_policy_decision = ifelse(grepl("Law, policy and decision-making", area_of_work), 1, 0),
    media_communication = ifelse(grepl("Media and communication", area_of_work), 1, 0)
  )



###### Q6.3 how much time spent with data? - good: one category only
# Reorder the levels in increasing time order
df_lca$time_spent_with_data <- factor(df_lca$Q6.3, levels = c("Below 10%", "10% to 30%", "30% to 50%", "50% to 70%", "Most of my time"))

# Convert time_spent_with_data to numeric scale
# Assign numeric values corresponding to increasing time commitment
df_lca$time_spent_with_data_numeric <- as.numeric(df_lca$time_spent_with_data) 

# do you spent half of your time or more with data?
df_lca$half_of_time_with_data <- ifelse(df_lca$time_spent_with_data_numeric >= 4,
                                  1, 0)




###### Q6.10 On a scale from 1 to 5, how would you rate your data literacy? - good: one category only

# Reorder the levels from bad to good
df_lca$data_literacy <- factor(df_lca$Q6.10, levels = c("Extremely bad", "Somewhat bad", "Neither good nor bad", "Somewhat good", "Extremely good"))


# Convert data_literacy to numeric scale
# Assign numeric values corresponding to increasing data literacy quality
df_lca$data_literacy_numeric <- as.numeric(df_lca$data_literacy)

# do you consider your data literacy somewhat good or better?
df_lca$data_literacy_binary <- ifelse(df_lca$data_literacy_numeric >= 4,
                                      1, 0)



###### Q4.2 what applies to you? produce, manage, use data? (7 different categories)

unique(df_lca$Q4.2)
library(dplyr)

# Assuming df_lca is your dataframe
df_lca <- df_lca %>%
  mutate(
    uses_biodiversity_data = ifelse(grepl("I use biodiversity data", Q4.2), 1, 0),
    produces_biodiversity_data = ifelse(grepl("I produce biodiversity data", Q4.2), 1, 0),
    manages_biodiversity_data = ifelse(grepl("I manage biodiversity data", Q4.2), 1, 0)
  )

# Check results
head(df_lca)

# This code: 
# Adds a column uses_biodiversity_data that equals 1 if the row contains "I use biodiversity data" and 0 otherwise.
# Adds a column produces_biodiversity_data that equals 1 if the row contains "I produce biodiversity data" and 0 otherwise.
# Adds a column manages_biodiversity_data that equals 1 if the row contains "I manage biodiversity data" and 0 otherwise.


###### Q6.9 To what extent do you use Essential Variables in your work?
# 6.9_2
# first need to rename

df_lca$EOV <- df_lca$Q6.9_2

df_lca$EOV[df_lca$EOV==""] <- "Never"

# Reorder the levels in increasing time order
df_lca$EOV_factor <- factor(df_lca$EOV, levels = c("Never", "Once", "Regularly", "Daily"))

table(df_lca$EOV)

df_lca$EOVs <- ifelse(df_lca$EOV_factor == "Regularly" | df_lca$EOV_factor == "Daily",1,0)

df_lca$eov_binary 

df_lca$EBV <- df_lca$Q6.9_1

# Reorder the levels in increasing time order
df_lca$EBV_factor <- factor(df_lca$EBV, levels = c("Never", "Once", "Regularly", "Daily"))

table(df_lca$EBV)

df_lca$EBVs <- ifelse(df_lca$EBV_factor == "Regularly" | df_lca$EBV_factor == "Daily",1,0)

df_lca$ebv_binary 

#######   Q7.3 How often do you use the following data repositories?
df_lca$emodnet <- df_lca$Q7.3_1 

# Reorder the levels in increasing time order
df_lca$emodnet_factor <- factor(df_lca$emodnet, levels = c("Never", "Once", "Regularly", "Daily"))

df_lca$emodnet <- ifelse(df_lca$emodnet_factor == "Regularly" | df_lca$emodnet_factor == "Daily",1,0)



df_lca$obis <- df_lca$Q7.3_2 

df_lca$obis[df_lca$obis==""] <- "Never"


# Reorder the levels in increasing time order
df_lca$obis_factor <- factor(df_lca$obis, levels = c("Never", "Once", "Regularly", "Daily"))

df_lca$obis <- ifelse(df_lca$obis_factor == "Regularly" | df_lca$obis_factor == "Daily",1,0)




df_lca$gbif <- df_lca$Q7.3_3 
df_lca$gbif[df_lca$gbif==""] <- "Never"

# Reorder the levels in increasing time order
df_lca$gbif_factor <- factor(df_lca$gbif, levels = c("Never", "Once", "Regularly", "Daily"))


df_lca$gbif <- ifelse(df_lca$gbif_factor == "Regularly" | df_lca$gbif_factor == "Daily",1,0)



df_lca$national <- df_lca$Q7.3_4 
df_lca$national[df_lca$national==""] <- "Never"


# Reorder the levels in increasing time order
df_lca$national_factor <- factor(df_lca$national, levels = c("Never", "Once", "Regularly", "Daily"))

df_lca$national_repos <- ifelse(df_lca$national_factor == "Regularly" | df_lca$national_factor == "Daily",1,0)



df_lca$project <- df_lca$Q7.3_5 
df_lca$project[df_lca$project==""] <- "Never"

# Reorder the levels in increasing time order
df_lca$project_factor <- factor(df_lca$project, levels = c("Never", "Once", "Regularly", "Daily"))

df_lca$project_repos <- ifelse(df_lca$project_factor == "Regularly" | df_lca$project_factor == "Daily",1,0)


#######  Q45 What type of biodiversity-related data do you use?

df_lca <- df_lca %>%
  mutate(
    geological = ifelse(grepl("Geological data", Q45), 1, 0),
    biological = ifelse(grepl("Biological data", Q45), 1, 0),
    socioeconomic = ifelse(grepl("Socio-economic data", Q45), 1, 0),
    acoustic = ifelse(grepl("Acoustic data", Q45), 1, 0),
    fishery = ifelse(grepl("Fishery data", Q45), 1, 0),
    satellite = ifelse(grepl("Satelite data", Q45), 1, 0),
    pollution = ifelse(grepl("Pollution data", Q45), 1, 0),
    physical = ifelse(grepl("Physical data", Q45), 1, 0),
    chemical = ifelse(grepl("Chemical data", Q45), 1, 0),
    numeric = ifelse(grepl("Numeric data", Q45), 1, 0),
    visual = ifelse(grepl("Visual data", Q45), 1, 0)
  )




# Assuming df is your data frame
df_lca <- df_lca %>%
  mutate(
    reporting = ifelse(grepl("Reporting", Q6.1), 1, 0),
    spatial_planning = ifelse(grepl("Spatial planning", Q6.1), 1, 0),
    eia = ifelse(grepl("Environmental impact assessment", Q6.1), 1, 0),
    conservation = ifelse(grepl("Conservation measures", Q6.1), 1, 0),
    policy_eval = ifelse(grepl("Policy evaluation", Q6.1), 1, 0),
    policymaking = ifelse(grepl("Policy-making", Q6.1), 1, 0),
    decision_making = ifelse(grepl("Decision-making", Q6.1), 1, 0),
    scientific_research = ifelse(grepl("Scientific research", Q6.1), 1, 0),
    education = ifelse(grepl("Education", Q6.1), 1, 0),
    communication = ifelse(grepl("Communication", Q6.1), 1, 0),
    product_dev = ifelse(grepl("Product development", Q6.1), 1, 0),
    protected_area_mgmt = ifelse(grepl("Protected area management", Q6.1), 1, 0),
    indicator_dev = ifelse(grepl("Indicator development", Q6.1), 1, 0)
  )

# Verify results
head(df_lca)




# Assuming df is your data frame
df_lca <- df_lca %>%
  mutate(
    tools_integrate_data = ifelse(grepl("Tools to integrate data", Q6.6), 1, 0),
    maps = ifelse(grepl("Maps", Q6.6), 1, 0),
    scenarios = ifelse(grepl("Scenarios", Q6.6), 1, 0),
    models = ifelse(grepl("Models", Q6.6), 1, 0),
    graphs = ifelse(grepl("Graphs", Q6.6), 1, 0),
    other = ifelse(grepl("Other", Q6.6), 1, 0)
  )


#######  ecosystem
df_lca$ecosystem <- df_lca$Q2.5
df_lca <- df_lca %>% mutate(
    marine_ecosystem = ifelse(grepl("Marine", Q2.5), 1, 0),
    coastal_ecosystem = ifelse(grepl("Coastal", Q2.5), 1, 0),
    freshwater_ecosystem = ifelse(grepl("Freshwater", Q2.5), 1, 0),
    terrestrial_ecosystem = ifelse(grepl("Terrestrial", Q2.5), 1, 0))

#### country



# Define regions for countries
df_lca$region <- ifelse(df_lca$Q2.2 %in% c("France", "Germany", "Belgium", "Austria", "Netherlands", "Luxembourg", "Switzerland", "United Kingdom of Great Britain and Northern Ireland", "Ireland"), "Western Europe",
                        ifelse(df_lca$Q2.2 %in% c("Italy", "Spain", "Portugal", "Greece", "Malta", "Cyprus"), "Southern Europe",
                               ifelse(df_lca$Q2.2 %in% c("United States of America", "Canada"), "Northern America",
                                      ifelse(df_lca$Q2.2 %in% c("Argentina", "Brazil", "Chile", "Peru", "Venezuela", "Mexico", "Costa Rica", "Colombia", "Trinidad and Tobago", "Ecuador"), "Latin America & Caribbean",
                                             ifelse(df_lca$Q2.2 %in% c("Denmark", "Sweden", "Norway", "Finland",  "Iceland"), "Northern Europe",
                                                    ifelse(df_lca$Q2.2 %in% c("Poland", "Estonia", "Latvia"), "Eastern Europe",
                                                           ifelse(df_lca$Q2.2 %in% c("Turkey", "Armenia", "Israel"), "Western Asia",
                                                                  ifelse(df_lca$Q2.2 %in% c("China", "Malaysia", "Philippines", "India", "Sri Lanka", "Indonesia"), "Asia",
                                                                         ifelse(df_lca$Q2.2 %in% c("South Africa", "Ghana", "Algeria", "Cape Verde", "Somalia", "Tanzania", "Cameroon", "Mauritius"), "Africa",
                                                                                ifelse(df_lca$Q2.2 %in% c("Australia", "Fiji"), "Oceania", 
                                                                                       "Other"))))))))))




# Check the results
table(df_lca$region)

# Define binary variables for each region based on the existing 'region' variable
df_lca <- df_lca %>%
  mutate(
    western_europe = ifelse(region == "Western Europe", 1, 0),
    southern_europe = ifelse(region == "Southern Europe", 1, 0),
    northern_america = ifelse(region == "Northern America", 1, 0),
    latin_america_caribbean = ifelse(region == "Latin America & Caribbean", 1, 0),
    northern_europe = ifelse(region == "Northern Europe", 1, 0),
    eastern_europe = ifelse(region == "Eastern Europe", 1, 0),
    western_asia = ifelse(region == "Western Asia", 1, 0),
    asia = ifelse(region == "Asia", 1, 0),
    africa = ifelse(region == "Africa", 1, 0),
    oceania = ifelse(region == "Oceania", 1, 0),
    other_region = ifelse(region == "Other", 1, 0)
  )





# Create binary variables for each challenge, including combined responses
df_lca <- df_lca %>%
  mutate(
    access = ifelse(str_detect(Q6.7_1, "Most of the time|Always|About half the time"), 1, 0),
    data_availability = ifelse(str_detect(Q6.7_2, "Most of the time|Always|About half the time"), 1, 0),
    data_existence = ifelse(str_detect(Q6.7_3, "Most of the time|Always|About half the time"), 1, 0),
    data_knowledge = ifelse(str_detect(Q6.7_4, "Most of the time|Always|About half the time"), 1, 0),
    technical = ifelse(str_detect(Q6.7_9, "Most of the time|Always|About half the time"), 1, 0),
    data_timing = ifelse(str_detect(Q6.7_10, "Most of the time|Always|About half the time"), 1, 0),
    data_cost = ifelse(str_detect(Q6.7_11, "Most of the time|Always|About half the time"), 1, 0),
    data_consistency = ifelse(str_detect(Q6.7_8, "Most of the time|Always|About half the time"), 1, 0),
    lack_metadata = ifelse(str_detect(Q6.7_6, "Most of the time|Always|About half the time"), 1, 0),
    data_interoperable = ifelse(str_detect(Q6.7_5, "Most of the time|Always|About half the time"), 1, 0)
  )


# Create a vector of all variable names to recode
variables <- c(
  "half_of_time_with_data", 
  "uses_biodiversity_data",
  "produces_biodiversity_data", 
  "manages_biodiversity_data", 
  "EOVs", "EBVs",
  "emodnet", "obis", "gbif", "national_repos", "project_repos",
  "produces_biodiversity_data", "manages_biodiversity_data",
  "geological", "biological", "socioeconomic", "acoustic", "fishery", 
  "satellite", "pollution", "physical", "chemical", "numeric", "visual",
  "reporting", "spatial_planning", "eia", "conservation", "policy_eval", "policymaking",
  "scientific_research", "communication", "education", "decision_making", "protected_area_mgmt",
  "indicator_dev", "product_dev", "maps", "scenarios", "models", "graphs", 
  "tools_integrate_data", "other", "access", "data_availability", 
  "data_existence", "data_knowledge", "technical",
  "data_timing", "data_cost", "data_consistency", "lack_metadata", "data_interoperable",
  "marine_ecosystem", "coastal_ecosystem", "freshwater_ecosystem", "terrestrial_ecosystem",
  "southern_europe", "western_europe", "northern_america", "northern_europe"
  
)
length(variables)

# Recode 0 to 1 and 1 to 2
df_lca <- df_lca %>%
  mutate(across(all_of(variables), ~ ifelse(. == 0, 1, 2)))


# Select only the binary variables for correlation
binary_data <- df_lca %>% dplyr::select(all_of(variables))


# Compute the correlation matrix
cor_matrix <- cor(binary_data, use = "complete.obs")

# Define threshold for strong correlation
threshold <- 0.7

# Identify and print strong correlations (both positive and negative)
strong_correlations <- (cor_matrix > threshold | cor_matrix < -threshold) & (cor_matrix != 1)
print("Strong correlations (absolute value > 0.8):")
print(cor_matrix * strong_correlations)


# identify NAs

na_summary <- sapply(df_lca, function(x) sum(is.na(x)))
print(na_summary)



write.csv(df_lca, "df_lca.csv")
write_xlsx(df_lca, "df_lca.xlsx")

