Welcome to the repository of the publication Biodiversity monitoring as data practices: "Moving beyond actor-centric science-policy interfaces"

Here is a description of the overall workflow. 

The raw survey data contains personal information and can thus at this stage only be made available upon request. 
The raw survey data is processed in the file "1_data_prep_binary_lca.R" which produces the dataframe for the LCA ("df_lca.csv" / "df_lca.xlsx"). If the raw data file is located in the same folder as the R code, it will produce the dataframe ready for LCA.
The processed data ("df_lca.csv" / "df_lca.xlsx") is used in the file "2_binary_lca.R" to run the LCA model based on binary variables from the survey items (see figure 1 in the publication for a detailed description of the whole model). 
The file "2_binary_lca.R" creates the LCA model data ("lca_model.RData"). 
The file "3_lca_regressions.R" uses the LCA model data ("lca_model.RData") and adds possible explanatory variables directly from the full dataset ("df_lca.csv" / "df_lca.xlsx") to run the regression analysis of Table 5 of the publication.

In this way, the analysis can be reproduced.

In case of any questions don't hesitate to contact arne.langlet@univie.ac.at

