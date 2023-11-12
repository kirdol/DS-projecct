##### How do complex structures, systems, relationships and events influence a country's SDG scores?  #####

install.packages("corrplot")
install.packages("ggplot2")
install.packages("reshape2")
library(corrplot)
library(ggplot2)
library(reshape2)


data_question1 <- read.csv(here("scripts","data","data_question1.csv"))


Correlation_overall <- data_question1 %>% 
      select(population:ef_regulation) %>%
      select(-age.category)

cor_matrix <- cor(Correlation_overall, use = "everything")
print(cor_matrix)
corrplot(cor_matrix, method = "square")


#### Heatmap ####
cor_melted <- melt(cor_matrix)

ggplot(data = cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap')
#why does goal1 and goal10 NA values since cor_matrix? 

#### Check influence of variables on scores ####

# Define the dependent variables (replace with your actual variables)
dependent_vars <- c("goal1", 
                    "goal2", 
                    "goal3", 
                    "goal4",
                    "goal5",
                    "goal6",
                    "goal7",
                    "goal8",
                    "goal9",
                    "goal10",
                    "goal11",
                    "goal12",
                    "goal13",
                    "goal14",
                    "goal15",
                    "goal16",
                    "goal17" )

# Define the independent variables
independent_vars <- c( "unemployment.rate", "GDPpercapita" ,                
                      "MilitaryExpenditurePercentGDP", "internet.usage",              "pf_law",                      "pf_security",               "pf_movement",                  
                      "pf_religion",                  "pf_assembly",                   "pf_expression",                 "pf_identity",                   "ef_government",                
                      "ef_legal",                     "ef_money",                      "ef_trade",                      "ef_regulation")

# Function to run linear regression
run_regression <- function(dep_var, indep_vars) {
  formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + ")))
  return(summary(lm(formula, data = Correlation_overall)))
}

# Store results
results <- list()

# Loop over combinations
for (dep_var in dependent_vars) {
  for (i in 1:length(independent_vars)) {
    combn_vars <- combn(independent_vars, i, simplify = FALSE)
    for (vars in combn_vars) {
      model_summary <- run_regression(dep_var, vars)
      results[[paste(dep_var, paste(vars, collapse = "_"), sep = "_")]] <- model_summary
    }
  }
}

# Now, 'results' contains the summaries of all regressions


# Check if certain region and/or continents have better/worse scores for each goal

# What is in average the goal with the lowest score? With the highest score? Which goals have the highest disparities amongst countries/region?




write.csv(Correlation_overall, file = here("scripts","data","Correlation_overall.csv"))



