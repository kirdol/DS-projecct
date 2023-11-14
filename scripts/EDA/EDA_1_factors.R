##### How do complex structures, systems, relationships and events influence a country's SDG scores?  #####

library(factoextra)

#### data ####
data_question1 <- read.csv(here("scripts","data","data_question1.csv"))

#### Correlations between variables ####

Correlation_overall <- data_question1 %>% 
      select(population:ef_regulation) %>%
      select(-age.category)

cor_matrix <- cor(Correlation_overall, use = "pairwise.complete.obs")
print(cor_matrix)

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

#### cluster dendogram of data_question1 ####

distmat <- dist(Correlation_overall, method="euclidean", diag=TRUE,upper=TRUE)
avclust <- hclust(distmat, method="average")
plot(avclust, labels=data_question1[,1])

#too many observations? Possible to make a dendogram for clustering? 

clusters <- cutree(avclust, k = 5)
plot(clusters, labels=FALSE)
#rect.hclust(clusters, k = 5, border = 2)

#### Kmean ####

data1_scaled <- scale(Correlation_overall)
row.names(data1_scaled) <- data_question1[,1]
fviz_nbclust(data1_scaled, kmeans, method="wss")
kmean <- kmeans(data1_scaled, 7)
print(kmean)
fviz_cluster(kmean, data=data1_scaled, repel=TRUE, depth =NULL, ellipse.type = "norm")
# Result:  (between_SS / total_SS =  58.4 %)

#try with row.name = country
data1_scaled2 <- scale(Correlation_overall)
row.names(data1_scaled2) <- data_question1[,5]
fviz_nbclust(data1_scaled2, kmeans, method="wss")
kmean <- kmeans(data1_scaled2, 7)
print(kmean)
fviz_cluster(kmean, data=data1_scaled2, repel=TRUE, depth =NULL, ellipse.type = "norm")

#doesn't work as duplicates

#try with correlations
Corr_scaled <- scale(cor_matrix[,c(1:34)])
row.names(Corr_scaled) <- data_question1[,5]
kmean <- kmeans(Corr_scaled, 6)
print(kmean)
fviz_cluster(kmean, data=Corr_scaled, repel=TRUE)




#### boxplot goals ####

par(mfrow=c(2,3))
for (i in 2:18){
  boxplot(Correlation_overall[,i], main=names(Correlation_overall[i]), type="l")
}
par(mfrow=c(1,1))

#verify significance pval < 0.05

#### Check influence of variables on scores - Regressions ####

model_goal1 <- lm(goal1 ~ goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal2 <- lm(goal2 ~ goal1 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal3 <- lm(goal3 ~ goal1 + goal2 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal4 <- lm(goal4 ~ goal1 + goal2 + goal3 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal5 <- lm(goal5 ~ goal1 + goal2 + goal3 + goal4 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal6 <- lm(goal6 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal7 <- lm(goal7 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal8 <- lm(goal8 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal9 <- lm(goal9 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal10 <- lm(goal10 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal11 <- lm(goal11 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal12 <- lm(goal12 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal13 <- lm(goal13 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal15 <- lm(goal15 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal16 <- lm(goal16 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal17 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal17 <- lm(goal17 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + MilitaryExpenditurePercentGDP + internet.usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
summary(model_goal11)

# Function to build the model
build_model <- function(dependent_var, data) {
  formula <- as.formula(paste(dependent_var, "~ ."))
  lm(formula, data = Correlation_overall[, !(names(data) %in% dependent_var)])
}

# List of all goal variables
goal_vars <- paste0("goal", c(1:13, 15:17))

# List to store models
models <- list()

# Loop over each goal and build the model
for (goal_var in goal_vars) {
  models[[goal_var]] <- build_model(goal_var, Correlation_overall)
}

#### proving stargazer as visualization for regression ####

stargazer(model_goal2, title="Regression Results",
          align=TRUE, dep.var.labels=c("Overall Rating","High Rating"),
          covariate.labels=c("Handling of Complaints","No Special Privileges",
                             "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



# # Define the dependent variables (replace with your actual variables)

# # Now, 'results' contains the summaries of all regressions

# Check if certain region and/or continents have better/worse scores for each goal

# What is in average the goal with the lowest score? With the highest score? Which goals have the highest disparities amongst countries/region?




write.csv(Correlation_overall, file = here("scripts","data","Correlation_overall.csv"))



