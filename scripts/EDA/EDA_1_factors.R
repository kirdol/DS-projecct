##### How do complex structures, systems, relationships and events influence a country's SDG scores?  #####


# liste_de_scripts <- c("setup.R")
# 
# for (script in liste_de_scripts) { # execute each sript
#   source(here("scripts", "Clean", script))}
# 


#### data ####

data_question1 <- read.csv(here("scripts","data","data_question1.csv"))

#### Correlations between variables ####

Correlation_overall <- data_question1 %>% 
      select(population:ef_regulation) #take years into consideration? 

cor_matrix <- cor(Correlation_overall, use = "pairwise.complete.obs")
print(cor_matrix)
kable(cor_matrix)

#### Pearson's correlation coeff ####

#for goals

panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 2/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data_question1[,9:24], upper.panel=panel.cor, diag.panel=panel.hist)

#too many goals for making this in once

#for HFI scores 

panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 2/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data_question1[,30:41], upper.panel=panel.cor, diag.panel=panel.hist)

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

#### PCA ####

# for goals

myPCA_g <- PCA(data_question1[,9:20])
summary(myPCA_g)
myPCA_g$eig

#eigenvalue only >1 with dim1 

#for HFI scores
myPCA_s <- PCA(data_question1[,30:41])
summary(myPCA_s)
kable(myPCA_s)
myPCA_s$eig

#3 revelant component according to eigen value of Kaiser-Guttman’s rule

#### cluster dendogram of data_question1 ####

distmat <- dist(Correlation_overall, method="euclidean", diag=TRUE,upper=TRUE)
avclust <- hclust(distmat, method="average")
plot(avclust, labels=data_question1[,1])

#too many observations? Possible to make a dendogram for clustering? 

#### Kmean ####

data1_scaled <- scale(Correlation_overall)
row.names(data1_scaled) <- data_question1[,1]
fviz_nbclust(data1_scaled, kmeans, method="wss")
kmean <- kmeans(data1_scaled, 7, nstart = 25)
print(kmean)
fviz_cluster(kmean, data=data1_scaled, repel=TRUE, depth =NULL, ellipse.type = "norm")
# Result:  (between_SS / total_SS =  58.4 %)

#try with row.name = country
data1_scaled2 <- scale(Correlation_overall)
row.names(data1_scaled2) <- data_question1[,5]
fviz_nbclust(data1_scaled2, kmeans, method="wss")
kmean <- kmeans(data1_scaled2, 7, nstart = 25)
print(kmean)
fviz_cluster(kmean, data=data1_scaled2, repel=TRUE, depth =NULL, ellipse.type = "norm")

#doesn't work as duplicates

#I think that clusters are not possible if not merging all data per country or code. Too many obsvervations. 

#### boxplots ####

#for goals

boxplot(Correlation_overall[2:18], 
        las = 2,            # Makes the axis labels perpendicular to the axis
        par(mar = c(7, 5, 2, 1)),  # Adjusts the margins to fit all labels
        cex.axis = 0.7,      # Reduces the size of the axis labels
        cex.lab = 1,       # Reduces the size of the x and y labels
        notch = TRUE,       # Specifies whether to add notches or not
        main = "Merged goals boxplot", # Title of the boxplot
        xlab = "Goals",  # X-axis label
        ylab = "Score")     # Y-axis label

#for Human Freedom Index scores 

boxplot(Correlation_overall[23:34], 
        las = 2,            # Makes the axis labels perpendicular to the axis
        par(mar = c(7, 5, 2, 1)),  # Adjusts the margins to fit all labels
        cex.axis = 0.7,      # Reduces the size of the axis labels
        cex.lab = 1,       # Reduces the size of the x and y labels
        notch = TRUE,       # Specifies whether to add notches or not
        main = "Merged Human Freedom Index scores boxplot", # Title of the boxplot
        xlab = "Categories",  # X-axis label
        ylab = "Score")     # Y-axis label

# the rest

par(mfrow=c(2,3))
for (i in 19:22){
  boxplot(Correlation_overall[,i], main=names(Correlation_overall[i]), type="l")
}
par(mfrow=c(1,1))

#verify significance pval < 0.05

#### Check influence of variables on scores - Regressions ####

model_goal1 <- lm(goal1 ~ goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal2 <- lm(goal2 ~ goal1 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal3 <- lm(goal3 ~ goal1 + goal2 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal4 <- lm(goal4 ~ goal1 + goal2 + goal3 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal5 <- lm(goal5 ~ goal1 + goal2 + goal3 + goal4 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal6 <- lm(goal6 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal7 <- lm(goal7 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal8 <- lm(goal8 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal9 <- lm(goal9 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal10 <- lm(goal10 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal11 <- lm(goal11 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal12 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal12 <- lm(goal12 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal13 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal13 <- lm(goal13 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal15 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal15 <- lm(goal15 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal16 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal16 <- lm(goal16 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal17 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)
model_goal17 <- lm(goal17 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Correlation_overall)

#use VIF for testing multicolinearity!!! -> In Analysis? 

# List of all goals, explicitly excluding goal14

goals <- paste0("goal", c(1:13, 15:17))

# Independent variables - these remain constant across all models
independent_vars <- c("MilitaryExpenditurePercentGDP", "internet_usage", "pf_law", "pf_security", "pf_movement", "pf_religion", "pf_assembly", "pf_expression", "pf_identity", "ef_government", "ef_legal", "ef_money", "ef_trade", "ef_regulation")

# Initialize an empty list to store all models
models <- list()

# Loop through each goal and create a model
for (goal in goals) {
  # Create the formula by excluding the current goal from the independent variables
  formula <- as.formula(paste(goal, "~", paste(setdiff(goals, goal), collapse = " + "), "+", paste(independent_vars, collapse = " + ")))
  
  # Fit the model
  models[[goal]] <- lm(formula, data = Correlation_overall)
}

# Now `models` contains all linear models


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



