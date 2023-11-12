# Importation of the data for this question.
Q4 <- read.csv(here("scripts", "data", "data_question24.csv"))

# Select SDG score columns
sdg_scores <- Q4[, c('goal1', 'goal2', 'goal3', 'goal4', 'goal5', 'goal6',
                     'goal7', 'goal8', 'goal9', 'goal10', 'goal11', 'goal12',
                     'goal13', 'goal15', 'goal16', 'goal17')]

### creation of a Heatmap showing the correlation between the scores.

# Initialize matrices for correlation and p-values
cor_matrix <- matrix(nrow = ncol(sdg_scores), ncol = ncol(sdg_scores))
p_matrix <- matrix(nrow = ncol(sdg_scores), ncol = ncol(sdg_scores))
rownames(cor_matrix) <- colnames(sdg_scores)
rownames(p_matrix) <- colnames(sdg_scores)
colnames(cor_matrix) <- colnames(sdg_scores)
colnames(p_matrix) <- colnames(sdg_scores)

# Calculate correlation and p-values
for (i in 1:ncol(sdg_scores)) {
  for (j in 1:ncol(sdg_scores)) {
    test_result <- cor.test(sdg_scores[, i], sdg_scores[, j])
    cor_matrix[i, j] <- test_result$estimate
    p_matrix[i, j] <- test_result$p.value}}

# Reshape for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(sdg_scores)))

# Combine the datasets
plot_data <-
  cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create a heatmap and add correlation values with color based on significance
ggplot(plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  scale_color_manual(values = c("black", "yellow")) + # black when significant, yellow if not
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1),
        legend.position = "none") + # Hide the color legend
  labs(x = 'SDG Goals', y = 'SDG Goals', title = 'Correlation Matrix with Significance Indicator')















plot(Q4_SDG_only)

ggplot(melted_data, aes(x = variable, y = 1:nrow(sorted_data), fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green")) +
  labs(x = "Variables", y = "Observations", fill = "Donnée Manquante") +
  theme_minimal()


# Créer un dataframe indiquant les valeurs manquantes
missing_values <- is.na(sorted_data)

# Fondre les données pour le plotting
melted_data <- melt(missing_values, varnames = c("Variable", "Observation"))

# Créer la heatmap
ggplot(melted_data, aes(x = Variable, y = Observation, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green"), name = "Donnée Manquante") +
  labs(x = "Variables", y = "Observations") +
  theme_minimal()
