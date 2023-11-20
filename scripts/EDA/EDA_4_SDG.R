################
## Question 4 ##
################


# Can we explain the scores of certain SDGs using the scores of other SDGs?
# Can we predict a high score for one SDG using the
# high score of one or some others?
# (focus on relationship between SDGs)

# Importation of the data for this question
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
  labs(x = 'SDG Goals', y = 'SDG Goals',
       title = 'Correlation Matrix with Significance Indicator')


### General analysis

# # Create a matrix to represent data types
# Q4_bis <- matrix(nrow = 0, ncol = 0)
# Q4_bis[] <- lapply(Q4, function(x) {
#   if (is.numeric(x)) {
#     ifelse(is.na(x), 0, 1)
#   } else {
#     ifelse(is.na(x), 0, 2)
#   }
# })
# 
# # Add a row number column for melting
# Q4_bis$row_number <- seq_len(nrow(Q4_bis))
# 
# # Convert to long format for ggplot
# data_long <- melt(Q4_bis, id.vars = "row_number")
# 
# # Plot
# ggplot(data_long, aes(x = variable, y = row_number)) +
#   geom_tile(aes(fill = factor(value)), color = "white") +
#   scale_fill_manual(values = c("white", "orange", "red")) +
#   theme_minimal() +
#   labs(fill = "Data Type",
#        x = "Columns",
#        y = "Rows",
#        title = "Data Type Visualization: Numerical (Light Blue), Textual (Light Green), NA (White)")

