###Is the evolution in sustainable development influenced by uncontrollable events, such as economic crisis, health crises and natural disasters? ###

install.packages("corrplot")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("here")
library(corrplot)
library(ggplot2)
library(reshape2)
library(here)


# Importation of the data for this question
Q3.1 <- read.csv(here("scripts", "data", "data_question3_1.csv"))
Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
Q3.3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))


##Disasters
head(Q3.1)
str(Q3.1)

# Replace the missing values by zero
Q3.1[is.na(Q3.1)] <- 0
print(Q3.1)


#Correlations between variables
Q3.1_Correlation <- Q3.1 %>% 
  select(overallscore:total_damages)

#Calculate correlation matrix
Q3.1_corr_matrix <- cor(Q3.1_Correlation, use = "everything")
print(Q3.1_corr_matrix)

## Heatmap
# Melt the correlation matrix
#cor_melted <- melt(Q3.1_corr_matrix)

# Create p-value matrix
cor_matrix <- matrix(nrow = ncol(Q3.1_corr_matrix), ncol = ncol(Q3.1_corr_matrix))
p_matrix <- matrix(nrow = ncol(Q3.1_corr_matrix), ncol = ncol(Q3.1_corr_matrix))
rownames(cor_matrix) <- colnames(Q3.1_corr_matrix)
rownames(p_matrix) <- colnames(Q3.1_corr_matrix)
colnames(cor_matrix) <- colnames(Q3.1_corr_matrix)
colnames(p_matrix) <- colnames(Q3.1_corr_matrix)


# Calculate p-values
for (i in 1:ncol(Q3.1_corr_matrix)) {
  for (j in 1:ncol(Q3.1_corr_matrix)) {
    # Exclude missing values before performing the correlation test
    non_missing_rows <- complete.cases(Q3.1_corr_matrix[, c(i, j)])
    
    if (sum(non_missing_rows) > 1) {  # Check if there are enough non-missing observations
      test_result <- cor.test(Q3.1_corr_matrix[non_missing_rows, i], 
                              Q3.1_corr_matrix[non_missing_rows, j])
      cor_matrix[i, j] <- test_result$estimate
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- NA  # Set to NA if not enough non-missing observations
    }
  }
}

# Melt for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(Q3.1_corr_matrix)))

# Combine the datasets
plot_data <- cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create the ggplot2 plot
ggplot(data = plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  
  # Add text labels for correlation values and color based on significance
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1, size = 2.5) +
  
  # Customize the color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  
  # Customize the color legend for significance
  scale_color_manual(values = c("black", "turquoise")) + # black when significant, yellow if not
  
  # Adjust the appearance of the plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7, hjust = 1),
        axis.text.y = element_text(size= 7, hjust = 1),
        legend.position = "none") +
  
  # Little adjustments and labels
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap Disaters')





#2ème??

#Correlations between variables
Q3.1_Correlation <- Q3.1 %>% 
  select(overallscore:total_damages)

# Calculate the correlation matrix
matrice_correlation <- cor(Q3.1_Correlation, use = "complete.obs")

# Print  the correlation matrix
print(matrice_correlation)


# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(matrice_correlation))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Matrice de Corrélation et Heatmap')



#Faire des analyses avec les variables 
















#__________________________________________________________________________________


##COVID
Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
str(Q3.2)

#Correlations between variables
Q3.2_Correlation <- Q3.2 %>% 
  select(overallscore:stringency)

#Calculate correlation matrix
Q3.2_corr_matrix <- cor(Q3.2_Correlation, use = "everything")
print(Q3.2_corr_matrix)

## Heatmap

# Create p-value matrix
cor_matrix <- matrix(nrow = ncol(Q3.2_corr_matrix), ncol = ncol(Q3.2_corr_matrix))
p_matrix <- matrix(nrow = ncol(Q3.2_corr_matrix), ncol = ncol(Q3.2_corr_matrix))
rownames(cor_matrix) <- colnames(Q3.2_corr_matrix)
rownames(p_matrix) <- colnames(Q3.2_corr_matrix)
colnames(cor_matrix) <- colnames(Q3.2_corr_matrix)
colnames(p_matrix) <- colnames(Q3.2_corr_matrix)


# Calculate p-values
for (i in 1:ncol(Q3.2_corr_matrix)) {
  for (j in 1:ncol(Q3.2_corr_matrix)) {
    # Exclude missing values before performing the correlation test
    non_missing_rows <- complete.cases(Q3.2_corr_matrix[, c(i, j)])
    
    if (sum(non_missing_rows) > 1) {  # Check if there are enough non-missing observations
      test_result <- cor.test(Q3.2_corr_matrix[non_missing_rows, i], 
                              Q3.2_corr_matrix[non_missing_rows, j])
      cor_matrix[i, j] <- test_result$estimate
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- NA  # Set to NA if not enough non-missing observations
    }
  }
}

# Melt for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(Q3.2_corr_matrix)))

# Combine the datasets
plot_data <- cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create the ggplot2 plot
ggplot(data = plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  
  # Add text labels for correlation values and color based on significance
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1, size = 2.5) +
  
  # Customize the color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  
  # Customize the color legend for significance
  scale_color_manual(values = c("black", "turquoise")) + # black when significant, yellow if not
  
  # Adjust the appearance of the plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7, hjust = 1),
        axis.text.y = element_text(size=7, hjust = 1),
        legend.position = "none") +
  
  # Little adjustments and labels
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap Covid')





##Conflicts
Q3.3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))
str(Q3.3)

#Correlations between variables
Q3.3_Correlation <- Q3.3 %>% 
  select(overallscore:maxintensity)

#Calculate correlation matrix
Q3.3_corr_matrix <- cor(Q3.3_Correlation, use = "everything")
print(Q3.2_corr_matrix)

## Heatmap

# Create p-value matrix
cor_matrix <- matrix(nrow = ncol(Q3.3_corr_matrix), ncol = ncol(Q3.3_corr_matrix))
p_matrix <- matrix(nrow = ncol(Q3.3_corr_matrix), ncol = ncol(Q3.3_corr_matrix))
rownames(cor_matrix) <- colnames(Q3.3_corr_matrix)
rownames(p_matrix) <- colnames(Q3.3_corr_matrix)
colnames(cor_matrix) <- colnames(Q3.3_corr_matrix)
colnames(p_matrix) <- colnames(Q3.3_corr_matrix)


# Calculate p-values
for (i in 1:ncol(Q3.3_corr_matrix)) {
  for (j in 1:ncol(Q3.3_corr_matrix)) {
    # Exclude missing values before performing the correlation test
    non_missing_rows <- complete.cases(Q3.3_corr_matrix[, c(i, j)])
    
    if (sum(non_missing_rows) > 1) {  # Check if there are enough non-missing observations
      test_result <- cor.test(Q3.3_corr_matrix[non_missing_rows, i], 
                              Q3.3_corr_matrix[non_missing_rows, j])
      cor_matrix[i, j] <- test_result$estimate
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- NA  # Set to NA if not enough non-missing observations
    }
  }
}

# Melt for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(Q3.3_corr_matrix)))

# Combine the datasets
plot_data <- cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create the ggplot2 plot
ggplot(data = plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  
  # Add text labels for correlation values and color based on significance
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1, size = 2.5) +
  
  # Customize the color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  
  # Customize the color legend for significance
  scale_color_manual(values = c("black", "turquoise")) + # black when significant, yellow if not
  
  # Adjust the appearance of the plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7, hjust = 1),
        axis.text.y = element_text(size=7, hjust = 1),
        legend.position = "none") +
  
  # Little adjustments and labels
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatma Conflicts')

