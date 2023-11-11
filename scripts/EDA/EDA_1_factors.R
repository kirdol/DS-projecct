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






