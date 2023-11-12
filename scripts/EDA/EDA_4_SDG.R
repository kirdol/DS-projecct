Q4 <- read.csv(here("scripts", "data", "data_question24.csv"))

sorted_data <- arrange(Q4, year, country)

Q4_SDG_only <- select(sorted_data, goal1:goal17)

summary(Q4_SDG_only)

melted_data <- melt(is.na(sorted_data))



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
