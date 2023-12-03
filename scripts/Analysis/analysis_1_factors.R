#### Question 1 ####

data_question1 <- read.csv(here("scripts","data","data_question1.csv"))

##### Check first the influence of our overall variables over our sdg goals #####

# retake correlations EDA, then select only correlations related to goals

Correlation_overall <- data_question1 %>% 
  select(population:ef_regulation)

cor_matrix <- cor(Correlation_overall, use = "everything")
print(cor_matrix[2:18,])

cor_melted <- melt(cor_matrix[2:18,])

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

#partie par partie 

corr_matrix <- cor(data_question1[7:40])
p_matrix2 <- matrix(nrow = ncol(data_question1[7:40]), ncol = ncol(data_question1[7:40]))
for (i in 1:ncol(data_question1[7:40])) {
  for (j in 1:ncol(data_question1[7:40])) {
    test_result <- cor.test(data_question1[7:40][, i], data_question1[7:40][, j])
    p_matrix2[i, j] <- test_result$p.value
  }
}

corr_matrix[which(p_matrix2 > 0.05)] <- NA
melted_corr_matrix_goals <- melt(corr_matrix[2:18,2:18])

ggplot(melted_corr_matrix_goals, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(!is.na(value), sprintf("%.2f", value), '')),
            color = "black", size = 2) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Goals', y = 'Goals',
       title = 'Correlations Heatmap between goals')

#pourquoi difference avec heatmap Lodrik? --> il a seulement pris les SDG goals dans sa regression je pense

melted_corr_matrix_GVar <- melt(corr_matrix[19:34,2:18])

ggplot(melted_corr_matrix_GVar, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(!is.na(value), sprintf("%.2f", value), '')),
            color = "black", size = 2) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Goals', y = 'Goals',
       title = 'Correlations Heatmap between goals and our other variables')

melted_corr_matrix_Var <- melt(corr_matrix[19:34,19:34])

ggplot(melted_corr_matrix_Var, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(!is.na(value), sprintf("%.2f", value), '')),
            color = "black", size = 1.7) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Goals', y = 'Goals',
       title = 'Correlations Heatmap between other variables than SDG goals')

# grid.arrange(HMgoals, HMgoalvar,HMvar, ncol = 1, nrow = 3)

#avant reg -> essayer de retirer multicolinearité en enlevant les variables trop dépendantes

correlation_overall_matrix <- cor(Correlation_overall, use = "everything")
high_cor_pairs <- which(abs(correlation_overall_matrix) >= 0.8, arr.ind = TRUE)

for (i in 1:nrow(high_cor_pairs)) {
  row <- high_cor_pairs[i, "row"]
  col <- high_cor_pairs[i, "col"]
  
  # Avoiding duplicate pairs and diagonal elements
  if (row < col) {
    cat(sprintf("Variables: %s and %s, Correlation: %f\n", 
                names(Correlation_overall)[row], names(Correlation_overall)[col], correlation_overall_matrix[row, col]))
  }
}

# List of high-correlation pairs
correlation_pairs <- list(
  c("overallscore", "goal1"), c("overallscore", "goal3"), c("goal1", "goal3"),
  c("overallscore", "goal4"), c("goal1", "goal4"), c("goal3", "goal4"),
  c("overallscore", "goal6"), c("goal3", "goal6"),
  c("overallscore", "goal7"), c("goal1", "goal7"), c("goal3", "goal7"), c("goal4", "goal7"), c("goal6", "goal7"),
  c("overallscore", "goal9"), c("goal3", "goal9"),
  c("overallscore", "goal11"), c("goal3", "goal11"), c("goal4", "goal11"), c("goal6", "goal11"), c("goal7", "goal11"),
  c("goal9", "goal12"), c("goal12", "goal13"),
  c("overallscore", "goal16"), c("goal12", "goal16"),
  c("goal9", "GDPpercapita"), c("goal12", "GDPpercapita"),
  c("overallscore", "internet_usage"), c("goal9", "internet_usage"),
  c("goal12", "pf_law"), c("goal16", "pf_law"),
  c("pf_religion", "pf_assembly"), c("pf_assembly", "pf_expression"),
  c("goal9", "ef_legal"), c("goal12", "ef_legal"), c("goal16", "ef_legal"), c("pf_law", "ef_legal")
)

# Flatten the list and count the frequency of each variable
flattened_list <- unlist(correlation_pairs)
frequency_count <- table(flattened_list)
variables_to_remove <- c()

for (pair in correlation_pairs) {
  # Select the variable that appears more frequently for removal
  if (frequency_count[pair[1]] > frequency_count[pair[2]]) {
    variables_to_remove <- c(variables_to_remove, pair[1])
  } else if (frequency_count[pair[1]] < frequency_count[pair[2]]) {
    variables_to_remove <- c(variables_to_remove, pair[2])
  } else {
    # If both appear equally, arbitrarily choose one to remove
    variables_to_remove <- c(variables_to_remove, pair[1])
  }
}
variables_to_remove <- unique(variables_to_remove)
variables_to_remove <- sort(variables_to_remove)

print(variables_to_remove) 
#"ef_legal"     "goal11"       "goal12"       "goal16"       "goal3"        "goal4"        "goal7"       
#"goal9"        "overallscore" "pf_assembly" 



# regressions of every variable on each goals

# reg_goal1_all <- lm(goal1 ~ goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal2_all <- lm(goal2 ~ goal1 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal3_all <- lm(goal3 ~ goal1 + goal2 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal4_all <- lm(goal4 ~ goal1 + goal2 + goal3 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal5_all <- lm(goal5 ~ goal1 + goal2 + goal3 + goal4 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal6_all <- lm(goal6 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal7_all <- lm(goal7 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal8_all <- lm(goal8 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal9_all <- lm(goal9 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal10_all <- lm(goal10 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal11_all <- lm(goal11 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal12_all <- lm(goal12 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal13_all <- lm(goal13 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal15_all <- lm(goal15 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal16_all <- lm(goal16 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
# reg_goal17_all <- lm(goal17 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)

#when I get rid of variables highly correlated : 

reg_goal1_all_new <- lm(goal1 ~ goal2 + goal5 + goal6 +  goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal2_all_new <- lm(goal2 ~ goal1 + goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal3_all_new <- lm(goal3 ~ goal1 + goal2 + goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal4_all_new <- lm(goal4 ~ goal1 + goal2 + goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal5_all_new <- lm(goal5 ~ goal1 + goal2 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal6_all_new <- lm(goal6 ~ goal1 + goal2 + goal5 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal7_all_new <- lm(goal7 ~ goal1 + goal2 + goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal8_all_new <- lm(goal8 ~ goal1 + goal2 + goal5 + goal6  + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal9_all_new <- lm(goal9 ~ goal1 + goal2 + goal5 + goal6  + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal10_all_new <- lm(goal10 ~ goal1 + goal2 + goal5 + goal6 + goal8 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal11_all_new <- lm(goal11 ~ goal1 + goal2 + goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal12_all_new <- lm(goal12 ~ goal1 + goal2 + goal5 + goal6  + goal8 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal13_all_new <- lm(goal13 ~ goal1 + goal2 + goal5 + goal6  + goal8 + goal10  + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal15_all_new <- lm(goal15 ~ goal1 + goal2 + goal5 + goal6  + goal8 + goal10  + goal13 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal16_all_new <- lm(goal16 ~ goal1 + goal2 + goal5 + goal6  + goal8 + goal10  + goal13 + goal15 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal17_all_new <- lm(goal17 ~ goal1 + goal2 + goal5 + goal6  + goal8 + goal10 + goal13 + goal15  + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)

#Voir histogram 2D HFI/SDG Scores

summary_reg_goal1_all <- summary(reg_goal1_all)
coef_table_1 <- summary_reg_goal1_all$coefficients
significant_coefs_1 <- coef_table_1[coef_table_1[, "Pr(>|t|)"] < 0.05, ]

summary_reg_goal2_all <- summary(reg_goal2_all)
coef_table_2 <- summary_reg_goal1_all$coefficients
significant_coefs_2 <- coef_table_2[coef_table_2[, "Pr(>|t|)"] < 0.05, ]

summary_reg_goal3_all <- summary(reg_goal3_all)
coef_table_3 <- summary_reg_goal3_all$coefficients
significant_coefs_3 <- coef_table_3[coef_table_3[, "Pr(>|t|)"] < 0.05, ]

summary_reg_goal4_all <- summary(reg_goal4_all)
coef_table_4 <- summary_reg_goal4_all$coefficients
significant_coefs_4 <- coef_table_4[coef_table_4[, "Pr(>|t|)"] < 0.05, ]


sg1 <- stargazer(significant_coefs_1,
          significant_coefs_2,
          significant_coefs_3,
          #significant_coefs_4,
          title="Impact of variables over SDG goals",
          type="text",
          digits=3)

sg2 <- stargazer(reg_goal5_all,
          reg_goal6_all,
          reg_goal7_all,
          reg_goal8_all,
          title="Impact of variables over SDG goals",
          type="text",
          digits=3)

sg3 <- stargazer(reg_goal9_all,
          reg_goal10_all,
          reg_goal11_all,
          reg_goal12_all,
          title="Impact of variables over SDG goals",
          type="text",
          digits=3)

sg4 <- stargazer(reg_goal13_all,
          reg_goal15_all,
          reg_goal16_all,
          reg_goal17_all,
          title="Impact of variables over SDG goals",
          type="text",
          digits=3)

# merged_html <- paste0(sg1, sg2, sg3, sg4)
# writeLines(merged_html, "merged_tables.html")
# print(merged_html)


vif(reg_goal1_all_new)
vif(reg_goal2_all_new) #we have big multicollinearity problems between goal 1 & numerous variables. In addition, vif of goal2 over goal1 is low, compared to vif goal1 over goal2.

##### try to reduce dimensionality of linear model: AIC criteria, backward selection #####

#for reg1
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal1_all_new, scope=list(lower=nullmod, upper=reg_goal1_all_new), direction="backward") #vif(selmod) -> still high vif
summary(selmod)
vif(selmod) #only high vif for pf_law -> get rid of it? 
reg_goal1_all_new <- lm(goal1 ~ goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + 
                          unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + 
                          internet_usage + pf_movement + pf_religion + ef_government + 
                          ef_money + ef_trade + ef_regulation, data = data_question1)
selmod <- step(reg_goal1_all_new, scope=list(lower=nullmod, upper=reg_goal1_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod)
reg_goal1_all_new <- lm(goal1 ~ goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + 
                          unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + 
                          internet_usage + pf_movement + pf_religion + ef_government + 
                          ef_money + ef_trade + ef_regulation, data = data_question1)
#for reg2
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal2_all_new, scope=list(lower=nullmod, upper=reg_goal2_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) 
reg_goal2_all_new <- lm(goal2 ~ goal5 + goal6 + goal8 + goal10 + goal13 + goal15 + goal17 + 
                          unemployment.rate + MilitaryExpenditurePercentGDP + internet_usage + 
                          pf_law + pf_security + pf_movement + pf_identity + ef_money + 
                          ef_trade + ef_regulation + population, data = data_question1)
#reg5
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal5_all_new, scope=list(lower=nullmod, upper=reg_goal5_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #goal6 pf_law
reg_goal5_all_new <- lm(goal5 ~ goal1 + goal2 + goal10 + goal13 + goal15 + goal17 + 
                          unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + 
                          internet_usage + pf_law + pf_security + pf_movement + pf_religion + 
                          pf_expression + pf_identity + ef_government + ef_money + 
                          ef_trade + ef_regulation, data = data_question1)
selmod <- step(reg_goal5_all_new, scope=list(lower=nullmod, upper=reg_goal5_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #pf_law
reg_goal5_all_new <- lm(goal5 ~ goal1 + goal2 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + 
                          GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + 
                          pf_security + pf_movement + pf_religion + pf_identity + 
                          ef_government + ef_money + ef_trade + ef_regulation, data = data_question1)
selmod <- step(reg_goal5_all_new, scope=list(lower=nullmod, upper=reg_goal5_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) 
reg_goal5_all_new <- lm(goal5 ~ goal1 + goal2 + goal10 + goal13 + goal15 + goal17 + unemployment.rate + 
                          GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + 
                          pf_security + pf_movement + pf_religion + pf_identity + ef_government + 
                          ef_money + ef_regulation, data = data_question1)
#reg6
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal6_all_new, scope=list(lower=nullmod, upper=reg_goal6_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) 
reg_goal6_all_new <- lm(goal6 ~ goal1 + goal2 + goal5 + goal8 + unemployment.rate + GDPpercapita + 
                          MilitaryExpenditurePercentGDP + internet_usage + pf_security + 
                          pf_movement + pf_religion + pf_expression + pf_identity + 
                          ef_government + ef_money + population, data = data_question1)

#reg8
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal8_all_new, scope=list(lower=nullmod, upper=reg_goal8_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #goal6
reg_goal8_all_new <- lm(goal8 ~ goal1 + goal2 + goal13 + goal15 + unemployment.rate + 
                          internet_usage + pf_law + pf_security + pf_movement + pf_religion + 
                          pf_expression + pf_identity + ef_government + ef_trade + 
                          population, data = data_question1)

#reg10
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal10_all_new, scope=list(lower=nullmod, upper=reg_goal10_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #goal6
reg_goal10_all_new <- lm(goal10 ~ goal1 + goal2 + goal5 + goal8 + goal13 + goal15 + 
                           goal17 + unemployment.rate + GDPpercapita + internet_usage + 
                           pf_law + pf_security + pf_movement + pf_religion + pf_expression + 
                           ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)
selmod <- step(reg_goal10_all_new, scope=list(lower=nullmod, upper=reg_goal10_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #pf_law
reg_goal10_all_new <- lm(goal10 ~ goal1 + goal2 + goal5 + goal8 + goal13 + goal15 + goal17 + 
                           unemployment.rate + GDPpercapita + internet_usage + 
                           pf_security + pf_movement + pf_religion + pf_expression + 
                           ef_government + ef_money + ef_trade + ef_regulation + population, data = data_question1)

#reg13
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal13_all_new, scope=list(lower=nullmod, upper=reg_goal13_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) 
reg_goal13_all_new <- lm(goal13 ~ goal1 + goal2 + goal5 + goal8 + goal10 + goal17 + unemployment.rate + 
                           GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + 
                           pf_law + pf_religion + ef_government + ef_regulation, data = data_question1)

#reg15
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal15_all_new, scope=list(lower=nullmod, upper=reg_goal15_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) 
reg_goal15_all_new <- lm(goal15 ~ goal1 + goal2 + goal5 + goal8 + goal10 + goal17 + unemployment.rate + 
                           GDPpercapita + internet_usage + pf_law + pf_security + pf_religion + 
                           pf_expression + pf_identity + ef_government + population, data = data_question1)

#reg16
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal16_all_new, scope=list(lower=nullmod, upper=reg_goal16_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #goal16 ~ goal1 + goal2 + goal5 + goal6 + goal8 + goal10 + goal13 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_regulation + population
#get rid of goal6
reg_goal16_all_new <- lm(goal16 ~ goal1 + goal2 + goal5 + goal8 + goal10 + goal13 + 
                           unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + 
                           internet_usage + pf_law + pf_security + pf_movement + pf_religion + 
                           pf_expression + pf_identity + ef_government + ef_money + 
                           ef_regulation + population, data = data_question1)
selmod <- step(reg_goal16_all_new, scope=list(lower=nullmod, upper=reg_goal16_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod)#pf_law
reg_goal16_all_new <- lm(goal16 ~ goal1 + goal2 + goal5 + goal8 + goal10 + goal13 + unemployment.rate + 
  GDPpercapita + MilitaryExpenditurePercentGDP + pf_security + 
  pf_movement + pf_religion + pf_expression + pf_identity + 
  ef_government + ef_money + ef_regulation + population, data = data_question1)
selmod <- step(reg_goal16_all_new, scope=list(lower=nullmod, upper=reg_goal16_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #pf_law
reg_goal16_all_new <- lm(goal16 ~ goal1 + goal2 + goal5 + goal8 + goal10 + goal13 + unemployment.rate + 
                           GDPpercapita + MilitaryExpenditurePercentGDP + pf_security + 
                           pf_movement + pf_religion + pf_expression + pf_identity + 
                           ef_government + ef_money + ef_regulation + population, data = data_question1)

#reg17
nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal17_all_new, scope=list(lower=nullmod, upper=reg_goal17_all_new), direction="backward") #vif(selmod) -> still high vif
vif(selmod) #goal17 ~ goal1 + goal2 + goal5 + goal10 + goal13 + goal15 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_money + ef_trade + ef_regulation + population
#get rid of pf_law
reg_goal17_all_new <- lm(goal17 ~ goal1 + goal2 + goal5 + goal10 + goal13 + goal15 + unemployment.rate + 
                           GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_security + pf_movement + pf_religion + pf_expression + 
                           pf_identity + ef_government + ef_money + ef_trade + ef_regulation + 
                           population, data=data_question1)

#####what about with overallscore#####

reg_goal_overall <- lm(overallscore ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
stargazer(reg_goal_overall,
          title="Impact of unemployment over goals",
          type='html',
          digits=3)

huxtable <- huxreg(reg_goal_overall)
caption(huxtable) <- "Regressing our Overall SDG Score over our variables"
print(huxtable)

# find a way to regress all our variables and only display interesting/pertinent informations in the regression tableau
# could check the correlations with overallscore first, then investigate per goal interesting correlations with specific variables.

#### check the evolution of correlations by region ####

Re_SubAfrica <- data_question1 %>%
  filter(data_question1[, 6]=='Sub-Saharan Africa')

reg_goal_overall_Re_SubAfrica <- lm(overallscore ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Re_SubAfrica)
try1 <- stargazer(reg_goal_overall_Re_SubAfrica,
          title="Impact of variables over overall SDG goals",
          type='text',
          digits=3)

Re_EastEU <- data_question1 %>%
  filter(data_question1[, 6]=='Eastern Europe')

reg_goal_overall_Re_EastEU <- lm(overallscore ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = Re_EastEU)
try2 <- stargazer(reg_goal_overall_Re_EastEU,
          title="Impact of variables over overall SDG goals",
          type='text',
          digits=3)

Re_MidNorthAfrica <- data_question1 %>%
  filter(data_question1[, 6]=='Middle East & North Africa')

Re_LatinAmerica <- data_question1 %>%
  filter(data_question1[, 6]=='Latin America & the Caribbean')

Re_CentralAsia <- data_question1 %>%
  filter(data_question1[, 6]=='Caucasus & Central Asia')

Re_Oceania <- data_question1 %>%
  filter(data_question1[, 6]=='Oceania')

Re_WestEU <- data_question1 %>%
  filter(data_question1[, 6]=='Western Europe')

Re_SouthAsia <- data_question1 %>%
  filter(data_question1[, 6]=='South Asia')

Re_SubAfrica <- data_question1 %>%
  filter(data_question1[, 6]=='Sub-Saharan Africa')

Re_NorthAmerica <- data_question1 %>%
  filter(data_question1[, 6]=='North America')

Re_EastAsia <- data_question1 %>%
  filter(data_question1[, 6]=='East Asia')

merged <- paste0(try1, try2)
writeLines(merged, "merged_tables.html")
print(merged)

##### geom point #####

#print values with correlation > 0.8 and make plots

# Filtering values where the absolute value is greater than 0.8
highcorrelations <- melted_corr_matrix_GVar %>% filter(value > 0.8)

ggplot(data_question1, aes(internet_usage, overallscore)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Scarplot overallscore and internet usage")

ggplot(data_question1, aes(GDPpercapita, goal9)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Scarplot overallscore and internet usage")

ggplot(data_question1, aes(internet_usage,goal9)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Scarplot overallscore and internet usage")

ggplot(data_question1, aes(ef_legal,goal9)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Scarplot overallscore and internet usage")

ggplot(data_question1, aes(pf_law, goal16)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Scarplot overallscore and internet usage")

ggplot(data_question1, aes(ef_legal, goal16)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Scarplot overallscore and internet usage")




HFI <- data_question1 %>% select(pf_law, pf_security, pf_movement, pf_religion,
                                 pf_assembly, pf_expression, pf_identity, ef_government,
                                 ef_legal, ef_money, ef_trade, ef_regulation)
SDGGoals <- data_question1 %>% select(goal1, goal2, goal3, goal4,
                                      goal5, goal6, goal7, goal8,
                                      goal9, goal10, goal11, goal12,
                                      goal13, goal15, goal16, goal17)

variable_names <- c("pf_law", "pf_security", "pf_movement", "pf_religion",
                    "pf_assembly", "pf_expression", "pf_identity", "ef_government",
                    "ef_legal", "ef_money", "ef_trade", "ef_regulation",
                    "goal1", "goal2", "goal3", "goal4",
                    "goal5", "goal6", "goal7", "goal8",
                    "goal9", "goal10", "goal11", "goal12",
                    "goal13", "goal15", "goal16", "goal17")
long_data <- data_question1 %>%
  pivot_longer(cols = variable_names, names_to = "variable", values_to = "value") %>%
  mutate(type = ifelse(variable %in% c("pf_law", "pf_security", "pf_movement", "pf_religion",
                                       "pf_assembly", "pf_expression", "pf_identity", "ef_government",
                                       "ef_legal", "ef_money", "ef_trade", "ef_regulation"), "HFI", "SDG"))

ggplot(long_data, aes(x = variable, y = value, color = type)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x labels for clarity
  labs(x = "SDG and HFI Variables", y = "Value", color = "Variable Type")

hfi_variables <- c("pf_law", "pf_security", "pf_movement", "pf_religion",
                   "pf_assembly", "pf_expression", "pf_identity", "ef_government",
                   "ef_legal", "ef_money", "ef_trade", "ef_regulation")

sdg_variables <- c("goal1", "goal2", "goal3", "goal4",
                   "goal5", "goal6", "goal7", "goal8",
                   "goal9", "goal10", "goal11", "goal12",
                   "goal13", "goal15", "goal16", "goal17")

# Function to create a scatter plot for a pair of variables
plot_pair <- function(sdg_var, hfi_var) {
  ggplot(data, aes_string(x = sdg_var, y = hfi_var)) +
    geom_point() +
    labs(x = sdg_var, y = hfi_var)
}

# Create a list to store plots
plot_list <- list()
plot_index <- 1

# Generate plots for each combination of SDG and HFI variables
for (sdg_var in sdg_variables) {
  for (hfi_var in hfi_variables) {
    plot_list[[plot_index]] <- plot_pair(sdg_var, hfi_var)
    plot_index <- plot_index + 1
  }
}

# Arrange the plots in a grid
do.call(grid.arrange, c(plot_list, ncol = length(hfi_variables)))








Data_geompoint <- c(HFI,SDGGoals)

ggplot(Data_geompoint, aes(HFI,SDGGoals)) +
  geom_point(aes(color = class)) + geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency decreases with engine size",
       subtitle = str_wrap("Two seaters (sports cars) are an exception
because of their light weight", width = 45),
       caption = "Data from fueleconomy.gov")

ggplot(data_question1, aes(goal1, pf_law)) +
  geom_point()+ geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency decreases with engine size",
       subtitle = str_wrap("Two seaters (sports cars) are an exception
because of their light weight", width = 45),
       caption = "Data from fueleconomy.gov")

Data_geompoint <- left_join(HFI, SDGGoals, by = "X")

# Use ggplot with specific columns
ggplot(Data_geompoint, aes(x = goal1:goal17, y = pf_law:ef_regulation)) +
  geom_point(aes(color = class)) + geom_smooth(se = FALSE) +
  labs(title = "Your Title",
       subtitle = "Your Subtitle",
       caption = "Your Caption")
