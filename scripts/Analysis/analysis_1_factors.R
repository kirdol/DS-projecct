#### Question 1 ####

data_question1 <- read.csv(here("scripts","data","data_question1.csv"))
library(car)
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

# regressions of every variable on each goals

reg_goal1_all <- lm(goal1 ~ goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal2_all <- lm(goal2 ~ goal1 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal3_all <- lm(goal3 ~ goal1 + goal2 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal4_all <- lm(goal4 ~ goal1 + goal2 + goal3 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal5_all <- lm(goal5 ~ goal1 + goal2 + goal3 + goal4 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal6_all <- lm(goal6 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal7_all <- lm(goal7 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal8_all <- lm(goal8 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal9_all <- lm(goal9 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal10_all <- lm(goal10 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal11_all <- lm(goal11 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal12_all <- lm(goal12 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal13_all <- lm(goal13 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal15_all <- lm(goal15 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal16_all <- lm(goal16 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)
reg_goal17_all <- lm(goal17 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation + population, data = data_question1)

reg_goal1_all2.0 <- lm(goal1 ~ unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
#Ajouter variable population? 
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

huxreg(reg_goal1_all,
       reg_goal2_all,
       reg_goal3_all,
       reg_goal4_all,
       reg_goal5_all,
       reg_goal6_all,
       reg_goal7_all,
       reg_goal8_all,
       reg_goal9_all,
       reg_goal10_all,
       reg_goal11_all,
       reg_goal12_all,
       reg_goal13_all,
       reg_goal15_all,
       reg_goal16_all,
       reg_goal17_all)

anova(reg_goal1_all)
vif(reg_goal1_all)
vif(reg_goal2_all) #we have big multicollinearity problems between goal 1 & numerous variables. In addition, vif of goal2 over goal1 is low, compared to vif goal1 over goal2.

# try to reduce dimensionality of linear model: AIC criteria, backward selection

nullmod <- lm(goal1 ~ 1, data = data_question1)
selmod <- step(reg_goal1_all, scope=list(lower=nullmod, upper=reg_goal1_all), direction="backward") #vif(selmod) -> still high vif
summary(selmod)

#what about with overallscore

reg_goal_overall <- lm(overallscore ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
stargazer(reg_goal_overall,
          title="Impact of unemployment over goals",
          type='html',
          digits=3)

huxtable <- huxreg(reg_goal_overall)
caption(huxtable) <- "Regressing our Overall SDG Score over our variables"
print(huxtable)

# find a way to regress all our variables and only display interesting/pertinent informations in the regression tableau
# do not forget to conduct VIF for covariance problems/ other controlling methods
# could check the correlations with overallscore first, then investigate per goal interesting correlations with specific variables.

# check the evolution of correlations through years


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
