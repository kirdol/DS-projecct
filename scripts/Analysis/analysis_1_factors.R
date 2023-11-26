#### Question 1 ####

data_question1 <- read.csv(here("scripts","data","data_question1.csv"))
library(car)
##### Check first the influence of our overall variables over our sdg goals #####

# retake correlations EDA, then select only correlations related to goals

Correlation_overall <- data_question1 %>% 
  select(population:ef_regulation)

cor_matrix <- cor(Correlation_overall, use = "everything")
print(cor_matrix[2:18,])

# regressions of every variable on each goals

reg_goal1_all <- lm(goal1 ~ goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal2_all <- lm(goal2 ~ goal1 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal3_all <- lm(goal3 ~ goal1 + goal2 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal4_all <- lm(goal4 ~ goal1 + goal2 + goal3 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal5_all <- lm(goal5 ~ goal1 + goal2 + goal3 + goal4 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal6_all <- lm(goal6 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal7_all <- lm(goal7 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal8_all <- lm(goal8 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal9_all <- lm(goal9 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal10_all <- lm(goal10 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal11_all <- lm(goal11 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal12_all <- lm(goal12 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal13_all <- lm(goal13 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal15_all <- lm(goal15 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal16_all <- lm(goal16 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
reg_goal17_all <- lm(goal17 ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)

sg1 <- stargazer(reg_goal1_all,
          reg_goal2_all,
          reg_goal3_all,
          reg_goal4_all,
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
