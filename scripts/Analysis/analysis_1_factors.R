#### Question 1 ####

data_question1 <- read.csv(here("scripts","data","data_question1.csv"))

# Check first the influence of our overall variables (except sdg goals) over our sdg goals 

RegTry1 <- lm(goal1~unemployment.rate,data = data_question1)
RegTry2 <- lm(goal2~unemployment.rate,data = data_question1)
stargazer(RegTry1, RegTry2,  
          title="Impact of unemployment over goal 1",
          type='html',
          digits=3)

huxreg(RegTry1,RegTry2)

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

stargazer(reg_goal1_all,
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
          reg_goal17_all,
          title="Impact of unemployment over goals",
          type='html',
          digits=3)

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

#what about with overallscore

reg_goal_overall <- lm(overallscore ~ goal1 + goal2 + goal3 + goal4 + goal5 + goal6 + goal7 + goal8 + goal9 + goal10 + goal11 + goal12 + goal13 + goal15 + goal16 + goal17 + unemployment.rate + GDPpercapita + MilitaryExpenditurePercentGDP + internet_usage + pf_law + pf_security + pf_movement + pf_religion + pf_assembly + pf_expression + pf_identity + ef_government + ef_legal + ef_money + ef_trade + ef_regulation, data = data_question1)
stargazer(reg_goal_overall,
          title="Impact of unemployment over goals",
          type='html',
          digits=3)

huxreg(reg_goal_overall)
#careful, regressions with different variables will lead to different correlations for the same test variables over the same Y --> then should I include the influnence of goals between them? 
#find a way to regress all our variables and only display interesting/pertinent informations in the regression tableau
# do not forget to conduct VIF for covariance problems/ other controlling methods

# then check the influence of sdg goals over other goals 


