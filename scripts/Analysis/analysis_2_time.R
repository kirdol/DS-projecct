### Question 1 analysis ###
data_question2 <- read.csv(here("scripts","data","data_question24.csv"))

binary2015 <- data_question2 %>% 
  mutate(after2015 = ifelse(year > 2015, 1, 0))

reg2.1 <- lm(overallscore ~ after2015, data=binary2015, year>=2010)
summary(reg2.1)

library(plm)
# Create a panel data object
panel_data <- pdata.frame(binary2015, index = c("country", "year")) %>% filter(year==2015|year==2016)

# Run the difference-in-differences model
did_model <- plm(overallscore ~ after2015 + year + after2015:year, 
                 data = panel_data,
                 model = "within")
summary(did_model)

reg2.3 <- lm(overallscore ~ after2015 + after2015:year, data=binary2015, year>=2010)
summary(reg2.3)

ggplot(data = binary2015, aes(x = year, y = overallscore)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)
  
ggplot(data = binary2015, aes(x = year, y = goal1)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)

ggplot(data = binary2015, aes(x = year, y = goal2)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)

ggplot(data = binary2015, aes(x = year, y = goal3)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)

ggplot(data = binary2015, aes(x = year, y = goal4)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)

ggplot(data = binary2015, aes(x = year, y = goal5)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)

ggplot(data = binary2015, aes(x = year, y = goal6)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5))

ggplot(data = binary2015, aes(x = year, y = goal7)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)

ggplot(data = binary2015, aes(x = year, y = goal8)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)
