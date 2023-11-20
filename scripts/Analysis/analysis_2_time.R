### Question 1 analysis ###
data_question2 <- read.csv(here("scripts","data","data_question24.csv"))

binary2015 <- data_question2 %>% 
  mutate(after2015 = ifelse(year > 2015, 1, 0))

reg2.1 <- lm(overallscore ~ after2015, data=binary2015, year>=2010)
summary(reg2.1)

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
