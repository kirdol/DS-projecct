### Question 2 analysis ###
data_question2 <- read.csv(here("scripts","data","data_question24.csv"))

# Create a new column (binary variable) with value 1 if the year is after 2015 and zero otherwise. 
binary2015 <- data_question2 %>% 
  mutate(after2015 = ifelse(year > 2015, 1, 0)) %>%
  filter(as.numeric(year)>=2009)

# Simple linear regression of the overall score on the variables "after2015"
reg2.1 <- lm(overallscore ~ after2015, data=binary2015)
summary(reg2.1)

library(plm)
# Create a panel data object
panel_data <- pdata.frame(binary2015, index = c("country", "year"))

# Run the difference-in-differences model to take into account the general evolution over the years
reg2.2 <- plm(overallscore ~ after2015 + year + after2015:year, 
                 data = panel_data,
                 model = "within")
summary(reg2.2)

# taking into account the years in a multiple regression
reg2.3 <- lm(overallscore ~ after2015 + as.factor(year), data=binary2015)
summary(reg2.3)

# controlling for the region
reg2.4 <- lm(overallscore ~ after2015 + as.factor(year) + region, data=binary2015)
summary(reg2.4)

# Graphs to show the jump (or not) in 2015
library(plotly)

# Filter data
data_after_2015 <- filter(binary2015, as.numeric(year) > 2015)
data_before_2016 <- filter(binary2015, as.numeric(year) <= 2015)

# Create Plotly graph
graph1 <- plot_ly() %>%
  add_trace(data = data_after_2015, x = ~year, y = ~overallscore, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
  add_trace(data = data_before_2016, x = ~year, y = ~overallscore, type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
  layout(title = "Overallscore Over Time",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Overallscore"))

graph1 <- ggplot(data = binary2015, aes(x = year, y = overallscore)) +
  geom_smooth(data = filter(binary2015, year > 2015), method = "lm", se = FALSE)+
  geom_smooth(data = filter(binary2015, year>=2010 & year <= 2015), aes(col="red"), method = "lm", se = FALSE)
plotly::ggplotly()
  
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
