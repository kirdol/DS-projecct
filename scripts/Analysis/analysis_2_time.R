### Question 2 analysis ###
data_question2 <- read.csv(here("scripts","data","data_question24.csv"))

data_question2 <- data_question2 %>% select(-X)

binary2015 <- data_question2 %>% 
  group_by(code) %>%
  mutate(across(5:21, ~ . - lag(.), .names = "diff_{.col}"))

# Create a new column (binary variable) with value 1 if the year is after 2015 and zero otherwise. 
binary2015 <- binary2015 %>% 
  mutate(after2015 = ifelse(year > 2015, 1, 0)) %>%
  filter(as.numeric(year)>=2009)

# histogram of difference in scores between years

unique_years <- unique(binary2015$year)
plot_ly() %>%
  add_trace(
    type = "histogram", 
    data = binary2015, 
    x = ~diff_overallscore
  ) %>%
  layout(
    title = "Distribution of SDG evolution",
    xaxis = list(title = "Year difference SDG score", range = c(-3,3)),
    yaxis = list(title = "Frequency", range = c(0,40)),
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Year: "),
        steps = lapply(seq_along(unique_years), function(i) {
          year <- unique_years[i]
          list(
            label = as.character(year),
            method = "restyle",
            args = list(
              list(x = list(binary2015$diff_overallscore[binary2015$year == year]))
            )
          )
        })
      )
    )
  )

# Simple linear regression of the overall score on the variables "after2015"
reg2.1 <- lm(diff_overallscore ~ after2015, data=binary2015)

summary(reg2.1)

library(huxtable)
huxreg(reg2.1)

stargazer(reg2.1, 
          title="Impact of the 2015 adoption of SDG by the UN",
          type='html',
          digits=3)

library(plm)
# Create a panel data object
panel_data <- pdata.frame(binary2015, index = c("country", "year"))

# Run the difference-in-differences model to take into account the general evolution over the years
reg2.2 <- plm(diff_overallscore ~ after2015 + year + after2015:year, 
                 data = panel_data,
                 model = "within")
summary(reg2.2)
huxreg(reg2.2)

# taking into account the years in a multiple regression
reg2.3 <- lm(diff_overallscore ~ after2015 + as.factor(year), data=binary2015)
summary(reg2.3)

# controlling for the region
reg2.4 <- lm(diff_overallscore ~ after2015 + as.factor(region), data=binary2015)
summary(reg2.4)

stargazer(reg2.1, reg2.2, reg2.3, reg2.4, 
          title="Impact of the 2015 adoption of SDG by the UN",
          type='html',
          column.labels=c("Simple", "DiD", "+ years", "+ regions"),
          digits=3)

huxreg(reg2.1, reg2.2, reg2.3, reg2.4)


# Graphs to show the jump (or not) in 2015
library(plotly)

# Filter data
data_after_2015 <- filter(binary2015, as.numeric(year) >= 2015)
data_before_2016 <- filter(binary2015, as.numeric(year) <= 2015)

graph1 <- plot_ly() %>%
  add_trace(data = data_after_2015, x = ~year, y = ~fitted(lm(overallscore ~ year, data = data_after_2015)), type = 'scatter', mode = 'lines', line = list(color = 'blue'), name = "After 2015") %>%
  add_trace(data = data_before_2016, x = ~year, y = ~fitted(lm(overallscore ~ year, data = data_before_2016)), type = 'scatter', mode = 'lines', line = list(color = 'red'), name = "Before 2015") %>%
  layout(title = "Different patterns across SDGs before and after 2015",
         xaxis = list(title = "Year"),
         yaxis = list(title = "SDG achievement score", range = c(30, 85)),
         shapes = list(
           list(
             type = 'line',
             x0 = 2015,
             x1 = 2015,
             y0 = 0,
             y1 = 1,
             yref = 'paper',
             line = list(color = 'grey', width = 2, dash = 'dot')
           )
         ),
         updatemenus = list(
           list(
             buttons = list(
               list(
                 args = list("y", list(
                   ~fitted(lm(overallscore ~ year, data = data_after_2015)),
                   ~fitted(lm(overallscore ~ year, data = data_before_2016))
                 )),
                 label = "Overall score",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal1 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal1 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 1",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal2 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal2 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 2",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal3 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal3 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 3",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal4 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal4 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 4",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal5 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal5 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 5",
                 method = "restyle"
               ), 
               list(
                 args = list("y", list(
                   ~fitted(lm(goal6 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal6 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 6",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal7 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal7 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 7",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal8 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal8 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 8",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal9 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal9 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 9",
                 method = "restyle"
               ), 
               list(
                 args = list("y", list(
                   ~fitted(lm(goal10 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal10 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 10",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal11 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal11 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 11",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal12 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal12 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 12",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal13 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal13 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 13",
                 method = "restyle"
               ), 
               list(
                 args = list("y", list(
                   ~fitted(lm(goal15 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal15 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 15",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal16 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal16 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 16",
                 method = "restyle"
               ),
               list(
                 args = list("y", list(
                   ~fitted(lm(goal17 ~ year, data = data_after_2015)),
                   ~fitted(lm(goal17 ~ year, data = data_before_2016))
                 )),
                 label = "Goal 17",
                 method = "restyle"
               )
             )
           )
         )
  )
graph1
