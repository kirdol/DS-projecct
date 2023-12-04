### Question 2 analysis ###
data_question2 <- read.csv(here("scripts","data","data_question24.csv"))
data_question2 <- data_question2 %>% select(-X)

binary2015 <- data_question2 %>% 
  group_by(code) %>%
  mutate(across(5:21, ~ . - dplyr::lag(.), .names = "diff_{.col}")) %>%
  ungroup()

# Create a new column (binary variable) with value 1 if the year is after 2015 and zero otherwise. 
binary2015 <- binary2015 %>% 
  mutate(after2015 = ifelse(year > 2015, 1, 0)) %>%
  filter(as.numeric(year)>=2009)

###### Distribution of score difference #####
# histogram of difference in scores between years
unique_years <- unique(binary2015$year)
library(viridis)
plot_ly() %>%
  add_trace(
    type = "histogram", 
    data = binary2015, 
    x = ~diff_overallscore[year == 2009],
    marker = list(color = "lightgreen")
  ) %>%
  layout(
    title = "Distribution of SDG evolution",
    xaxis = list(title = "Year difference SDG score", range = c(-3, 3)),
    yaxis = list(title = "Frequency", range = c(0, 40)),
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

top_n_values <- 5

# Test with ggpot2
custom_colors <- c("blue", "darkblue", "cyan", "green", "darkgreen", "lightgreen", "lightblue","turquoise", "lightgrey", "darkgrey")
library(patchwork)

# Get unique regions in the dataset
unique_regions <- unique(binary2015$region)

# Create a color dictionary mapping each region to a specific color
region_colors <- setNames(custom_colors[1:length(unique_regions)], unique_regions)

# Create a common legend manually
legend_data <- data.frame(region = unique_regions)
legend_plot <- ggplot(legend_data, aes(x = region, fill = region)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = region_colors) +  # Use the specified colors
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(angle = 0, hjust = 1)  # Adjust hjust to place text on the right
  ) +
  coord_flip()

plots <- list()

for (year in unique_years) {
  top_countries <- binary2015[binary2015$year == year, ] %>%
    arrange(desc(year), desc(diff_overallscore)) %>%
    head(n = top_n_values)%>%
    select(c(year, country, diff_overallscore, region))
  
  print(top_countries$diff_overallscore)
  print(top_countries$region)
  
  plot <- ggplot(data = top_countries, mapping = aes(x = country, y = diff_overallscore, fill = region)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = region_colors) +  # Use the specified colors
    labs(title = paste("Year", year), x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none") + 
    scale_y_continuous(limits = c(0, 3))
  
  plots[[as.character(year)]] <- plot
}

# Arrange the plots in a 4x4 grid using patchwork
wrap_plots(plots, ncol = 5)
legend_plot

# Best improvements
data_long <- binary2015 %>%
  pivot_longer(cols = c(starts_with("diff_goal"), "diff_overallscore"),
               names_to = "goal", values_to = "improvement") %>%
  group_by(goal) %>%
  top_n(20, wt = improvement) %>%
  ungroup()

plot_ly() %>%
  add_trace(
    type = "bar",
    data = data_long,
    x = ~country[after2015 == 1 & goal == "diff_overallscore"],
    y = ~improvement[after2015 == 1 & goal == "diff_overallscore"],
    legendgroup = "after 2015",
    name = "after 2015",
    marker = list(color = "blue", size = 10),
    showlegend = TRUE
  ) %>%
  add_trace(
    type = "bar",
    x = ~country[after2015 == 0 & goal == "diff_overallscore"],
    y = ~improvement[after2015 == 0 & goal == "diff_overallscore"],
    legendgroup = "before 2015",
    name = "before 2015",
    marker = list(color = "red", size = 10),
    showlegend = TRUE
  ) %>%
  layout(
    title = paste("Top ", top_n_values, " SDG Score evolution"),
    yaxis = list(title = "Year difference SDG score", range = c(0, 50)),
    xaxis = list(title = "Countries", categoryorder = "total ascending"),
    barmode = "stack",
    updatemenus = list(
      list(
        buttons = list(
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_overallscore"],
                  ~improvement[after2015 == 0 & goal == "diff_overallscore"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_overallscore"],
                  ~country[after2015 == 0 & goal == "diff_overallscore"]
                )
              )
            ),
            label = "Overall score",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal1"],
                  ~improvement[after2015 == 0 & goal == "diff_goal1"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal1"],
                  ~country[after2015 == 0 & goal == "diff_goal1"]
                )
              )
            ),
            label = "Goal 1",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal2"],
                  ~improvement[after2015 == 0 & goal == "diff_goal2"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal2"],
                  ~country[after2015 == 0 & goal == "diff_goal2"]
                )
              )
            ),
            label = "Goal 2",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal3"],
                  ~improvement[after2015 == 0 & goal == "diff_goal3"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal3"],
                  ~country[after2015 == 0 & goal == "diff_goal3"]
                )
              )
            ),
            label = "Goal 3",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal4"],
                  ~improvement[after2015 == 0 & goal == "diff_goal4"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal4"],
                  ~country[after2015 == 0 & goal == "diff_goal4"]
                )
              )
            ),
            label = "Goal 4",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal5"],
                  ~improvement[after2015 == 0 & goal == "diff_goal5"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal5"],
                  ~country[after2015 == 0 & goal == "diff_goal5"]
                )
              )
            ),
            label = "Goal 5",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal6"],
                  ~improvement[after2015 == 0 & goal == "diff_goal6"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal6"],
                  ~country[after2015 == 0 & goal == "diff_goal6"]
                )
              )
            ),
            label = "Goal 6",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal7"],
                  ~improvement[after2015 == 0 & goal == "diff_goal7"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal7"],
                  ~country[after2015 == 0 & goal == "diff_goal7"]
                )
              )
            ),
            label = "Goal 7",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal8"],
                  ~improvement[after2015 == 0 & goal == "diff_goal8"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal8"],
                  ~country[after2015 == 0 & goal == "diff_goal8"]
                )
              )
            ),
            label = "Goal 8",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal9"],
                  ~improvement[after2015 == 0 & goal == "diff_goal9"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal9"],
                  ~country[after2015 == 0 & goal == "diff_goal9"]
                )
              )
            ),
            label = "Goal 9",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal10"],
                  ~improvement[after2015 == 0 & goal == "diff_goal10"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal10"],
                  ~country[after2015 == 0 & goal == "diff_goal10"]
                )
              )
            ),
            label = "Goal 10",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal11"],
                  ~improvement[after2015 == 0 & goal == "diff_goal11"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal11"],
                  ~country[after2015 == 0 & goal == "diff_goal11"]
                )
              )
            ),
            label = "Goal 11",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal12"],
                  ~improvement[after2015 == 0 & goal == "diff_goal12"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal12"],
                  ~country[after2015 == 0 & goal == "diff_goal12"]
                )
              )
            ),
            label = "Goal 12",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal13"],
                  ~improvement[after2015 == 0 & goal == "diff_goal13"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal13"],
                  ~country[after2015 == 0 & goal == "diff_goal13"]
                )
              )
            ),
            label = "Goal 13",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal15"],
                  ~improvement[after2015 == 0 & goal == "diff_goal15"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal15"],
                  ~country[after2015 == 0 & goal == "diff_goal15"]
                )
              )
            ),
            label = "Goal 15",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal16"],
                  ~improvement[after2015 == 0 & goal == "diff_goal16"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal16"],
                  ~country[after2015 == 0 & goal == "diff_goal16"]
                )
              )
            ),
            label = "Goal 16",
            method = "restyle"
          ),
          list(
            args = list(
              list(
                y = list(
                  ~improvement[after2015 == 1 & goal == "diff_goal17"],
                  ~improvement[after2015 == 0 & goal == "diff_goal17"]
                ),
                x = list(
                  ~country[after2015 == 1 & goal == "diff_goal17"],
                  ~country[after2015 == 0 & goal == "diff_goal17"]
                )
              )
            ),
            label = "Goal 17",
            method = "restyle"
          )
        )
      )
    )
  )

##### Regressions #####

# Simple linear regression of the overall score on the difference in SDG scores variables "after2015"
reg2.1 <- lm(diff_overallscore ~ after2015, data=binary2015)
reg2.1.1 <- lm(diff_goal1 ~ after2015, data=binary2015)
reg2.1.2 <- lm(diff_goal2 ~ after2015, data=binary2015)
reg2.1.3 <- lm(diff_goal3 ~ after2015, data=binary2015)
reg2.1.4 <- lm(diff_goal4 ~ after2015, data=binary2015)
reg2.1.5 <- lm(diff_goal5 ~ after2015, data=binary2015)
reg2.1.6 <- lm(diff_goal6 ~ after2015, data=binary2015)
reg2.1.7 <- lm(diff_goal7 ~ after2015, data=binary2015)
reg2.1.8 <- lm(diff_goal8 ~ after2015, data=binary2015)
reg2.1.9 <- lm(diff_goal9 ~ after2015, data=binary2015)
reg2.1.10 <- lm(diff_goal10 ~ after2015, data=binary2015)
reg2.1.11 <- lm(diff_goal11 ~ after2015, data=binary2015)
reg2.1.12 <- lm(diff_goal12 ~ after2015, data=binary2015)
reg2.1.13 <- lm(diff_goal13 ~ after2015, data=binary2015)
reg2.1.15 <- lm(diff_goal15 ~ after2015, data=binary2015)
reg2.1.16 <- lm(diff_goal16 ~ after2015, data=binary2015)
reg2.1.17 <- lm(diff_goal17 ~ after2015, data=binary2015)

stargazer(reg2.1, reg2.1.1, reg2.1.2, reg2.1.3, reg2.1.4, reg2.1.5,
          title="Impact of the 2015 adoption of SDG by the UN (Goals 1-5)",
          type='html',
          column.labels=c("Overall score", "Goal 1", "Goal2", "Goal 3", "Goal 4","Goal 5"),
          digits=3)

stargazer(reg2.1.6, reg2.1.7, reg2.1.8, reg2.1.9, reg2.1.10, reg2.1.11,
          title = "Impact of the 2015 adoption of SDG by the UN (Goals 6-11)",
          type = 'html',
          column.labels = c("Goal 6", "Goal 7", "Goal 8", "Goal 9", "Goal 10", "Goal 11"),
          digits = 3)

stargazer(reg2.1.12, reg2.1.13, reg2.1.15, reg2.1.16, reg2.1.17,
          title = "Impact of the 2015 adoption of SDG by the UN (Goals 12-17)",
          type = 'html',
          column.labels = c("Goal 12", "Goal 13", "Goal 15", "Goal 16", "Goal 17"),
          digits = 3)

# Create a panel data object
panel_data <- pdata.frame(binary2015, index = c("country", "year"))

# Run the difference-in-differences model to take into account the general evolution over the years
reg2.2 <- plm(diff_overallscore ~ after2015 + year + after2015:year, 
                 data = panel_data,
                 model = "within")
reg2.2.1 <- plm(diff_goal1 ~ after2015 + year + after2015:year, 
              data = panel_data,
              model = "within")
reg2.2.2 <- plm(diff_goal2 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.3 <- plm(diff_goal3 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.4 <- plm(diff_goal4 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.5 <- plm(diff_goal5 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.6 <- plm(diff_goal6 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.7 <- plm(diff_goal7 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.8 <- plm(diff_goal8 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.9 <- plm(diff_goal9 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.10 <- plm(diff_goal10 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.11 <- plm(diff_goal11 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.12 <- plm(diff_goal12 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.13 <- plm(diff_goal13 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.15 <- plm(diff_goal15 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.16 <- plm(diff_goal16 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")
reg2.2.17 <- plm(diff_goal17 ~ after2015 + year + after2015:year, 
                data = panel_data,
                model = "within")

stargazer(reg2.2, reg2.2.1, reg2.2.2, reg2.2.3, reg2.2.4, reg2.2.5,
          title="Impact of the 2015 adoption of SDG by the UN (Goals 1-5)",
          type='html',
          column.labels=c("Overall score", "Goal 1", "Goal2", "Goal 3", "Goal 4","Goal 5"),
          digits=3,
          omit = "coef",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE
          
)

stargazer(reg2.2.6, reg2.2.7, reg2.2.8, reg2.2.9, reg2.2.10, reg2.2.11,
          title = "Impact of the 2015 adoption of SDG by the UN (Goals 6-11)",
          type = 'html',
          column.labels = c("Goal 6", "Goal 7", "Goal 8", "Goal 9", "Goal 10", "Goal 11"),
          digits = 3,
          omit = "coef",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

stargazer(reg2.2.12, reg2.2.13, reg2.2.15, reg2.2.16, reg2.2.17,
          title = "Impact of the 2015 adoption of SDG by the UN (Goals 12-17)",
          type = 'html',
          column.labels = c("Goal 12", "Goal 13", "Goal 15", "Goal 16", "Goal 17"),
          digits = 3,
          omit = "coef",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

# controlling for the region
reg2.3 <- lm(diff_overallscore ~ after2015 + as.factor(year) + region, data=binary2015)
summary(reg2.3)

##### Graphs to show the jump (or not) in 2015 #####

# Filter data
data_after_2015 <- filter(binary2015, as.numeric(year) >= 2015)
data_before_2016 <- filter(binary2015, as.numeric(year) <= 2015)

plot_ly() %>%
  add_trace(data = data_after_2015, x = ~year, y = ~fitted(lm(overallscore ~ year, data = data_after_2015)), type = 'scatter', mode = 'lines', line = list(color = 'blue'), name = "After 2015") %>%
  add_trace(data = data_before_2016, x = ~year, y = ~fitted(lm(overallscore ~ year, data = data_before_2016)), type = 'scatter', mode = 'lines', line = list(color = 'red'), name = "Before 2015") %>%
  plotly::layout(title = "Different patterns across SDGs before and after 2015",
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
