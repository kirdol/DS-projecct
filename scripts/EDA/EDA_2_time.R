##### How has the adoption of the SDGs in 2015 influenced the achievement of SDGs? #####

data_question2 <- read.csv(here("scripts", "data", "data_question24.csv"), sep=",")

### Big map

library(sf)
library(rnaturalearth)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data with the world map data
data0 <- merge(world, data_question2, by.x = "iso_a3", by.y = "code", all.x = TRUE)

data0 %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "lightgrey") +
  geom_sf(aes(fill = overallscore), color = NA) +
  scale_fill_gradientn(
    colors = c("darkred", "orange", "yellow", "darkgreen"),
    values = scales::rescale(c(0, 0.25, 0.5, 1)),
    name = "Overall Score",
    na.value = NA
  ) +
  coord_sf(datum = NA) +
  theme_minimal()

### First, look at the evolution of SDG achievement overall score over time

data1 <- data_question2 %>% group_by(year) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))

data2 <- data_question2 %>% group_by(year, continent) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))

data3 <- data_question2 %>% group_by(year, region) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))

ggplot(data1) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year), color="blue", lwd=1) +
  scale_y_continuous(limits = c(0, 100)) 

ggplot(data2) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year, color=continent), lwd=1) +
  scale_y_continuous(limits = c(0, 100)) 

ggplot(data3) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year, color=region), lwd=1) +
  scale_y_continuous(limits = c(0, 100))

### Second, look at the evolution of SDG achievement scores (16) over time

# Evolution over time of the SDGs' achievement: mean over all countries

data4 <- data_question2 %>%
  group_by(year) %>%
  summarise(across(starts_with("goal"), mean, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("goal"), names_to = "goal", values_to = "mean_value")

color_palette <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta", "yellow", "darkgreen", "darkblue", "darkred", "darkorange", "darkcyan")

ggplot(data = data4) +
  geom_line(mapping = aes(x = year, y = mean_value, color = goal), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  guides(
    color = guide_legend(
      ncol = 2,        # Number of columns
      title.position = "top",  # Position of the legend title
      title.hjust = 0.5        # Horizontal justification of the legend title
    )
  )

p <- ggplot(data = data4) +
  geom_line(mapping = aes(x = year, y = mean_value, color = goal), size = 0.7) +
  geom_point(mapping = aes(x = year, y = mean_value, color = goal), size = 1) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Evolution of the mean SDG achievement scores across the world",
       y = "Mean SDG Scores",
       x = "Year"
  ) +
  guides(
    color = guide_legend(
      ncol = 2,
      title.position = "top",
      title.hjust = 0.5
    )
  )

# Convert ggplot object to plotly
p <- plot_ly(
  ggplotly(p),
  source = "plotly-slider"
) %>%
  layout(
    sliders = list(
      list(
        currentvalue = list(
          prefix = "Selected Year Range: ",
          visible = TRUE,
          xanchor = "right",
          font = list(color = "#888", size = 12)
        ),
        steps = list(
          list(
            args = list("visible", c(TRUE, TRUE, TRUE)),
            label = "All",
            method = "restyle"
          ),
          list(
            args = list("visible", c(FALSE, TRUE, FALSE)),
            label = "2000-2010",
            method = "restyle"
          ),
          list(
            args = list("visible", c(FALSE, FALSE, TRUE)),
            label = "2011-2020",
            method = "restyle"
          )
        )
      )
    )
  )

# Print the plotly object
print(p)

ggplot(data = data4) +
  geom_line(mapping = aes(x = year, y = mean_value), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ goal, nrow = 4)

# Evolution over time of the SDGs' achievement: mean by continent
data5 <- data_question2 %>%
  group_by(year, continent) %>%
  summarise(across(starts_with("goal"), mean, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("goal"), names_to = "goal", values_to = "mean_value")

color_palette <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta", "yellow", "darkgreen", "darkblue", "darkred", "darkorange", "darkcyan")

ggplot(data = data5) +
  geom_line(mapping = aes(x = year, y = mean_value, color=continent), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ goal, nrow = 4)

# Evolution over time of the SDGs' achievement: mean by region
data6 <- data_question2 %>%
  group_by(year, region) %>%
  summarise(across(starts_with("goal"), mean, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("goal"), names_to = "goal", values_to = "mean_value")

color_palette <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta", "yellow", "darkgreen", "darkblue", "darkred", "darkorange", "darkcyan")

ggplot(data = data5) +
  geom_line(mapping = aes(x = year, y = mean_value, color=region), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ goal, nrow = 4)

# Evolution over time of the SDGs' achievement: mean by country

### Third, compare the SDG achievement scores before and after 2015 (adoption at the UN)

# Mean comparison 2015-2016

# Mean comparison 2013-15 vs 2016-18 (let them some time to take measures)

