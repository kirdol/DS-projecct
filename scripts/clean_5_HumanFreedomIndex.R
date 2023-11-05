data <- read.csv(here("scripts", "data", "human-freedom-index-2022.csv"))

# install.packages("viridis")
# install.packages("viridisLite")

library("dplyr")
library("tibble")
library("tidyr")
library("ggplot2")
library("viridis")
library("viridisLite")

#data in tibble 
datatibble <- tibble(data)

# erasing useless columns to keep only the general ones. 

datatibble <- select(datatibble, year, countries, region, hf_score, pf_rol, pf_ss, pf_movement, pf_religion, pf_assembly, pf_expression, pf_identity, pf_score, ef_government, ef_legal, ef_money, ef_trade, ef_regulation, ef_score)



##### VISUALIZATION ##### 



#Find NA percentage per country per variable 

na_percentage_by_country <- datatibble %>%
  group_by(countries) %>%
  summarise(across(everything(), ~mean(is.na(.))*100))

na_long <- na_percentage_by_country %>%
  pivot_longer(
    cols = -countries,
    names_to = "Variable",
    values_to = "NA_Percentage"
  )

# Order the countries with between 50 and 100 of NA values 

na_long <- na_long %>%
  group_by(countries) %>%
  mutate(Count_NA_50_100 = sum(NA_Percentage >= 50 & NA_Percentage <= 100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Count_NA_50_100))

# Now, visualize

heatmap_ordered <- ggplot(na_long, aes(x = reorder(countries, -Count_NA_50_100), y = Variable)) +
  geom_tile(aes(fill = NA_Percentage), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    title = "Heatmap of NA Percentages per Country and Variable",
    x = "Country",
    y = "Variable",
    fill = "NA Percentage"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 7)
  )

# If you want to save this plot
ggsave("heatmap_ordered.png", heatmap_ordered, width = 12, height = 8)

# Now try to check for which year these missing values take place, in order to just erase one year instead of a country



###### END VISUALIZATION ######



#Checking the number of variables per countries where NA values percentage >=50%

country_na_count <- na_long %>%
  filter(NA_Percentage >= 50) %>%
  group_by(countries) %>%
  summarise(Count_NA_50_100 = n()) %>%
  arrange(desc(Count_NA_50_100))

print(country_na_count)


#D5_0_Human_freedom_index --> name final tibble

# How to select the NA values of a specific variable, arranged.
# data <- data[is.na(data$column)]
# arrange(select(data, column)) |> print(n = ...) 
# length(unique(datatibble$country))


#IS THERE A POSSIBLE WAY TO FIND MISSING VALUES PER COUNTRY PER YEAR FOR EACH VARIABLE?
#HERE DONE PER COUNTRY PER VARIABLE 
#I DON'T WANT TO ERASE A COUNTRY, JUST THE YEAR OF A COUNTRY

# Only keep the years after 2000 and before 2022
datatibble <- datatibble %>%
  filter(year >= 2000 & year <= 2022)

# Rename the column coutries into country to match the other datbases
names(datatibble)[names(datatibble) == "countries"] <- "country"

# Make sure the encoding of the country names are UTF-8
datatibble$country <- iconv(datatibble$country, to = "UTF-8", sub = "byte")

# standardize country names
datatibble <- datatibble %>%
  mutate(country = countrycode(country, "country.name", "country.name"))

# Merge by country name
datatibble <- datatibble %>%
  left_join(D1_0_SDG_country_list, by = "country")

# Keep only the countries that are in our main dataset

datatibble <- datatibble %>% filter(code %in% list_country)
(length(unique(datatibble$code)))

# See which ones are missing

list_country_free <- c(unique(datatibble$code))
(missing <- setdiff(list_country, list_country_free))

# Turkey was missing but present in the initial database (it was a problem when stadardizing the country names of D1_0SDG_country_list that we corrected) and the other missing countries are:"AFG" "CUB" "MDV" "STP" "SSD" "TKM" "UZB" 

D5_0_Human_freedom_index <- datatibble
