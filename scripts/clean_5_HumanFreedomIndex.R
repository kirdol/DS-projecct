data <- read.csv(here("scripts", "data", "human-freedom-index-2022.csv"))

#data in tibble 
datatibble <- tibble(data)

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


# erasing useless columns to keep only the general ones. 

D5_0_Human_freedom_index <- select(D5_0_Human_freedom_index, year, country, region, hf_score, pf_rol, pf_ss, pf_movement, pf_religion, pf_assembly, pf_expression, pf_identity, pf_score, ef_government, ef_legal, ef_money, ef_trade, ef_regulation, ef_score)

D5_0_Human_freedom_index <- D5_0_Human_freedom_index %>%
  rename(
    pf_law = names(D5_0_Human_freedom_index)[5],      # Renames the 5th column to "pf_law"
    pf_security = names(D5_0_Human_freedom_index)[6]  # Renames the 6th column to "pf_security"
  )

##### VISUALIZATION ##### 

#Find NA percentage per country per variable 

na_percentage_by_country <- D5_0_Human_freedom_index %>%
  group_by(country) %>%
  summarise(across(everything(), ~mean(is.na(.))*100))

na_long <- na_percentage_by_country %>%
  pivot_longer(
    cols = -country,
    names_to = "Variable",
    values_to = "NA_Percentage"
  )

# Assuming your dataframe is called 'na_long'
overall_na_percentage <- na_long %>%
  group_by(Variable) %>%
  summarize(Avg_NA_Percentage = mean(NA_Percentage, na.rm = TRUE)) %>%
  arrange(desc(Avg_NA_Percentage))

# To view the result
print(overall_na_percentage)

# Order the countries with between 50 and 100 of NA values 

na_long <- na_long %>%
  group_by(country) %>%
  mutate(Count_NA_50_100 = sum(NA_Percentage >= 50 & NA_Percentage <= 100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Count_NA_50_100))

# Now, visualize

heatmap_ordered <- ggplot(na_long, aes(x = reorder(country, -Count_NA_50_100), y = Variable)) +
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
  group_by(country) %>%
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


