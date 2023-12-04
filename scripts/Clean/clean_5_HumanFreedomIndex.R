data <- read.csv(here("scripts", "data", "human-freedom-index-2022.csv"))

#data in tibble 
datatibble <- tibble(data)

# Rename the column countries into country to match the other datbases
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

D5_0_Human_freedom_index <- select(D5_0_Human_freedom_index, year, country, region, hf_score, pf_rol, pf_ss, pf_movement, pf_religion, pf_assembly, pf_expression, pf_identity, pf_score, ef_government, ef_legal, ef_money, ef_trade, ef_regulation, ef_score, code)

D5_0_Human_freedom_index <- D5_0_Human_freedom_index %>%
  rename(
    pf_law = names(D5_0_Human_freedom_index)[5],      # Renames the 5th column to "pf_law"
    pf_security = names(D5_0_Human_freedom_index)[6]  # Renames the 6th column to "pf_security"
  )

##### VISUALIZATION ##### 

#Find NA percentage per country per variable 

na_percentage_by_country <- D5_0_Human_freedom_index %>%
  group_by(country) %>%
  select(-code) %>%
  summarise(across(everything(), ~mean(is.na(.))*100))

na_long <- na_percentage_by_country %>%
  pivot_longer(
    cols = -country,
    names_to = "Variable",
    values_to = "NA_Percentage"
  )

overall_na_percentage <- na_long %>%
  group_by(Variable) %>%
  summarize(Avg_NA_Percentage = mean(NA_Percentage, na.rm = TRUE)) %>%
  arrange(desc(Avg_NA_Percentage))
print(overall_na_percentage)

# Order the countries with between 50 and 100 of NA values 

na_long <- na_long %>%
  group_by(country) %>%
  mutate(Count_NA_50_100 = sum(NA_Percentage >= 50 & NA_Percentage <= 100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Count_NA_50_100))

heatmap_ordered_all <- ggplot(na_long, aes(x = reorder(country, -Count_NA_50_100), y = Variable)) +
  geom_tile(aes(fill = NA_Percentage), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    title = "Heatmap of NA Percentages per Country and Variable",
    x = "Countries",
    y = "Variables",
    fill = "NA Percentage"
  ) +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 9)
  )
print(heatmap_ordered_all)

# Now, visualization with only countries with 50% and more of missing values for at least 1 variable. 
na_long_filtered <- na_long %>%
  group_by(country) %>%
  mutate(Count_NA_50_100 = sum(NA_Percentage >= 50 & NA_Percentage <= 100, na.rm = TRUE)) %>%
  filter(Count_NA_50_100 > 0) %>%
  ungroup() %>%
  arrange(desc(Count_NA_50_100))

heatmap_ordered_filtered <- ggplot(na_long_filtered, aes(x = reorder(country, -Count_NA_50_100), y = Variable)) +
  geom_tile(aes(fill = NA_Percentage), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    title = "Heatmap of NA Percentages per Country and Variable",
    x = "Countries",
    y = "Variables",
    fill = "NA Percentage"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 7)
  )
print(heatmap_ordered_filtered)

###### END VISUALIZATION ######

#Checking the number of variables per countries where NA values percentage >=50%

country_na_count <- na_long %>%
  filter(NA_Percentage >= 50) %>%
  group_by(country) %>%
  summarise(Count_NA_50_100 = n()) %>%
  arrange(desc(Count_NA_50_100))

print(country_na_count)

###### Partie quarto data.qmd #####

### D5_0_Human_freedom_index {.unnumbered}

```{r}
D5_0_Human_freedom_index_table_info <- data.frame(
  Column1 = c("code",
              "country",
              "year",
              "region",
              "pf_law",
              "pf_security",
              "pf_movement",
              "pf_religion",
              "pf_assembly",
              "pf_expression",
              "pf_identity",
              "ef_gouvernment",
              "ef_legal",
              "ef_money",
              "ef_trade",
              "ef_regulation"),
  Column2 = c("Country code (ISO)", 
              "Country name",
              "Year of the observation (2000-2022)",
              "Part of the world, group of countries (e.g. Eastern Europe, Dub-Saharan Africa, South Asia, etc.)",
              "Rule of law, mean score of: Procedural justice, Civil, justice, Criminal justice, Rule of law (V-Dem)",
              "Security and safety, mean score of: Homicide, Disappearances conflicts, terrorism",
              "Freedom of movement (V-Dem), Freedom of movement (CLD)",
              "Freedom of religion, Religious organization, repression",
              "Civil society entry and exit, Freedom of assembly, Freedom to form/run political parties, Civil society repression",
              "Direct attacks on the press, Media and expression (V-Dem), Media and expression (Freedom House), Media and expression (BTI), Media and expression (CLD)",
              "Same-sex relationships, Divorce, Inheritance rights, Female genital mutilation",
              "Government consumption, Transfers and subsidies, Government investment, Top marginal tax rate, State ownership of assets",
              "Judicial independence, Impartial courts, Protection of property rights, Military interference Integrity of the legal system Legal enforcementof contracts, Regulatory costs, Reliability of police",
              "Money growth, Standard deviation of inflation, Inflation: Most recent year, Freedom to own foreign currency",
              "Tariffs, Regulatory trade barriers, Black-market exchange rates, Movement of capital and people",
              "Credit market regulations, Labor market regulations, Business regulations"))

colnames(D5_0_Human_freedom_index_table_info) <- c("Variable Name",
                                                   "Explanation")

D5_0_Human_freedom_index_table_info %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, width = "35%")
```


