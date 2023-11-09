Freq_Missing_GDP <- ggplot(data = filtered_data_GDP) +
  geom_histogram(aes(x = GDPpercapita, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.25, 0.5, 1),
                                labels = c("0-24%", "25-49%", "50-100%"))),
                 bins = 100) +
  labs(title = "Histogram of GDP per capita", x = "GDP per capitaP", y = "Frequency") +
  scale_fill_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"), labels = c("0-24%", "25-49%", "50-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Freq_Missing_GDP)

###### TEST #####

test <- data_question1 %>% filter(year>=2005)
mean(is.na(test))

see_missing_test <- test %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))