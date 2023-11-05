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