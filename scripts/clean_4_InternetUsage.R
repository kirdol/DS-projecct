
## 4 Internet Usage
D4_0_Internet_usage <- # import the dataset
  read.csv(here("scripts/data/share-of-individuals-using-the-internet-2.csv"))

D4_0_Internet_usage <- # make sure that we have a datafraame
  as.data.frame(D4_0_Internet_usage)

D2_1_Unemployment_rate <- # keep only the data after 2000 to match the main dataset
  D2_1_Unemployment_rate[D2_1_Unemployment_rate$time >= 2000, ]
