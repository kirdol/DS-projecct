
## 4 Internet Usage
D4_0_Internet_usage <- # import the dataset
  read.csv(here("scripts","data","share-of-individuals-using-the-internet-2.csv"))

D4_0_Internet_usage <- # make sure that we have a datafraame
  as.data.frame(D4_0_Internet_usage)

D4_0_Internet_usage <- # keep only the data after 2000 to match the main datast
  D4_0_Internet_usage[D4_0_Internet_usage$Year >= 2000, ]
