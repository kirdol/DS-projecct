
data <- read.csv(here("scripts", "data","Microhistorydatabase.csv"))

library(dplyr)
library(tibble)
library(tidyr)

datatibble <- tibble(data)

#Ranking which columns have most of the missing values
na_counts <- colSums(is.na(datatibble))
sorted_na_counts <- sort(na_counts, decreasing = TRUE) # Sort the columns by the number of NA values in descending order

print(sorted_na_counts)

# housing_capgain_ipolated             eq_dp_interp        eq_capgain_interp             eq_tr_interp 
# 2713                     2712                     2711                     2711 
# rent_ipolated                     tbus                      thh               capital_tr 
# 2689                     1419                     1344                      888 

# investigating why only 5 observations contain the variable "housing_capgain_ipolated"
#1 if housing capital gains and total returns interpolated e.g. wartime

filter(datatibble, housing_capgain_ipolated > 0) 

#Only concerning wartime and only in Belgium. get rid of this data. 

# investigating on the column containing the lowest number of NA values 
na_rows <- datatibble[is.na(datatibble$peg_strict), ]

#we notice that these observations also have a lot of NA value ->  get rid of them 

column_order <- order(na_counts)

datatibble <- datatibble[, column_order]
datatibble <- select(datatibble, -(ltd:housing_capgain_ipolated))

dataWTNA <- drop_na(datatibble)

dataWTNA <- select(dataWTNA, year, country, iso, ifs, pop, everything())

# D8_0_Macro_history --> name final tibble

