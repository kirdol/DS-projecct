
data <- read.csv(here("scripts", "data", "human-freedom-index-2022.csv"))

library("dplyr")
library("tibble")
library("tidyr")

#data in tibble 
datatibble <- tibble(data)


#Ranking which columns have most of the missing values
na_counts <- colSums(is.na(datatibble))
sorted_na_counts <- sort(na_counts, decreasing = TRUE) # Sort the columns by the number of NA values in descending order

print(sorted_na_counts)

# pf_identity_inheritance_widows    pf_identity_inheritance_daughters                    pf_rol_procedural 
# 2548                                 2548                                 2075 
# pf_rol_civil                      pf_rol_criminal       pf_ss_disappearances_organized 
# 2075                                 2075                                 1494 

#rank the columns by increasing order of NA missing values and get rid of the columns with more than 10% of missing values. 

column_order <- order(na_counts)

datatibble <- datatibble[, column_order]
datatibble <- select(datatibble, -(ef_regulation_credit_ownership:pf_identity_inheritance_daughters))

#Now all our columns have at least 85% of the total entries.
na_rows <- datatibble[is.na(datatibble$ef_gender)|is.na(datatibble$pf_identity), ]

#When investigating the columns containing the lowest NA values, we can see that the observations are also 
#having NA values for the highest NA values columns. Therefore, it is more simple to get rid of the NA value. 

dataWTNA <- drop_na(datatibble)
dataWTNA <- select(dataWTNA,year, countries, region,hf_score, hf_rank, hf_quartile, everything())

#D5_0_Human_freedom_index --> name final tibble

# How to select the NA values of a specific variable, arranged.
# data <- data[is.na(data$column)]
# arrange(select(data, column)) |> print(n = ...) 
