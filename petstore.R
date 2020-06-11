library(tidyverse)

petstore <- read_csv("Data/petstore_data.csv")

sapply(petstore, mode)
sapply(petstore, class)


petstore[, c(1:3)] <- sapply(petstore[, c(1:3)], as.character)

petstore[, c(2:3)] <- sapply(petstore[, c(2:3)], as.numeric)


table(apply(petstore, MARGIN = 1, function(x) sum(is.na(x))))

# 0 = # of rows that are not missing any values
# 1 = # of rows that are missing a value in a single column
# 2 = # of rows that are missing values in two columns. 
# etc
# Add them all up to get the total number of rows. 