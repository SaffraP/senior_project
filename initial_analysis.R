# Load libraries
library(tidyverse)

###RUN # Read in the data 
train <- read_csv("Data/train.csv")

# Data structure
head(train) #9 mil rows, 83 cols
class(train) #df

# The data is evenly divided into 50/50 has detections
# target class = 'HasDetections'  
train %>% 
  count(HasDetections) %>% 
  mutate(paste(percentage = round(n / sum(n) * 100), '%')) #Taken from 450 script

# For each feature, how many observations are in each 'HasDetections' group?
# First, subset the data into has detections positive and negative:
train_negative <- train %>% 
  filter(HasDetections == 0)
train_positive <- train %>% 
  filter(HasDetections == 1)
# Then count how many NA's there are per group per feature:
neg <- as.data.frame(colSums(is.na(train_negative))) %>% 
  rownames_to_column(., "Features")
pos <- as.data.frame(colSums(is.na(train_positive))) %>% 
  rownames_to_column(., "Features")
whole <- as.data.frame(colSums(is.na(train))) %>% 
  rownames_to_column(., "Features")
# Finally, combine them into a single dataset:
count_of_na <- merge(neg, pos, by = "Features") %>% merge(., whole, by = "Features")
colnames(count_of_na)[2] <- "Neg"
colnames(count_of_na)[3] <- "Pos"
colnames(count_of_na)[4] <- "Whole"


# # Tried to group the train data instead of doing them seperately but couldn't get it to work:
# dat_1 <- train %>%
#   group_by(HasDetections) %>%
#   summarise_each(funs(sum(!is.na(.)))) #https://stackoverflow.com/questions/41150212/r-group-by-counting-non-na-values

# Observations:

#These are the features without missing values (39):
count_of_na %>% 
  filter(Neg == 0 & Pos == 0)

#These are the features with missing values (44):
count_of_na %>% 
  filter(Neg != 0 & Pos != 0)

# How many of the infected machines are identified as VM's?
train %>% 
  filter(HasDetections == 1) %>% 
  count(Census_IsVirtualDevice == 1)
# In the HasDetections=1 group, there are 12,172 VM's, 4,438,599 non-VM, and 8,121 not identified. 


## Additional Questions
# Do machines that get updated regularly tend to get infected less often?
# Is there a specific country that seems to be targeted?


## Scratch Code. Anything here can be deleted. 
View(train %>% select(Census_ActivationChannel))
train %>% count(SkuEdition)
train %>% count(SmartScreen == "NA")
train %>% filter(HasDetections == 1) %>% count(Census_IsWIMBootEnabled)
## End Scratch Code.

