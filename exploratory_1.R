# Load libraries
library(tidyverse)

# Read in the data
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

##########################################################
# Read in data about feature descriptions
feature_descriptions <- read_csv("Data/feature_descriptions.csv")

# How many columns are being kept and how many are being discarded. 
table(feature_descriptions$Worth_Keeping_Overall)

# Data type of each column (https://swcarpentry.github.io/r-novice-inflammation/13-supp-data-structures/)
sapply(train, class)

# Goal: Group the data into numeric, categorical, and binary. 
numeric_type <- select_if(train, is.numeric)
character_type <- select_if(train, is.character)
names(numeric_type)
names(character_type)

# Create a list of each group
numeric_features <- train %>% 
  select(Census_InternalPrimaryDiagonalDisplaySizeInInches,
                                     Census_PrimaryDiskTotalCapacity,
                                     Census_ProcessorCoreCount,
                                     Census_SystemVolumeTotalCapacity,
                                     Census_TotalPhysicalRAM) 
numeric_features[, c(1:5)] <- sapply(numeric_features[, c(1:5)], as.numeric)
sapply(numeric_features, class)


logical_features <- train %>% select(Census_HasOpticalDiskDrive, 
                                    Census_IsAlwaysOnAlwaysConnectedCapable, 
                                    Census_IsPenCapable, 
                                    Census_IsPortableOperatingSystem, 
                                    Census_IsSecureBootEnabled, 
                                    Census_IsTouchEnabled, 
                                    Census_IsVirtualDevice, 
                                    Firewall, 
                                    HasTpm, 
                                    IsProtected, 
                                    IsSxsPassiveMode, 
                                    SMode, 
                                    Wdft_IsGamer)
logical_features[, c(1:13)] <- sapply(logical_features[, c(1:13)], as.logical)
sapply(logical_features, class)
lapply(logical_features, table) # Shows how many values are T/F. Need to figure out what happened to the NA's though and possibly drop them or fill them in with other values. 


catagorical_features <- train %>% select(-c(Census_InternalPrimaryDiagonalDisplaySizeInInches,
                                            Census_PrimaryDiskTotalCapacity,
                                            Census_ProcessorCoreCount,
                                            Census_SystemVolumeTotalCapacity,
                                            Census_TotalPhysicalRAM,
                                            Census_HasOpticalDiskDrive, 
                                            Census_IsAlwaysOnAlwaysConnectedCapable, 
                                            Census_IsPenCapable, 
                                            Census_IsPortableOperatingSystem, 
                                            Census_IsSecureBootEnabled, 
                                            Census_IsTouchEnabled, 
                                            Census_IsVirtualDevice, 
                                            Firewall, 
                                            HasTpm, 
                                            IsProtected, 
                                            IsSxsPassiveMode, 
                                            SMode, 
                                            Wdft_IsGamer))
# I broke this part up into three smaller sections so that I can better tell if the program is frozen or not. 
catagorical_features[, c(1:20)] <- sapply(catagorical_features[, c(1:20)], as.character)
catagorical_features[, c(21:40)] <- sapply(catagorical_features[, c(21:40)], as.character)
catagorical_features[, c(40:65)] <- sapply(catagorical_features[, c(40:65)], as.character)
sapply(catagorical_features, class)



## Figure out how many NA's are in each logical feature
logical_na <- count_of_na %>% 
  filter(Features %in% c("Census_HasOpticalDiskDrive", 
         "Census_IsAlwaysOnAlwaysConnectedCapable", 
         "Census_IsPenCapable", 
         "Census_IsPortableOperatingSystem", 
         "Census_IsSecureBootEnabled", 
         "Census_IsTouchEnabled", 
         "Census_IsVirtualDevice", 
         "Firewall", 
         "HasTpm", 
         "IsProtected", 
         "IsSxsPassiveMode", 
         "SMode", 
         "Wdft_IsGamer"))

table(apply(logical_features, MARGIN = 1, function(x) sum(is.na(x)))) 
# This indicates that ~7 mil rows don't have missing values. 995147 are missing at least one value. 

table(apply(train, MARGIN = 1, function(x) sum(is.na(x)))) 


## Count how many unique values are in each column (grouped by catagorical, numeric, and logical)
library(tidyr)

logical_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:13)

catagorical_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:65)

numeric_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:5)


# One Hot encode catagorical_features
library(caret)

dmy <- dummyVars(" ~ ProductName", data = catagorical_features)
trsf <- data.frame(predict(dmy, newdata = catagorical_features))
trsf
