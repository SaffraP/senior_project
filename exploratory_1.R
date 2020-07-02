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

##########################################################
###RUN # Read in data about feature descriptions
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


categorical_features <- train %>% select(-c(Census_InternalPrimaryDiagonalDisplaySizeInInches,
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
# I broke this part up into smaller sections so that I can better tell if the program is frozen or not. 
categorical_features[, c(1:20)] <- sapply(categorical_features[, c(1:20)], as.character)
categorical_features[, c(21:40)] <- sapply(categorical_features[, c(21:40)], as.character)
categorical_features[, c(40:50)] <- sapply(categorical_features[, c(40:50)], as.character)
categorical_features[, c(50:65)] <- sapply(categorical_features[, c(50:65)], as.character)

sapply(categorical_features, class)



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


## Count how many unique values are in each column (grouped by categorical, numeric, and logical)
library(tidyr)

unique_values_logical <- logical_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:13) %>% 
  rename(Feature_Name = Feature)  
unique_values_logical$Group <- ("Logical")

unique_values_categorical <- categorical_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:65) %>% 
  rename(Feature_Name = Feature)
unique_values_categorical$Group <- ("Categorical")

unique_values_numeric <- numeric_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:5) %>% 
  rename(Feature_Name = Feature)
unique_values_numeric$Group <- ("Numeric")

unique_values_count <- rbind(unique_values_logical, unique_values_categorical, unique_values_numeric) 


# One Hot encode categorical_features
library(caret)
#dmy <- dummyVars(" ~ ProductName", data = categorical_features)
#trsf <- data.frame(predict(dmy, newdata = categorical_features))
#trsf


###RUN # All the categorical features that are worth keeping
temp <- feature_descriptions %>% 
  filter(Data_Type == "Categorical", Worth_Keeping_Overall == 1) %>% 
  select(Worth_Keeping_Overall, Feature_Name)
###RUN # All the categorical features that are worth keeping overall and their unique count. 
View(merge(temp, unique_values_categorical, by = "Feature_Name", all.x = T))

# Beginning to change the training data into a version that can be one hot encoded. 
###RUN # AppVersion
train$AppVersion_2 <- str_sub(train$AppVersion, 1, 4)

# Census_FirmwareVersionIdentifier NOT WORKING
train$Census_FirmwareVersionIdentifier <- train %>% 
  mutate(Census_FirmwareVersionIdentifier = case_when(
    Census_FirmwareVersionIdentifier >= 1 || Census_FirmwareVersionIdentifier < 10000 ~ "1",
    Census_FirmwareVersionIdentifier >= 10000 || Census_FirmwareVersionIdentifier < 20000 ~ "2",
    Census_FirmwareVersionIdentifier >= 20000 || Census_FirmwareVersionIdentifier < 30000 ~ "3",
    Census_FirmwareVersionIdentifier >= 30000 || Census_FirmwareVersionIdentifier < 40000 ~ "4",
    Census_FirmwareVersionIdentifier >= 40000 || Census_FirmwareVersionIdentifier < 50000 ~ "5",
    Census_FirmwareVersionIdentifier >= 50000 || Census_FirmwareVersionIdentifier < 60000 ~ "6",
    Census_FirmwareVersionIdentifier >= 60000 || Census_FirmwareVersionIdentifier < 70000 ~ "7",
    Census_FirmwareVersionIdentifier >= 70000 ~ "8",
    T ~ "NA"
  ))

train$Census_FirmwareVersionIdentifier <- train %>% 
  mutate(Census_FirmwareVersionIdentifier = case_when(
    Census_FirmwareVersionIdentifier < 10000 ~ "1",
    Census_FirmwareVersionIdentifier < 20000 ~ "2",
    Census_FirmwareVersionIdentifier < 30000 ~ "3",
    Census_FirmwareVersionIdentifier < 40000 ~ "4",
    Census_FirmwareVersionIdentifier < 50000 ~ "5",
    Census_FirmwareVersionIdentifier < 60000 ~ "6",
    Census_FirmwareVersionIdentifier < 70000 ~ "7",
    Census_FirmwareVersionIdentifier >= 70000 ~ "8",
    T ~ "NA"
  ))

# OSBuildLab 
train$OsBuildLab_2 <- str_sub(train$OsBuildLab, 1, 5)
View(table(train$OsBuildLab_2))

# Census_OSVersion 
View(table(train$Census_OSVersion))

train$Census_OSVersion_2 <- str_sub(train$Census_OSVersion, 1, 5)
View(table(train$Census_OSVersion))

train_2 <- as.data.frame(train)

pattern_1 <- "\\d\\.\\d\\.\\d\\."
pattern_2 <- "\\d[[:punct:]]\\d[[:punct:]]\\d[[:punct:]]"
View(str_extract_all(train_2$Census_OSVersion, pattern_1, simplify = T))
regmatches(train_2$Census_OSVersion, gregexpr(pattern_2, train_2$Census_OSVersion)) 


# IeVerIdentifier
View(table(train$OsBuildLab_2))


# GeoNameIdentifier. Idk
View(table(train$GeoNameIdentifier))

train$GeoNameIdentifier <- as.numeric(train$GeoNameIdentifier)

train$GeoNameIdentifier_2 <- train %>% 
  mutate(GeoNameIdentifier_2 = case_when(GeoNameIdentifier == 100 ~ "first",
                                         GeoNameIdentifier < 200 ~ "second",
                                         GeoNameIdentifier < 300 ~ "third",
                                         T ~ "NA"))
View(table(train$GeoNameIdentifier_2)) #Error in View : attempt to make a table with >= 2^31 elements

# Census_OSBuildRevision
View(table(train$Census_OSBuildRevision))

# LocaleEnglishNameIdentifier
View(table(train$Census_OSBuildRevision))

# CountryIdentifier
View(table(train$CountryIdentifier))

# Census_OSBuildNumber
View(table(train$Census_OSBuildNumber))

# Census_OSUILocaleIdentifier
View(table(train$Census_OSUILocaleIdentifier))

# OsBuild
View(table(train$OsBuild))

# EngineVersion
View(table(train$EngineVersion))

# OsVer
View(table(train$OsVer))

# Census_ChassisTypeName
View(table(train$Census_ChassisTypeName))

# OrganizationIdentifier
View(table(train$OrganizationIdentifier))










