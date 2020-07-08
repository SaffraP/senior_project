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

###RUN # Select only the data that is considered "worth keeping"
worth_keeping <- as.list(feature_descriptions %>% 
                           filter(Worth_Keeping_Overall == 1) %>% 
                           select(Feature_Name))
worth_keeping <- unlist(worth_keeping, use.names=FALSE)

train_filtered <- train %>% 
  select(!!worth_keeping)

# How many columns are being kept and how many are being discarded. 
table(feature_descriptions$Worth_Keeping_Overall)

# Data type of each column (https://swcarpentry.github.io/r-novice-inflammation/13-supp-data-structures/)
sapply(train, class)

# Goal: Group the data into numeric, categorical, and binary. 
numeric_type <- select_if(train_filtered, is.numeric)
character_type <- select_if(train_filtered, is.character)
names(numeric_type)
names(character_type)

## Create a list of each group (this is with the train data. The train_filtered is below)

# numeric_features <- train %>% 
#   select(Census_InternalPrimaryDiagonalDisplaySizeInInches,
#                                      Census_PrimaryDiskTotalCapacity,
#                                      Census_ProcessorCoreCount,
#                                      Census_SystemVolumeTotalCapacity,
#                                      Census_TotalPhysicalRAM) 
# numeric_features[, c(1:5)] <- sapply(numeric_features[, c(1:5)], as.numeric)
# sapply(numeric_features, class)
# 
# 
# logical_features <- train %>% select(Census_HasOpticalDiskDrive, 
#                                     Census_IsAlwaysOnAlwaysConnectedCapable, 
#                                     Census_IsPenCapable, 
#                                     Census_IsPortableOperatingSystem, 
#                                     Census_IsSecureBootEnabled, 
#                                     Census_IsTouchEnabled, 
#                                     Census_IsVirtualDevice, 
#                                     Firewall, 
#                                     HasTpm, 
#                                     IsProtected, 
#                                     IsSxsPassiveMode, 
#                                     SMode, 
#                                     Wdft_IsGamer)
# logical_features[, c(1:13)] <- sapply(logical_features[, c(1:13)], as.logical)
# sapply(logical_features, class)
# lapply(logical_features, table) # Shows how many values are T/F. Need to figure out what happened to the NA's though and possibly drop them or fill them in with other values. 
# 
# 
# categorical_features <- train %>% select(-c(Census_InternalPrimaryDiagonalDisplaySizeInInches,
#                                             Census_PrimaryDiskTotalCapacity,
#                                             Census_ProcessorCoreCount,
#                                             Census_SystemVolumeTotalCapacity,
#                                             Census_TotalPhysicalRAM,
#                                             Census_HasOpticalDiskDrive, 
#                                             Census_IsAlwaysOnAlwaysConnectedCapable, 
#                                             Census_IsPenCapable, 
#                                             Census_IsPortableOperatingSystem, 
#                                             Census_IsSecureBootEnabled, 
#                                             Census_IsTouchEnabled, 
#                                             Census_IsVirtualDevice, 
#                                             Firewall, 
#                                             HasTpm, 
#                                             IsProtected, 
#                                             IsSxsPassiveMode, 
#                                             SMode, 
#                                             Wdft_IsGamer))
# # I broke this part up into smaller sections so that I can better tell if the program is frozen or not. 
# categorical_features[, c(1:20)] <- sapply(categorical_features[, c(1:20)], as.character)
# categorical_features[, c(21:40)] <- sapply(categorical_features[, c(21:40)], as.character)
# categorical_features[, c(40:50)] <- sapply(categorical_features[, c(40:50)], as.character)
# categorical_features[, c(50:65)] <- sapply(categorical_features[, c(50:65)], as.character)
# 
# sapply(categorical_features, class)



## Create a list of each group (this is with the train_filtered data. The train is above)
numeric_features <- train_filtered %>% 
  select(Census_PrimaryDiskTotalCapacity,
         Census_ProcessorCoreCount,
         Census_SystemVolumeTotalCapacity,
         Census_TotalPhysicalRAM) 
numeric_features[, c(1:4)] <- sapply(numeric_features[, c(1:4)], as.numeric)
sapply(numeric_features, class)


logical_features <- train_filtered %>% select(Census_HasOpticalDiskDrive, 
                                              Census_IsAlwaysOnAlwaysConnectedCapable, 
                                              Census_IsPenCapable, 
                                              Census_IsPortableOperatingSystem, 
                                              Census_IsSecureBootEnabled, 
                                              Census_IsTouchEnabled, 
                                              Firewall, 
                                              HasTpm, 
                                              IsProtected, 
                                              IsSxsPassiveMode, 
                                              SMode, 
                                              Wdft_IsGamer)
logical_features[, c(1:12)] <- sapply(logical_features[, c(1:12)], as.logical)
sapply(logical_features, class)
lapply(logical_features, table) # Shows how many values are T/F. Need to figure out what happened to the NA's though and possibly drop them or fill them in with other values. 


categorical_features <- train_filtered %>% select(-c(Census_PrimaryDiskTotalCapacity,
                                                     Census_ProcessorCoreCount,
                                                     Census_SystemVolumeTotalCapacity,
                                                     Census_TotalPhysicalRAM,
                                                     Census_HasOpticalDiskDrive, 
                                                     Census_IsAlwaysOnAlwaysConnectedCapable, 
                                                     Census_IsPenCapable, 
                                                     Census_IsPortableOperatingSystem, 
                                                     Census_IsSecureBootEnabled, 
                                                     Census_IsTouchEnabled,
                                                     Firewall, 
                                                     HasTpm, 
                                                     IsProtected, 
                                                     IsSxsPassiveMode, 
                                                     SMode, 
                                                     Wdft_IsGamer))
# I broke this part up into smaller sections so that I can better tell if the program is frozen or not. 
categorical_features[, c(1:20)] <- sapply(categorical_features[, c(1:20)], as.character)
categorical_features[, c(21:40)] <- sapply(categorical_features[, c(21:40)], as.character)
categorical_features[, c(40:48)] <- sapply(categorical_features[, c(40:48)], as.character)

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







# All the categorical features that are worth keeping
temp <- feature_descriptions %>% 
  filter(Data_Type == "Categorical", Worth_Keeping_Overall == 1) %>% 
  select(Worth_Keeping_Overall, Feature_Name)
# All the categorical features that are worth keeping overall and their unique count. 
View(merge(temp, unique_values_categorical, by = "Feature_Name", all.x = T))

# Filtering out the categorical features that can't be one hot encoded because there are too many unique values
# Features I'm temp dropping until I figure out how to handle them: Census_ProcessorModelIdentifier, GeoNameIdentifier

train_filtered <- train_filtered %>% 
  select(!c("MachineIdentifier",
            "CityIdentifier",
            "Census_FirmwareVersionIdentifier",
            "AVProductStatesIdentifier",
            "Census_OSVersion",
            "Census_OSBuildRevision",
            "LocaleEnglishNameIdentifier",
            "OsBuild",
            "Census_ProcessorModelIdentifier",
            "GeoNameIdentifier"
  ))

###### 
# At this point, I can do a couple things. 
# 1. one hot > RF 
# 2. wrangle (categorical and logical) > one hot > RF
# 3. drop most of the categorical > one hot > RF
# I should probably go with option #2
#
######





## Beginning to change the training data into a version that can be one hot encoded. 

###RUN THESE FEATURES:

# AppVersion
train_filtered$AppVersion <- str_sub(train_filtered$AppVersion, 1, 4)

# OSBuildLab 
train_filtered$OsBuildLab <- as.data.frame(str_sub(train_filtered$OsBuildLab, 1, 5))
train_filtered <- train_filtered %>% 
  mutate(OsBuildLab = case_when(OsBuildLab == 16299 ~ 16299,
                                OsBuildLab == 17134 ~ 17134,
                                T ~ 0))

# IeVerIdentifier
train_filtered <- train_filtered %>% 
  mutate(IeVerIdentifier = case_when(IeVerIdentifier == 137 ~ 137,
                                     IeVerIdentifier == 117 ~ 117,
                                     T ~ 0))

# CountryIdentifier ... This one I'm just leaving it as it is. 222 groups. 

# Census_OSBuildNumber
train_filtered <- train_filtered %>% 
  mutate(Census_OSBuildNumber = case_when(Census_OSBuildNumber == 17134 ~ 17134,
                                          Census_OSBuildNumber == 16299 ~ 16299,
                                          T ~ 0))

# Census_OSUILocaleIdentifier
train_filtered <- train_filtered %>% 
  mutate(Census_OSUILocaleIdentifier = case_when(Census_OSUILocaleIdentifier == 31 ~ 31,
                                                 Census_OSUILocaleIdentifier == 34 ~ 34,
                                                 T ~ 0))

# Census_ChassisTypeName
train_filtered <- train_filtered %>% 
  mutate(Census_ChassisTypeName = case_when(Census_ChassisTypeName == "Notebook" ~ "Notebook",
                                            Census_ChassisTypeName == "Desktop" ~ "Desktop",
                                            T ~ "Other"))

# OrganizationIdentifier
train_filtered <- train_filtered %>% 
  mutate(OrganizationIdentifier = case_when(OrganizationIdentifier == 18 ~ 18,
                                            OrganizationIdentifier == 27 ~ 27,
                                            T ~ 0))

# Census_OEMNameIdentifier
train_filtered <- train_filtered %>% 
  mutate(Census_OEMNameIdentifier = case_when(Census_OEMNameIdentifier == 2668 ~ 2668,
                                              Census_OEMNameIdentifier == 2102 ~ 2102,
                                              Census_OEMNameIdentifier == 1443 ~ 1443,
                                              Census_OEMNameIdentifier == 2206 ~ 2206,
                                              Census_OEMNameIdentifier == 585 ~ 585,
                                              Census_OEMNameIdentifier == 525 ~ 525,
                                              T ~ 0))

# Census_OEMNameIdentifier
train_filtered <- train_filtered %>% 
  mutate(Census_FirmwareManufacturerIdentifier = case_when(Census_FirmwareManufacturerIdentifier == 142 ~ 142,
                                                           Census_FirmwareManufacturerIdentifier == 628 ~ 628,
                                                           Census_FirmwareManufacturerIdentifier == 554 ~ 554,
                                                           Census_FirmwareManufacturerIdentifier == 355 ~ 355,
                                                           Census_FirmwareManufacturerIdentifier == 556 ~ 556,
                                                           T ~ 0))







### DON'T RUN THESE FEATURES (UNDER CONSTRUCTION):

# GeoNameIdentifier. Idk
View(table(train_filtered$GeoNameIdentifier))

train_filtered$GeoNameIdentifier <- as.numeric(train_filtered$GeoNameIdentifier)

train_filtered$GeoNameIdentifier_2 <- train_filtered %>% 
  mutate(GeoNameIdentifier_2 = case_when(GeoNameIdentifier == 100 ~ "first",
                                         GeoNameIdentifier < 200 ~ "second",
                                         GeoNameIdentifier < 300 ~ "third",
                                         T ~ "NA"))
View(table(train_filtered$GeoNameIdentifier_2)) #Error in View : attempt to make a table with >= 2^31 elements

# EngineVersion NOT WORKING
View(table(train_filtered$EngineVersion))
train_filtered <- train_filtered %>% 
  mutate(EngineVersion = case_when(EngineVersion == as.numeric('1.1.15200.1') ~ as.numeric('1.1.15200.1'),
                                   EngineVersion == as.numeric('1.1.15100.1') ~ as.numeric('1.1.15100.1'),
                                   T ~ 0))

train_filtered$EngineVersion <- as.numeric(train_filtered$EngineVersion)

train_filtered <- train_filtered %>% 
  mutate(EngineVersion = case_when(EngineVersion == `1.1.15200.1` ~ `1.1.15200.1`,
                                   EngineVersion == `1.1.15100.1` ~ `1.1.15100.1`,
                                   T ~ 0))
View(table(train_filtered$EngineVersion))


# OsVer NOT WORKING
View(table(train_filtered$OsVer))

train_filtered$OsVer_2 <- as.character(train_filtered$OsVer)

train_filtered <- train_filtered %>% 
  mutate(OsVer_2 = case_when(OsVer == `10.0.0.0` ~ `10.0.0.0`,
                             T ~ 0))


# AvSigVersion NOT WORKING
train_filtered$AvSigVersion <- str_sub(train_filtered$AvSigVersion, 1, 5)

train_filtered <- train_filtered %>% 
  mutate(AvSigVersion = case_when(AvSigVersion == `1.273` ~ `1.273`,
                                  AvSigVersion == `1.275` ~ `1.275`,
                                  T ~ 0))

# Census_ProcessorModelIdentifier Not sure how to deal with this feature
View(table(train_filtered$Census_ProcessorModelIdentifier))
train_filtered <- train_filtered %>% 
  mutate(Census_ProcessorModelIdentifier = case_when(Census_ProcessorModelIdentifier == 2668 ~ 2668,
                                                     Census_ProcessorModelIdentifier == 2102 ~ 2102,
                                                     Census_OEMNameIdentifier == 1443 ~ 1443,
                                                     Census_OEMNameIdentifier == 2206 ~ 2206,
                                                     Census_OEMNameIdentifier == 585 ~ 585,
                                                     Census_OEMNameIdentifier == 525 ~ 525,
                                                     T ~ 0))




#################
## These are the features I started manipulating but they ended up being dropped. 

# Census_FirmwareVersionIdentifier NOT WORKING
train_filtered <- train_filtered %>% 
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


# Census_OSVersion 
View(table(train_filtered$Census_OSVersion))

train_filtered$Census_OSVersion_2 <- str_sub(train_filtered$Census_OSVersion, 1, 5)
View(table(train_filtered$Census_OSVersion))

train_2 <- as.data.frame(train_filtered)

pattern_1 <- "\\d\\.\\d\\.\\d\\."
pattern_2 <- "\\d[[:punct:]]\\d[[:punct:]]\\d[[:punct:]]"
View(str_extract_all(train_2$Census_OSVersion, pattern_1, simplify = T))
regmatches(train_2$Census_OSVersion, gregexpr(pattern_2, train_2$Census_OSVersion)) 

# Census_OSBuildRevision
View(table(train_filtered$Census_OSBuildRevision))

# LocaleEnglishNameIdentifier
View(table(train_filtered$Census_OSBuildRevision))

# OsBuild
View(table(train_filtered$OsBuild))

train_filtered <- train_filtered %>% 
  mutate(OsBuild = case_when(OsBuild == 31 ~ 31,
                             OsBuild == 34 ~ 34,
                             T ~ 0))

#############################################################################
# Now that the data is organized properly, it's time to start one hot encoding the categorical features. 

# categorical_features is the dataframe we want to work with


## One Hot encode categorical_features
#library(mltools) #https://cran.r-project.org/web/packages/mltools/mltools.pdf and https://www.rdocumentation.org/packages/mltools/versions/0.3.5/topics/one_hot
#library(data.table)
#categorical_features <- as.data.table(categorical_features)
#temp_3 <- one_hot(categorical_features, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE, dropCols = TRUE, dropUnusedLevels = FALSE)
#temp_4 <- one_hot(categorical_features)

## Apparently one-hot encoding can be done automatically by most R modeling paths. It doesn't need to be explicitly defined 
# https://www.r-bloggers.com/encoding-categorical-variables-one-hot-and-beyond/

summary(train_filtered)

# Convert character fields to factors 
#categorical_factors <- as.factor(categorical_features) #This takes forever to run
