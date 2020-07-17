# Preparing the test data, following the same pattern as the train data. 

###RUN # Read in the data 
test <- read_csv("Data/test.csv")

###RUN # Read in data about feature descriptions
feature_descriptions <- read_csv("Data/feature_descriptions.csv")

###RUN # Select only the data that is considered "worth keeping"
worth_keeping_test <- as.list(feature_descriptions %>% 
                           filter(Worth_Keeping_Overall == 1) %>% 
                           select(Feature_Name))
worth_keeping_test <- unlist(worth_keeping, use.names=FALSE)


# Remove HasDetections from the list
worth_keeping_test <- as.data.frame(worth_keeping_test)
worth_keeping_test <- worth_keeping_test %>% 
  filter(worth_keeping_test != 'HasDetections')

worth_keeping_test <- as.list.data.frame(worth_keeping_test)
worth_keeping_test <- unlist(worth_keeping_test, use.names=FALSE)


# Removing the columns that aren't worth keeping
test_filtered <- test %>% 
  select(!!worth_keeping_test)


# Data type of each column (https://swcarpentry.github.io/r-novice-inflammation/13-supp-data-structures/)
sapply(test_filtered, class)




# Goal: Group the data into numeric, categorical, and binary. 
numeric_type <- select_if(test_filtered, is.numeric)
character_type <- select_if(test_filtered, is.character)
#names(numeric_type)
#names(character_type)

## Create a list of each group (this is with the test_filtered data)
# These are the columns that should be numeric 
numeric_features <- test_filtered %>% 
  select(Census_PrimaryDiskTotalCapacity,
         Census_ProcessorCoreCount,
         Census_SystemVolumeTotalCapacity,
         Census_TotalPhysicalRAM) 
# Converting to numeric
numeric_features[, c(1:4)] <- sapply(numeric_features[, c(1:4)], as.numeric)
# Confirming that each column is numeric
#sapply(numeric_features, class)


logical_features <- test_filtered %>% select(Census_HasOpticalDiskDrive, 
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
#sapply(logical_features, class)
lapply(logical_features, table) # Shows how many values are T/F. Need to figure out what happened to the NA's though and possibly drop them or fill them in with other values. 


categorical_features <- test_filtered %>% select(-c(Census_PrimaryDiskTotalCapacity,
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
categorical_features[, c(40:47)] <- sapply(categorical_features[, c(40:47)], as.character)
#sapply(categorical_features, class)



########## This section goes over how many NA's there are. I'm omiting it now for the sake of time. 
# ## Figure out how many NA's are in each logical feature
# # Note: Neg means malware was not detected, pos means malware was detected.
# logical_na <- count_of_na %>% 
#   filter(Features %in% c("Census_HasOpticalDiskDrive", 
#                          "Census_IsAlwaysOnAlwaysConnectedCapable", 
#                          "Census_IsPenCapable", 
#                          "Census_IsPortableOperatingSystem", 
#                          "Census_IsSecureBootEnabled", 
#                          "Census_IsTouchEnabled", 
#                          "Census_IsVirtualDevice", 
#                          "Firewall", 
#                          "HasTpm", 
#                          "IsProtected", 
#                          "IsSxsPassiveMode", 
#                          "SMode", 
#                          "Wdft_IsGamer"
#                          ))
# 
# table(apply(logical_features, MARGIN = 1, function(x) sum(is.na(x)))) 
# # This indicates that ~7 mil rows don't have missing values. 995147 are missing at least one value. 
# 
# # This takes about 10 minutes to run. 
# #table(apply(train, MARGIN = 1, function(x) sum(is.na(x)))) 
# # This indicates that every row in the training data has at least one missing value. Most of the rows contain
# # between 3 and 11 NA's
# 
# # Since we still have 7931423 observations without missing values, I'm going to drop any rows that have NA. 
# # Later it could be interesting to interpolate what those values would be. 
# logical_features <- na.omit(logical_features) #Went from 8921483 to 7931423 rows




## Count how many unique values are in each column (grouped by categorical, numeric, and logical)
library(tidyr)

unique_values_logical <- logical_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:12) %>% 
  rename(Feature_Name = Feature)  
unique_values_logical$Group <- ("Logical")

unique_values_categorical <- categorical_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:47) %>% 
  rename(Feature_Name = Feature)
unique_values_categorical$Group <- ("Categorical")

unique_values_numeric <- numeric_features %>% 
  summarise_all(n_distinct) %>% 
  gather("Feature", "Unique_Count", 1:4) %>% 
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
# This drops the data down to 54 features.
test_filtered <- test_filtered %>% 
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



## Beginning to change the training data into a version that can be one hot encoded. 

###RUN THESE FEATURES:

# AppVersion
test_filtered$AppVersion <- str_sub(test_filtered$AppVersion, 1, 4)

# OSBuildLab 
test_filtered$OsBuildLab <- as.data.frame(str_sub(test_filtered$OsBuildLab, 1, 5))
test_filtered <- test_filtered %>% 
  mutate(OsBuildLab = case_when(OsBuildLab == 16299 ~ 16299,
                                OsBuildLab == 17134 ~ 17134,
                                T ~ 0))

# IeVerIdentifier
test_filtered <- test_filtered %>% 
  mutate(IeVerIdentifier = case_when(IeVerIdentifier == 137 ~ 137,
                                     IeVerIdentifier == 117 ~ 117,
                                     T ~ 0))

# CountryIdentifier ... This one I'm just leaving it as it is. 222 groups. 

# Census_OSBuildNumber
test_filtered <- test_filtered %>% 
  mutate(Census_OSBuildNumber = case_when(Census_OSBuildNumber == 17134 ~ 17134,
                                          Census_OSBuildNumber == 16299 ~ 16299,
                                          T ~ 0))

# Census_OSUILocaleIdentifier
test_filtered <- test_filtered %>% 
  mutate(Census_OSUILocaleIdentifier = case_when(Census_OSUILocaleIdentifier == 31 ~ 31,
                                                 Census_OSUILocaleIdentifier == 34 ~ 34,
                                                 T ~ 0))

# Census_ChassisTypeName
test_filtered <- test_filtered %>% 
  mutate(Census_ChassisTypeName = case_when(Census_ChassisTypeName == "Notebook" ~ "Notebook",
                                            Census_ChassisTypeName == "Desktop" ~ "Desktop",
                                            T ~ "Other"))

# OrganizationIdentifier
test_filtered <- test_filtered %>% 
  mutate(OrganizationIdentifier = case_when(OrganizationIdentifier == 18 ~ 18,
                                            OrganizationIdentifier == 27 ~ 27,
                                            T ~ 0))

# Census_OEMNameIdentifier
test_filtered <- test_filtered %>% 
  mutate(Census_OEMNameIdentifier = case_when(Census_OEMNameIdentifier == 2668 ~ 2668,
                                              Census_OEMNameIdentifier == 2102 ~ 2102,
                                              Census_OEMNameIdentifier == 1443 ~ 1443,
                                              Census_OEMNameIdentifier == 2206 ~ 2206,
                                              Census_OEMNameIdentifier == 585 ~ 585,
                                              Census_OEMNameIdentifier == 525 ~ 525,
                                              T ~ 0))

# Census_OEMNameIdentifier
test_filtered <- test_filtered %>% 
  mutate(Census_FirmwareManufacturerIdentifier = case_when(Census_FirmwareManufacturerIdentifier == 142 ~ 142,
                                                           Census_FirmwareManufacturerIdentifier == 628 ~ 628,
                                                           Census_FirmwareManufacturerIdentifier == 554 ~ 554,
                                                           Census_FirmwareManufacturerIdentifier == 355 ~ 355,
                                                           Census_FirmwareManufacturerIdentifier == 556 ~ 556,
                                                           T ~ 0))




####################################################

# Removing NA's
test_filtered_2 <- na.omit(test_filtered)
# Went from 7,853,253 down to 806,625 observations
# I should find a different way to address NA's other than just dropping those rows. 











