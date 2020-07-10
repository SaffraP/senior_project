# Load libraries
library(tidyverse)


# RF on small_train
# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9


# Somewhere along the way I lost the HasDetections column. I need to add that back in before I can run anything. 
rf <- randomForest(
  num ~ .,
  data=small_train
)

