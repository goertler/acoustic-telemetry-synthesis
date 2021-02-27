# Tests

source("R/make_DFA_matrix_MEJ.R") # fin$distance_m and sum(ff$total_dist) need to be closer
library(ggplot2)

ggplot(test, aes(x = Date, y=Distance_m)) +
  geom_point(aes(group = FishID, color = FishID)) +
  geom_path(aes(group = FishID, color = FishID)) +
  theme_minimal()


ggplot(jsats[jsats$FishID == test$FishID[1], ],
       aes(x = DateTime_PST, y = RKM)) +
  geom_point() +
  geom_path(aes(group = FishID))
