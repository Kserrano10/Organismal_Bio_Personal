### my comments preceded by `CPK:`
library(tidyverse)

## Question 1
body_dat <- read_csv("body_data.csv")
head_dat <- read_csv("head_xy.csv") 

# Horizontal distance between maxilla and lower jaw
head_wide <- head_dat %>%
  select(fish,specimen,point,X) %>%
  pivot_wider(
    names_from = point,
    values_from = X
  ) %>%
  mutate(
    jaw_length = abs(maxilla - lowerjaw)
  ) #CPK: excellent

# Means for each species
body_dat %>%
  left_join(
    head_wide %>% select(fish, specimen, jaw_length),
    by = c("fish","specimen")
  ) %>%
  group_by(fish) %>%
  summarise(
    mean_body_length = mean(body_length),
    mean_body_depth = mean(body_depth),
    mean_jaw_length = mean(jaw_length)
  ) %>%
  print()







###CPK: Great work, Kayla! This is really coming along.




#Total points: 10