### my comments preceded by `CPK:`
library(tidyverse)

## Question 1
dat <- read_csv("fish_data.csv")

## Question 2
ggplot(dat, aes(x = body_length,
                y = eye_size,
                color = fish)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Body Length",
       y = "Eye Size",
       color = "Fish") +
  theme_classic()

## Question 3
# Model without interactions
lm1 <- lm(eye_size ~ body_length + fish, data = dat)

# Model with interactions
lm2 <-  lm(eye_size ~ body_length * fish, data = dat)

## Question 4
AIC(lm1, lm2)

## Question 5
anova(lm2)




###CPK: Boooom! Nailed it!.




#Total points: 10

