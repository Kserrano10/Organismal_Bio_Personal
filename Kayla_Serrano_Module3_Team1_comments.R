### my comments preceded by `#CPK:`
library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(MuMIn)

#1 Establish the anole.log data tibble

anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole %>%
  left_join(anole.eco) %>%
  filter(!Ecomorph %in% c("U", "CH")) %>%
  na.omit()

anole.log <- anole2 %>%
  mutate_at(c("SVL", "HTotal", "PH", "ArbPD"), log)


#2 Fit two linear models with additive perch variables

lm.PH <- lm(HTotal ~ SVL + PH, data = anole.log)
lm.PD <- lm(HTotal ~ SVL + ArbPD, data = anole.log)


#3 Plot the residuals of the two linear models by ecomorph

anole.log <- anole.log %>%
  mutate(
    res.PH = residuals(lm.PH),
    res.PD = residuals(lm.PD)
  )

anole.log %>%
  ggplot(aes(x = Ecomorph2, y = res.PH)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Ecomorph", y = "Perch-height model residual")

anole.log %>%
  ggplot(aes(x = Ecomorph2, y = res.PD)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Ecomorph", y = "Perch-diameter model residual")


#4 Fit three PGLS models under Brownian motion

anole.tree <- read.tree("anole.tre")

pgls.PH <- gls(
  HTotal ~ SVL + PH,
  correlation = corBrownian(1, phy = anole.tree, form = ~Species),
  data = anole.log,
  method = "ML"
)

pgls.PD <- gls(
  HTotal ~ SVL + ArbPD,
  correlation = corBrownian(1, phy = anole.tree, form = ~Species),
  data = anole.log,
  method = "ML"
)

pgls.both <- gls(
  HTotal ~ SVL + PH + ArbPD,
  correlation = corBrownian(1, phy = anole.tree, form = ~Species),
  data = anole.log,
  method = "ML"
)

#5 Compare the models using AICc and AIC weights

anole.phylo.aic <- AICc(pgls.PH, pgls.PD, pgls.both)
print(anole.phylo.aic)

anole.phylo.weights <- aicw(anole.phylo.aic$AICc)
print(anole.phylo.weights)

anova(pgls.both)

### The model with both perch height and perch diameter fit the data best. 
### It had the lowest AICc (-75.53) and the highest AIC weight (0.700). 
### After accounting for body size and phylogeny, perch height (p = 0.0407) and perch diameter (p = 0.0005) were both significant predictors of hind-limb length.


#6 Plot residuals from the best-fitting PGLS model

anole.log <- anole.log %>%
  mutate(phylo.res = residuals(pgls.both))

#CPK: I'm not sure this is the most logical plot, though it's on the right track. I boxplot of residuals would have sufficed and been more clear [-2]. It would capture how HTotal across each ecomorph deviates from the SVL+height+diam trend. 
anole.log %>%
  select(Ecomorph2, PH, ArbPD, phylo.res) %>%
  pivot_longer(
    cols = c(PH, ArbPD),
    names_to = "perch.variable",
    values_to = "perch.measure"
  ) %>%
  ggplot(aes(x = perch.measure, y = phylo.res, color = Ecomorph2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  facet_wrap(~perch.variable, scales = "free_x") +
  labs(
    x = "Log perch measurement",
    y = "Best-fitting PGLS residual",
    color = "Ecomorph"
  )
# AI Statement
### AI was not used to complete this project. The module 3 project instructions and class book were used to complete this project. 




###CPK: Excellent work, team. This was a considerable amount of work to taken on, some of a very technical nature. Congrats!




#Total points: 23


#Peer evaluation:Your peer(s) found that your contributions could have been more substantial in all aspects of the project.
