library(dplyr)
ncaa_sims <- read.csv("Predictions/ncaa_sims.csv", as.is = T)
ncaa_sims[,-c(1:3)] <- ncaa_sims[,-c(1:3)]/100

### Team Cost
ncaa_sims <- mutate(ncaa_sims, 
                    "cost" = case_when(
                      seed == 1 ~ 75,
                      seed == 2 ~ 40,
                      seed == 3 ~ 25,
                      seed == 4 ~ 20,
                      seed == 5 ~ 17,
                      seed == 6 ~ 15,
                      seed == 7 ~ 12,
                      seed == 8 ~ 10,
                      seed == 9 ~ 9,
                      seed == 10 ~ 8,
                      seed == 11 ~ 7,
                      seed == 12 ~ 6,
                      seed == 13 ~ 5,
                      seed == 14 ~ 4,
                      seed == 15 ~ 3,
                      seed == 16 ~ 1
                    ))

### Expected Points
ncaa_sims <- mutate(ncaa_sims, 
                    "expected_pts" = r32 + 2 * s16 + 3 * e8 + 5 * f4 + 
                      8 * ncg + 13 * champ)

### Solve Knapsack Problem
library(adagio)
kn <- knapsack(ncaa_sims$cost, ncaa_sims$expected_pts, 224)
ncaa_sims$team[kn$indices]
