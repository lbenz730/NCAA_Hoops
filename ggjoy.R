library(ggplot2)
library(ggridges)
library(dplyr)
library(viridis)

x <- read.csv("2.0_Files/Power_Rankings/Powerrankings.csv", as.is = T)
x %>% mutate(group = reorder(Conference, YUSAG_Coefficient, median)) %>%
  ggplot(aes(x = YUSAG_Coefficient, y = group, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 3) + theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) + 
  scale_x_continuous(expand = c(0.01, 0)) +
  theme(legend.position="none") +
  scale_fill_viridis(name = "group", option = "C") +
  labs(y = "Conference", title = "NCAA Men's Basketball Power Rankings")



for(i in 1:8) {
  sims$team[(10000*i - 9999):(10000*i)] <- names(simresults)[i]
  sims$wins[(10000*i - 9999):(10000*i)] <- simresults[,i]
  sims$colors[(10000*i - 9999):(10000*i)] <- cols[9-i]
}

sims %>% mutate(team = reorder(team, wins, mean)) %>%
  ggplot(aes(x = wins, y = team, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3) + theme_ridges() + 
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Conference Wins", option = "C") +
  theme(legend.position="right") +
  labs(y = "Team", x= "Ivy League Wins", 
       title = "Distribution of Ivy League Men's Basketball Conference Wins",
       subtitle = "Based on 10,000 Simulations of Ivy League Season \n@recspecs730/@YaleSportsGroup")

