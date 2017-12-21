library(ggplot2)
library(ggridges)
library(dplyr)
library(viridis)

x <- read.csv("2.0_Files/Power_Rankings/Powerrankings.csv", as.is = T)
x %>% mutate(group = reorder(Conference, YUSAG_Coefficient, median)) %>%
  ggplot(aes(x = YUSAG_Coefficient, y = group, fill = ..x..)) + 
  geom_joy_gradient(scale = 3) + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) + 
  scale_x_continuous(expand = c(0.01, 0)) +
  theme(legend.position="none") +
  scale_fill_viridis(name = "group", option = "C") +
  labs(y = "Conference", title = "NCAA Men's Basketball Power Rankings")