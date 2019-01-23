library(ggridges)
conf <- "Big 12"
sims <- conf_fast_sim(conf, 10000)
standings <- 
  group_by(sims, team) %>%
  summarise("avg_wins" = mean(n_wins)) %>%
  arrange(desc(avg_wins)) %>%
  mutate("rank" = nrow(.):1) %>%
  arrange(team) %>%
  inner_join(
    filter(ncaa_colors, conference == "Big 12") %>%
      select(ncaa_name, primary_color), by = c("team" = "ncaa_name")
    )

sims$team <- reorder(sims$team, rep(standings$rank, 10000))

ggplot(sims, aes(x = n_wins, y = team)) + 
  geom_density_ridges(aes(fill = team), stat = "binline", scale = 1, bins = 16) + 
  scale_fill_manual(values = arrange(standings, rank) %>% pull(primary_color)) + 
  theme_bw() + 
  labs(x ="# of Wins", 
       y = "Density",
       title = "Distirbution of Conference Wins",
       subtitle = conf) +
  geom_vline(xintercept = 12, size = 1.2, lty = 2) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) 


champ_wins <- 
  group_by(sims, sim) %>%
  summarise("champ" = max(n_wins)) %>%
  ungroup() %>%
  pull(champ)
table(champ_wins)/length(champ_wins)

mean(champ_wins <= 13)
