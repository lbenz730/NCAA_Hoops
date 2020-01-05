und <- filter(x, date <= Sys.Date(), !is.na(team_score)) %>%
  group_by(team) %>%
  summarise("loss" = sum(team_score < opp_score)) %>%
  ungroup() %>%
  filter(loss == 0) %>%
  pull(team)

und <- und[und != "Butler"]

x$lower <- x$pred_score_diff - 5
x$upper <- x$pred_score_diff + 5

df <- filter(x, date > Sys.Date(), team %in% und) %>%
  select(team, wins, date, lower, upper)

df <- bind_rows(df, data.frame("team" = und,
                               "wins" = 1,
                               "date" = Sys.Date(),
                               "lower" = 100,
                               "upper" = 100))


df$lower_p <- round(predict(glm.pointspread, newdata = rename(df, "pred_score_diff" = lower), type = "response"), 3)
df$upper_p <- round(predict(glm.pointspread, newdata = rename(df, "pred_score_diff" = upper), type = "response"), 3)


df$surv_p <- NA
df <- arrange(df, date)
for(team in und) {
  df$surv_p[df$team == team] <- cumprod(df$wins[df$team == team])
  df$surv_p_up[df$team == team] <- cumprod(df$upper_p[df$team == team])
  df$surv_p_low[df$team == team] <- cumprod(df$lower_p[df$team == team])
}


theme_set(theme_bw() +
            theme(axis.title = element_text(size = 20),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 24, hjust  = 0.5),
                  plot.subtitle = element_text(size = 18, hjust = 0.5),
                  strip.text = element_text(size = 14),
                  legend.position = "none"))


df <- mutate(df, "days" = date - Sys.Date()) %>%
  mutate("ev" = surv_p * (1-wins) * days) %>%
  group_by(team) %>%
  summarise("ev" = round(as.numeric(sum(ev)), 1)) %>%
  inner_join(df)

ggplot(df, aes(x = date, y = surv_p)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.3) +
  geom_stepribbon(aes(ymin = surv_p_low, ymax = surv_p_up, fill = team), alpha = 0.4) +
  geom_step(aes(color = team)) + 
  facet_wrap(~team) +
  scale_color_manual(values = ncaa_colors$primary_color[ncaa_colors$ncaa_name %in% und]) +
  scale_fill_manual(values = ncaa_colors$secondary_color[ncaa_colors$ncaa_name %in% und]) +
  scale_y_continuous(labels = function(x) {paste0(100*x, "%")}) +
  labs(x = "Date",
       y = "Survival Proability",
       title = "Probability of Remaining Undefeated by Given Date",
       subtitle = "NCAA Men's Basketball Remaining Unbeaten Teams",
       color = "",
       fill = "") +
  geom_text(x = as.Date("2020-02-10"), y = 0.9, 
            aes(label = paste("Expected # of Days Undefeated:", ev)), size = 3.5) 

ggsave("survplot.png", height = 9/1.2, width = 16/1.2)



