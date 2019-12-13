library(shiny)
library(DT)
library(lubridate)
library(ggridges)
library(ggimage)

shinyServer(function(input, output, session) {
  
  updateSelectInput(session, inputId = "conf", 
                    choices = sort(unique(confs$conference)), selected = "A-10")
  
  updateSelectInput(session, inputId = "team", 
                    choices = sort(unique(x$team)), selected = rankings_clean$team[1])
  
  ###################################### Rankings Tab ############################ 
  ### Rankings
  output$rankings <- DT::renderDataTable({
    datatable(
      rankings,
      rownames = F,
      options = list(paging = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    ) %>%
      formatRound(columns = c(4,5,6), 
                  digits = 2) %>%
      formatStyle("Net Rating", backgroundColor = styleInterval(sort(rankings$`Net Rating`[-1]), cm.colors(353)[353:1])) %>%
      formatStyle("Off. Rating", backgroundColor = styleInterval(sort(rankings$`Off. Rating`[-1]), cm.colors(353)[353:1])) %>%
      formatStyle("Def. Rating", backgroundColor = styleInterval(sort(rankings$`Def. Rating`[-1]), cm.colors(353)[353:1]))
    
    
  })
  
  ### Update Date
  output$update <- renderText({
    paste("Updated:", as.character(as.Date(max(history$date))))
  })
  
  
  ############################ Conference Breakdown ##############################  
  ### Conf Summary Table
  conf_table <- eventReactive(input$conf, {
    filter(rankings, Conference == input$conf) %>%
      inner_join(conf_projections, by = c("Team" = "team",
                                          "Conference" = "team_conf")) %>%
      mutate("Conference Rank" = 1:nrow(.)) %>%
      select(`Conference Rank`, `Team`,  `Net Rating`, `Off. Rating`, `Def. Rating`,
             n_win, n_loss, conf_wins, 
             conf_losses,
             everything()) %>%
      select(-Conference) %>%
      rename("Overall Rank" = Rank,
             "Proj. Wins" = n_win,
             "Proj. Loss" = n_loss,
             "Proj. Conf. Wins" = conf_wins,
             "Proj. Conf. Loss" = conf_losses)
  })
  
  output$conf_standings <- DT::renderDataTable({
    l <- conf_table()$`Proj. Conf. Wins`[1] + conf_table()$`Proj. Conf. Loss`[1]
    datatable(conf_table(),
              rownames = F,
              options = list(paging = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) %>%
      formatRound(columns = c(3,4,5,6,7,8,9), 
                  digits = 2) %>%
      formatStyle("Net Rating", backgroundColor = styleInterval(sort(rankings$`Net Rating`[-1]), cm.colors(353)[353:1])) %>%
      formatStyle("Off. Rating", backgroundColor = styleInterval(sort(rankings$`Off. Rating`[-1]), cm.colors(353)[353:1])) %>%
      formatStyle("Def. Rating", backgroundColor = styleInterval(sort(rankings$`Def. Rating`[-1]), cm.colors(353)[353:1])) %>%
      formatStyle("Proj. Wins", backgroundColor = styleInterval(0:31, cm.colors(33)[33:1])) %>%
      formatStyle("Proj. Loss", backgroundColor = styleInterval(0:31, cm.colors(33))) %>%
      formatStyle("Proj. Conf. Wins", backgroundColor = styleInterval(0:(l-1), cm.colors(l+1)[(l+1):1])) %>%
      formatStyle("Proj. Conf. Loss", backgroundColor = styleInterval(0:(l-1), cm.colors(l+1)))
    
      
  })
  
  #### Universe Plote
  universe_plot <- eventReactive(input$conf, {
    df <- rankings_clean %>%
      inner_join(select(ncaahoopR::ncaa_colors, -conference),
                 by = c("team" = "ncaa_name"))
    
    
    ggplot(df, aes(x = off_coeff, y = def_coeff)) +
      geom_hline(yintercept = 0, lty = 2, alpha = 0.5, size = 2) + 
      geom_vline(xintercept = 0, lty = 2, alpha = 0.5, size = 2) + 
      geom_point(alpha = 0.5, aes(color = yusag_coeff), size = 3) +
      geom_image(data = filter(df, conference == input$conf), aes(image = logo_url)) +
      scale_color_viridis_c(option = "C") +
      labs(x = "Offensive Points Relative to Average \nNCAA Division 1 Team",
           y = "Defensive Points Relative to Average \nNCAA Division 1 Team",
           color = "Net Points Relative to Average \nNCAA Division 1 Team",
           title = "Division 1 Men's Basketball Universe"
      )
  })
  
  output$uni_plot <- renderPlot(universe_plot())
  
  ### Conference Box Plot
  box_plot <- eventReactive(input$conf, {
    mutate(rankings_clean, conference = reorder(conference, yusag_coeff, median)) %>%
      ggplot(aes(x = conference, y = yusag_coeff)) +
      geom_boxplot(alpha = 0) + 
      geom_boxplot(data = filter(rankings_clean, conference != input$conf), fill = "orange", alpha = 0.2) + 
      geom_boxplot(data = filter(rankings_clean, conference == input$conf), fill = "skyblue", alpha = 0.7) +
      geom_point(alpha = 0.2) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Conference",
           y = "Points Relative to Average NCAA Division 1 Team",
           title = "Conference Rankings")
    
    
  })
  
  
  output$conf_box_plot <- renderPlot(box_plot())
  
  
  ### Standings Plot
  standings_plot <- eventReactive(input$conf, {
    sims <- read_csv(paste0("3.0_Files/Predictions/conf_sims/", input$conf, ".csv"))
    
    standings <- 
      group_by(sims, team) %>%
      summarise("avg_wins" = mean(n_wins)) %>%
      arrange(desc(avg_wins)) %>%
      mutate("rank" = nrow(.):1) %>%
      arrange(team) %>%
      left_join(select(ncaahoopR::ncaa_colors, ncaa_name, primary_color), 
                 by = c("team" = "ncaa_name"))
    
    champion <- 
      group_by(sims, sim) %>%
      summarise("n_wins" = max(n_wins)) %>%
      group_by(n_wins)%>%
      summarise("champ_freq" = n()/nrow(.))
    
    sims$team <- as.factor(sims$team)
    sims$team <- reorder(sims$team, rep(standings$rank, 10000))
    standings <- arrange(standings, avg_wins)
    
   ggplot(sims, aes(x = n_wins, y = team, fill = team)) + 
      geom_density_ridges(stat = "binline", scale = 0.7, bins = max(champion$n_wins)) + 
      labs(x ="# of Wins", 
           y = "Team",
           title = "Distribution of Conference Wins",
           subtitle = input$conf) +
      theme(legend.position = "none") +
      scale_fill_manual(values = c(standings$primary_color)) 
  })
  
  output$conf_standings_plot <- renderPlot(standings_plot())
  
  ###################################### Game Predictions ##############################################
  gp <- eventReactive(input$proj_date, {
    df <- filter(x, date == input$proj_date) %>% 
      mutate("id" = case_when(
        location == "H" ~ paste(team, opponent),
        location == "V" ~ paste(opponent, team),
        location == "N" & team < opponent  ~ paste(team, opponent),
        T ~ paste(opponent, team)
      )) %>%
      arrange(rank) %>%
      filter(!duplicated(id)) %>%
      select(team, opponent, location, rank, opp_rank,
             pred_team_score, pred_opp_score,
             team_score, opp_score, pred_score_diff) %>%
      mutate("win_prob" = predict(glm.pointspread, newdata = ., type = "response")) %>%
      select(team, opponent, location, rank, opp_rank, pred_team_score,
             pred_opp_score, win_prob, team_score, opp_score)
    names(df) <- c("Team", "Opponent", "Location", "Team Rank",
                   "Opponent Rank", "Pred. Team Score", "Pred. Opp. Score",
                   "Win Prob.","Team Score", "Opp. Score")
    df
  })
  
  
  output$game_projections <- DT::renderDataTable({
    datatable(gp(),
              rownames = F,
              options = list(paging = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      
      formatRound(columns = c(6, 7), 
                  digits = 1) %>%
      formatPercentage(columns = c(8), 1) %>%
      formatStyle("Win Prob.", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Team Rank", backgroundColor = styleInterval(1:352, cm.colors(353))) %>%
      formatStyle("Opponent Rank", backgroundColor = styleInterval(1:352, cm.colors(353))) %>%
      formatStyle("Pred. Team Score", backgroundColor = styleInterval(40:100, cm.colors(62)[62:1])) %>%
      formatStyle("Pred. Opp. Score", backgroundColor = styleInterval(40:100, cm.colors(62)[62:1]))
  })
  
  
  ################################## Team Breakdowns ###########################
  rhp <- eventReactive(input$team, {
    M <- filter(history, team == input$team) %>%
      pull(yusag_coeff) %>%
      max()
    
    m <- filter(history, team == input$team) %>%
      pull(yusag_coeff) %>%
      min()
    
    color_team <- ncaahoopR::ncaa_colors %>%
      filter(ncaa_name == input$team) %>%
      pull(primary_color)
    color_team <- ifelse(length(color_team) == 0, "orange", color_team)
    
    
    ggplot(filter(history, team == input$team), aes(x = date, y = yusag_coeff)) %>% +
      geom_line(color = color_team, size = 2) +
      scale_y_continuous(limits = c(-3 + m, 3 + M)) +
      geom_label(data = filter(history, team == input$team, date %in% c(as.Date("2019-11-05") + seq(0, 140, 7), max(history$date))),
                 aes(label = sprintf("%.2f", yusag_coeff))) +
      labs(x = "Date",
           y = "Points Relative to Average NCAA Division 1 Team",
           title = "Evolution of Net Rating Over Time",
           subtitle = input$team)
  })
  
  rahp <- eventReactive(input$team, {
    M <- filter(history, team == input$team) %>%
      pull(rank) %>%
      max()
    
    m <- filter(history, team == input$team) %>%
      pull(rank) %>%
      min()
    
    color_team <- ncaahoopR::ncaa_colors %>%
      filter(ncaa_name == input$team) %>%
      pull(primary_color)
    color_team <- ifelse(length(color_team) == 0, "orange", color_team)
    
    ggplot(filter(history, team == input$team), aes(x = date, y = rank)) %>% +
      geom_line(color = color_team, size = 2) +
      geom_label(data = filter(history, team == input$team, date %in% c(as.Date("2019-11-05") + seq(0, 140, 7), max(history$date))),
                 aes(label = rank)) +
      scale_y_reverse(limits = c(min(c(353, M + 20)), max(c(1, m - 20))) ) +
      labs(x = "Date",
           y = "Rank",
           title = "Evolution of Rank Over Time",
           subtitle = input$team)
  })
  
  output$ratings_plot <- renderPlot(rhp())
  output$rankings_plot <- renderPlot(rahp())
  
  ts1 <- eventReactive(input$team, {
    df <- read_csv(paste0("3.0_Files/Results/2019-20/NCAA_Hoops_Results_",
                          paste(gsub("^0", "", unlist(strsplit(as.character(max(history$date)), "-"))[c(2,3,1)]), collapse = "_"),
                          ".csv")) %>% 
      filter(D1 == 1) %>%
      filter(team == input$team) %>%
      mutate("date" = as.Date(paste(year, month, day, sep = "-")),
             "team_score" = teamscore,
             "opp_score" = oppscore,
             "pred_team_score" = NA,
             "pred_opp_score" = NA,
             "opp_team_score" = NA,
             "opp_rank" = NA,
             "wins" = case_when(teamscore > oppscore ~ 1, 
                                teamscore < oppscore ~ 0,
                                opponent == "TBA" ~ 0.5,
                                T ~ 1.0001)
      ) %>%
      select(date, opponent, opp_rank, location, team_score, opp_score, pred_team_score, pred_opp_score, wins) %>%
      bind_rows(filter(x, team == input$team) %>%
                  select(date, opponent, opp_rank, location, team_score, opp_score, pred_team_score, pred_opp_score, wins)) %>% 
      arrange(date) %>%
      select(date, opponent, opp_rank, location, team_score, opp_score, pred_team_score, pred_opp_score, wins)
    df[df$wins %in% c(0,1), c("pred_team_score", "pred_opp_score")] <- NA
    df$wins[df$wins %in% c(0,1)] <- NA
    df$wins[df$wins > 1] <- 1
    df$result <- NA
    df$result[df$team_score > df$opp_score] <- "W"
    df$result[df$team_score < df$opp_score] <- "L"
    df <- select(df, date, opponent, result, everything())
    names(df) <- c("Date", "Opponent", "Result", "Opp. Rank", "Location", "Team Score", "Opponent Score", "Pred. Team Score",
                   "Pred. Opp. Score", "Win Probability")
    
    
    df
    
  })
  
  
  output$team_schedule <- DT::renderDataTable({
    datatable(ts1(),
              rownames = F,
              options = list(paging = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                             
              )
    ) %>%
      formatRound(columns = c(8, 9), 
                  digits = 1) %>%
      formatPercentage(columns = c(10), 1) %>%
      formatStyle("Result", target = "row", 
                  backgroundColor = styleEqual(c("W", "L"), c("palegreen", "tomato"))
      )
  })
  
  
})








