library(shiny)
library(DT)
library(lubridate)


shinyServer(function(input, output, session) {
  
  updateSelectInput(session, inputId = "conf", 
                    choices = sort(unique(confs$conference)), selected = "A-10")
  
  updateSelectInput(session, inputId = "team", 
                    choices = sort(unique(x$team)), selected = rankings_clean$team[1])
  
  ###################################### Rankings Tab ############################ 
  ### Rankings
  output$rankings <- DT::renderDataTable({
    
    datatable(rankings, 
              rownames = F,
              options = list(paging = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                             )
    ) %>%
      formatRound(columns = c(4,5,6), 
                  digits = 2)
    
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
      select(`Conference Rank`, `Team`, n_win, n_loss, conf_wins, 
             conf_losses, `Net Rating`, `Off. Rating`, `Def. Rating`,
             everything()) %>%
      select(-Conference) %>%
      rename("Overall Rank" = Rank,
             "Proj. Wins" = n_win,
             "Proj. Loss" = n_loss,
             "Proj. Conf. Wins" = conf_wins,
             "Prof. Conf. Loss" = conf_losses)
  })
  
  output$conf_standings <- DT::renderDataTable({
    datatable(conf_table(),
              rownames = F,
              options = list(paging = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) %>%
      formatRound(columns = c(3,4,5,6,7,8,9), 
                  digits = 2)
  })
  
  #### Universe Plote
  universe_plot <- eventReactive(input$conf, {
    ggplot(rankings_clean, aes(x = off_coeff, y = def_coeff)) +
      geom_point(alpha = 0.5, aes(color = yusag_coeff), size = 3) +
      geom_point(data = filter(rankings_clean, conference == input$conf), size = 3) +
      geom_label_repel(data = filter(rankings_clean, conference == input$conf), 
                       aes(label = team), force = 2, size = 5, min.segment.length = 0.25) +
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
                   "Opponent Rank", "Pred. Team Score", "Pred Opp Score",
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
      formatPercentage(columns = c(8), 1)
  })
  
  
  ################################## Team Breakdowns ###########################
  rhp <- eventReactive(input$team, {
    ggplot(filter(history, team == input$team), aes(x = date, y = yusag_coeff)) %>% +
      geom_line(color = "orange", size = 2) +
      scale_y_continuous(limits = c(-25, 25)) +
      geom_label(data = filter(history, team == input$team, date %in% c(as.Date("2019-11-05") + seq(0, 140, 7), max(history$date))),
                 aes(label = sprintf("%.2f", yusag_coeff))) +
      labs(x = "Date",
           y = "Points Relative to Average NCAA Division 1 Team",
           title = "Evolution of Net Rating Over Time",
           subtitle = input$team)
  })
  
  rahp <- eventReactive(input$team, {
    ggplot(filter(history, team == input$team), aes(x = date, y = rank)) %>% +
      geom_line(color = "orange", size = 2) +
      geom_label(data = filter(history, team == input$team, date %in% c(as.Date("2019-11-05") + seq(0, 140, 7), max(history$date))),
                 aes(label = rank)) +
      scale_y_reverse(limits = c(353, 1)) +
      labs(x = "Date",
           y = "Rank",
           title = "Evolution of Rank Over Time",
           subtitle = input$team)
  })
  
  output$ratings_plot <- renderPlot(rhp())
  output$rankings_plot <- renderPlot(rahp())
  
  ts1 <- eventReactive(input$team, {
    df <- read_csv(paste0("3.0_Files/Results/2019-20/NCAA_Hoops_Results_",
                          paste(unlist(strsplit(as.character(max(history$date)), "-"))[c(2,3,1)], collapse = "_"),
                          ".csv")) %>% 
      filter(D1 == 1) %>%
      filter(team == input$team) %>%
      mutate("date" = as.Date(paste(year, month, day, sep = "-")),
             "team_score" = teamscore,
             "opp_score" = oppscore,
             "pred_team_score" = NA,
             "pred_opp_score" = NA,
             "opp_team_score" = NA,
             "wins" = case_when(teamscore > oppscore ~ 1, 
                                teamscore < oppscore ~ 0,
                                T ~ 1.0001)
      ) %>%
      select(date, opponent, location, team_score, opp_score, pred_team_score, pred_opp_score, wins) %>%
      bind_rows(filter(x, team == input$team) %>%
                  select(date, opponent, location, team_score, opp_score, pred_team_score, pred_opp_score, wins)) %>% 
      arrange(date) %>%
      select(date, opponent, location, team_score, opp_score, pred_team_score, pred_opp_score, wins)
    df[df$wins %in% c(0,1), c("pred_team_score", "pred_opp_score")] <- NA
    df$wins[df$wins %in% c(0,1)] <- NA
    df$wins[df$wins > 1] <- 1
    names(df) <- c("Date", "Opponent", "Location", "Team Score", "Opponent Score", "Pred. Team Score",
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
      formatRound(columns = c(6, 7), 
                  digits = 1) %>%
      formatPercentage(columns = c(8), 1)
  })
  
  
})








