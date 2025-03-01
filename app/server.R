library(shiny)
library(DT)
library(lubridate)
library(ggridges)
library(ggimage)
library(rsvg)
library(purrr)

html_clean <- function(html) {
  html <- tools::toTitleCase(gsub('\\+', ' ', gsub('%27s', '\'', html)))
  print(html)
  return(html)
}

shinyServer(function(input, output, session) {
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    tab <- query$tab
    team <- query$team
    conf <- query$conf
    conft <- query$conft
    
    if(length(tab) > 0) {
      updateNavbarPage(session, inputId = "navbar", selected = tab)
    }
    if(length(team) > 0) {
      updateSelectInput(session, inputId = "team", selected = html_clean(team))
    }
    if(length(conf) > 0) {
      updateSelectInput(session, inputId = "conf", selected = html_clean(conf))
    }
    if(length(conft) > 0) {
      updateSelectInput(session, inputId = "conft", selected = html_clean(conft))
    }
    
  })
  
  
  updateSelectInput(session, inputId = "conft", 
                    choices = sort(t_confs), selected = sort(t_confs)[1])
  
  updateSelectInput(session, inputId = "team", 
                    choices = sort(unique(x$team)), selected = rankings_clean$team[1])
  
  ###################################### Rankings Tab ############################ 
  ### Rankings
  output$rankings <- DT::renderDataTable({
    datatable(
      rankings,
      rownames = F,
      options = list(paging = FALSE,
                     searching = F,
                     info  = F,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    ) %>%
      formatRound(columns = c(4,5,6), 
                  digits = 2) %>%
      formatStyle("Net Rating", backgroundColor = styleInterval(sort(rankings$`Net Rating`[-1]), cm.colors(364)[364:1])) %>%
      formatStyle("Off. Rating", backgroundColor = styleInterval(sort(rankings$`Off. Rating`[-1]), cm.colors(364)[364:1])) %>%
      formatStyle("Def. Rating", backgroundColor = styleInterval(sort(rankings$`Def. Rating`[-1]), cm.colors(364)[364:1]))
    
    
  })
  
  ### Update Date
  output$update <- renderText({
    paste("Updated:", as.character(as.Date(max(history$date))))
  })
  
  ### Update Date
  output$update2 <- renderText({
    paste("Updated:", as.character(as.Date(max(history$date))))
  })
  
  
  ############################ Conference Breakdown ##############################  
  ### Conf Summary Table
  conf_table <- eventReactive(input$conf, {
    print('Conf Table') 
    filter(rankings, Conference == input$conf) %>%
      left_join(conf_projections, by = c("Team" = "team",
                                         "Conference" = "team_conf")) %>%
      mutate("Conference Rank" = 1:nrow(.)) %>%
      inner_join(bracket_odds, by = c("Team" = "team")) %>% 
      select(`Conference Rank`, `Team`,  `Net Rating`, `Off. Rating`, `Def. Rating`,
             n_win, n_loss, conf_wins, 
             conf_losses,
             everything()) %>%
      select(-Conference) %>%
      rename("Overall Rank" = Rank,
             "Proj. Wins" = n_win,
             "Proj. Loss" = n_loss,
             "Proj. Conf. Wins" = conf_wins,
             "Proj. Conf. Loss" = conf_losses,
             'Auto-Bid' = auto_bid,
             'At-Large' = at_large,
             'NCAA Odds' = overall)
    
  })
  
  output$conf_standings <- DT::renderDataTable({
    l <- max(c(1, conf_table()$`Proj. Conf. Wins` + conf_table()$`Proj. Conf. Loss`), na.rm = T)
    datatable(conf_table(),
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) %>%
      formatRound(columns = c(3,4,5,6,7,8,9), digits = 2) %>%
      formatPercentage(columns = 13:15, digits = 1) %>%
      formatStyle("Net Rating", backgroundColor = styleInterval(sort(rankings$`Net Rating`[-1]), cm.colors(364)[364:1])) %>%
      formatStyle("Off. Rating", backgroundColor = styleInterval(sort(rankings$`Off. Rating`[-1]), cm.colors(364)[364:1])) %>%
      formatStyle("Def. Rating", backgroundColor = styleInterval(sort(rankings$`Def. Rating`[-1]), cm.colors(364)[364:1])) %>%
      formatStyle("Proj. Wins", backgroundColor = styleInterval(0:31, cm.colors(33)[33:1])) %>%
      formatStyle("Proj. Loss", backgroundColor = styleInterval(0:31, cm.colors(33))) %>%
      formatStyle("Proj. Conf. Wins", backgroundColor = styleInterval(0:(l-1), cm.colors(l+1)[(l+1):1])) %>%
      formatStyle("Proj. Conf. Loss", backgroundColor = styleInterval(0:(l-1), cm.colors(l+1))) %>% 
      formatStyle("At-Large", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Auto-Bid", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("NCAA Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1]))
    
    
  })
  
  #### Universe Plots
  universe_plot <- eventReactive(input$conf, {
    print('Universe Plot')
    df <- rankings_clean %>% 
      inner_join(df_img, by  = 'team')
    
    
    ggplot(df, aes(x = off_coeff, y = def_coeff)) +
      geom_hline(yintercept = 0, lty = 1, alpha = 0.5, size = 2) + 
      geom_vline(xintercept = 0, lty = 1, alpha = 0.5, size = 2) + 
      geom_abline(slope = rep(-1, 11), intercept = seq(25, -25, -5), alpha = 0.5, lty  = 2) +
      geom_point(alpha = 0.5, aes(color = yusag_coeff), size = 3) +
      geom_image(data = filter(df, conference == input$conf), aes(image = logo_file)) +
      scale_color_viridis_c(option = "C") +
      labs(x = "Offensive Points Relative to Average \nNCAA Division 1 Team",
           y = "Defensive Points Relative to Average \nNCAA Division 1 Team",
           color = "Net Points Relative to Average \nNCAA Division 1 Team",
           title = "Division 1 Men's Basketball Universe"
      )
  })
  
  output$uni_plot <- renderPlot(universe_plot(),
                                width = 1000, 
                                height = 600)
  
  ### Conference Box Plot
  box_plot <- eventReactive(input$conf, {
    print('Conf Box Plot')
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
    print('Standings Plot')
    p <- make_standings_plot(input$conf)
    p
  })
  
  output$conf_standings_plot <- renderPlot(standings_plot(),
                                           height = 600,
                                           width = 1200)
  
  cs <- eventReactive(input$conf, {
    print('Standings Place Table')
    df <- read_csv(paste0("3.0_Files/Predictions/conf_sims/", input$conf, "_win_matrix.csv"))
    df
  })
  
  output$conf_sims <- DT::renderDataTable({
    datatable(cs(),
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      formatPercentage(columns = 2:ncol(cs()), 1) %>%
      formatStyle(names(cs())[-1], backgroundColor = styleInterval(seq(0, 1, 0.01), heat.colors(102)[102:1]))

  })
  
  conf_schedule <- eventReactive(input$conf, { 
    ### Schedule Plot
    print('Schedule Plot')
    visualize_schedule(input$conf)
    })
  
  output$conf_schedule_plot <- renderPlot(conf_schedule(),
                                          height = 600,
                                          width = 1200)

  
  
  
  
  ###################################### Game Predictions ##############################################
  gp <- eventReactive(input$proj_date, {
    df <- filter(x, date == input$proj_date) 
    print(names(df))
    print(df)
    if(nrow(df) > 0) {
      df <- 
        df %>% 
        mutate("id" = case_when(
          location == "H" ~ paste(team, opponent),
          location == "V" ~ paste(opponent, team),
          location == "N" & team < opponent  ~ paste(team, opponent),
          T ~ paste(opponent, team)
        )) %>%
        arrange(rank) %>%
        filter(!duplicated(id)) %>%
        mutate('team_score' = case_when(postponed ~ "Postponed",
                                        canceled ~ "Canceled",
                                        T ~ as.character(team_score))) %>% 
        select(team, opponent, location, rank, opp_rank,
               pred_team_score, pred_opp_score,
               team_score, opp_score, pred_score_diff) %>%
        mutate("win_prob" = predict(glm.pointspread, newdata = ., type = "response")) %>%
        select(team, opponent, location, rank, opp_rank, pred_team_score,
               pred_opp_score, win_prob, team_score, opp_score)
      names(df) <- c("Team", "Opponent", "Location", "Team Rank",
                     "Opponent Rank", "Pred. Team Score", "Pred. Opp. Score",
                     "Win Prob.","Team Score", "Opp. Score")
    } else {
      df <- 
        df %>% 
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
        mutate(win_prob = NA) %>% 
        select(team, opponent, location, rank, opp_rank, pred_team_score,
               pred_opp_score,win_prob, team_score, opp_score) %>% 
        slice(0)
      names(df) <- c("Team", "Opponent", "Location", "Team Rank",
                     "Opponent Rank", "Pred. Team Score", "Pred. Opp. Score",
                     "Win Prob.","Team Score", "Opp. Score")
    }
    df
  })
  
  
  output$game_projections <- DT::renderDataTable({
    datatable(gp(),
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      
      formatRound(columns = c(6, 7), 
                  digits = 1) %>%
      formatPercentage(columns = c(8), 1) %>%
      formatStyle("Win Prob.", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Team Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Opponent Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Pred. Team Score", backgroundColor = styleInterval(40:100, cm.colors(62)[62:1])) %>%
      formatStyle("Pred. Opp. Score", backgroundColor = styleInterval(40:100, cm.colors(62)[62:1]))
  })
  
  
  
  
  ################################## Team Breakdowns ###########################
  ### Profile:
  output$logo <- renderText(paste('<img src=', ncaa_colors$logo_url[ncaa_colors$ncaa_name == input$team], ', height = 100, width = 100</>'))
  output$team <- renderText(paste('<b>Team:</b>', input$team))
  output$team_conf <- renderText(paste('<b>Conference:</b>', confs$conference[confs$team == input$team]))
  output$team_record <- renderText(paste('<b>Record:</b>', paste(win_totals[win_totals$team == input$team, c('n_win', 'n_loss')], collapse = '-')))
  output$conf_record <- renderText(paste('<b>Conference Record:</b>', paste(win_totals[win_totals$team == input$team, c('conf_wins', 'conf_losses')], collapse = '-')))
  output$ratings <- renderText(paste('<b>Ratings:</b><br>Overall:',
                                     sprintf('%0.1f', rankings$`Net Rating`[rankings$Team == input$team]),
                                     paste0('(', rankings$Rank[rankings$Team == input$team], ')'),
                                     '<br>Offense:',
                                     sprintf('%0.1f', rankings$`Off. Rating`[rankings$Team == input$team]),
                                     paste0('(', rankings$`Off. Rank`[rankings$Team == input$team], ')'),
                                     '<br>Defense:',
                                     sprintf('%0.1f', rankings$`Def. Rating`[rankings$Team == input$team]),
                                     paste0('(', rankings$`Def. Rank`[rankings$Team == input$team], ')')
  ))
  
  output$record_breakdown <- renderText(
    paste('<b>Record Breakdown</b>',
          paste0('<br>Tier I: ', records_actual$n_win_a[records_actual$team == input$team], '-', records_actual$n_loss_a[records_actual$team == input$team]),
          paste0('<br>Tier II: ', records_actual$n_win_b[records_actual$team == input$team], '-', records_actual$n_loss_b[records_actual$team == input$team]),
          paste0('<br>Tier III: ', records_actual$n_win_c[records_actual$team == input$team], '-', records_actual$n_loss_c[records_actual$team == input$team]),
          paste0('<br>Tier IV: ', records_actual$n_win_d[records_actual$team == input$team], '-', records_actual$n_loss_d[records_actual$team == input$team]),
          paste0('<br>Non-D1: ', max(0, non_d1_actual$n_win[non_d1_actual$team == input$team], na.rm = T), '-', max(0, non_d1_actual$n_loss[non_d1_actual$team == input$team], na.rm = T))
          
          
    ))
  
  output$ncaa_odds <- renderText(
    paste('<b>NCAA Tournament Odds</b>',
          paste0('<br>Auto Bid: ', sprintf('%0.1f', 100*bracket_odds$auto_bid[bracket_odds$team == input$team]), '%'),
          paste0('<br>At-Large Bid: ', sprintf('%0.1f', 100*bracket_odds$at_large[bracket_odds$team == input$team]), '%'),
          paste0('<br>Overall: ', sprintf('%0.1f', 100*bracket_odds$overall[bracket_odds$team == input$team]), '%')
          
    ))
  
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
      geom_label(data = filter(history, team == input$team, date %in% sapply(as.Date("2024-11-04") + seq(0, 140, 7), function(x) {max(history$date[history$date <= x])})
      ),
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
      geom_label(data = filter(history, team == input$team, date %in% sapply(as.Date("2024-11-04") + seq(0, 140, 7), function(x) {max(history$date[history$date <= x])})
      ),
      aes(label = rank)) +
      scale_y_reverse(limits = c(min(c(364, M + 20)), max(c(1, m - 20))) ) +
      labs(x = "Date",
           y = "Rank",
           title = "Evolution of Rank Over Time",
           subtitle = input$team)
  })
  
  output$ratings_plot <- renderPlot(rhp())
  output$rankings_plot <- renderPlot(rahp())
  
  
  ts1 <- eventReactive(input$team, {
    df <- read_csv(paste0("3.0_Files/Results/2024-25/NCAA_Hoops_Results_",
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
      select(date, opponent, opp_rank, location, team_score, opp_score, pred_team_score, pred_opp_score, wins, canceled, postponed) %>%
      bind_rows(filter(x, team == input$team) %>%
                  select(date, opponent, opp_rank, location, team_score, opp_score, pred_team_score, pred_opp_score, wins, canceled, postponed)) %>% 
      arrange(date) %>%
      mutate('team_score' = case_when(postponed ~ "Postponed",
                                      canceled ~ "Canceled",
                                      T ~ as.character(team_score))) %>% 
      select(date, opponent, opp_rank, location, team_score, opp_score, pred_team_score, pred_opp_score, wins)
    df[df$wins %in% c(0,1), c("pred_team_score", "pred_opp_score")] <- NA
    df$wins[df$wins %in% c(0,1)] <- NA
    df$wins[df$wins > 1] <- 1
    df$result <- NA
    df$result[as.numeric(df$team_score) > as.numeric(df$opp_score)] <- "W"
    df$result[as.numeric(df$team_score) < as.numeric(df$opp_score)] <- "L"
    df <- 
      df %>% 
      mutate('tier' = 
               case_when(
                 opp_rank <= 50 & location == "N" ~ 'I',
                 opp_rank <= 30 & location == "H" ~ 'I',
                 opp_rank <=  75 & location == "V" ~ 'I',
                 
                 opp_rank >= 51 & opp_rank <= 100 & location == "N" ~ 'II',
                 opp_rank >= 31 & opp_rank <= 75 & location == "H" ~ 'II', 
                 opp_rank >= 76 & opp_rank <= 135 & location == "V" ~ 'II',
                 
                 opp_rank >= 101 & opp_rank <= 200 & location == "N" ~ 'III',
                 opp_rank >= 76 & opp_rank <= 160 & location == "H" ~ 'III', 
                 opp_rank >= 136 & opp_rank <= 240 & location == "V" ~ 'III',
                 
                 opp_rank >= 201 & location == "N" ~ 'IV',
                 opp_rank >= 161 & location == "H" ~ 'IV', 
                 opp_rank >= 241 & location == "V" ~ 'IV',
                 
                 T ~ 'Non-D1')
             
      )
    df <- select(df, date, opponent, tier, result, everything())
    names(df) <- c("Date", "Opponent", "Tier", "Result", "Opp. Rank", "Location", "Team Score", "Opponent Score", "Pred. Team Score",
                   "Pred. Opp. Score", "Win Probability")
    
    
    df
    
  })
  
  
  output$team_schedule <- DT::renderDataTable({
    datatable(ts1(),
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                             
              )
    ) %>%
      formatRound(columns = c(9, 10), 
                  digits = 1) %>%
      formatPercentage(columns = c(11), 1) %>%
      formatStyle("Result", target = "row", 
                  backgroundColor = styleEqual(c("W", "L"), c("palegreen", "tomato"))
      )
  })
  
  
  
  
  ################################## Bracketology
  output$bracket <- DT::renderDataTable({
    df <- 
      bracket %>% 
      inner_join(select(bracket_odds, team, auto_bid, overall)) %>% 
      select(seed_line, seed_overall, everything(), -blend, -avg)
    
    df$odds <- 1/100 * df$odds
    names(df)[c(1:13, ncol(df) + c(-1, 0))] <- c("Seed Line", "Seed Overall", "Team", "Conference", 
                                                 "Net Rating", "Strength of Record", "Wins Above Bubble",
                                                 "Resume", "Rating Rank", "SOR Rank", "Resume Rank", 
                                                 "WAB Rank", "At-Large Odds", "Auto-Bid Odds", "Overall Odds")
    
    
    datatable(df,
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                               list(visible=FALSE, targets=c(13, 14, 15))
                                               
                             ))
    ) %>% 
      formatRound(columns = c(5, 6, 7, 8),  digits = 2) %>%
      formatStyle("Team",  valueColumns = "autobid", fontWeight = styleEqual(T, "bold")) %>%
      formatStyle("Team",  valueColumns = "first4", "font-style" = styleEqual(T, "italic")) %>%
      formatPercentage(columns = c(13, ncol(df) + c(-1,0)), 1) %>%
      formatStyle("At-Large Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Auto-Bid Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Overall Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      
      formatStyle("WAB Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("SOR Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Resume Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Rating Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Wins Above Bubble", backgroundColor = styleInterval(sort(bracket_math$wab[1:99]), cm.colors(100)[100:1])) %>%
      formatStyle("Strength of Record", backgroundColor = styleInterval(sort(bracket_math$sor[1:99]), cm.colors(100)[100:1])) %>%
      formatStyle("Net Rating", backgroundColor = styleInterval(sort(rankings$`Net Rating`[1:99]), cm.colors(100)[100:1])) %>%
      
      formatStyle("Resume", backgroundColor = styleInterval(sort(bracket_math$qual_bonus[1:99]), cm.colors(100)[100:1]))
    
    
  })
  
  
  output$bubble <- DT::renderDataTable({
    df <- select(bubble, seed_overall, everything(), -blend, -avg, -mid_major,
                 -wins, -losses, -seed, -autobid, -loss_bonus) %>% 
      inner_join(select(bracket_odds, team, auto_bid, overall))
    df$odds <- 1/100 * df$odds
    print(names(df))
    names(df)[1:14] <- c("Seed Overall", "Team", "Conference", 
                         "Net Rating",  "Strength of Record", "Wins Above Bubble",
                         "Resume", "Rating Rank",  "SOR Rank","Resume Rank", "WAB Rank",
                          "At-Large Odds", "Auto-Bid Odds", 'Overall Odds')
    
    
    datatable(df,
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                             
              )
    ) %>% 
      formatRound(columns = c(4, 5, 6, 7),  digits = 2) %>%
      formatPercentage(columns = c(12, 13, 14), 1) %>%
      formatStyle("At-Large Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Auto-Bid Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("Overall Odds", backgroundColor = styleInterval(seq(0, 0.99, 0.01), cm.colors(101)[101:1])) %>%
      formatStyle("WAB Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("SOR Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Resume Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>%
      formatStyle("Rating Rank", backgroundColor = styleInterval(1:363, cm.colors(364))) %>% 
      formatStyle("Wins Above Bubble", backgroundColor = styleInterval(sort(bracket_math$wab[1:363]), cm.colors(364)[364:1])) %>%
      formatStyle("Strength of Record", backgroundColor = styleInterval(sort(bracket_math$sor[1:363]), cm.colors(364)[364:1])) %>%
      formatStyle("Net Rating", backgroundColor = styleInterval(sort(rankings$`Net Rating`[1:363]), cm.colors(364)[364:1])) %>%
      formatStyle("Resume", backgroundColor = styleInterval(sort(bracket_math$qual_bonus[1:363]), cm.colors(364)[364:1]))
    
    
  })
  
  output$bid_breakdown <- DT::renderDataTable({
    df <- bids
    names(df) <- c("Conference", "Bids")
    datatable(df,
              rownames = F,
              options = list(paging = FALSE,
                             searching = F,
                             info  = F,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                             
              )) %>%
      formatStyle("Bids", background = styleColorBar(c(0, max(bids$n_bid)), 'lightblue'))
    
  })
  
  ### NCAA Simulations
  ncaa_gt <- eventReactive(input$region, {
    make_table(ncaa_sims, input$region)
  })
  output$ncaa_sims <- render_gt(ncaa_gt())
  
  
  ### Ivy
  output$ivy_sims <- render_gt(ivy_gt)
  output$ivy_psf <- render_gt(ivy_psf_gt)
  output$ivy_history <- renderPlot(ivy_history_plot)
  output$ivy_barplot <- renderPlot(ivy_bar)
  output$ivy_snap <- renderPlot(ivy_snapsnot)
  output$ivy_4th <- render_gt(ivy_4th)
  output$ivy_matrix <- render_gt(tab)
  
  
  ### Conf T Sims
  ctsim <- eventReactive(input$conft, {
    df_sim <- read_csv(paste0('3.0_Files/Predictions/conf_tourney_sims/2024-25/', input$conft, '.csv'))
    
    
    df <- 
      df_sim %>% 
      inner_join(df_img, by = 'team') %>% 
      inner_join(select(rankings_clean, team, yusag_coeff)) %>% 
      inner_join(select(confs, team, eliminated)) %>% 
      arrange(desc(champ), desc(finals)) %>% 
      select(team, logo_file, seed, yusag_coeff, finals, champ, eliminated)
    
    table <-
      gt(df %>% filter(!eliminated) %>% select(-eliminated)) %>% 
      cols_label('team' = '', 
                 'logo_file' = '',
                 'seed' = 'Seed',
                 'yusag_coeff' = 'Rating',
                 'finals' = 'Finals',
                 'champ' = 'Champ') %>% 
      
      ### Hightlight Columns 
      data_color(
        columns = c(finals, champ),
        fn = scales::col_numeric(
          palette = ggsci::rgb_material('amber', n = 68),
          domain = c(0,1),
        )
      ) %>% 
      
      data_color(
        columns = c(yusag_coeff),
        fn = scales::col_numeric(
          palette = 'RdYlGn',
          domain = range(df$yusag_coeff)
        ),
      ) %>% 
      
      ### Percent
      fmt_percent(
        columns = c(finals, champ),
        decimals = 1) %>% 
      
      fmt_number(
        columns = c(yusag_coeff),
        decimals = 1) %>% 
      
      
      ### Align Columns
      cols_align(
        align = "center",
        columns = gt::everything()
      ) %>% 
      
      ### Borders
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels(
            columns = gt::everything()
          )
        )
      ) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(yusag_coeff)
          )
        )
      ) %>% 
      text_transform(
        locations = cells_body(c(logo_file)),
        fn = function(x) {
          local_image(
            filename = gsub('amp;', '', x),
            height = 30
          )
        }
      ) %>% 
      tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>%
      tab_source_note("Rating = Points relative to average NCAA team on neutral court") %>%
      tab_header(
        title = md(paste("**2025", input$conf, "Men's Basketball Tournament Odds**")),
      ) %>% 
      tab_options(column_labels.font.size = 16,
                  column_labels.font.weight = 'bold',
                  heading.title.font.size = 30,
                  heading.subtitle.font.size = 20,
                  heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold'
      )
    
    table
    
  })
  
  output$ct_sims <- render_gt(ctsim(),
                              width = 1200,
                              height = 700)
  
  
  
  # outputOptions(output, 'conf_schedule_plot', suspendWhenHidden = FALSE,  priority = 3)
  # outputOptions(output, 'conf_standings', suspendWhenHidden = FALSE, priority = 2)
  # outputOptions(output, 'rankings', suspendWhenHidden = FALSE, priority = 20)
  # outputOptions(output, 'uni_plot', suspendWhenHidden = FALSE, priority = 3)
  
  # outputOptions(output, 'conf_sims', suspendWhenHidden = FALSE, priority = 5)
  # # outputOptions(output, 'conf_box_plot', suspendWhenHidden = FALSE, priority = 6)
  # outputOptions(output, 'conf_standings_plot', suspendWhenHidden = FALSE, priority = 7)
  
})









