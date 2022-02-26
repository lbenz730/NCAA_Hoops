x %>% 
  group_by(team, team_conf) %>% 
  summarise('conf_wins' = sum(wins[conf_game])) %>% 
  arrange(desc(conf_wins)) %>% 
  ungroup() %>% 
  filter(team_conf == 'Sunbelt') %>% 
  pull(team) %>% 
  paste0("'", ., "'") %>% 
  paste(collapse = ', ') %>% 
  cat()

df_sim <- 
  tourney_sim(teams = c('Texas St.', 'Appalachian St.',  'Georgia St.', 'Troy',
                        'South Alabama', 'Arkansas St.', 'Coastal Carolina', 'Louisiana', 
                        'UT Arlington', 'Ga. Southern', 'ULM', 'Little Rock'),
              seeds = 1:12,
              byes = 4,
              double_byes = 0,
              hca = NA,
              nsims = 2000)


df <- 
  df_sim %>% 
  inner_join(select(ncaa_colors, 'team' = ncaa_name, logo_url)) %>% 
  inner_join(select(power_rankings, team, yusag_coeff)) %>% 
  arrange(desc(champ), desc(finals)) %>% 
  select(team, logo_url, seed, yusag_coeff, finals, champ)


gt(df) %>% 
  cols_label('team' = '', 
             'logo_url' = '',
             'seed' = 'Seed',
             'yusag_coeff' = 'Rating',
             'finals' = 'Finals',
             'champ' = 'Champ') %>% 
  
  ### Hightlight Columns 
  data_color(
    columns = c(finals, champ),
    colors = scales::col_numeric(
      palette = ggsci::rgb_material('amber', n = 68),
      domain = c(0,1),
    )
  ) %>% 
  
  data_color(
    columns = c(yusag_coeff),
    colors = scales::col_numeric(
      palette = ggsci::rgb_material('amber', n = 358),
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
    locations = cells_body(c(logo_url)),
    fn = function(x) {
      web_image(
        url = x,
        height = 30
      )
    }
  ) %>% 
  # tab_source_note("2022 Tournament hosted by Harvard University") %>%
  # tab_source_note("Based on 5,000 Simulations. Ties broken according to official Ivy League tiebreaking rules.") %>%

  tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>%
  tab_header(
    title = md("**2022 Sunbelt Men's Basketball Tournament Odds**"),
    # subtitle = md(paste0('**', table_region, " Region**"))
  ) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )



