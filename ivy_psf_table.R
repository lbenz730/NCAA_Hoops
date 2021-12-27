
ivy_psf <- read_rds('3.0_Files/Predictions/ivy_psf_full.rds')
ivy_psf_gt <- 
  ivy_psf %>% 
  inner_join(select(ncaa_colors, ncaa_name, logo_url), by = c('home' = 'ncaa_name')) %>% 
  inner_join(select(ncaa_colors, ncaa_name, logo_url), by = c('away' = 'ncaa_name'), suffix = c('_home', '_away')) %>% 
  mutate_if(is.numeric, ~{.x/100}) %>% 
  mutate('home_bar' = paste0('3.0_Files/Predictions/psf_figures/home_', 1:nrow(.), '.png'),
         'away_bar' = paste0('3.0_Files/Predictions/psf_figures/away_', 1:nrow(.), '.png')) %>% 
  select(date, logo_url_home, logo_url_away, psf, auto_bid_sf, home_bar, away_bar) %>% 
  
  gt() %>% 
  cols_label('date' = 'Date',
             'logo_url_home' = 'Home',
             'logo_url_away' = 'Away',
             'psf' = 'Playoffs',
             'auto_bid_sf' = 'Auto Bid',
             'home_bar' = 'If Home Wins',
             'away_bar' = 'If Away Wins') %>% 
  
  tab_spanner(label = 'Leverage', columns = c('psf', 'auto_bid_sf')) %>% 
  tab_spanner(label = 'Playoff Odds', columns = c('home_bar', 'away_bar')) %>% 
  
  ### Hightlight Columns 
  data_color(
    columns = c(auto_bid_sf, psf),
    colors = scales::col_numeric(
      palette = ggsci::rgb_material('amber', n = 100),
      domain = c(0,1),
    )
  ) %>% 
  
  ### Percent
  fmt_percent(
    columns = c(auto_bid_sf, psf),
    decimals = 1) %>% 
  
  ### Align Columns
  cols_align(
    align = "center",
    columns = any_of(c('home_bar', 'away_bar', names(ivy_psf)))
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
        columns = c(logo_url_away, auto_bid_sf, home_bar)
      )
    )
  ) %>% 
  text_transform(
    locations = cells_body(c(logo_url_home, logo_url_away)),
    fn = function(x) {
      web_image(
        url = x,
        height = 50
      )
    }
  ) %>% 
  text_transform(
    locations = cells_body(c(home_bar, away_bar)),
    fn = function(x) {
      local_image(
        filename  = x,
        height = 200
      )
    }
  ) %>% 
  tab_source_note('@recspecs730') %>% 
  tab_source_note('Leverage = total swing in all teams\' playoff/auto-bid odds between the two possible outcomes.') %>% 
  tab_source_note("2022 Tournament hosted by Harvard University") %>%
  tab_source_note("Based on 1,000 Simulations of each outcome. Ties broken according to official Ivy League tiebreaking rules.") %>% 
  tab_header(title = paste0(
    'Ivy League Playoff Leverage: ', 
    min(ivy_psf$date), 
    ifelse(n_distinct(ivy_psf$date) > 1, paste(' -', max(ivy_psf$date)), ''))) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )