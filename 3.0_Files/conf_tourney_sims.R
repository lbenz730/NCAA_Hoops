### Enter Seeds 


update_conf_seeds <- function() {
  # Sunbelt To to Edit Sunbelt
  seed_teams <-
    c('South Alabama',
      'James Madison',
      'Troy',
      'Arkansas St.',
      'Marshall',
      'App State',
      'Texas St.',
      'Georgia St.',
      'Ga. Southern',
      'Old Dominion',
      'Louisiana',
      'Southern Miss.',
      'Coastal Carolina',
      'ULM')
  
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-Sun
  seed_teams <-
    c('Lipscomb',
      'North Ala.',
      'FGCU',
      'Jacksonville',
      'Eastern Ky.',
      'Queens (NC)',
      'Austin Peay',
      'North Florida',
      'Stetson',
      'Central Ark.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # CAA
  seed_teams <-
    c('Towson',
      'UNCW',
      'Col. of Charleston',
      'William & Mary',
      'Campbell',
      'Monmouth',
      'Northeastern',
      'Drexel',
      'Elon',
      'Hampton',
      'Hofstra',
      'Delaware',
      'Stony Brook',
      'N.C. A&T')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southern
  seed_teams <-
    c('Chattanooga',
      'UNC Greensboro',
      'ETSU',
      'Samford',
      'Furman',
      'Wofford',
      'VMI',
      'Mercer',
      'Western Caro.',
      'The Citadel')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # NEC
  seed_teams <-
    c('Central Conn. St.',
      'LIU',
      'Saint Francis (PA)',
      'Fairleigh Dickinson',
      'Stonehill',
      'Wagner',
      'Chicago St.',
      'Le Moyne')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # # Big South
  seed_teams <-
    c('High Point',
      'UNC Asheville',
      'Winthrop',
      'Radford',
      'Presbyterian',
      'Longwood',
      'Charleston So.',
      'Gardner-Webb',
      'USC Upstate')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Patriot
  seed_teams <-
    c('Bucknell',
      'American',
      'Colgate',
      'Boston U.',
      'Navy',
      'Army West Point',
      'Lafayette',
      'Loyola Maryland',
      'Lehigh',
      'Holy Cross')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # OVC
  seed_teams <-
    c('Southeast Mo. St.',
      'SIUE',
      'Tennessee St.',
      'Little Rock',
      'Tennessee Tech',
      'Lindenwood',
      'Morehead St.',
      'UT Martin')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Horizon
  seed_teams <-
    c('Robert Morris',
      'Cleveland St.',
      'Milwaukee',
      'Youngstown St.',
      'Purdue Fort Wayne',
      'Oakland',
      'Northern Ky.',
      'Wright St.',
      'IU Indy',
      'Detroit Mercy',
      'Green Bay')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # WCC
  seed_teams <-
    c('Saint Mary\'s (CA)',
      'Gonzaga',
      'San Francisco',
      'Santa Clara',
      'Oregon St.',
      'Washington St.',
      'LMU (CA)',
      'Portland',
      'Pepperdine',
      'Pacific',
      'San Diego')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Summit
  seed_teams <-
    c('Omaha',
      'St. Thomas (MN)',
      'South Dakota St.',
      'North Dakota St.',
      'South Dakota',
      'North Dakota',
      'Denver',
      'Kansas City',
      'Oral Roberts')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MVC
  seed_teams <-
    c('Drake',
      'Bradley',
      'UNI',
      'Belmont',
      'Illinois St.',
      'UIC',
      'Murray St.',
      'Southern Ill.',
      'Indiana St.',
      'Evansville',
      'Valparaiso',
      'Missouri St.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # # Big Sky
  # seed_teams <- 
  #   c('Eastern Wash.',
  #     'Northern Colo.',
  #     'Montana', 
  #     'Weber St.', 
  #     'Montana St.', 
  #     'Portland St.', 
  #     'Northern Ariz.',
  #     'Idaho',
  #     'Idaho St.',
  #     'Sacramento St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # 
  # # Am. East
  # seed_teams <- 
  #   c('Vermont', 
  #     'UMass Lowell',
  #     'Bryant',
  #     'New Hampshire', 
  #     'Binghamton',
  #     'Maine',
  #     'UMBC',
  #     'UAlbany')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MEAC
  # seed_teams <- 
  #   c('Norfolk St.',
  #     'N.C. Central',
  #     'South Carolina St.',
  #     'Howard', 
  #     'Morgan St.',
  #     'Delaware St.',
  #     'UMES', 
  #     'Coppin St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Southland
  # seed_teams <- 
  #   c('McNeese',
  #     'A&M-Corpus Christi',
  #     'Nicholls',
  #     'Lamar University',
  #     'Southeastern La.',
  #     'Northwestern St.',
  #     'Tex. A&M-Commerce',
  #     'New Orleans')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MAC
  # seed_teams <-
  #   c('Toledo', 
  #     'Akron', 
  #     'Ohio',
  #     'Central Mich.',
  #     'Bowling Green', 
  #     'Western Mich.',
  #     'Miami (OH)',
  #     'Kent St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Ivy
  # seed_teams <-
  #   c('Princeton',
  #     'Yale',  
  #     'Cornell',
  #     'Brown')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # C-USA
  # seed_teams <- 
  #   c('Sam Houston',
  #     'Louisiana Tech',
  #     'Western Ky.',
  #     'Liberty',
  #     'UTEP',
  #     'New Mexico St.',
  #     'Middle Tenn.',
  #     'Jacksonville St.',
  #     'FIU')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # A-10
  # seed_teams <- 
  #   c('Richmond',
  #     'Loyola Chicago',
  #     'Dayton',
  #     'Massachusetts',
  #     'VCU',
  #     'Duquesne',
  #     'St. Bonaventure',
  #     'George Mason',
  #     'Saint Joseph\'s',
  #     'La Salle',
  #     'Rhode Island',
  #     'Fordham',
  #     'Davidson',
  #     'Saint Louis',
  #     'George Washington')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MAAC
  # seed_teams <- 
  #   c('Quinnipiac',
  #     'Fairfield',
  #     'Marist',
  #     'Rider',
  #     'Saint Peter\'s',
  #     'Niagara',
  #     'Iona',
  #     'Mount St. Mary\'s',
  #     'Canisius',
  #     'Manhattan',
  #     'Siena')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # SEC
  # seed_teams <-  
  #   c('Tennessee',
  #     'Kentucky',
  #     'Alabama',
  #     'Auburn',
  #     'South Carolina',
  #     'Florida',
  #     'Texas A&M',
  #     'LSU',
  #     'Mississippi St.',
  #     'Ole Miss',
  #     'Georgia',
  #     'Arkansas',
  #     'Vanderbilt',
  #     'Missouri')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big East
  # seed_teams <- 
  #   c('UConn',
  #     'Marquette',
  #     'Creighton',
  #     'Seton Hall',
  #     'St. John\'s (NY)',
  #     'Villanova',
  #     'Providence',
  #     'Butler',
  #     'Xavier',
  #     'Georgetown',
  #     'DePaul')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # ACC
  # seed_teams <-
  #   c('North Carolina',
  #     'Duke',
  #     'Virginia',
  #     'Pittsburgh',
  #     'Wake Forest',
  #     'Clemson',
  #     'Syracuse',
  #     'Virginia Tech',
  #     'Florida St.',
  #     'NC State',
  #     'Boston College',
  #     'Notre Dame',
  #     'Georgia Tech',
  #     'Miami (FL)',
  #     'Louisville')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # SWAC
  # seed_teams <- 
  #   c('Grambling',
  #     'Alcorn',
  #     'Texas Southern',
  #     'Southern U.',
  #     'Bethune-Cookman',
  #     'Jackson St.',
  #     'Alabama A&M',
  #     'Alabama St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big 12
  # seed_teams <- 
  #   c('Houston',
  #     'Iowa St.',
  #     'Baylor',
  #     'Texas Tech',
  #     'BYU',
  #     'Kansas',
  #     'Texas',
  #     'TCU',
  #     'Oklahoma',
  #     'Kansas St.',
  #     'Cincinnati',
  #     'UCF',
  #     'Oklahoma St.',
  #     'West Virginia')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Pac 12
  # seed_teams <-
  #   c('Arizona',
  #     'Washington St.',
  #     'Colorado',
  #     'Oregon',
  #     'UCLA',
  #     'Utah',
  #     'California',
  #     'Washington',
  #     'Southern California',
  #     'Stanford',
  #     'Arizona St.',
  #     'Oregon St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # 
  # # MWC
  # seed_teams <-
  #   c('Utah St.',
  #     'Nevada',
  #     'Boise St.',
  #     'UNLV',
  #     'San Diego St.',
  #     'New Mexico',
  #     'Colorado St.',
  #     'Wyoming',
  #     'Fresno St.',
  #     'San Jose St.',
  #     'Air Force')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # WAC
  # seed_teams <-
  #   c('Grand Canyon',
  #     'Tarleton St.',
  #     'UT Arlington',
  #     'Seattle U',
  #     'Utah Valley',
  #     'Abilene Christian',
  #     
  #     'SFA',
  #     'California Baptist')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big West
  # seed_teams <-
  #   c('UC Irvine',
  #     'UC Davis',
  #     'Hawaii',
  #     'Long Beach St.',
  #     'UC Riverside',
  #     'UC Santa Barbara',
  #     'CSUN',
  #     'CSU Bakersfield')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # AAC
  # seed_teams <-
  #   c('South Fla.',
  #     'Fla. Atlantic',
  #     'Charlotte',
  #     'UAB',
  #     'Memphis',
  #     'SMU',
  #     'North Texas',
  #     'East Carolina',
  #     'Tulsa',
  #     'Tulane',
  #     'Temple',
  #     'Wichita St.',
  #     'Rice',
  #     'UTSA')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big 10
  # seed_teams <-
  #   c('Purdue',
  #     'Illinois',
  #     'Nebraska',
  #     'Northwestern',
  #     'Wisconsin',
  #     'Indiana',
  #     'Iowa',
  #     'Michigan St.',
  #     'Minnesota',
  #     'Ohio St.',
  #     'Penn St.',
  #     'Maryland',
  #     'Rutgers',
  #     'Michigan')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  
  ### Elimiate teams that didn't even make their conf tournament
  confs <- 
    confs %>% 
    group_by(conference) %>% 
    mutate('conf_seeded' = any(!is.na(conf_seed))) %>% 
    ungroup() %>% 
    mutate('eliminated' = ifelse(conf_seeded & is.na(conf_seed), T, eliminated)) %>% 
    select(-conf_seeded)
  
  write_csv(confs, '3.0_Files/Info/conferences.csv')
  return(confs)
}


conf_tourney_graphics <- function(year = '2024-25') {
  if(!dir.exists(paste0('graphics/conf_tourneys/', year))) {
    dir.create(paste0('graphics/conf_tourneys/', year))
  }
  
  img_files <- dir('app/www/')
  df_img <- 
    tibble('team' = teams,
           'logo_file' = paste0('app/www/', map_chr(teams, ~case_when(paste0(.x, '.png') %in% img_files ~ paste0(.x, '.png'), 
                                                                      paste0(.x, '.svg') %in% img_files ~ paste0(.x, '.svg'), 
                                                                      paste0(.x, '.jpg') %in% img_files ~ paste0(.x, '.jpg')))))
  
  
  
  for(conf in sort(unique(confs$conference))) {
    print(conf)
    if(file.exists(paste0('3.0_Files/Predictions/conf_tourney_sims/', year, '/', conf, '.csv'))) {
      df_sim <- read_csv(paste0('3.0_Files/Predictions/conf_tourney_sims/', year, '/', conf, '.csv'), col_types = cols()) 
      
      
      df <- 
        df_sim %>% 
        inner_join(df_img, by = 'team') %>% 
        inner_join(select(power_rankings, team, yusag_coeff), by = 'team') %>% 
        inner_join(select(confs, team, eliminated), by = 'team') %>% 
        arrange(desc(champ), desc(finals)) %>% 
        select(team, logo_file, seed, yusag_coeff, finals, champ, eliminated)
      
      library(gt)
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
          title = md(paste("**2025", conf, "Men's Basketball Tournament Odds**")),
        ) %>% 
        tab_options(column_labels.font.size = 16,
                    column_labels.font.weight = 'bold',
                    heading.title.font.size = 30,
                    heading.subtitle.font.size = 20,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold'
        )
      gtsave(table, paste0('graphics/conf_tourneys/', year, '/', conf, '.png'))
    }
  }
}



conf_seeding <- function(conf_) {
  teams <- 
    x %>% 
    filter(conf_game) %>% 
    filter(reg_season) %>% 
    filter(team_conf == conf_) %>% 
    group_by(team) %>% 
    summarise('n_wins' = sum(wins, na.rm = T),
              'n_loss' = sum(1-wins, na.rm = T)) %>% 
    arrange(-n_wins) %>% 
    pull(team)  
  
  cat('c(',  paste(paste0("'", teams, "'"), collapse = ',\n'), ')', sep = '')
}
