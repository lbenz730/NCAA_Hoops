library(shiny)
library(DT)

# Define UI 
shinyUI(navbarPage("recspecs730 Basketball Central",
                   id = 'navbar',
                   tabPanel("Rankings",
                            # Application title
                            titlePanel("NCAA Men's Basketball Power Rankings"),
                            
                            sidebarLayout(position = "left",
                                          sidebarPanel(
                                            p(align = "center", 
                                              textOutput("update"),
                                              tags$head(tags$style("#update{text-align: center; color: ; font-weight: bold;}"))
                                              
                                            ),
                                            
                                            p(tags$b("Net Rating"), "denotes the number of",
                                              "points a given team would expect to",
                                              "win/lose by against", tags$b("an average college basketball team on a neutral court.")
                                            ),
                                            
                                            p(tags$b("Offensive Rating"), "denotes the number of points more/less than average",
                                              "a given team would score against the", 
                                              tags$b("average college basketball team on a neutral floor")
                                            ),
                                            
                                            p(tags$b("Deffensive Rating"), "denotes the number of points more/less than average",
                                              "a given team would allow against the", 
                                              tags$b("average college basketball team on a neutral floor")
                                            ),
                                            
                                            p("It’s easy to want to interpret these as meaures of offensive and defensive strength",
                                              "but it’s extremely important we don’t fall for this temptation. On a points per game basis,",
                                              "a team fewer points than the average college basketball team, but this could be because their",
                                              "elite defense would often slow down the game, leaving fewer possessions for their offense to score.",
                                              "Overall team strength is well measured by,", tags$b("Net Rating"), "and",  tags$b("Offensive Rating"), "and",
                                              tags$b("Defensive Rating"), "show from which area(s) of the game teams may derive their strength."
                                            ),
                                            
                                            p("More on the mathematical methodology behind these ratings can be found", a("here", href= "https://lukebenz.com/post/hoops_methodology/")
                                            ),
                                            
                                            p(align = "left",
                                              tags$b(tags$i("If you enjoy this site and would like to help keep it free")),
                                              tags$b(tags$i("(in order to offset the cost of server time), consider donating by
                                                            Venmo (@lbenz730) or PayPal (lsbenz30@yahoo.com)")))
                                            
                                          ),
                                          
                                          ### Render Table
                                          mainPanel(
                                            DT::dataTableOutput("rankings")
                                            
                                          )
                            )
                   ),
                   
                   ### Conference Breakdowns
                   tabPanel("Conference Breakdowns",
                            value = 'conf_breakdowns',
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("conf", 
                                            label = "Select a Conference",
                                            choices = c(""), 
                                            selected = NULL
                                            
                                )
                              ),
                              
                              
                              mainPanel(
                                ### Render Table
                                DT::dataTableOutput("conf_standings"),
                                
                                ### Conference Schedule
                                plotOutput("conf_schedule_plot", width = "100%", height = "600px"),
                                
                                br(),
                                br(),
                                
                                ### Render D1 Universe
                                plotOutput("uni_plot", width = "100%", height = "600px"),
                                
                                br(),
                                br(),
                                
                                
                                ### Render Conference Standings Plot
                                plotOutput("conf_standings_plot", width = "100%", height = "600px"),
                                
                                br(),
                                br(),
                                
                                ### Conf Sims
                                tags$h2("Distributions of Regular Season Conference Finish"),
                                tags$i("Ties left unbroken."),
                                br(),
                                
                                DT::dataTableOutput("conf_sims"),
                                
                                br(),
                                br(),
                                
                                ### Render Conference Box Plots
                                plotOutput("conf_box_plot", width = "100%", height = "600px")
                                
                                
                              )
                              
                            )
                   ),
                   
                   
                   ### Team Breakdowns
                   tabPanel("Team Breakdowns",
                            value = 'team_breakdowns',
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("team", 
                                            label = "Select a Team",
                                            choices = c(""), 
                                            selected = NULL
                                            
                                ),
                                
                                tags$h2('Team Profile', align = 'center'),
                                htmlOutput('logo'),
                                tags$head(tags$style('#logo{display: block;margin-left: 42.5%; width: 50%;}')),
                                htmlOutput('team'),
                                tags$head(tags$style("#team{text-align: center;}")),
                                htmlOutput('team_conf'),
                                tags$head(tags$style("#team_conf{text-align: center;}")),
                                htmlOutput('team_record'),
                                tags$head(tags$style("#team_record{text-align: center;}")),
                                htmlOutput('conf_record'),
                                tags$head(tags$style("#conf_record{text-align: center;}")),
                                htmlOutput('ratings'),
                                tags$head(tags$style("#ratings{text-align: center;}")),
                                htmlOutput('record_breakdown'),
                                tags$head(tags$style("#record_breakdown{text-align: center;}")),
                                htmlOutput('ncaa_odds'),
                                tags$head(tags$style("#ncaa_odds{text-align: center;}"))
                                
                              ),
                              
                              mainPanel(
                                DT::dataTableOutput("team_schedule"),
                                plotOutput("ratings_plot", width = "100%", height = "600px"),
                                plotOutput("rankings_plot", width = "100%", height = "600px")
                                
                              )
                              
                            )
                   ),
                   
                   ### Game Projections
                   tabPanel("Game Projections",
                            value = 'game_projections',
                            sidebarLayout(
                              sidebarPanel(
                                "Games are sorted by highest rated team.",
                                "Win probability shown is from \"Team\" perspective.",
                                br(),
                                br(),
                                dateInput("proj_date", label = "Select Date", value = NULL,
                                          format = "MM d, yyyy",
                                          min = as.Date("2019-11-05")
                                )
                              ),
                              
                              mainPanel(
                                DT::dataTableOutput("game_projections") 
                              )
                              
                            )
                   ),
                   
                   ### Bracketology
                   tabPanel("Bracketology",
                            value = 'bracketology',
                            sidebarLayout(position = "left",
                                          sidebarPanel(tags$h3("Bracketology"),
                                                       
                                                       p(align = "center", 
                                                         textOutput("update2"),
                                                         tags$head(tags$style("#update2{text-align: center; color: ; font-weight: bold;}"))
                                                         
                                                       ),
                                                       
                                                       "Automatic bids are denoted by", tags$b("bold"), 
                                                       ", while the First Four (last four at-large bids, worst four automatic bids) are denoted by", tags$i("italics."), 
                                                       "Teams are selected and seeded using a combination of logistic regression based at-large odds",
                                                       "and linear regression to predict the seed of selected teams based on historical data since the 2015-2016 season.",
                                                       
                                                       br(),
                                                       
                                                       "All metrics are projected end of season values/ranks.",
                                                       
                                                       br(),
                                                       br(),
                                                       
                                                       tags$ul(
                                                         tags$li(tags$b("Net Rating:"), "Points +/- Average NCAA Team"),
                                                         tags$li(tags$b("Wins Above Bubble:"), "How you do on schedule compared to how we would expect average bubble team to do on your schedule."),
                                                         tags$li(tags$b("Strength of Record:"), "How you do on schedule compared to how we would expect average Top-25 team to do on your schedule"),
                                                         tags$li(tags$b("Resume:"), "Weighted sum of win quality based on committe team sheet tiers")
                                                         
                                                         
                                                         
                                                       )
                                                       
                                          ),
                                          mainPanel(
                                            
                                            p(align = "center", tags$h1("Bracket")),
                                            DT::dataTableOutput("bracket"),
                                            br(),
                                            p(align = "center", tags$h1("Bubble")),
                                            DT::dataTableOutput("bubble"),
                                            br(),
                                            p(align = "center", tags$h1("Bid-Breakdown")),
                                            DT::dataTableOutput("bid_breakdown")
                                          )
                            )
                   ),
                   
                   ### NCAA Tournament Odds
                   tabPanel("NCAA Tournament Odds",
                            value = 'ncaa',
                            
                            
                            
                            mainPanel(
                              gt_output('ncaa_sims')
                            )
                            
                   ),
                   
                   
                   ### Ivy League
                   tabPanel("Ivy League",
                            value = 'ivy_league',
                            
                            mainPanel(
                              gt_output('ivy_sims'),
                              plotOutput('ivy_history'),
                              gt_output('ivy_psf'),
                              plotOutput('ivy_barplot')
                            )
                            
                   ),
                   
                   ### About
                   tabPanel("About",
                            value = 'about',
                            
                            
                            sidebarLayout(position = "left",
                                          sidebarPanel(
                                            
                                            p(align = "left",
                                              "My name is", tags$b("Luke Benz"), "and I",
                                              "designed this site to put together",
                                              "several of the various college basketball",
                                              "modules I maintain all in on centralized place."),
                                            
                                            p(align = "left",
                                              "Many of these projects began when I was an undergrad",
                                              "participating in the", a("Yale Undergraduate Sports Analytics Group", 
                                                                        href = "http://sports.sites.yale.edu")
                                            ),
                                            
                                            p(align = "left",
                                              "Integrating all of the various modules will take some time",
                                              "but I'm hoping to have everything up and running before too long.",
                                              "Thanks for your patientice in the mean time"),
                                            
                                            p(aligin = "left", 
                                              tags$b(tags$u("Useful Links")),
                                              tags$ul(
                                                tags$li(a("Rankings Methodology", href = "https://lukebenz.com/post/hoops_methodology/")),
                                                tags$li(a("GitHub Repository", href = "https://github.com/lbenz730/NCAA_Hoops")),
                                                tags$li(a("ncaahoopR Package", href = "https://github.com/lbenz730/ncaahoopR/"))
                                                
                                                
                                              )
                                            ),
                                            
                                            p(align = "left",
                                              "Follow me on twitter", a("@recspecs730", href = "https://twitter.com/recspecs730"),
                                              "for more hoops analysis."),
                                            
                                            p(align = "left",
                                              tags$b(tags$i("If you enjoy this site and would like to help keep it free")),
                                              tags$b(tags$i("(in order to offset the cost of server time), consider donating by
                                                            Venmo (@lbenz730) or PayPal (lsbenz30@yahoo.com)")))
                                            
                                          ),
                                          mainPanel()
                            )
                   )
                   
))
