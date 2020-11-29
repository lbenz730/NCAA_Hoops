# NCAA_Hoops

All the code that powers my NCAA Men's Basketball R Shiny Web Application [application](https://lbenz730.shinyapps.io/recspecs_basketball_central/).


### Application Files:
* __ui.R:__ User Interface for web application
* __sever.R:__ Backend for web application
* __global.R:__ Pre-loads data/helper functions for web application.

### Other Files:
__Model_3.0.R:__ Control panel for everything NCAA Hoops related that the application ultimately shows:

* Power Rankings
* Predictions
* Conference simulations and distributions
* Bracketology
* Ivy League simulations/playoff swing factor (not in application)

__ncaa_hoop_scraper.R:__ An algorithim to scrape game schedule/result data from the NCAA website. This script is mostly copied (with slight modifications) from a scrape written by Prof. Jay Emerson and used in STAT 230: Introductory Data Analysis (Spring 2016).

__3.0_Files/:__ A collection of files that are essentially the "inner workings" of everything done in this project. Every script in this directory contains functions. Those functions are executed in the master file, __Model_3.0.R__.
* __Ivy_Sims.R:__ Simulates the Ivy League Basketball season in order to estimate playoff odds and calcuates the "Playoff Swing Factor" of each conference game.
* __bracketology.R:__ Assembles the predicted bracket:
* __helpers.R:__ A file with miscellanious functions used throughout the project.
* __powerrankings.R:__ Computes the team rankings.
* __record_evaluator.R:__ Examines the quality of each team's resume by computing Quality Wins (as recently redefinined by the NCAA tournament selection committee), Strength of Record, and Wins Above Bubble.
* __rpi.R:__ Predictions end of season RPI for each team.
* __tourney_sim.R:__ Function for simulating college basketball tournaments with parameters left to the user. The user specifies ```teams``` (from best seed to worst seed), along with a vector of ```seeds```. Note that after games have been played, the ```seeds``` vector must be entered in the order of highest possible seed for each given slot. For example, if we have quarterfinals where the matchups are 1 vs. 9, 2 vs. 7, 3 vs. 14, and 4 vs. 12, we'd set ```seeds``` = (1, 2, 3, 4, 12, 14, 7, 9), as 5, 6, 7, 8 are the "chalk" seeds occupied by 12, 14, 7, and 9 in this hypothetical 15 team tournament. The user must also enter the number of single ```byes```, the number of ```double_byes```, the number of simulations to run ```nsims```, and a parameter for home court advantage, ```hca```. If the tournament is played at a neutral site, set ```hca = NA```. If the higher seed is always given home court advantage, set ```hca = "seed"```. If one team hosts the tournament (even if not the top seed), set ```hca = INSERT_TEAM_NAME```.
* __Bracketology/:__ Collection of .csv files used in bracket creation
  * __bids.csv:__ Table of tournament bids broken down by conference.
  * __bracket.csv:__ The final bracket produced.
  * __bracket_math.csv:__ Table of bracket metrics for all 357 Division-1 teams.
  * __bubble.csv:__ Bracket metrics for the first 16 teams missing the field as at-large bids.
  * __resumes.csv:__ Subset of bracket metrics (resume evaluation, strength of record, wins above bubble) produced by __record_evaluator.R__. 
  * __rpi.csv:__ Projected end of season RPI for each team. Produced by __rpi.R__.
  * __historical/:__ A collection of files used to predict NCAA Tournament seed from the various metrics in this directory.
* __Info/:__ A collection of information used to adjust model weights and determine postseason status.
  * __conferences.csv:__ List of teams with their conference, postseason eligibility status, and elimination status from automatic bid contention.
  * __mins_YYYY.csv:__ Percentage of team's minutes returning during the next season. Acquired from [Bart Tovrik](http://www.barttorvik.com/returningmins.php).
  * __recruiting.csv:__ [247Sports recruiting scores](http://247sports.com/Season/2017-Basketball/CompositeTeamRankings) for each team's incoming freshman class.
  * __transfers.csv:__ Data on transfers eligible to play in the 2017-18 season, pulled from http://www.barttorvik.com/trankpre.php.
* __History/:__ Some historical files of interest.
* __Power_Rankings/:__ Collection of .csv files produced by __powerrankings.R__.
  * __power_rankings.csv:__ Ranking of all 357 teams
  * __conf_summary.csv:__ Ranking of the 32 Division 1 conferences, by median ranking.
  * __pr_by_conf.csv:__ Ranking of teams by YUSAG Coefficient, sorted by conference.
* __Predictions/:__ 
  * __playoffs.csv:__ Ivy League playoff odds.
  * __psf.csv:__ [Playoff Swing Factor](http://yaledailynews.com/downthefield/2017/01/31/by-the-numbers-ivy-hoops-games-to-watch/) for most recent week of Ivy League conference games.
* __Results/:__ Complete NCAA Basketball schedule/results through a given date. Sub-directories indicate the year/season, with .csv files given in NCAA_Hoops_results_day_month_year.csv format.
