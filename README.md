# NCAA_Hoops
__Model_2.0.R:__ Control panel for everything NCAA Hoops related that I do for the [Yale Undergraduate Sports Analytics Group](http://sports.sites.yale.edu) (YUSAG). Everything is controlled with this script, including creating the prediction model, making the [YUSAG Bracket](http://sports.sites.yale.edu/yusag-bracketology), and simulating Ivy League games. Note that large chunks of code are commented out. These code chunks correspond to the upcoming 2017-18 college basketball season, and will be uncommented as soon as the NCAA releases the official 2017-18 schedule (and the appropriate data is obtained). Be sure to follow my NCAA Basketball coverage this season on twitter [@YaleSportsGroup](https://twitter.com/YaleSportsGroup) and [@recspecs730](https://twitter.com/recspecs730).

__NCAA_Scrape.R:__ An algorithim to scrape game schedule/result data from the NCAA website. This script is mostly copied (with slight modifications) from a scrape written by Prof. Jay Emerson and used in STAT 230: Introductory Data Analysis (Spring 2016).

__2.0_Files/:__ A collection of files that are essentially the "inner workings" of everything done in this project. Every script in this directory contains functions. Those functions are executed in the master file, __Model_2.0.R__.
* __Ivy_Sims.R:__ Simulates the Ivy League Basketball season in order to estimate playoff odds and cacluates the "Playoff Swing Factor" of each conference game.
* __bracketology.R:__ Assembles the [YUSAG Bracket](http://sports.sites.yale.edu/yusag-bracketology).
* __helpers.R:__ A file with miscellanious functions used throughout the project.
* __powerrankings.R:__ Computes the [YUSAG NCAA Power Rankings](http://sports.sites.yale.edu/ncaa-power-rankings).
* __season_shift.R:__ Code used to create __ncaa_season_shifts.pdf__, which is used to generate the [GIF seen here](https://twitter.com/recspecs730/status/961986171365621761).
* __record_evaluator.R:__ Examines the quality of each team's resume by computing Quality Wins (as recently redefinined by the NCAA tournament selection committee), Strength of Record, and Wins Above Bubble.
* __rpi.R:__ Predictions end of season RPI for each team.
* __tourney_sim.R:__ Function for simulating college basketball tournaments with parameters left to the user. The user specifies ```teams``` (in order of seed), the number of single ```byes```, the number of ```double_byes```, the number of simulations to run ```nsims```, and a parameter for home court advantage, ```hca```. If the tournament is played at a neutral site, set ```hca = NA```. If the higher seed is always given home court advantage, set ```hca = "seed"```. If one team hosts the tournament (even if not the top seed), set ```hca = INSERT_TEAM_NAME```.
* __Bracketology/:__ Collection of .csv files used in [YUSAG Bracketology](http://sports.sites.yale.edu/yusag-bracketology).
  * __bids.csv:__ Table of tournament bids broken down by conference.
  * __bracket.csv:__ The final bracket produced for [YUSAG Bracketology](http://sports.sites.yale.edu/yusag-bracketology).
  * __bracket_math.csv:__ Table of bracket metrics for all 351 Division-1 teams. See [YUSAG Bracket Math](http://sports.sites.yale.edu/bracket-math) for more.
  * __bubble.csv:__ Bracket metrics for the first 16 teams missing the field as at-large bids.
  * __resumes.csv:__ Subset of bracket metrics (resume evaluation, strength of record, wins above bubble) produced by __record_evaluator.R__. 
  * __rpi.csv:__ Projected end of season RPI for each team. Produced by __rpi.R__.
* __Info/:__ A collection of information used to adjust model weights and determine postseason status.
  * __conferences.csv:__ List of teams with their conference, postseason eligibility status, and elimination status from automatic bid contention.
  * __mins.csv:__ Percentage of team's 2016-17 minutes returning during the 2017-18 season. Acquired from [Bart Tovrik](http://www.barttorvik.com/returningmins.php).
  * __recruiting.csv:__ [247Sports recruiting scores](http://247sports.com/Season/2017-Basketball/CompositeTeamRankings) for each team's incoming freshman class.
  * __transfers.csv:__ Data on transfers eligible to play in the 2017-18 season, pulled from http://www.barttorvik.com/trankpre.php.
* __History/:__ Model prediction history, with actual score differentials and predicted score differentials from a week in advance.
  * __2017_18_history.csv:__ 2017-18 prediction history.
* __Power_Rankings/:__ Collection of .csv files produced by __powerrankings.R__.
  * __Powerrankings.csv:__ Ranking of all 351 teams by [YUSAG Coefficient](http://sports.sites.yale.edu/ncaa-mens-basketball-power-rankings).
  * __conf_summary.csv:__ Ranking of the 32 Division 1 conferences, by median YUSAG coefficient. 
  * __pr_by_conf.csv:__ Ranking of teams by YUSAG Coefficient, sorted by conference. For more, [click here](http://sports.sites.yale.edu/ncaa-mens-basketball-power-rankings-0).
* __Predictions/:__ 
  * __playoffs.csv:__ Ivy League playoff odds.
  * __psf.csv:__ [Playoff Swing Factor](http://yaledailynews.com/downthefield/2017/01/31/by-the-numbers-ivy-hoops-games-to-watch/) for most recent week of Ivy League conference games.
* __Results/:__ Complete NCAA Basketball schedule/results through a given date. Sub-directories indicate the year/season, with .csv files given in NCAA_Hoops_results_day_month_year.csv format. __NCAA_Hoops_results_6_29_2017.csv__ is the complete results file for the 2016-17 season. __NCAA_Hoops_results_10_30_2017.csv__ gives the preseason schedule for the 2017-18 season.
* __html.py:__ Takes power ranking and bracketology .csv files and coverts them to the HTML seen in __html__ for use on the YUSAG Website.