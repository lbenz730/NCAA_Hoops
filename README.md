# NCAA_Hoops
__Model_2.0.R:__ Control panel for everything NCAA Hoops related that I do for the [Yale Undergraduate Sports Analytics Group](http://sports.sites.yale.edu) (YUSAG). Everything is controlled with this script, including creating the model, making the [YUSAG Bracket](http://sports.sites.yale.edu/yusag-bracketology), and simulating Ivy League games. Note that large chunks of code are commented out. These code chunks correspond to the upcoming 2017-18 college basketball season, and will be uncommented as soon as the NCAA releases the official 2017-18 schedule (and the appropriate data is obtained). Be sure to follow my NCAA Basketball coverage this season on twitter [@YaleSportsGroup](https://twitter.com/YaleSportsGroup) and [@Recspecs730](https://twitter.com/recspecs730).

__NCAA_Scrape.R:__ An algorithim to scrape game schedule/result data from the NCAA website. This script is mostly copied (with slight modifications) from a scrape written by Prof. Jay Emerson and used in STAT 230: Introductory Data Analysis (Spring 2016).

__2.0_Files/:__ A collection of files that are essentially the "inner workings" of everything done in this project.
* __Ivy_Sims.R:__ Simulates the Ivy League Basketball season in order to estimate playoff odds and cacluates the "Playoff Swing Factor" of each conference game.
* __bracketology.R:__ Assembles the [YUSAG Bracket](http://sports.sites.yale.edu/yusag-bracketology)
* __powerrankings.R:__ Computes the [YUSAG NCAA Power Rankings](http://sports.sites.yale.edu/ncaa-power-rankings)
* __record_evaluator.R:__ Examines the quality of each team's resume by computing Quality Wins (as recently redefinined by the NCAA tournament selection committee), Strength of Record, and Wins Above Bubble.
* __rpi.R:__ Predictions end of season RPI for each team.

