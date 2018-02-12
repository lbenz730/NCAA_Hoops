tourney_sim  <- function(teams, byes, double_byes, hca, nsims) {
  n <- length(teams)
  games <- data.frame("team" = rep(NA, n-1),
                      "opponent" = rep(NA, n-1),
                      "location" = rep(NA, n-1),
                      "comment" = rep(NA, n-1))
}