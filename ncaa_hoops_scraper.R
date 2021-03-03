### Adapted from a file produced by Prof. Jay Emerson used in
### Introductory Data Analysis (STAT 230), Spring 2016
library(dplyr)

# Get date materials
today <- unclass(as.POSIXlt(Sys.time()))
year <- 1900 + today$year
month <- 1 + today$mon
day <- today$mday

# Stripwhite function 
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### Fix 
fix_team <- function(x) {
  possible_fix <- teamid$team[sapply(teamid$team, grepl, x)]
  return(possible_fix[nchar(possible_fix) == max(nchar(possible_fix))])
}

### Read in ID's table
teamid <- 
  read.csv("3.0_Files/Info/conferences.csv", as.is = T) %>%
  select(team, ncaa_id, conference) %>%
  arrange(team)

################################################################################
baseurl <- 'https://stats.ncaa.org/teams/'
tournaments <- c('Empire Classic (Riverside)', 'Myrtle Beach Invitational', 
                 'Maui Invitational (Mainland)', 'Cayman Islands (Mainland)',
                 "- MBB Negro League Baseball Museum Tipoff Classic",
                 'Hawaiian Airlines Diamond Head Classic') 
conf_tournaments <- paste0('-\\sMBB\\s+', c("Big South", "Patriot League", "Northeast Conference",
                             "Horizon League", "Mountain West", "ASUN", "MAC", "WCC", 
                             "America East", "Sun Belt", "SWAC", "MEAC", "Big East", 
                             "Big 10", "Big 12", "ACC", "SEC", "Ivy League"), "\\s+(Championship)+")


z <- NULL
bad <- NULL
for (i in 1:nrow(teamid)) {
  cat("Getting", i, teamid$team[i], "\n")
  
  # Elegantly scan and handle hiccups:
  ct <- 0
  while (ct >= 0 && ct <= 5) {
    x <- try(readr::read_lines(paste(baseurl, teamid$ncaa_id[i], sep="")))
    if (class(x) != "try-error") {
      ct <- -1
    } else {
      warning(paste(ct, "try(scan) failed, retrying team ID",
                    teamid$id[i], "\n"))
      Sys.sleep(0.5)
      ct <- ct + 1
      if (ct > 5) {
        warning("Big internet problem")
        bad <- c(bad, teamid$ncaa_id[i])
      }
    }
  }
  if (ct <= 5 & i != 315) { ### UMES cancelled
    x <- x[-grep("^\\s*$", x)]  # Drop lines with only whitespace
    
    # Which lines contain dates surrounded immediately by > ... < ?
    datelines <- grep("\\d\\d/\\d\\d/\\d\\d\\d\\d", x)
    
    dates <- stripwhite(gsub("<[^<>]*>", "", x[datelines]))
    
    dates <- gsub("\\(.*", "", gsub(" .*", "", dates))
    ix <- !dates %in% c("Start", "Team", "End", "UMES")
    if(sum(ix) > 0) {
      datelines <- datelines[ix]
      dates <- dates[ix]
      dates <- matrix(as.numeric(unlist(strsplit(dates, "/"))),
                      ncol=3, byrow=TRUE)
      
      opploc <- stripwhite(gsub("<[^<>]*>", "", x[datelines+2]))
      loc <- rep("H", length(opploc))
      loc[grep("@", opploc, fixed=TRUE)] <- "N"
      loc[grep("^@", opploc)] <- "V"
      opp <- opploc
      opp[loc == "V"] <- gsub("@", "", gsub("@ ", "", opp[loc == "V"]))
      opp[loc == "N"] <- substring(opp, 1, regexpr("@", opp)-2)[loc == "N"]
      
      
      
      result <- stripwhite(gsub("<[^<>]*>", "", x[datelines+5]))
      OT <- suppressWarnings(as.numeric(gsub("^.*\\((\\d)OT\\)",
                                             "\\1", result))) # warnings okay
      result <- gsub(" \\(.*\\)", "", result)
      canceled <- result == 'Canceled'
      postponed <- result == 'Postponed' | result == 'Ppd'
      result[canceled|postponed] <- ""
      result <- strsplit(substring(result, 3, 20), "-")
      if (any(sapply(result, length) == 0)) {
        result[sapply(result, length) == 0] <- list(c(NA, NA))
      }
      result <- matrix(as.numeric(unlist(result)), ncol=2, byrow=TRUE)
      
      res <- data.frame(year=dates[,3],
                        month=dates[,1],
                        day=dates[,2],
                        team=teamid$team[i],
                        opponent=opp,
                        location=loc,
                        teamscore=result[,1],
                        oppscore=result[,2],
                        canceled=canceled,
                        postponed=postponed,
                        OT=OT, stringsAsFactors=FALSE)
      res$date <- paste(res$month, res$day, sep = "_") 
      res$opponent <- stripwhite(gsub("&amp;", "&", gsub("&x;", "'", gsub("[0-9]", "", gsub("#", "", res$opponent)))))
      #fix <- sapply(res$opponent, function(x) { any(sapply(teamid$team, grepl, x)) }) &
      #  !res$opponent %in% teamid$team
      #res$opponent[fix] <- sapply(res$opponent[fix], fix_team)
      
      for(tourney in tournaments) {
        res$opponent <- stripwhite(gsub(tourney, "", res$opponent))
      }
      
      for(tourney in conf_tournaments) {
        res$opponent <- stripwhite(gsub(tourney, "", res$opponent))
      }
      
      # Fix non-unique dates problem
      uni_dates <- unique(res$date)
      z <- rbind(z, res[uni_dates %in% res$date, -ncol(res)])
    }
    
  }
}

# Extract D1 Games
# rows <- strsplit(z$opponent[grep("@", z$opponent)], "@")
# if(length(rows) > 1) {
#   for (i in 1:length(z$opponent[grep("@", z$opponent)])) {
#     z$opponent[grep("@", z$opponent)][1] <- rows[[i]][1]
#   }
# }
z$opponent <- stripwhite(z$opponent)

z$D1 <- z$team %in% teamid$team + z$opponent %in% teamid$team

### Save Results
write.csv(z, paste("3.0_Files/Results/2020-21/NCAA_Hoops_Results_", month, "_", 
                   day, "_", year, ".csv", sep=""), row.names = F)

