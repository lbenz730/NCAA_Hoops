### Adapted from a file produced by Prof. Jay Emerson used in
### Introductory Data Analysis (STAT 230), Spring 2016

# Get date materials
today <- unclass(as.POSIXlt(Sys.time()))
year <- 1900 + today$year
month <- 1 + today$mon
day <- today$mday

# Year specific code
code <- 12620

# Stripwhite function 
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

url = "http://stats.ncaa.org/team/inst_team_list?academic_year=2018&conf_id=-1&division=1&sport_code=MBB"
x <- scan(url, what="", sep="\n")

# Focus attention on lines containing the links 
these <- grep(paste("/team/[0-9]*/", code, sep=""), x)
x <- x[these]

id <- as.numeric(gsub("^.*team/([0-9]*)/.*$", "\\1", x))
name <- gsub("<[^<>]*>", "", x)
name <- stripwhite(name)

teamid <- data.frame(team=name, id=id, stringsAsFactors=FALSE)
teamid$team <- gsub("&#x27;", "'", teamid$team)
teamid$team <- gsub("&amp;", "&", teamid$team)

################################################################################
baseurl <- paste('http://stats.ncaa.org/team/index/',
                 code, '?org_id=', sep="")
z <- NULL
bad <- NULL
#for (i in 1:2) {    # For testing purposes
for (i in 1:nrow(teamid)) {
  cat("Getting", i, teamid$team[i], "\n")
  
  # Elegantly scan and handle hiccups:
  ct <- 0
  while (ct >= 0 && ct <= 5) {
    x <- try(scan(paste(baseurl, teamid$id[i], sep=""),
                  what="", sep="\n"))
    if (class(x) != "try-error") {
      ct <- -1
    } else {
      warning(paste(ct, "try(scan) failed, retrying team ID",
                    teamid$id[i], "\n"))
      Sys.sleep(0.5)
      ct <- ct + 1
      if (ct > 5) {
        warning("Big internet problem")
        bad <- c(bad, teamid$id[i])
      }
    }
  }
  if (ct <= 5) {
    x <- x[-grep("^\\s*$", x)]  # Drop lines with only whitespace
    
    # Which lines contain dates surrounded immediately by > ... < ?
    datelines <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x)
    
    dates <- stripwhite(gsub("<[^<>]*>", "", x[datelines]))
    dates <- matrix(as.numeric(unlist(strsplit(dates, "/"))),
                    ncol=3, byrow=TRUE)
    
    opploc <- stripwhite(gsub("<[^<>]*>", "", x[datelines+2]))
    loc <- rep("H", length(opploc))
    loc[grep("^@", opploc)] <- "V"
    loc[grep(" @ ", opploc, fixed=TRUE)] <- "N"
    opp <- opploc
    opp[loc == "V"] <- gsub("@ ", "", opp[loc == "V"])
    opp[loc == "N"] <- substring(opp, 1, regexpr("@", opp)-2)[loc == "N"]
    
    result <- stripwhite(gsub("<[^<>]*>", "", x[datelines+5]))
    OT <- suppressWarnings(as.numeric(gsub("^.*\\((\\d)OT\\)",
                                           "\\1", result))) # warnings okay
    result <- gsub(" \\(.*\\)", "", result)
    result <- strsplit(substring(result, 3, 20), "-")
    if (any(sapply(result, length) == 0))
      result[sapply(result, length) == 0] <- list(c(NA, NA))
    result <- matrix(as.numeric(unlist(result)), ncol=2, byrow=TRUE)
    
    res <- data.frame(year=dates[,3],
                      month=dates[,1],
                      day=dates[,2],
                      team=teamid$team[i],
                      opponent=opp,
                      location=loc,
                      teamscore=result[,1],
                      oppscore=result[,2],
                      OT=OT, stringsAsFactors=FALSE)
    res$date <- paste(res$month, res$day, collapse = "_")
    
    # Fix non-unique dates problem
    uni_dates <- unique(res$date)
    z <- rbind(z, res[uni_dates %in% res$date, -ncol(res)])
  }
}

# Extract D1 Games
rows <- strsplit(z$opponent[grep("@", z$opponent)], "@")
for (i in 1:length(z$opponent[grep("@", z$opponent)])) {
  z$opponent[grep("@", z$opponent)][1] <- rows[[i]][1]
}


z$D1 <- rep(0, nrow(z))

for(i in 1:nrow(z)) {
  z$D1[i] <- length(teamid$team[teamid$team == z$team[i]]) + 
    length(teamid$team[teamid$team == z$opponent[i]])
}

z <- z[z$D1 == 2,]

### NC State vs. North Carolina St. Bug Fix
z$team <- gsub("NC State", "North Carolina St.", z$team)
z$opponent <- gsub("NC State", "North Carolina St.", z$opponent)
z$team <- gsub("A&M-Corpus Christi", "A&M-Corpus Chris", z$team)
z$opponent <- gsub("A&M-Corpus Christi", "A&M-Corpus Chris", z$opponent)

write.csv(z, paste("2.0_Files/Results/2017-18/NCAA_Hoops_Results_", month, "_", 
                   day, "_", year, ".csv", sep=""), row.names=FALSE)

