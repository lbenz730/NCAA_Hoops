library(dplyr)
library(stringdist)

kaggle_ids <- read.csv("3.0_Files/Stage2DataFiles/Teams.csv", as.is = T)

east <- c("Duke", "Michigan St.", "LSU", "Virginia Tech", 
          "Mississippi St.", "Maryland", "Louisville",
          "VCU", "UCF", "Minnesota", "Belmont", "Temple",
          "Liberty", "Saint Louis", "Yale", "Bradley", 
          "North Dakota St.", "N.C. Central")
south <- c("Virginia", "Tennessee", "Purdue", "Kansas St.",
           "Wisconsin", "Villanova", "Cincinnati", "Ole Miss",
           "Oklahoma", "Iowa", "Saint Mary's (CA)", "Oregon",
           "UC Irvine", "Old Dominion", "Colgate", "Gardner-Webb")
west <- c("Gonzaga", "Michigan", "Texas Tech", "Florida St.", 
          "Marquette", "Buffalo", "Nevada", "Syracuse", 
          "Baylor", "Florida", "Arizona St.", "St. John's (NY)",
          "Murray St.", "Vermont", "Northern Ky.", "Montana",
          "Fairleigh Dickinson", "Prairie View")
midwest <- c("North Carolina", "Kentucky", "Houston", "Kansas",
             "Auburn", "Iowa St.", "Wofford", "Utah St.", 
             "Washington", "Seton Hall", "Ohio St.", "New Mexico St.", 
             "Northeastern", "Georgia St.", "Abilene Christian", "Iona")

tourney <- c(east, west, midwest, south)

kaggle_ids <- filter(kaggle_ids, TeamName %in% tourney) 
preds <- 
  expand.grid(kaggle_ids$TeamID, kaggle_ids$TeamID) %>%
  filter(Var1 < Var2)

preds <- 
  inner_join(
  inner_join(preds, select(kaggle_ids, TeamID, TeamName), by = c("Var1" = "TeamID")),
             select(kaggle_ids, TeamID, TeamName), by = c("Var2" = "TeamID"))
names(preds) <- c("team_id1", "team_id2", "team", "opponent")  
preds$location <- "N"           
preds$pred_score_diff <- predict(lm.hoops, newdata = preds)
preds <- mutate(preds, "ID" = paste("2019", team_id1, team_id2, sep = "_"),
                "Pred" = win_prob) %>%
  select(ID, Pred)


xyz <- read.csv("https://storage.googleapis.com/kaggle-competitions-data/kaggle/11999/346871/SampleSubmissionStage2.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1553256204&Signature=R9f%2FkGFhZHJDrgv0uS4x0AW4oc7ngi4QrcABuR6XIcTrHAGuR%2BWEra4KeGkXgqm9ZYcq2%2BRdj1enZIPrveOVx9sejhSR28Oq87WtuAWHAJvrZTenRRGRjaoZA0ZY5NMp0%2F0RJlSjTucJWx6jYyunQaqzHNeH3lfP3I1%2F%2ByXZ7u%2FXhkLCq1bAGl34384v7PnyduI8SK0ap87CzNV9yo9bC2quAFl5dpTO%2F%2BfweZ1zuoW0rMDK0DjYnvXQGag5Ii%2BaqjND%2FKz793pJXmzidGnnIP3SE7WOxKw1aiNGgiG9Do4yo3Enwo4rJ3NgF49jWc2Va%2BNCC8uC0nxNvHv5k27Lvw%3D%3D", as.is = T)

setdiff(preds$ID, xyz$ID)

write.csv(preds, "3.0_Files/Stage2DataFiles/benz_kaggle_predictions.csv", row.names = F)
