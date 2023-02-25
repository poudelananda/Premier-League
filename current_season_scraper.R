#url <- "https://fbref.com/en/comps/9/11160/schedule/2022-2023-Premier-League-Scores-and-Fixtures"
url <- "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures"
currentSeason <- read_html(url) %>%
  html_nodes("table") %>%
  html_table()
currentSeason <- as.data.frame(currentSeason[[1]])
currentSeason <- currentSeason[which(currentSeason$Score != ""),]
currentSeason$Wk <- paste("Gameweek", currentSeason$Wk)
currentSeason$Home <- gsub("Utd", "United", currentSeason$Home)
currentSeason$Away <- gsub("Utd", "United", currentSeason$Away)
currentSeason$Home <- gsub("Wolves", "Wolverhampton-Wanderers", currentSeason$Home)
currentSeason$Away <- gsub("Wolves", "Wolverhampton-Wanderers", currentSeason$Away)
currentSeason$Home <- gsub("Brighton", "Brighton-and-Hove-Albion", currentSeason$Home)
currentSeason$Away <- gsub("Brighton", "Brighton-and-Hove-Albion", currentSeason$Away)
currentSeason$Home <- gsub("West Ham", "West-Ham-United", currentSeason$Home)
currentSeason$Away <- gsub("West Ham", "West-Ham-United", currentSeason$Away)
currentSeason$Home <- gsub("Tottenham", "Tottenham-Hotspur", currentSeason$Home)
currentSeason$Away <- gsub("Tottenham", "Tottenham-Hotspur", currentSeason$Away)
currentSeason$Home <- gsub("Nott'ham Forest", "Nottingham-Forest", currentSeason$Home)
currentSeason$Away <- gsub("Nott'ham Forest", "Nottingham-Forest", currentSeason$Away)
currentSeason$Home <- gsub(" ", "-", currentSeason$Home)
currentSeason$Away <- gsub(" ", "-", currentSeason$Away)

setwd("/Users/apoudel53/Desktop/ML Practice/Sports Analytics/2022-2023 PL/")
write.csv(currentSeason, "currentSeason.csv")

