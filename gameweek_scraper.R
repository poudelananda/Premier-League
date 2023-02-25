#Set gameweek   
gameweek <- 18
teamid <- data.frame(
  leicester = c("Leicester-City", "a2d435b3"),
  liverpool = c("Liverpool", "822bd0ba"),
  everton = c("Everton", "d3fd31cc"),
  astonVilla = c("Aston-Villa", "8602292d"),
  arsenal = c("Arsenal", "18bb7c10"),
  crystalPalace = c("Crystal-Palace", "47c64c55"),
  leedsUnited = c("Leeds-United", "5bfb9659"),
  tottenham = c("Tottenham-Hotspur", "361ca564"),
  chelsea = c("Chelsea", "cff3d9bb"),
  newcastle = c("Newcastle-United", "b2b47a98"),
  westHam = c("West-Ham-United","7c21e445"),
  brighton = c("Brighton-and-Hove-Albion", "d07537b9"),
  manchesterCity = c("Manchester-City","b8fd03ef"),
  manchesterUnited = c("Manchester-United", "19538871"),
  southampton = c("Southampton", "33c895d4"),
  wolves = c("Wolverhampton-Wanderers", "8cec06e1"),
  westBrom = c("West-Brom", "60c6b05f"),
  burnley = c("Burnley", "943e8050"),
  sheffield = c("Sheffield-United", "1df6b87e"),
  fulham = c("Fulham", "fd962109"),
  brentford = c("Brentford", "cd051869"),
  norwich = c("Norwich-City", "1c781004"),
  watford = c("Watford", "2abfe087"),
  bournemouth = c("Bournemouth", "4ba7cbea"),
  nottingham = c("Nottingham-Forest", "e4a775cb")
)
teamid <- t(teamid)
teamid <- as.data.frame(teamid)
names(teamid) <- c("Teams", "ID")

url <- "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures"  


fixtures <- read_html(url) %>%
  html_nodes("table") %>%
  html_table()
fixtures <- as.data.frame(fixtures)

fixlist <- fixtures[which(fixtures$Wk == gameweek & fixtures$Score != ""),]
fixlist$Home <- gsub("Utd", "United", fixlist$Home)
fixlist$Away <- gsub("Utd", "United", fixlist$Away)
fixlist$Home <- gsub("Wolves", "Wolverhampton-Wanderers", fixlist$Home)
fixlist$Away <- gsub("Wolves", "Wolverhampton-Wanderers", fixlist$Away)
fixlist$Home <- gsub("Brighton", "Brighton-and-Hove-Albion", fixlist$Home)
fixlist$Away <- gsub("Brighton", "Brighton-and-Hove-Albion", fixlist$Away)
fixlist$Home <- gsub("West Ham", "West-Ham-United", fixlist$Home)
fixlist$Away <- gsub("West Ham", "West-Ham-United", fixlist$Away)
fixlist$Home <- gsub("Tottenham", "Tottenham-Hotspur", fixlist$Home)
fixlist$Away <- gsub("Tottenham", "Tottenham-Hotspur", fixlist$Away)
fixlist$Home <- gsub("Nott'ham Forest", "Nottingham-Forest", fixlist$Home)
fixlist$Away <- gsub("Nott'ham Forest", "Nottingham-Forest", fixlist$Away)
fixlist$Home <- gsub(" ", "-", fixlist$Home)
fixlist$Away <- gsub(" ", "-", fixlist$Away)

templinks <- read_html(url) %>%
  html_nodes("td.left a") %>%
  html_attr('href')

templinks <- as.data.frame(templinks)
links2022 <- templinks[grepl("2022-Premier-League", templinks$templinks),]
links2022 <- as.data.frame(links2022)
colnames(links2022) <- "links"
links2023 <- templinks[grepl("2023-Premier-League", templinks$templinks),]
links2023 <- as.data.frame(links2023)
colnames(links2023) <- "links"
links <- rbind(links2022, links2023)


weekLink <- data.frame(matrix(data = NA, nrow = nrow(fixlist), ncol = 1))
for (i in 1:nrow(fixlist)) {
  weekLink[i,1] <- links[grepl(paste(fixlist$Home[i], "-", fixlist$Away[i], sep=""), links$links),]
}

finalSummary <- data.frame(0)
finalPassing <- data.frame(0) 
finalPassType <- data.frame(0)
finalDefense <- data.frame(0)
finalPossession <- data.frame(0)
finalMiscellaneous <- data.frame(0)
finalGK <- data.frame(0)
Sys.sleep(5)
formation <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes("th") %>%
  html_text()
Sys.sleep(5)
homeSummary <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],"_summary",sep="")) %>%
  html_table()
homeSummary <- as.data.frame(homeSummary[[1]])
homeSummary$Team <- fixlist$Home[1]
homeSummary$teamFormation <- formation[1]
homeSummary$oppositionFormation <- formation[3]
homeSummary$Location <- "Home"

Sys.sleep(5)
homePassing <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],"_passing",sep="")) %>%
  html_table()
homePassing <- as.data.frame(homePassing[[1]])
Sys.sleep(5)
homePassType <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],"_passing_types",sep="")) %>%
  html_table()
homePassType <- as.data.frame(homePassType[[1]])
Sys.sleep(5)
homeDefense <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],"_defense",sep="")) %>%
  html_table()
homeDefense <- as.data.frame(homeDefense[[1]])
Sys.sleep(5)
homePossession <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],"_possession",sep="")) %>%
  html_table()
homePossession <- as.data.frame(homePossession[[1]])
Sys.sleep(5)
homeMiscellaneous <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],"_misc",sep="")) %>%
  html_table()
homeMiscellaneous <- as.data.frame(homeMiscellaneous[[1]])
Sys.sleep(5)
homeGK <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#keeper_stats_",teamid[which(teamid$Teams == fixlist$Home[1]),2],sep="")) %>%
  html_table()
homeGK <- as.data.frame(homeGK[[1]])
Sys.sleep(5)
formation <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes("th") %>%
  html_text()

awaySummary <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],"_summary",sep="")) %>%
  html_table()
awaySummary <- as.data.frame(awaySummary[[1]])
awaySummary$Team <- fixlist$Away[1]
awaySummary$teamFormation <- formation[3]
awaySummary$oppositionFormation <- formation[1]
awaySummary$Location <- "Away"
Sys.sleep(5)
awayPassing <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],"_passing",sep="")) %>%
  html_table()
awayPassing <- as.data.frame(awayPassing[[1]])
Sys.sleep(5)
awayPassType <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],"_passing_types",sep="")) %>%
  html_table()
awayPassType <- as.data.frame(awayPassType[[1]])
Sys.sleep(5)
awayDefense <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],"_defense",sep="")) %>%
  html_table()
awayDefense <- as.data.frame(awayDefense[[1]])
Sys.sleep(5)
awayPossession <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],"_possession",sep="")) %>%
  html_table()
awayPossession <- as.data.frame(awayPossession[[1]])
Sys.sleep(5)
awayMiscellaneous <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],"_misc",sep="")) %>%
  html_table()
awayMiscellaneous <- as.data.frame(awayMiscellaneous[[1]])
Sys.sleep(5)
awayGK <- read_html(paste("https://fbref.com", weekLink[1,],sep="")) %>%
  html_nodes(paste("#keeper_stats_",teamid[which(teamid$Teams == fixlist$Away[1]),2],sep="")) %>%
  html_table()
awayGK <- as.data.frame(awayGK[[1]])

finalSummary <- rbind(homeSummary, awaySummary)
finalPassing <- rbind(homePassing, awayPassing)
finalPassType <- rbind(homePassType, awayPassType)
finalDefense <- rbind(homeDefense, awayDefense)
finalPossession <- rbind(homePossession, awayPossession)
finalMiscellaneous <- rbind(homeMiscellaneous, awayMiscellaneous)
finalGK <- rbind(homeGK, awayGK)
for (i in 2:nrow(weekLink)) {
  
  formation <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes("th") %>%
    html_text()
  
  Sys.sleep(5)
  homeSummary <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],"_summary",sep="")) %>%
    html_table()
  homeSummary <- as.data.frame(homeSummary[[1]])
  homeSummary$Team <- fixlist$Home[i]
  homeSummary$teamFormation <- formation[1]
  homeSummary$oppositionFormation <- formation[3]
  homeSummary$Location <- "Home"
  
  Sys.sleep(5)
  homePassing <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],"_passing",sep="")) %>%
    html_table()
  homePassing <- as.data.frame(homePassing[[1]])
  Sys.sleep(5)
  homePassType <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],"_passing_types",sep="")) %>%
    html_table()
  homePassType <- as.data.frame(homePassType[[1]])
  Sys.sleep(5)
  homeDefense <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],"_defense",sep="")) %>%
    html_table()
  homeDefense <- as.data.frame(homeDefense[[1]])
  Sys.sleep(5)
  homePossession <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],"_possession",sep="")) %>%
    html_table()
  homePossession <- as.data.frame(homePossession[[1]])
  Sys.sleep(5)
  homeMiscellaneous <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],"_misc",sep="")) %>%
    html_table()
  homeMiscellaneous <- as.data.frame(homeMiscellaneous[[1]])
  Sys.sleep(5)
  homeGK <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#keeper_stats_",teamid[which(teamid$Teams == fixlist$Home[i]),2],sep="")) %>%
    html_table()
  homeGK <- as.data.frame(homeGK[[1]])
  Sys.sleep(5)
  awaySummary <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],"_summary",sep="")) %>%
    html_table()
  awaySummary <- as.data.frame(awaySummary[[1]])
  awaySummary$Team <- fixlist$Away[i]
  awaySummary$teamFormation <- formation[3]
  awaySummary$oppositionFormation <- formation[1]
  awaySummary$Location <- "Away"
  Sys.sleep(5)
  awayPassing <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],"_passing",sep="")) %>%
    html_table()
  awayPassing <- as.data.frame(awayPassing[[1]])
  Sys.sleep(5)
  awayPassType <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],"_passing_types",sep="")) %>%
    html_table()
  awayPassType <- as.data.frame(awayPassType[[1]])
  Sys.sleep(5)
  awayDefense <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],"_defense",sep="")) %>%
    html_table()
  awayDefense <- as.data.frame(awayDefense[[1]])
  Sys.sleep(5)
  awayPossession <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],"_possession",sep="")) %>%
    html_table()
  awayPossession <- as.data.frame(awayPossession[[1]])
  Sys.sleep(5)
  awayMiscellaneous <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],"_misc",sep="")) %>%
    html_table()
  awayMiscellaneous <- as.data.frame(awayMiscellaneous[[1]])
  Sys.sleep(5)
  awayGK <- read_html(paste("https://fbref.com", weekLink[i,],sep="")) %>%
    html_nodes(paste("#keeper_stats_",teamid[which(teamid$Teams == fixlist$Away[i]),2],sep="")) %>%
    html_table()
  awayGK <- as.data.frame(awayGK[[1]])
  finalSummary <- rbind(finalSummary, homeSummary, awaySummary)
  finalPassing <- rbind(finalPassing, homePassing, awayPassing)
  finalPassType <- rbind(finalPassType, homePassType, awayPassType)
  finalDefense <- rbind(finalDefense, homeDefense, awayDefense)
  finalPossession <- rbind(finalPossession, homePossession, awayPossession)
  finalMiscellaneous <- rbind(finalMiscellaneous, homeMiscellaneous, awayMiscellaneous)
  finalGK <- rbind(finalGK, homeGK, awayGK)
}
nrow(finalSummary)
names(finalSummary) <- finalSummary[1,]
finalSummary <- finalSummary[-grep("Player", finalSummary$Player),]

nrow(finalPassing)
names(finalPassing) <- finalPassing[1,]
finalPassing <- finalPassing[-grep("Player", finalPassing$Player),]

nrow(finalPassType)
names(finalPassType) <- finalPassType[1,]
finalPassType <- finalPassType[-grep("Player", finalPassType$Player),]

nrow(finalDefense)
names(finalDefense) <- finalDefense[1,]
finalDefense <- finalDefense[-grep("Player", finalDefense$Player),]

nrow(finalPossession)
names(finalPossession) <- finalPossession[1,]
finalPossession <- finalPossession[-grep("Player", finalPossession$Player),]

nrow(finalMiscellaneous)
names(finalMiscellaneous) <- finalMiscellaneous[1,]
finalMiscellaneous <- finalMiscellaneous[-grep("Player", finalMiscellaneous$Player),]

nrow(finalGK)
names(finalGK) <- finalGK[1,]
finalGK <- finalGK[-grep("Player", finalGK$Player),]

GW <- data.frame(
  players = finalSummary[,"Player"],
  teams = finalSummary[,fixlist$Home[1]],
  playerNumber = finalSummary[,"#"],
  nation = finalSummary[,"Nation"],
  position = finalSummary[,"Pos"],
  age = finalSummary[,"Age"],
  mins = finalSummary[,"Min"],
  goals = as.numeric(finalSummary[,"Gls"]),
  assists = as.numeric(finalSummary[,"Ast"]),
  penalties = as.numeric(finalSummary[,"PK"]),
  penaltiesAttempted = as.numeric(finalSummary[,"PKatt"]),
  shots = as.numeric(finalSummary[,"Sh"]),
  shotsOnTarget = as.numeric(finalSummary[,"SoT"]),
  yellowCard = as.numeric(finalSummary[,"CrdY"]),
  redCard = as.numeric(finalSummary[,"CrdR"]),
  shotCreatingActions = as.numeric(finalSummary[,"SCA"]),
  goalCreatingActions = as.numeric(finalSummary[,"GCA"]),
  totalPassCompleted = as.numeric(finalPassing[,7]),
  totalPassAttempted = as.numeric(finalPassing[,8]),
  totalPassCompletedPct = as.numeric(finalPassing[,9]),
  totalProgressiveDistancePassed = as.numeric(finalPassing[,11]),
  shortPassCompleted = as.numeric(finalPassing[,12]), #passes between 5 and 15 yards
  shortPassAttempted = as.numeric(finalPassing[,13]),
  shortPassCompletedPct = as.numeric(finalPassing[,14]),
  mediumPassCompleted = as.numeric(finalPassing[,15]), #passes between 15 and 30 yards
  mediumPassAttempted = as.numeric(finalPassing[,16]),
  mediumPassCompletedPct = as.numeric(finalPassing[,17]),
  longPassCompleted = as.numeric(finalPassing[,18]),#longer than 30 yards
  longPassAttempted = as.numeric(finalPassing[,19]),
  longPassCompletedPct = as.numeric(finalPassing[,20]),
  keyPasses = as.numeric(finalPassing[,24]),#Passes that lead directly to a shot
  passesIntoFinalThird = as.numeric(finalPassing[,25]),
  passesIntoPenaltyArea = as.numeric(finalPassing[,26]),
  crossesIntoPenaltyArea = as.numeric(finalPassing[,27]),
  progressivePasses = as.numeric(finalPassing[,28]), #Completed passes that move the ball towards the opponent's goal at least 10 yards from its furthest point in the last six passes, or completed passes into the penalty area. Excludes passes from the defending 40% of the pitch
  liveBallPassAttempt = as.numeric(finalPassType[,"Live"]),
  deadBallPassAttempt = as.numeric(finalPassType[,"Dead"]),
  freekickPassAttempt = as.numeric(finalPassType[,"FK"]),
  throughBalls = as.numeric(finalPassType[,"TB"]),
  #passesUnderPressure = as.numeric(finalPassType[,"Press"]), #data removed on new fbref patch
  passSwitches = as.numeric(finalPassType[,"Sw"]),#Passes that travel more than 40 yards of the pitch
  crosses = as.numeric(finalPassType[,"Crs"]),
  cornerKicks = as.numeric(finalPassType[,"CK"]),
  inswingCorner = as.numeric(finalPassType[,"In"]),
  outswingCorner = as.numeric(finalPassType[,"Out"]),
  straightCorner = as.numeric(finalPassType[,"Str"]),
  #groundPasses = as.numeric(finalPassType[,"Ground"]), #data removed on new fbref patch
  #lowPasses = as.numeric(finalPassType[,"Low"]),#leave the ground but below shoulder level #data removed on new fbref patch
  #highPasses = as.numeric(finalPassType[,"High"]), #passes above shoulder level at peak height
  #leftFootPassesAttempted = as.numeric(finalPassType[,"Left"]), #data removed on new fbref patch
  #rightFootPassesAttempted = as.numeric(finalPassType[,"Right"]),#data removed on new fbref patch
  #headPassesAtempted = as.numeric(finalPassType[,"Head"]),#data removed on new fbref patch
  throws = as.numeric(finalPassType[,"TI"]),
  #otherPartsPassAttempt = as.numeric(finalPassType[,"Other"]), #Passes attempted using other parts of the body besides head or feet. #data removed on new fbref patch
  passesCompleted = as.numeric(finalPassType[,"Cmp"]),
  offsidePass = as.numeric(finalPassType[,"Off"]),
  outOfBoundsPass = as.numeric(finalPassType[,"Out"]),
  #interceptedPass = as.numeric(finalPassType[,"Int"]), #data removed on new fbref patch
  blockedPass = as.numeric(finalPassType[,"Blocks"]),
  tacklesAttempted = as.numeric(finalDefense[,7]),
  tacklesWon = as.numeric(finalDefense[,8]),
  tacklesInDefensiveThird = as.numeric(finalDefense[,9]),
  tacklesInMiddleThird = as.numeric(finalDefense[,10]),
  tacklesInAttackingThird = as.numeric(finalDefense[,11]),
  dribblersTackled = as.numeric(finalDefense[,12]),
  dribblersTackleAttempted = as.numeric(finalDefense[,13]),
  dribblersTacklePct = as.numeric(finalDefense[,14]),
  dribblersPassedBy = as.numeric(finalDefense[,15]),
  #applyingPressureAttempts = as.numeric(finalDefense[,16]),#data removed on new fbref patch
  #applyingPressureCompleted = as.numeric(finalDefense[,17]),#data removed on new fbref patch
  #applyingPressureCompletedPct = as.numeric(finalDefense[,18]),#data removed on new fbref patch
  #applyingPressureDefensiveThird = as.numeric(finalDefense[,19]),#data removed on new fbref patch
  #applyingPressureMiddleThird = as.numeric(finalDefense[,20]),#data removed on new fbref patch
  #applyingPressureAttackingThird = as.numeric(finalDefense[,21]),#data removed on new fbref patch
  ballsBlocked = as.numeric(finalDefense[,16]),
  shotsBlocked = as.numeric(finalDefense[,17]),
  #shotsOnTargetBlocked = as.numeric(finalDefense[,24]),#data removed on new fbref patch
  passesBlocked = as.numeric(finalDefense[,18]),
  interceptions = as.numeric(finalDefense[,19]),
  clearance = as.numeric(finalDefense[,20]),
  errors = as.numeric(finalDefense[,21]),  #errors leading to opponents shot
  touches = as.numeric(finalPossession[,"Touches"]),
  defensivePenaltyAreaTouches = as.numeric(finalPossession[,"Def Pen"]),
  defensiveThirdAreaTouches = as.numeric(finalPossession[,"Def 3rd"]),
  middleThirdAreaTouches = as.numeric(finalPossession[,"Mid 3rd"]),
  attackingThirdAreaTouches = as.numeric(finalPossession[,"Att 3rd"]),
  attackingPenaltyAreaTouches = as.numeric(finalPossession[,"Att Pen"]),
  dribblesCompleted = as.numeric(finalPossession[,"Succ"]),
  dribblesAttempted = as.numeric(finalPossession[,"Att"]),
  dribblesCompletedPct = as.numeric(finalPossession[,"Succ%"]),
  #dribbledPastPlayers = as.numeric(finalPossession[,"#Pl"]),#data removed on new fbref patch
  #megs = as.numeric(finalPossession[,"Megs"]),#data removed on new fbref patch
  #playerBallControlOnFeet = as.numeric(finalPossession[,"Carries"]),#data removed on new fbref patch
  #totalDistanceMoved = as.numeric(finalPossession[,"TotDist"]), #Moving anywhere#data removed on new fbref patch
  #progressiveDistanceMoved = as.numeric(finalPossession[,"PrgDist"]), #Towards opponents goal#data removed on new fbref patch
  progressivePassReceived = as.numeric(finalPossession[,"Prog"]), #progressive passes received towards opponents goal with moving at least 10 yards.
  #oneThirdCarries = as.numeric(finalPossession[,"1/3"]), #Carrying the ball to 1/3rd of the pitch closest to the goal
  #penaltyBoxCarries = as.numeric(finalPossession[,"CPA"]), #Carrying the ball to the 18yard penalty box
  
  #passTargets = as.numeric(finalPossession[,"Targ"]), #number of times the player was the target of an attempted pass.#data removed on new fbref patch
  receivedPass = as.numeric(finalPossession[,"Rec"]), #number of times the player succesfully received the pass
  #passesReceivedPct = as.numeric(finalPossession[,"Rec%"]), #need to play at least 30 minutes. #data removed on new fbref patch
  miscontrol = as.numeric(finalPossession[,"Mis"]), #number of times a player failed when attempting to gain control of the ball
  disposessed = as.numeric(finalPossession[,"Dis"]), # number of times player loses the ball when tackled by the opposing player
  fouls = as.numeric(finalMiscellaneous[,"Fls"]),
  foulsDrawn = as.numeric(finalMiscellaneous[,"Fld"]),
  offsides = as.numeric(finalMiscellaneous[,"Off"]),
  penaltyKicksWon = as.numeric(finalMiscellaneous[,"PKwon"]),
  penaltyKicksConceded = as.numeric(finalMiscellaneous[,"PKcon"]),
  ownGoals = as.numeric(finalMiscellaneous[,"OG"]),
  looseBallsRecovered = as.numeric(finalMiscellaneous[,"Recov"]),
  aerialDuelsWon = as.numeric(finalMiscellaneous[,"Won"]),
  aerialDuelsLost = as.numeric(finalMiscellaneous[,"Lost"]),
  aerialDuelsWonPct = as.numeric(finalMiscellaneous[,"Won%"]),
  gameweek = gameweek,
  teamFormation = as.factor(finalSummary[,ncol(finalSummary)-2]),
  oppositionFormation = as.factor(finalSummary[,ncol(finalSummary)-1]),
  location = as.factor(finalSummary[,ncol(finalSummary)])
)



combined <- read.csv("combined.csv") %>% dplyr::select(players:opposition)
combined$gameweek <- gsub("[^0-9.-]", "", combined$gameweek)       
combined$gameweek <- as.numeric(combined$gameweek)
currentSeason <- read.csv("currentSeason.csv")
currentSeason$Wk <- gsub("[^0-9.-]", "", currentSeason$Wk)       
currentSeason$Wk <- as.numeric(currentSeason$Wk)
GW$opposition <- 0
for (i in 1:nrow(GW)) {
  for (j in 1:nrow(currentSeason)) {
    if (GW$gameweek[i] == currentSeason$Wk[j]) {
      if (GW$teams[i] == currentSeason$Home[j]) {
        GW$opposition[i] = currentSeason$Away[j]
      }
      if (GW$teams[i] == currentSeason$Away[j]) {
        GW$opposition[i] = currentSeason$Home[j]
      }
    }
  }
}


if (gameweek %in% unique(combined$gameweek)) {
  combined <- combined[-which(combined$gameweek == gameweek),]
  combined <- plyr::rbind.fill(combined, GW)
} else {
  combined <- plyr::rbind.fill(combined, GW)
}



#If adding opposition doesn't work uncoment the following code and run it for the selected gameweek.
#combined <- combined[-which(combined$gameweek == "Gameweek 11"),]
#combined <- rbind(combined, GW)

setwd("/Users/apoudel53/Desktop/ML Practice/Sports Analytics/2022-2023 PL/")
write.csv(GW, paste("GW", gameweek, sep="",".csv"))
write.csv(combined, "combined.csv")

