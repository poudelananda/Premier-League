library(rvest)
library(xml2)
library(XML)
library(tidyr)
library(shiny)
library(shinythemes)
library(stringr)
library(reshape2)
library(psych)
library(stringi)
library(glmnet)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(ggthemes)
library(tidytext)
library(reactable)
library(DT)
library(caret)
library(MASS)
library(factoextra)
library(tidytext)
library(ggpubr)
library(RCurl)
library(dplyr)

currentSeason <- read.csv(text = getURL("https://raw.githubusercontent.com/poudelananda/Premier-League/main/currentSeason.csv"))
combined <- read.csv(text = getURL("https://raw.githubusercontent.com/poudelananda/Premier-League/main/combined.csv"))
fixtures <- read.csv(text = getURL("https://raw.githubusercontent.com/poudelananda/Premier-League/main/fixtures.csv"))

combined$Pos <- 0
combined$Pos <- substr(combined$position, 1,2)
combined$teamFormation <- as.character(combined$teamFormation)
combined$formationTeam <- sub(".* ", "",combined$teamFormation)
combined$formationTeam[which(combined$formationTeam == "(4-4-2◆)")] = "(4-4-2)"
combined$formationTeam[which(combined$formationTeam == "(3-4-3◆)")] = "(3-4-3)"
combined$formationTeam <- as.factor(combined$formationTeam)
unique(combined$formationTeam)

combined$oppositionFormation <- as.character(combined$oppositionFormation)
combined$formationOpp <- sub(".* ", "",combined$oppositionFormation)
combined$formationOpp[which(combined$formationOpp == "(4-4-2◆)")] = "(4-4-2)"
combined$formationOpp[which(combined$formationOpp == "(3-4-3◆)")] = "(3-4-3)"
combined$formationOpp <- as.factor(combined$formationOpp)
unique(combined$formationOpp)

combined$gameweek <- gsub("[^0-9.-]", "", combined$gameweek)       
combined$gameweek <- as.numeric(combined$gameweek)
combined <- combined[order(combined$gameweek),]
unique(combined$gameweek)

combined$oppositionLocation <- "Home"
for (i in 1:nrow(combined)) {
    if (combined$location[i] == "Home") {
        combined$oppositionLocation[i] = "Away"
    }
}

currentSeason <- currentSeason[, c("Wk", "Home", "Score", "Away")]
currentSeason$Wk <- gsub("[^0-9.-]", "", currentSeason$Wk)
currentSeason$Wk <- as.numeric(currentSeason$Wk)
currentSeason$Score <- gsub("–", "-", currentSeason$Score)

combined$age <- str_sub(combined$age, 1, 2)
combined$age <- as.numeric(combined$age)


fixtures <- fixtures[!is.na(fixtures$Wk),]
fixtures <- fixtures[which(fixtures$Score == ""),]



fixtures <- fixtures[!is.na(fixtures$Wk),]
fixtures <- fixtures[which(fixtures$Score == ""),]



ui <- fluidPage( 
    theme = shinytheme("sandstone"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            conditionalPanel(
                condition = "input.tabselected == 1",
                selectInput(
                    inputId = "gwSummary",
                    label = "Select Gameweek",
                    choices = c("Overall Average" ,unique(combined$gameweek))
                ),
                selectInput(
                    inputId = "gwCategory",
                    label = "Select Category",
                    choices = c("Attacking", "Passing", "Possession", "Defense")
                ),
                reactableOutput("gwResults1")
            ),
            conditionalPanel(
                condition = "input.tabselected == 2",
                selectInput(
                    inputId = "teamSelectSummary",
                    label = "Select Team",
                    choices = c(unique(combined$teams))
                ),
                selectInput(
                    inputId = "teamDataTypeSummary",
                    label = "Select Gameweek",
                    choices = c("Overall Average", "Player Weekly", unique(combined$gameweek))
                ),
                selectInput(
                    inputId = "teamCategorySummary",
                    label = "Select Category",
                    choices = c("Creativity", "Attacking", "Passing", "Possession", "Defense")
                ),
                reactableOutput("gwResults2")
                #htmlOutput("playerInfo")
            ),
            conditionalPanel(
                condition = "input.tabselected == 3",
                selectInput(
                    inputId = "offenseHomeTeam",
                    label = "Select Home Team",
                    choices = unique(combined$teams)
                ),
                selectInput(
                    inputId = "offenseHomeFormation",
                    label = "Select Home Formation",
                    choices = c("All", unique(as.character(combined$formationTeam)))
                ),
                selectInput(
                    inputId = "offenseAwayTeam",
                    label = "Select Away Team",
                    choices = unique(combined$teams)
                ),
                selectInput(
                    inputId = "offenseAwayFormation",
                    label = "Select Away Formation",
                    choices = c("All", unique(as.character(combined$formationTeam)))
                )
            ),
            conditionalPanel(
                condition = "input.tabselected == 4",
                selectInput(
                    inputId = "defenseHomeTeam",
                    label = "Select Home Team",
                    choices = unique(combined$teams)
                ),
                selectInput(
                    inputId = "defenseAwayTeam",
                    label = "Select Away Team",
                    choices = unique(combined$teams)
                )
            )
        ),
        mainPanel(
            width = 9,
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Gameweek Summary",
                    value = 1,
                    fluidRow(
                        column(width = 8,style = 'padding:10px',
                               plotlyOutput("gameweekPlot"))
                    )
                ),
                tabPanel(
                    "Player Summary",
                    value = 2,
                    fluidRow(
                        plotlyOutput("playerPlot"),
                    )
                ),
                tabPanel(
                    "Offense Analytics",
                    value = 3,
                    fluidRow(
                        splitLayout(
                            cellWidths = c("50%", "50%"),
                            plotlyOutput("homeOffensePlot"),
                            plotlyOutput("awayOffensePlot")
                        )
                    ),
                    fluidRow(
                        splitLayout(
                            cellWidths = c("50%", "50%"),
                            checkboxInput(
                                inputId = "selectHomeOffenseGraph",
                                label = "Display Results for Home Games.",
                                FALSE
                            ),
                            checkboxInput(
                                inputId = "selectAwayOffenseGraph",
                                label = "Display Results for Away Games.",
                                FALSE
                            )
                        )
                    ),
                    fluidRow(
                        dataTableOutput("offenseTeamOverallStat")
                    )
                ),
                tabPanel(
                    "Defense Analytics",
                    value = 4,
                    fluidRow(
                        splitLayout(
                            cellWidths = c("50%", "50%"),
                            plotlyOutput("homeDefensePlot"),
                            plotlyOutput("awayDefensePlot")
                        )
                    ),
                    fluidRow(
                        splitLayout(
                            cellWidths = c("50%", "50%"),
                            checkboxInput(
                                inputId = "selectHomeDefenseGraph",
                                label = "Display Results for Home Games.",
                                FALSE
                            ),
                            checkboxInput(
                                inputId = "selectAwayDefenseGraph",
                                label = "Display Results for Away Games.",
                                FALSE
                            )
                        )
                    ),
                    fluidRow(
                        dataTableOutput("defenseTeamOverallStat")
                    )
                ),
                id = "tabselected"
            ))
    )
)
server = function(input, output) {
    
    output$gwResults1 <- output$gwResults2 <- renderReactable({
        df <- currentSeason
        colnames(df) <- c("Week", "Home", "Score", "Away")
        theme <- reactableTheme(
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)")
        reactable(
            df,
            groupBy = "Week",
            theme = theme,
            style = list(fontFamily = "Work Sans, sans-serif", fontSize = "1rem"),
            resizable = TRUE,
            wrap = TRUE,
            bordered = TRUE,
            columns = list(
                Week = colDef(width = 45),
                Home = colDef(width = 80),
                Score = colDef(width = 45),
                Away = colDef(width = 80)
            )
        )
    })
    
    output$gameweekPlot <- renderPlotly({
        #DO sum by gameweek then aggregate again by teams
        if(input$gwSummary == "Overall Average") {
            df1 <- combined
            df1 <- df1 %>% select(teams, gameweek, goals, shots, shotsOnTarget, totalPassAttempted,
                                  totalPassCompleted, shortPassAttempted,
                                  shortPassCompleted, mediumPassAttempted, mediumPassCompleted,
                                  longPassAttempted,longPassCompleted,
                                  tacklesAttempted, tacklesWon,
                                  defensiveThirdAreaTouches, middleThirdAreaTouches,
                                  attackingThirdAreaTouches,
                                  ballsBlocked, interceptions)
            
            
            x <- df1 %>% 
                group_by(teams, gameweek) %>%
                summarise_all(sum)
            x <- x %>%
                group_by(teams) %>%
                summarise_all(mean)
            x$totalPassCompletedPct <- (x$totalPassCompleted/x$totalPassAttempted)*100
            x$shortPassCompletedPct <- (x$shortPassCompleted/x$shortPassAttempted)*100
            x$mediumPassCompletedPct <- (x$mediumPassCompleted/x$mediumPassAttempted)*100
            x$longPassCompletedPct <- (x$longPassCompleted/x$longPassAttempted)*100
            x$tacklesWonPct <- (x$tacklesWon/x$tacklesAttempted)*100
            #x$applyingPressurePct <- (x$applyingPressureCompleted/x$applyingPressureAttempts)*100
            ##Needed completed and attempts to calculate the percentage because averaging the percentage that's already in the data set does not give the right value
            
            colnames(x) <- c("Teams", "Gameweek", "Goals", "Shots",
                             "Shots On Target", "Total Pass Attempted", "Total Pass Completed", 
                             "Short Pass Attempted", "Short Pass Completed",
                             "Medium Pass Attempted", "Medium Pass Completed",
                             "Long Pass Attempted", "Long Pass Completed",
                             "Tackles Attempted", "Tackles Won",
                             "Defensive Touches", "Middle Touches",
                             "Attacking Touches",
                             "Balls Blocked", "Interceptions", 
                             "Total Pass %",
                             "Short Pass %", 
                             "Medium Pass %", 
                             "Long Pass %", 
                             "Tackles %", "Applying Pressure %")
            col_order <- c("Teams", "Goals", "Shots", "Shots On Target", 
                           "Total Pass Attempted","Total Pass %",
                           "Short Pass Attempted","Short Pass %",  "Medium Pass Attempted", "Medium Pass %",
                           "Long Pass Attempted", "Long Pass %",
                           "Defensive Touches", "Middle Touches",
                           "Attacking Touches", "Balls Blocked",
                           "Interceptions", "Tackles Attempted", "Tackles %")
            
            x <- x[,col_order]
            
            table_df1 <- x %>%
                melt(., id.vars = c("Teams"))
            
            
            attacking <- c("Goals", "Shots", "Shots On Target")
            passing <- c("Total Pass Attempted","Total Pass %",
                         "Short Pass Attempted", "Short Pass %", 
                         "Medium Pass Attempted", "Medium Pass %", 
                         "Long Pass Attempted", "Long Pass %")
            possession <- c("Progressive Distance","Key Passes", "Defensive Touches", "Middle Touches",
                            "Attacking Touches")
            defense <- c("Tackles Attempted", "Tackles %",
                         "Balls Blocked", "Interceptions")
            
            table_df1$Category <- ifelse(table_df1$variable %in% attacking, "Attacking",
                                         ifelse(table_df1$variable %in% passing, "Passing",
                                                ifelse(table_df1$variable %in% possession, "Possession",
                                                       ifelse(table_df1$variable %in% defense,"Defense",""))))
            
            meandf <- table_df1 %>% group_by(variable, Category) %>% 
                summarize(mean_val = round(mean(value),3))
            
            meandf <- meandf[which(meandf$Category == input$gwCategory),]
            meandf <- meandf %>% select (-Category)
            table_df1 <- table_df1[which(table_df1$Category == input$gwCategory),]
            table_df1 <- left_join(table_df1, meandf, by = "variable")
            
            #table_df1 <- table_df1[which(table_df1$Teams == input$gwTeam1 | table_df1$Teams == input$gwTeam2), ]
            
            p <- ggplot(table_df1, aes(x = reorder_within(Teams, -value, variable), y = value, fill = Teams,
                                       text = paste0("Team: ", Teams, "\nCategory: ",
                                                     Category, "\nValue: ", value))) +
                geom_bar(stat="identity", position = "dodge") +
                facet_wrap(~variable, scales = "free", ncol = 2) +
                theme_solarized() +
                xlab("") + ylab("") +
                theme(panel.spacing.y = unit(5, "mm"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                geom_hline(data = meandf, aes(yintercept = mean_val), linetype = "dotted") + coord_flip() +
                geom_text(aes(x = reorder_within(Teams, -value, variable), y = mean_val, 
                              label = Teams),size = 2) +
                geom_text(aes(x = 19, y = mean_val/3,
                              label = paste("Mean\n", mean_val), size = 2))
            ggplotly(p, width = 1000, height = 900, tooltip = "text")
            
            
        } else {
            df2 <- combined
            df2$tacklesWonPct <- (df2$tacklesWon/df2$tacklesAttempted) * 100
            df2 <- df2 %>% select(gameweek, teams, goals, shots, shotsOnTarget, totalPassAttempted,
                                  totalPassCompleted, shortPassAttempted,
                                  shortPassCompleted, mediumPassAttempted, mediumPassCompleted,
                                  longPassAttempted,longPassCompleted,
                                  tacklesAttempted, tacklesWon,
                                  defensiveThirdAreaTouches, middleThirdAreaTouches,
                                  attackingThirdAreaTouches,
                                  ballsBlocked, interceptions)
            df2[is.na(df2)] <- 0
            x <- df2 %>% 
                group_by(teams, gameweek) %>%
                summarise_all(sum)
            
            x$totalPassCompletedPct <- (x$totalPassCompleted/x$totalPassAttempted)*100
            x$shortPassCompletedPct <- (x$shortPassCompleted/x$shortPassAttempted)*100
            x$mediumPassCompletedPct <- (x$mediumPassCompleted/x$mediumPassAttempted)*100
            x$longPassCompletedPct <- (x$longPassCompleted/x$longPassAttempted)*100
            x$tacklesWonPct <- (x$tacklesWon/x$tacklesAttempted)*100
            #x$applyingPressurePct <- (x$applyingPressureCompleted/x$applyingPressureAttempts)*100
            x[is.na(x)] <- 0
            
            colnames(x) <- c("Teams", "Gameweek", "Goals", "Shots",
                             "Shots On Target", "Total Pass Attempted", "Total Pass Completed", 
                             "Short Pass Attempted", "Short Pass Completed",
                             "Medium Pass Attempted", "Medium Pass Completed",
                             "Long Pass Attempted", "Long Pass Completed",
                             "Tackles Attempted", "Tackles Won",
                             "Defensive Touches", "Middle Touches",
                             "Attacking Touches",
                             "Balls Blocked", "Interceptions", 
                             "Total Pass %",
                             "Short Pass %", 
                             "Medium Pass %", 
                             "Long Pass %", 
                             "Tackles %")
            col_order <- c("Teams", "Gameweek", "Goals", "Shots", "Shots On Target", 
                           "Total Pass Attempted","Total Pass %",
                           "Short Pass Attempted","Short Pass %",  "Medium Pass Attempted", "Medium Pass %",
                           "Long Pass Attempted", "Long Pass %",
                           "Defensive Touches", "Middle Touches",
                           "Attacking Touches", "Balls Blocked",
                           "Interceptions", "Tackles Attempted", "Tackles %")
            
            x <- x[,col_order]
            table_df2 <- melt(x, id.vars = c("Teams", "Gameweek"))
            attacking <- c("Goals", "Shots", "Shots On Target")
            passing <- c("Total Pass Attempted","Total Pass %",
                         "Short Pass Attempted", "Short Pass %", 
                         "Medium Pass Attempted", "Medium Pass %", 
                         "Long Pass Attempted", "Long Pass %")
            possession <- c("Progressive Distance","Key Passes", "Defensive Touches", "Middle Touches",
                            "Attacking Touches")
            defense <- c("Tackles Attempted", "Tackles %",
                         "Balls Blocked", "Interceptions")
            table_df2$Category <- ifelse(table_df2$variable %in% attacking, "Attacking",
                                         ifelse(table_df2$variable %in% passing, "Passing",
                                                ifelse(table_df2$variable %in% possession, "Possession",
                                                       ifelse(table_df2$variable %in% defense,"Defense",""))))
            
            
            table_df2 <- table_df2[which(table_df2$Gameweek == input$gwSummary & 
                                             table_df2$Category == input$gwCategory),]
            meandf <- table_df2 %>% group_by(variable, Category) %>% 
                summarize(mean_val = round(mean(value), 3))
            
            meandf <- meandf[which(meandf$Category == input$gwCategory),]
            meandf <- meandf %>% select(-Category)
            table_df2 <- left_join(table_df2, meandf, by = "variable")
            
            p <- ggplot(table_df2, aes(x = reorder_within(Teams, -value, variable), y = value, fill = Teams,
                                       text = paste0("Team: ", Teams, "\nCategory: ", 
                                                     Category, "\nValue: ", value))) + 
                geom_bar(stat="identity") +
                facet_wrap(~variable, scales = "free", ncol = 2) + theme_solarized() +
                xlab("") + ylab("") +
                theme(panel.spacing.y = unit(5, "mm"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                geom_hline(data = meandf, aes(yintercept = mean_val), linetype = "dotted") + coord_flip() +
                geom_text(aes(x = reorder_within(Teams, -value, variable), y = mean_val, 
                              label = Teams),size = 2) +
                geom_text(aes(x = 19, y = mean_val/3,
                              label = paste("Mean\n", mean_val), size = 2))
            
            ggplotly(p, width = 1000, height = 900, tooltip = "text")
            
        }
    })
    
    output$playerInfo <- renderUI({
        if (input$playerSelectSummary == "All") {
            paste("Select a player for more information.")
        } else {
            nameText <- as.character(unique(combined$players[which(combined$players == input$playerSelectSummary)]))
            nameText <- paste(nameText, collapse = " ")
            ageText <- as.character(max(combined$age[which(combined$players == input$playerSelectSummary)]))
            ageText <- paste(ageText, collapse = " ")
            minsPerGame <- as.character(round(sum(combined$mins[which(combined$players == input$playerSelectSummary)])/
                                                  nrow(combined[which(combined$players == input$playerSelectSummary),])),3)
            minsPerGame <- paste(minsPerGame, collapse = "")
            total90sPlayed <- as.character(round(sum(combined$mins[which(combined$players ==
                                                                             input$playerSelectSummary)])/90,3))
            total90sPlayed <- paste(total90sPlayed, collapse = "")
            totalGoals <- as.character(sum(combined$goals[which(combined$players == input$playerSelectSummary)]))
            totalGoals <- paste(totalGoals, collapse = " ")
            totalAssists <- as.character(sum(combined$assists[which(combined$players == input$playerSelectSummary)]))
            totalAssists <- paste(totalAssists, collapse = " ")
            gaPerGame <- round(sum(combined$goals[which(combined$players == input$playerSelectSummary)],
                                   combined$assists[which(combined$players == input$playersSelectSummary)])/
                                   length(combined$players[which(combined$players == input$playerSelectSummary)]),3)
            gaPerGame <- paste(as.character(gaPerGame), collapse = " ")
            str1 <- paste("Name: ", nameText)
            str2 <- paste("Age: ", ageText)
            str3 <- paste("Minutes Per Game: ", minsPerGame)
            str4 <- paste("Total 90 mins played: ", total90sPlayed)
            str5 <- paste("Total Goals: ", totalGoals)
            str6 <- paste("Total Assists: ", totalAssists)
            str7 <- paste("Total Goals and Assists Per Game: ", gaPerGame)
            HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = "<br/>")) 
        }
    })
    
    output$playerPlot <- renderPlotly({
        df <- combined[which(combined$teams == input$teamSelectSummary),]
        df <- df %>% select(teams, gameweek, location, players,
                            shotCreatingActions, goalCreatingActions, keyPasses, progressivePasses,
                            goals, shots, shotsOnTarget, totalPassAttempted,
                            totalPassCompleted, shortPassAttempted,
                            shortPassCompleted, mediumPassAttempted, mediumPassCompleted,
                            longPassAttempted,longPassCompleted,
                            tacklesAttempted, tacklesWon,
                            defensiveThirdAreaTouches, middleThirdAreaTouches,
                            attackingThirdAreaTouches,
                            ballsBlocked, interceptions)
        df[is.na(df)] <- 0
        clean_data <- function(x) {
            x$totalPassCompletedPct <- (x$totalPassCompleted/x$totalPassAttempted)*100
            x$shortPassCompletedPct <- (x$shortPassCompleted/x$shortPassAttempted)*100
            x$mediumPassCompletedPct <- (x$mediumPassCompleted/x$mediumPassAttempted)*100
            x$longPassCompletedPct <- (x$longPassCompleted/x$longPassAttempted)*100
            x$tacklesWonPct <- (x$tacklesWon/x$tacklesAttempted)*100
            #x$applyingPressurePct <- (x$applyingPressureCompleted/x$applyingPressureAttempts)*100
            x[is.na(x)] <- 0
            
            colnames(x) <- c("Players", "Location", "Gameweek",
                             "Shot Creating Actions", "Goal Creating Actions", "Key Passes", 
                             "Progressive Passes",
                             "Goals", "Shots",
                             "Shots On Target", "Total Pass Attempted", "Total Pass Completed", 
                             "Short Pass Attempted", "Short Pass Completed",
                             "Medium Pass Attempted", "Medium Pass Completed",
                             "Long Pass Attempted", "Long Pass Completed",
                             "Tackles Attempted", "Tackles Won",
                             "Defensive Touches", "Middle Touches",
                             "Attacking Touches",
                             "Balls Blocked", "Interceptions", 
                             "Total Pass %",
                             "Short Pass %", 
                             "Medium Pass %", 
                             "Long Pass %", 
                             "Tackles %")
            col_order <- c("Gameweek", "Location", "Players",
                           "Shot Creating Actions", "Goal Creating Actions", "Key Passes",
                           "Progressive Passes",
                           "Goals", "Shots", "Shots On Target", 
                           "Total Pass Attempted","Total Pass %",
                           "Short Pass Attempted","Short Pass %",  "Medium Pass Attempted", "Medium Pass %",
                           "Long Pass Attempted", "Long Pass %",
                           "Defensive Touches", "Middle Touches",
                           "Attacking Touches", "Balls Blocked",
                           "Interceptions", "Tackles Attempted", "Tackles %")
            
            x <- x[,col_order]
            x <- melt(x, id.vars = c("Gameweek", "Location", "Players"))
            creativity <- c("Shot Creating Actions", "Goal Creating Actions", "Key Passes",
                            "Progressive Passes")
            attacking <- c("Goals", "Shots", "Shots On Target")
            passing <- c("Total Pass Attempted","Total Pass %",
                         "Short Pass Attempted", "Short Pass %", 
                         "Medium Pass Attempted", "Medium Pass %", 
                         "Long Pass Attempted", "Long Pass %")
            possession <- c("Progressive Distance","Key Passes", "Defensive Touches", "Middle Touches",
                            "Attacking Touches")
            defense <- c("Tackles Attempted", "Tackles %",
                         "Balls Blocked", "Interceptions")
            x$Category <- ifelse(x$variable %in% attacking, "Attacking",
                                 ifelse(x$variable %in% passing, "Passing",
                                        ifelse(x$variable %in% possession, "Possession",
                                               ifelse(x$variable %in% defense,"Defense","Creativity"))))
            return(x)
        }
        
        if (input$teamDataTypeSummary == "Overall Average") {
            x <- df %>% 
                group_by(players, gameweek, location) %>%
                summarise_if(is.numeric, sum)
            x <- x %>% 
                group_by(players, location) %>%
                summarise_if(is.numeric, mean)
            
            table_df2 <- clean_data(x)
            df1 <- table_df2[which(table_df2$Category == input$teamCategorySummary),]
            meandf <- df1 %>% group_by(variable) %>% 
                summarize(mean_val = round(mean(value), 3))
            df1 <- left_join(df1, meandf, by = "variable")
            
            p <- ggplot(df1, aes(x = reorder_within(Players,value,variable), y = value, fill = Location,
                                 text = paste0("Player: ", Players, "\nCategory: ", 
                                               Category, "\nValue: ", value))) + 
                geom_bar(stat="identity") +
                facet_wrap(~variable, scales = "free", ncol = 2) + theme_solarized() +
                scale_fill_brewer(palette = "Accent") + xlab("") + ylab("") +
                theme(panel.spacing.y = unit(5, "mm"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
                coord_flip() + 
                geom_text(aes(x = reorder_within(Players, value, variable), y = mean_val,
                              label = Players),size = 2)
            #geom_hline(data = meandf, aes(yintercept = mean_val), linetype = "dotted") 
            
            ggplotly(p, width = 950, height = 1000, tooltip = "text")
        } else if(input$teamDataTypeSummary == "Player Weekly") {
            x <- df %>% 
                group_by(players, location, gameweek) %>%
                summarise_if(is.numeric, sum)
            table_df2 <- clean_data(x)
            df2 <- table_df2
            df2 <- df2[which(df2$Category == input$teamCategorySummary),]
            #meandf <- df2 %>% group_by(variable) %>% summarize(mean_val = mean(value))
            #df2 <- left_join(df2, meandf, by = "variable")
            df2$Players <- as.factor(df2$Players)
            df2$Category <- as.factor(df2$Category)
            df2$Gameweek <- as.factor(df2$Gameweek)
            
            p <- ggplot(df2, aes(x = Gameweek, y = value, fill = variable,
                                 text = paste0("Player: ", Players, "\nVariable: ", 
                                               variable, "\nValue: ", value))) + 
                geom_point(size = 3) + geom_line(aes(group = 1)) + 
                facet_wrap(~Players, scales = "free", ncol = 2) + theme_solarized() +
                scale_fill_brewer(palette = "Accent") + xlab("") + ylab("") +
                theme(panel.spacing.y = unit(5, "mm")) 
            #geom_text(aes(x = reorder_within(Players, value, variable), y = mean_val,
            #             label = Players),size = 2)
            #geom_hline(data = meandf, aes(yintercept = mean_val), linetype = "dotted") 
            
            ggplotly(p, width = 950, height = 1500, tooltip = "text")
            
        } else {
            x <- df %>% 
                group_by(players, location, gameweek) %>%
                summarise_if(is.numeric, sum)
            
            table_df2 <- clean_data(x)
            df2 <- table_df2
            df2 <- df2[which(df2$Gameweek == input$teamDataTypeSummary & 
                                 df2$Category == input$teamCategorySummary),]
            meandf <- df2 %>% group_by(variable) %>% 
                summarize(mean_val = round(mean(value), 3))
            df2 <- left_join(df2, meandf, by = "variable")
            
            p <- ggplot(df2, aes(x = reorder_within(Players,value,variable), y = value, fill = Location,
                                 text = paste0("Player: ", Players, "\nCategory: ", 
                                               Category, "\nValue: ", value))) + 
                geom_bar(stat="identity", position = "stack") +
                facet_wrap(~variable, scales = "free", ncol = 2) + theme_solarized() +
                scale_fill_brewer(palette = "Accent") + xlab("") + ylab("") +
                theme(panel.spacing.y = unit(5, "mm"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
                coord_flip() +
                geom_text(aes(x = reorder_within(Players, value, variable), y = mean_val,
                              label = Players),size = 2)
            #geom_hline(data = meandf, aes(yintercept = mean_val), linetype = "dotted") 
            
            ggplotly(p, width = 950, height = 1000, tooltip = "text")
        }
        
        
    })
    
    #output$playerOffense 
    
    #output$playerDefense
    
    output$offenseTeamOverallStat <- renderDataTable({
        #Return a data table with teams
        if (input$selectHomeOffenseGraph == FALSE & input$selectAwayOffenseGraph == FALSE) {
            df <-
                melt(
                    combined,
                    id.vars = c("teams"),
                    measure.vars = c("shots", "shotsOnTarget", "goals")
                )
            df <-
                aggregate(df$value,
                          by = list(df$teams, df$variable),
                          FUN = sum)
            colnames(df) <- c("teams", "variable", "value")
            df <- dcast(df, teams ~ variable, value.var = c("value"))
        }
        else if (input$selectHomeOffenseGraph == TRUE & input$selectAwayOffenseGraph == FALSE) {
            currentSeason[which(currentSeason$Home == input$offenseHomeTeam), ]
        }
        else if (input$selectHomeOffenseGraph == FALSE & input$selectAwayOffenseGraph == TRUE) {
            currentSeason[which(currentSeason$Away == input$offenseAwayTeam), ]
        }
        else {
            currentSeason[which(currentSeason$Away == input$offenseAwayTeam | currentSeason$Home == input$offenseHomeTeam), ]
        }
    })
    
    output$homeOffensePlot <- renderPlotly ({
        df <- combined[which(combined$location == "Home"),]
        df <- melt(df, id.vars = c("teams", "gameweek", "formationTeam"), measure.vars = c("shots", 'shotsOnTarget', 'goals'))
        if(input$offenseHomeFormation == "All"){
            df <- df[which(df$teams == input$offenseHomeTeam),]
            df <- aggregate(df$value, by = list(df$gameweek, df$variable), FUN = sum)
            colnames(df) <- c("Gameweek", "StatType","Value")
            df <- data.frame(teams = input$offenseHomeTeam, df)
            ggplot(df, aes(x = Gameweek, y = Value, color = StatType)) + geom_point() + geom_line() + 
                facet_wrap(~teams) + theme_stata()
        }
        else if (!(input$offenseHomeFormation %in% df$formationTeam[which(df$teams == input$offenseHomeTeam)]))
        {
            formText <- as.character(unique(df$formationTeam[which(df$teams == input$offenseHomeTeam)]))
            formText <- paste(formText, collapse = " ")
            ggplot() + annotate("text", x = 1, y = 1, size = 5, 
                                label = paste("No visualization with the selected formation",
                                              "Select from the following Formations",
                                              formText,
                                              sep = "\n")) + 
                theme_void()
        }
        else {
            df <- df[which(df$teams == input$offenseHomeTeam & df$formationTeam == input$offenseHomeFormation),]
            df <- aggregate(df$value, by = list(df$gameweek, df$variable), FUN = sum)
            colnames(df) <- c("Gameweek", "StatType","Value")
            df <- data.frame(teams = input$offenseHomeTeam, df)
            ggplot(df, aes(x = Gameweek, y = Value, color = StatType)) + geom_point() + geom_line() + 
                facet_wrap(~teams) + theme_stata()
        }
    })
    
    output$awayOffensePlot <- renderPlotly ({
        df2 <- combined[which(combined$location == "Away"),]
        df2 <- melt(df2, id.vars = c("teams", "gameweek", "formationTeam"), measure.vars = c("shots", 'shotsOnTarget', 'goals'))
        if(input$offenseAwayFormation == "All"){
            df2 <- df2[which(df2$teams == input$offenseAwayTeam),]
            df2 <- aggregate(df2$value, by = list(df2$gameweek, df2$variable), FUN = sum)
            colnames(df2) <- c("Gameweek", "StatType","Value")
            df2 <- data.frame(teams = input$offenseAwayTeam, df2)
            ggplot(df2, aes(x = Gameweek, y = Value, color = StatType)) + geom_point() + geom_line() + 
                facet_wrap(~teams) + theme_stata()
        }
        else if (!(input$offenseAwayFormation %in% df2$formationTeam[which(df2$teams == input$offenseAwayTeam)]))
        {
            formText <- as.character(unique(df2$formationTeam[which(df2$teams == input$offenseAwayTeam)]))
            formText <- paste(formText, collapse = " ")
            ggplot() + annotate("text", x = 1, y = 1, size = 5, 
                                label = paste("No visualization with the selected formation",
                                              "Select from the following Formations",
                                              formText,
                                              sep = "\n")) + 
                theme_void()
        }
        else {
            df2 <- df2[which(df2$teams == input$offenseAwayTeam & df2$formationTeam == input$offenseAwayFormation),]
            df2 <- aggregate(df2$value, by = list(df2$gameweek, df2$variable), FUN = sum)
            colnames(df2) <- c("Gameweek", "StatType","Value")
            df2 <- data.frame(teams = input$offenseAwayTeam, df2)
            ggplot(df2, aes(x = Gameweek, y = Value, color = StatType)) + geom_point() + geom_line() + 
                facet_wrap(~teams) + theme_stata()
        }
    })
    
    output$defenseTeamOverallStat <- renderDataTable({
        #Return a data table with teams
        if (input$selectHomeDefenseGraph == FALSE & input$selectAwayDefenseGraph == FALSE) {
            df <-
                melt(
                    combined,
                    id.vars = c("opposition"),
                    measure.vars = c("shots", "shotsOnTarget", "goals")
                )
            df <-
                aggregate(df$value,
                          by = list(df$opposition, df$variable),
                          FUN = sum)
            colnames(df) <- c("teams", "variable", "value")
            df <- dcast(df, teams ~ variable, value.var = c("value"))
        }
        else if (input$selectHomeDefenseGraph == TRUE & input$selectAwayDefenseGraph == FALSE) {
            currentSeason[which(currentSeason$Home == input$defenseHomeTeam), ]
        }
        else if (input$selectHomeDefenseGraph == FALSE & input$selectAwayDefenseGraph == TRUE) {
            currentSeason[which(currentSeason$Away == input$defenseAwayTeam), ]
        }
        else {
            currentSeason[which(currentSeason$Away == input$defenseAwayTeam | currentSeason$Home == input$defenseHomeTeam), ]
        }
    })
    
    output$homeDefensePlot <- renderPlotly ({
        df <- combined[which(combined$oppositionLocation == "Home"),]
        df <- melt(df, id.vars = c("opposition", "gameweek"), measure.vars = c("shots", 'shotsOnTarget', 'goals'))
        df <- df[which(df$opposition == input$defenseHomeTeam),]
        df <- aggregate(df$value, by = list(df$gameweek, df$variable), FUN = sum)
        colnames(df) <- c("Gameweek", "StatType","Value")
        df <- data.frame(teams = input$defenseHomeTeam, df)
        ggplot(df, aes(x = Gameweek, y = Value, color = StatType)) + geom_point() + geom_line() + 
            facet_wrap(~teams) + theme_stata()
    })
    
    output$awayDefensePlot <- renderPlotly ({
        df <- combined[which(combined$oppositionLocation == "Away"),]
        df <- melt(df, id.vars = c("opposition", "gameweek"), measure.vars = c("shots", 'shotsOnTarget', 'goals'))
        df <- df[which(df$opposition == input$defenseAwayTeam),]
        df <- aggregate(df$value, by = list(df$gameweek, df$variable), FUN = sum)
        colnames(df) <- c("Gameweek", "StatType","Value")
        df <- data.frame(teams = input$defenseAwayTeam, df)
        ggplot(df, aes(x = Gameweek, y = Value, color = StatType)) + geom_point() + geom_line() + facet_wrap(~teams) +
            theme_stata()
    })
    
}
shinyApp(ui, server)