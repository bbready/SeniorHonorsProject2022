#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(tidyverse)

league_details <- read_csv("country_options.csv", show_col_types = FALSE)

clean_tables <- function(first_table){
    names(first_table) <- c("date", "2", "home", "4", "result", "6", "away", "8", "9", "10", "11")
    first_table <- first_table %>%
        select(-c(2, 4, 6, 8, 9, 10, 11))
    first_table <- suppressWarnings(first_table %>%
                                        filter(., substr(result, 1, 1) == "-" | substr(home, 1, 1) == "(") %>%
                                        separate(., result, into = c("home_score", "away_score"), sep = ":") %>%
                                        separate(., date, into = c("time", "day"), sep = "\t\t\t\t\t\t\t\t\t\t", fill = "right") %>%
                                        select(-time) %>%
                                        fill(., day, dir("down")) %>%
                                        separate(., home, into = c("home_position", "home_team"), sep = "\\)") %>%
                                        separate(., away, into = c("away_team", "away_position"), sep = "\\("))
    if(is.na(first_table$home_team[1])){
        first_table <- first_table %>%
            rename(., home_team = home_position, home_position = home_team)
    }
    first_table <- first_table[, c("day", "home_team", "home_position", "home_score", "away_score", "away_team", "away_position")]
    first_table <- first_table %>%
        select(-c(home_position, away_position))
    first_table <- suppressWarnings(first_table %>%
                                        mutate(home_team = str_trim(home_team, "both"), away_team = str_trim(away_team, "both"),
                                               home_score = as.numeric(home_score), away_score = as.numeric(away_score),
                                               day = str_trim(day, "both")) %>%
                                        mutate(., day = as.Date(day, "%m/%d/%y")) %>%
                                        rename(., Date = day, 'Home team' = home_team, 'Away team' = away_team,
                                               'Home score' = home_score, 'Away score' = away_score))
    return(first_table)
}

finished_table <- function(tables){
    full_table <- data.frame(matrix(nrow = 0, ncol = 7))
    for (x in 1:length(tables)){
        temp_table <- tables[[x]]
        full_table <- rbind(full_table, clean_tables(temp_table))
    }
    return(full_table)
}

standings_func <- function(data){
    data <- data %>%
        rename(home_team = 'Home team', away_team = 'Away team', home_score = 'Home score', away_score = 'Away score')
    home_goals <- data %>%
        group_by(home_team) %>%
        summarise(home_goals_for = sum(home_score, na.rm=TRUE), home_goals_against = sum(away_score, na.rm=TRUE))
    away_goals <- data %>%
        group_by(away_team) %>%
        summarise(away_goals_for = sum(away_score, na.rm=TRUE), away_goals_against = sum(home_score, na.rm=TRUE))
    total_goals <- cbind(home_goals, away_goals) %>%
        select(-c(away_team)) %>%
        rename(team = home_team) %>%
        mutate(goals_scored = (home_goals_for + away_goals_for), goals_conceded = (home_goals_against + away_goals_against), 
               goal_differential = goals_scored - goals_conceded)
    home_points <- data %>%
        group_by(home_team) %>%
        mutate(points = ifelse(home_score > away_score, 3, ifelse(home_score == away_score, 1, 0))) %>%
        summarise(home_points = sum(points, na.rm=TRUE), home_wins = sum(points == 3, na.rm = TRUE), 
                  home_ties = sum(points == 1, na.rm = TRUE), home_losses = sum(points == 0, na.rm = TRUE))
    away_points <- data %>%
        group_by(away_team) %>%
        mutate(points = ifelse(away_score > home_score, 3, ifelse(away_score == home_score, 1, 0))) %>%
        summarise(away_points = sum(points, na.rm=TRUE), away_wins = sum(points == 3, na.rm = TRUE), 
                  away_ties = sum(points == 1, na.rm = TRUE), away_losses = sum(points == 0, na.rm = TRUE))
    total_points <- cbind(home_points, away_points) %>%
        select(-c(away_team)) %>%
        rename(team = home_team) %>%
        mutate(points = home_points + away_points, wins = home_wins + away_wins, ties = home_ties + away_ties, 
               losses = home_losses + away_losses, record = paste(wins, losses, ties, sep = "-"))
    standings <- cbind(total_goals, total_points) %>%
        select(-c(9)) %>%
        arrange(desc(points), desc(goal_differential)) %>%
        select(team, points, goal_differential, goals_scored, goals_conceded, wins, losses, ties, record) %>%
        rename(Team = team, Points = points, 'Goal Differential' = goal_differential, 'Goals Scored' = goals_scored, 
               'Goals Conceded' = goals_conceded, Wins = wins, Losses = losses, Ties = ties, 'Record (W-L-T)' = record)
    return(standings)
}


# RPI function
rpi <- function(data, w1=(1/4), w2=(1/2), w3=(1/4)){
    data <- data %>%
        rename(home_team = 'Home team', away_team = 'Away team', home_score = 'Home score', away_score = 'Away score') %>%
        filter(!is.na(home_score & away_score))
    home_points <- data %>%
        group_by(home_team) %>%
        mutate(points = ifelse(home_score > away_score, 3, ifelse(home_score ==    away_score, 1, 0)))  %>%
        summarise(home_points = sum(points, na.rm=TRUE), home_games = sum(!is.na(points)))
    away_points <- data %>%
        group_by(away_team) %>%
        mutate(points = ifelse(away_score > home_score, 3, ifelse(away_score == home_score, 1, 0))) %>%
        summarise(away_points = sum(points, na.rm=TRUE), away_games = sum(!is.na(points)))
    teams_ppg <- cbind(home_points, away_points) %>%
        select(-c(away_team)) %>%
        rename(team = home_team) %>%
        mutate(ppg = (home_points+away_points)/(home_games+away_games))
    data <- data %>%
        rename(team = home_team, opponent = away_team, team_score = home_score, opp_score = away_score)
    data2 <- data %>%
        rename(team = opponent, opponent = team, team_score = opp_score, opp_score = team_score)
    double_table <- bind_rows(data, data2)
    ppg1 <- left_join(double_table, teams_ppg, by = c("opponent" = "team")) %>%
        select(-c(home_points, away_points, home_games, away_games)) %>%
        rename(opp_ppg = ppg)
    opp_ppg <- ppg1 %>%
        group_by(team) %>%
        mutate(avg_opp_ppg = mean(opp_ppg)) %>%
        distinct(team, .keep_all = T) %>%
        select(c(team, avg_opp_ppg))
    new_teams_ppg <- left_join(teams_ppg, opp_ppg, by = c("team"))
    ppg2 <- left_join(double_table, new_teams_ppg, by = c("opponent" = "team")) %>%
        select(-c(home_points, away_points, home_games, away_games)) %>%
        rename(opp_ppg = ppg)
    new_opp_ppg <- ppg2 %>%
        group_by(team) %>%
        mutate(avg_opp_opp_ppg = mean(avg_opp_ppg)) %>%
        distinct(team, .keep_all = T) %>%
        select(c(team, avg_opp_opp_ppg))
    full_teams_ppg <- left_join(new_teams_ppg, new_opp_ppg, by = c("team")) %>%
        mutate(rpi = (ppg*w1+avg_opp_ppg*w2+avg_opp_opp_ppg*w3)) %>%
        arrange(desc(rpi)) %>%
        select(c(team, rpi, ppg, avg_opp_ppg, avg_opp_opp_ppg, home_points, home_games, away_points, away_games)) %>%
        rename(Team = team, RPI = rpi, 'Points per game (PPG)' = ppg, 'Opponent\'s PPG' = avg_opp_ppg, 'Opponent\'s Opponent\'s PPG' = avg_opp_opp_ppg, 
               'Home points' = home_points, 'Home games' = home_games, 'Away points' = away_points, 'Away games' = away_games) %>%
        mutate_if(is.numeric, round, digits=2)
    return(full_teams_ppg)
}

# To get a double table
doubleTableFunc <- function(games){
    data <- games %>%
        rename(team = 'Home team', opponent = 'Away team', team_score = 'Home score', opp_score = 'Away score') %>%
        filter(!is.na(team_score & opp_score))
    data2 <- data %>%
        rename(team = opponent, opponent = team, team_score = opp_score, opp_score = team_score)
    double_table <- bind_rows(data, data2)
    double_table$Location = NA
    double_table$Location[0:length(double_table$Date)/2] = 1
    double_table[is.na(double_table)] = -1
    return(double_table)
}

# Poisson function
poissonFunc <- function(games){
    model <- glm(team_score ~ team + opponent + Location, data = games, family = poisson)
    original <- data.frame(x = model$coefficients, rating = exp(model$coefficients))
    original$team <- rownames(original)
    row.names(original) <- NULL
    original <- original %>%
        select(team, x, rating)
    defAVG <- (original %>%
                   filter(str_detect(team, "opponent") == TRUE) %>%
                   summarise(sum(x)/(n()+1)))[1,1]
    offAVG <- (original %>%
                   filter(str_detect(team, "team") == TRUE) %>%
                   summarise(sum(x)/(n()+1)))[1,1]
    intercept <- (original$x)[1]
    homeAdj <- original$rating[length(original$rating)]
    adjusted <- data.frame(team = unique(games$team))
    adjusted <- data.frame(team = adjusted[order(adjusted$team),])
    adjusted$Off = append(0, original$x[2:(length(original$x)/2)])
    adjusted$Def = append(0, original$x[(length(original$x)/2+1):(length(original$x)-1)])
    adjusted$OffAdj = adjusted$Off+defAVG+intercept
    adjusted$DefAdj = adjusted$Def+offAVG+intercept
    adjusted$expOff = exp(adjusted$Off+defAVG+intercept)
    adjusted$expDef = exp(adjusted$Def+offAVG+intercept)
    avgPoisOffCoef <- mean(adjusted$OffAdj)
    avgPoisDefCoef <- mean(adjusted$DefAdj)
    avgPoisCoef <- exp((avgPoisDefCoef+avgPoisOffCoef)/2)
    scoreRate <- mean(games$team_score)
    C = scoreRate/avgPoisCoef
    poissonOFFavg = mean(adjusted$expOff)
    poissonDEFavg = mean(adjusted$expDef)
    D = sqrt(C*(poissonDEFavg/poissonOFFavg))
    adjusted$newOffExp = adjusted$expOff*D 
    adjusted$newDefExp = adjusted$expDef*(C/D)
    returnedTable <- adjusted %>%
        mutate(OffRating = newOffExp, DefRating = newDefExp, OverallRating = newOffExp/newDefExp) %>%
        select(team, OffRating, DefRating, OverallRating) %>%
        arrange(desc(OffRating))
    returnedTable$OffRank = 1:nrow(returnedTable)
    returnedTable <- returnedTable %>%
        arrange(DefRating)
    returnedTable$DefRank = 1:nrow(returnedTable)
    returnedTable <- returnedTable %>%
        arrange(desc(OverallRating)) %>%
        mutate_if(is.numeric, round, digits=2) %>%
        select(team, OverallRating, OffRating, OffRank, DefRating, DefRank)
    avgOffRating <- mean(returnedTable$OffRating)
    return(list(returnedTable, homeAdj, avgOffRating))
}

# Forecasts function
forecastFunc <- function(data, BIFATable, HomeAdj, AvgOffRating){
    data <- data %>%
        rename(date = "Date", home_team = 'Home team', away_team = 'Away team', home_score = 'Home score', away_score = 'Away score')
    firstMerge <- merge(data, BIFATable, by.x = "home_team", by.y = "team") %>%
        select(-c(OverallRating, OffRank, DefRank)) %>%
        rename(home_offRating = OffRating, home_defRating = DefRating)
    predictions <- merge(firstMerge, BIFATable, by.x = "away_team", by.y = "team") %>%
        select(-c(OverallRating, OffRank, DefRank)) %>%
        rename(away_offRating = OffRating, away_defRating = DefRating)
    predictions$home_expGoals = round((predictions$home_offRating*predictions$away_defRating*HomeAdj)/AvgOffRating, digits = 2)
    predictions$away_expGoals = round((predictions$away_offRating*predictions$home_defRating)/(AvgOffRating*HomeAdj), digits = 2)
    predictions <- predictions %>%
        select(-c(home_offRating, home_defRating, away_offRating, away_defRating))
    forecastedTable <- left_join(data, predictions, by = c("date", "home_team", "home_score", "away_score", "away_team")) %>%
        rename(Date = "date", 'Home team' = home_team, 'Away team' = away_team, 'Home score' = home_score, 'Away score' = away_score,
               'Home Expected Goals' = home_expGoals, 'Away Expected Goals' = away_expGoals)
    return(forecastedTable)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    games <- reactive(
        {
            rowk <- which(league_details$country == input$country)
            url <- paste("https://www.transfermarkt.us/", 
                         league_details$competition[rowk],
                         "/gesamtspielplan/wettbewerb/", 
                         league_details$country_code[rowk],
                         "/saison_id/", 
                         input$year, sep = "")
            print(url)
            tables <- url %>% 
                read_html() %>%
                html_table(fill = TRUE)
            tables <- tables[4:length(tables)]
            finished_table(tables)
        }
    )
    
    standings <- reactive(
        standings_func(games())
    )
    
    BIFATable <- reactive(
        poissonFunc(doubleTableFunc(games()))[[1]]
    )
    
    HomeAdj <- reactive(
        poissonFunc(doubleTableFunc(games()))[[2]]
    )
    
    AvgOffRating <- reactive(
        poissonFunc(doubleTableFunc(games()))[[3]]
    )
    
    output$standings <- renderDataTable(
        
        {
            standings()
        }
        
    )
    
    output$games <- renderDataTable(
        
        {
            games()
        }
    )
    
    output$rpi <- renderDataTable(
        
        {
            rpi(games())
        }
        
    )
    
    output$BIFAtext <- renderText({
        paste("Average Rating =", round(AvgOffRating(), digits = 3), " Home Adjustment =", round(HomeAdj(), digits = 3), sep = " ")
    })
    
    output$poisson <- renderDataTable(
        
        {
            BIFATable()
        }
    )
    
    output$forecasts <- renderDataTable(
        
        {
            forecastFunc(games(), BIFATable(), HomeAdj(), AvgOffRating())
        }
    )
    
    output$downloadGames <- downloadHandler(
        filename = "games.csv",
        content = function(file) {
            write.csv(games(), file, row.names = FALSE)
        }
    )
    
    output$downloadStandings <- downloadHandler(
        filename = "standings.csv",
        content = function(file) {
            write.csv(standings(), file, row.names = FALSE)
        }
    )
    
    output$downloadRPI <- downloadHandler(
        filename = "RPI.csv",
        content = function(file) {
            write.csv(rpi(games()), file, row.names = FALSE)
        }
    )
    
    output$downloadBIFA <- downloadHandler(
        filename = "BIFA.csv",
        content = function(file) {
            write.csv(BIFATable(), file, row.names = FALSE)
        }
    )
    
    output$downloadForecasts <- downloadHandler(
        filename = "forecasts.csv",
        content = function(file) {
            write.csv(forecastFunc(games(), BIFATable(), HomeAdj(), AvgOffRating()), file, row.names = FALSE)
        }
    )
    
})
