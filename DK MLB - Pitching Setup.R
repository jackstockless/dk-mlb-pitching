#### DRAFTKINGS MLB - PITCHING ####

# libraries
library(rvest)
library(tidyverse)
library(data.table)
library(gsubfn)
library(magrittr)
library(rlist)
library(lubridate)
library(devtools)
require(baseballr)
library(coda)
library(rjags)
library(R2jags)
load.module("glm")
library(ggplot2)
library(MASS)
library(randomForest)

# names and functions
teams = c("NYY","TBR","BOS","TOR","BAL","MIN","CLE","CHW","KCR","DET","HOU","OAK","TEX","LAA","SEA",
          "ATL","WSN","NYM","PHI","MIA","STL","MIL","CHC","CIN","PIT","LAD","ARI","SFG","COL","SDP")
b_names = c("Rank","Game","Date","Home","Opponent","Result","PA","AB","R","H","2B","3B","HR","RBI","BB",
            "IBB","SO","HBP","SH","SF","ROE","GDP","SB","CS","BA","OBP","SLG","OPS","LOB","Indiv_Batters",
            "P_Hand","Opp_P","Team")
mlb_fn = function(team, year, set){
  url = paste("https://www.baseball-reference.com/teams/tgl.cgi?team=", team, "&t=", set, "&year=", year, 
              sep = "")
  log = read_html(url) %>% html_nodes("table")
  log = log[[length(log)]] %>% html_table()
  log$Team = team
  return(log)
}

#### PAST SEASONS ####

## TEAM BATTING STATS

batting_box_17 = lapply(teams, mlb_fn, year = 2017, set = "b") %>%
  do.call("rbind", .)
colnames(batting_box_17) = b_names
batting_box_17 = batting_box_17 %>% dplyr::select(-Rank)
batting_box_17 = batting_box_17[batting_box_17$Opponent != "Opp",]
batting_box_17$Home = ifelse(batting_box_17$Home == "@", "Away", "Home")
cols = c(1,6:29)
batting_box_17[,cols] = sapply(batting_box_17[,cols], as.numeric)
batting_box_17$Season = 2017
month = word(batting_box_17$Date, 1)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
          ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
          ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = word(batting_box_17$Date, 2)
batting_box_17$Date = as.Date(paste(batting_box_17$Season, "-", month, "-", day, sep = ""))

batting_box_18 = lapply(teams, mlb_fn, year = 2018, set = "b") %>%
  do.call("rbind", .)
colnames(batting_box_18) = b_names
batting_box_18 = batting_box_18 %>% dplyr::select(-Rank)
batting_box_18 = batting_box_18[batting_box_18$Opponent != "Opp",]
batting_box_18$Home = ifelse(batting_box_18$Home == "@", "Away", "Home")
cols = c(1,6:29)
batting_box_18[,cols] = sapply(batting_box_18[,cols], as.numeric)
batting_box_18$Season = 2018
month = word(batting_box_18$Date, 1)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
          ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
          ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = word(batting_box_18$Date, 2)
batting_box_18$Date = as.Date(paste(batting_box_18$Season, "-", month, "-", day, sep = ""))

batting_box_19 = lapply(teams, mlb_fn, year = 2019, set = "b") %>%
  do.call("rbind", .)
colnames(batting_box_19) = b_names
batting_box_19 = batting_box_19 %>% dplyr::select(-Rank)
batting_box_19 = batting_box_19[batting_box_19$Opponent != "Opp",]
batting_box_19$Home = ifelse(batting_box_19$Home == "@", "Away", "Home")
cols = c(1,6:29)
batting_box_19[,cols] = sapply(batting_box_19[,cols], as.numeric)
batting_box_19$Season = 2019
month = word(batting_box_19$Date, 1)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
          ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
          ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = word(batting_box_19$Date, 2)
batting_box_19$Date = as.Date(paste(batting_box_19$Season, "-", month, "-", day, sep = ""))

batting_box_20 = lapply(teams, mlb_fn, year = 2020, set = "b") %>%
  do.call("rbind", .)
colnames(batting_box_20) = b_names
batting_box_20 = batting_box_20 %>% dplyr::select(-Rank)
batting_box_20 = batting_box_20[batting_box_20$Opponent != "Opp",]
batting_box_20$Home = ifelse(batting_box_20$Home == "@", "Away", "Home")
cols = c(1,6:29)
batting_box_20[,cols] = sapply(batting_box_20[,cols], as.numeric)
batting_box_20$Season = 2020
month = word(batting_box_20$Date, 1)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = word(batting_box_20$Date, 2)
batting_box_20$Date = as.Date(paste(batting_box_20$Season, "-", month, "-", day, sep = ""))

batting_box_21 = lapply(teams, mlb_fn, year = 2021, set = "b") %>%
  do.call("rbind", .)
colnames(batting_box_21) = b_names
batting_box_21 = batting_box_21 %>% dplyr::select(-Rank)
batting_box_21 = batting_box_21[batting_box_21$Opponent != "Opp",]
batting_box_21$Home = ifelse(batting_box_21$Home == "@", "Away", "Home")
cols = c(1,6:29)
batting_box_21[,cols] = sapply(batting_box_21[,cols], as.numeric)
batting_box_21$Season = 2021
month = word(batting_box_21$Date, 1)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = word(batting_box_21$Date, 2)
batting_box_21$Date = as.Date(paste(batting_box_21$Season, "-", month, "-", day, sep = ""))

# bind the years together
batting_box = bind_rows(batting_box_17, batting_box_18, batting_box_19, batting_box_20,
                           batting_box_21)

# iterate through and get all team summaries for all years
batting_sums = batting_box

# team summaries for each season (label as opponent bc matching with pitcher's opponent)
seasons = unique(batting_sums$Season)
batting_save = data.frame()
for(i in seasons){
  for(j in teams){
    df = batting_sums[batting_sums$Team == j & batting_sums$Season == i,]
    df = data.frame(as.numeric(i) + 1, j, mean(df$PA), mean(df$AB), mean(df$R), mean(df$H), mean(df$`2B`), 
                    mean(df$`3B`), mean(df$HR), mean(df$BB), mean(df$SO), mean(df$HBP), mean(df$SF))
    batting_save = bind_rows(batting_save, df)
  }
}
colnames(batting_save) = c("Season", "Opponent", "opp_prev_PA", "opp_prev_AB", "opp_prev_R", "opp_prev_H",
                           "opp_prev_2B", "opp_prev_3B", "opp_prev_HR", "opp_prev_BB", "opp_prev_SO",
                           "opp_prev_HBP", "opp_prev_SF")

# rolling stats
batting_rolling = data.frame()
for(i in seasons[2:length(seasons)]){
  season = batting_sums[batting_sums$Season == i,]
  for(j in teams){
    team = season[season$Team == j,]
    for(k in team$Game){
      df = batting_sums[batting_sums$Season == i & batting_sums$Team == j & batting_sums$Game < k,]
      df2 = batting_sums[batting_sums$Season == i & batting_sums$Team == j & batting_sums$Game <= k,]
      df = data.frame(i, j, k, df2$Date[df2$Game == k], mean(df$PA), mean(df$AB), mean(df$R), mean(df$H), mean(df$`2B`), 
                      mean(df$`3B`), mean(df$HR), mean(df$BB), mean(df$SO), mean(df$HBP), mean(df$SF))
      batting_rolling = bind_rows(batting_rolling, df)
    }
  }
}
colnames(batting_rolling) = c("Season", "Opponent", "Opp_Game", "Date", "opp_rolling_PA", "opp_rolling_AB", "opp_rolling_R", "opp_rolling_H",
                              "opp_rolling_2B", "opp_rolling_3B", "opp_rolling_HR", "opp_rolling_BB", "opp_rolling_SO",
                              "opp_rolling_HBP", "opp_rolling_SF")

# clean up
batting_box = batting_box %>% dplyr::select(Team, Game, Date, Opponent, Season)
batting_box = batting_box[batting_box$Season != 2017,]

## ELO

mlb_elo = fread("https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv")
mlb_elo = mlb_elo[mlb_elo$season >= 2018,]
mlb_elo = mlb_elo[mlb_elo$playoff == "",]
mlb_elo$team1 = ifelse(mlb_elo$team1 == "TBD", "TBR", mlb_elo$team1)
mlb_elo$team1 = ifelse(mlb_elo$team1 == "ANA", "LAA", mlb_elo$team1)
mlb_elo$team1 = ifelse(mlb_elo$team1 == "FLA", "MIA", mlb_elo$team1)
mlb_elo$team2 = ifelse(mlb_elo$team2 == "TBD", "TBR", mlb_elo$team2)
mlb_elo$team2 = ifelse(mlb_elo$team2 == "ANA", "LAA", mlb_elo$team2)
mlb_elo$team2 = ifelse(mlb_elo$team2 == "FLA", "MIA", mlb_elo$team2)

# function to get game numbers for each team
mlb_elo = mlb_elo[order(mlb_elo$date,  decreasing = FALSE),]
mlb_elo_18 = mlb_elo[mlb_elo$season == 2018,]
mlb_elo_19 = mlb_elo[mlb_elo$season == 2019,]
mlb_elo_20 = mlb_elo[mlb_elo$season == 2020,]
mlb_elo_21 = mlb_elo[mlb_elo$season == 2021,]
game_num_fn = function(entry, data){
  df = data[1:entry,]
  t1 = df$team1[entry]
  t2 = df$team2[entry]
  team1_game = nrow(df[df$team1 == t1,]) + nrow(df[df$team2 == t1,])
  team2_game = nrow(df[df$team1 == t2,]) + nrow(df[df$team2 == t2,])
  games = data.frame(team1_game, team2_game)
  return(games)
}
game_num_18 = lapply(1:nrow(mlb_elo_18), game_num_fn, data = mlb_elo_18) %>% do.call("rbind", .)
mlb_elo_18 = bind_cols(mlb_elo_18, game_num_18)
game_num_19 = lapply(1:nrow(mlb_elo_19), game_num_fn, data = mlb_elo_19) %>% do.call("rbind", .)
mlb_elo_19 = bind_cols(mlb_elo_19, game_num_19)
game_num_20 = lapply(1:nrow(mlb_elo_20), game_num_fn, data = mlb_elo_20) %>% do.call("rbind", .)
mlb_elo_20 = bind_cols(mlb_elo_20, game_num_20)
game_num_21 = lapply(1:nrow(mlb_elo_21), game_num_fn, data = mlb_elo_21) %>% do.call("rbind", .)
mlb_elo_21 = bind_cols(mlb_elo_21, game_num_21)
mlb_elo = bind_rows(mlb_elo_21, mlb_elo_20, mlb_elo_19, mlb_elo_18)

away_elo = mlb_elo
away_elo$Team = away_elo$team2
mlb_elo$Team = mlb_elo$team1
mlb_elo = bind_rows(mlb_elo, away_elo)
mlb_elo$rating_diff = ifelse(mlb_elo$Team == mlb_elo$team1, mlb_elo$rating1_pre - mlb_elo$rating2_pre,
                             mlb_elo$rating2_pre - mlb_elo$rating1_pre) 
mlb_elo$Game = ifelse(mlb_elo$Team == mlb_elo$team1, mlb_elo$team1_game, mlb_elo$team2_game)
mlb_elo$Opp_Game = ifelse(mlb_elo$Team == mlb_elo$team1, mlb_elo$team2_game, mlb_elo$team1_game)
colnames(mlb_elo) = c("Date", "Season", "Neutral", "Playoff", colnames(mlb_elo)[5:32])

## TEAM BATTING MATCHED WITH ELO

team_batting = left_join(batting_box, mlb_elo, by = c("Team", "Game", "Season"))
team_batting = left_join(team_batting, batting_rolling, by = c("Opponent", "Opp_Game", "Season"))
team_batting$Season = as.numeric(team_batting$Season)
team_batting = left_join(team_batting, batting_save, by = c("Opponent", "Season"))
team_batting = team_batting %>% dplyr::select(-c(Date.y, Neutral, Playoff, elo1_pre, elo2_pre, elo_prob1, 
                                                 elo_prob2, elo1_post, elo2_post, pitcher1_adj, 
                                                 pitcher2_adj, rating_prob1, rating_prob2, rating1_post, 
                                                 rating2_post, score1, score2, team1_game, team2_game, 
                                                 Date))
colnames(team_batting)[3] = "Date"
team_batting$Date = as.Date(team_batting$Date)
team_batting$Pitcher = ifelse(team_batting$Team == team_batting$team1, team_batting$pitcher1, 
                              team_batting$pitcher2)
team_batting$RGS = ifelse(team_batting$Team == team_batting$team1, team_batting$pitcher1_rgs, 
                              team_batting$pitcher2_rgs)

# set NaN to 0
for(i in 1:length(team_batting)){
  for(j in 1:nrow(team_batting)){
    team_batting[[i]][[j]] = ifelse(is.nan(team_batting[[i]][[j]]), 0, team_batting[[i]][[j]])
  }
}

# create weighted values
team_batting$opp_weight_PA = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_PA + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_PA)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_PA + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_PA)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_AB = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_AB + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_AB)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_AB + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_AB)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_R = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_R + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_R)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_R + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_R)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_H = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_H + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_H)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_H + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_H)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_2B = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_2B + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_2B)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_2B + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_2B)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_3B = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_3B + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_3B)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_3B + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_3B)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_HR = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_HR + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_HR)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_HR + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_HR)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_BB = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_BB + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_BB)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_BB + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_BB)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_SO = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_SO + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_SO)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_SO + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_SO)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_HBP = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_HBP + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_HBP)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_HBP + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_HBP)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_SF = ifelse(team_batting$Season == 2021,
                                    (60*team_batting$opp_prev_SF + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_SF)/(60 + exp((team_batting$Opp_Game - 1)/3)),
                                    (162*team_batting$opp_prev_SF + exp((team_batting$Opp_Game - 1)/3)*
                                       team_batting$opp_rolling_SF)/(162 + exp((team_batting$Opp_Game - 1)/3)))
team_batting$opp_weight_BA = team_batting$opp_weight_H/team_batting$opp_weight_AB
team_batting$opp_weight_OBP = (team_batting$opp_weight_H + team_batting$opp_weight_BB + team_batting$opp_weight_HBP)/(team_batting$opp_weight_AB + team_batting$opp_weight_BB + team_batting$opp_weight_HBP + team_batting$opp_weight_SF)
team_batting$opp_weight_SLG = ((team_batting$opp_weight_H - team_batting$opp_weight_2B + team_batting$opp_weight_3B + team_batting$opp_weight_HR) + 2*team_batting$opp_weight_2B + 3*team_batting$opp_weight_3B + 4*team_batting$opp_weight_HR)/team_batting$opp_weight_AB
team_batting$opp_weight_HR_pct = team_batting$opp_weight_HR/team_batting$opp_weight_PA
team_batting$opp_weight_SO_pct = team_batting$opp_weight_SO/team_batting$opp_weight_PA
team_batting$opp_weight_BB_pct = team_batting$opp_weight_BB/team_batting$opp_weight_PA

## PITCHER STATS

# function to pull game logs
pitcher_fn = function(list, year){
  # URLs to try
  url1 = paste("https://www.baseball-reference.com/players/gl.fcgi?id=", 
               tolower(str_sub(word(list, 2), 1, 5)), tolower(str_sub(word(list, 1), 1, 2)), 
               "01&t=p&year=", year, sep = "")
  url2 = paste("https://www.baseball-reference.com/players/gl.fcgi?id=", 
               tolower(str_sub(word(list, 2), 1, 5)), tolower(str_sub(word(list, 1), 1, 2)), 
               "02&t=p&year=", year, sep = "")
  url3 = paste("https://www.baseball-reference.com/players/gl.fcgi?id=", 
               tolower(str_sub(word(list, 2), 1, 5)), tolower(str_sub(word(list, 1), 1, 2)), 
               "03&t=p&year=", year, sep = "")
  
  # try each URL to get stats table
  logs1 = url1 %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  logs2 = if(length(logs1) == 0){
    url2 %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
  } else{
    c()
  }
  logs3 = if(length(logs1) == 0 & length(logs2) == 0){
    url3 %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
  } else{
    c()
  }
  
  # return
  logs = rbind(logs1, logs2, logs3)
  return(logs)
}

pitching_fn = function(year, set){
  url = paste("https://www.baseball-reference.com/leagues/MLB/", year, "-", set, "-pitching.shtml",
              sep = "")
  log = read_html(url)
  # First get the commented nodes
  alt_tables = xml2::xml_find_all(log,"//comment()") %>% {
    #Find only commented nodes that contain the regex for html table markup
    raw_parts = as.character(.[grep("\\</?table", as.character(.))])
    # Remove the comment begin and end tags
    strip_html = stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                  vectorize_all = FALSE)
    # Loop through the pieces that have tables within markup and 
    # apply the same functions
    lapply(grep("<table", strip_html, value = TRUE), function(i){
      rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
        .[[1]]
    })
  }
  alt_tables = alt_tables[[1]]
  if(set == "advanced"){
    colnames(alt_tables) = alt_tables[1,]
    alt_tables = alt_tables[-1,]
  }
  alt_tables$Name = str_remove_all(alt_tables$Name, "[*+]")
  return(alt_tables)
}

# function to remove duplicates
rm_dups = function(player, set){
  guy = set[set$Name == player,]
  total_stats = guy[1,]
  team = guy$Tm[nrow(guy)]
  total_stats$Tm = team
  return(total_stats)
}

# 2015
# summary
pitching_15 = pitching_fn(2015, "standard")
cols = c(1,3,6:35)
pitching_15[,cols] = sapply(pitching_15[,cols], as.numeric)
pitching_15$Season = 2015
pitching_15 = lapply(unique(pitching_15$Name), rm_dups, set = pitching_15) %>%
  do.call("rbind", .)
pitching_15 = pitching_15 %>% dplyr::select(-Rk)

# advanced
pitching_adv_15 = pitching_fn(2015, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_15[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_15[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_15[,cols] = sapply(pitching_adv_15[,cols], as.numeric)
pitching_adv_15 = lapply(unique(pitching_adv_15$Name), rm_dups, set = pitching_adv_15) %>%
  do.call("rbind", .)
pitching_15 = pitching_15 %>% dplyr::select(-Rk)

# join
pitching_15 = left_join(pitching_15, pitching_adv_15, by = c("Name", "Age", "Tm"))

# 2016
# summary
pitching_16 = pitching_fn(2016, "standard")
cols = c(1,3,6:35)
pitching_16[,cols] = sapply(pitching_16[,cols], as.numeric)
pitching_16$Season = 2016
pitching_16 = lapply(unique(pitching_16$Name), rm_dups, set = pitching_16) %>%
  do.call("rbind", .)
pitching_16 = pitching_16 %>% dplyr::select(-Rk)

# advanced
pitching_adv_16 = pitching_fn(2016, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_16[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_16[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_16[,cols] = sapply(pitching_adv_16[,cols], as.numeric)
pitching_adv_16 = lapply(unique(pitching_adv_16$Name), rm_dups, set = pitching_adv_16) %>%
  do.call("rbind", .)
pitching_adv_16 = pitching_adv_16 %>% dplyr::select(-Rk)

# join
pitching_16 = left_join(pitching_16, pitching_adv_16, by = c("Name", "Age", "Tm"))

# 2017
# summary
pitching_17 = pitching_fn(2017, "standard")
cols = c(1,3,6:35)
pitching_17[,cols] = sapply(pitching_17[,cols], as.numeric)
pitching_17$Season = 2017
pitching_17 = lapply(unique(pitching_17$Name), rm_dups, set = pitching_17) %>%
  do.call("rbind", .)
pitching_17 = pitching_17 %>% dplyr::select(-Rk)

# advanced
pitching_adv_17 = pitching_fn(2017, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_17[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_17[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_17[,cols] = sapply(pitching_adv_17[,cols], as.numeric)
pitching_adv_17 = lapply(unique(pitching_adv_17$Name), rm_dups, set = pitching_adv_17) %>%
  do.call("rbind", .)
pitching_adv_17 = pitching_adv_17 %>% dplyr::select(-Rk)

# join
pitching_17 = left_join(pitching_17, pitching_adv_17, by = c("Name", "Age", "Tm"))

# 2018
# summary
pitching_18 = pitching_fn(2018, "standard")
cols = c(1,3,6:35)
pitching_18[,cols] = sapply(pitching_18[,cols], as.numeric)
pitching_18$Season = 2018
pitching_18 = lapply(unique(pitching_18$Name), rm_dups, set = pitching_18) %>%
  do.call("rbind", .)
pitching_18 = pitching_18 %>% dplyr::select(-Rk)

# advanced
pitching_adv_18 = pitching_fn(2018, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_18[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_18[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_18[,cols] = sapply(pitching_adv_18[,cols], as.numeric)
pitching_adv_18 = lapply(unique(pitching_adv_18$Name), rm_dups, set = pitching_adv_18) %>%
  do.call("rbind", .)
pitching_adv_18 = pitching_adv_18 %>% dplyr::select(-Rk)

# join
pitching_18 = left_join(pitching_18, pitching_adv_18, by = c("Name", "Age", "Tm"))

# players to include
pitchers_18 = pitching_18[pitching_18$IP/pitching_18$G >= 4,]
pitchers_18 = pitchers_18$Name
pitchers_18 = pitchers_18[!is.na(pitchers_18)]
pitchers_18 = unique(pitchers_18)

# fake space character to be removed
name_char = str_sub(pitchers_18[1],6,6)

# split names into multiple words
pitchers_18 = str_replace_all(pitchers_18, name_char, " ")

# all available game logs
games_18 = sapply(1:length(pitchers_18), function(i) try(pitcher_fn(pitchers_18[i], year = 2018), TRUE))
test = data.frame()
for(i in 1:length(games_18)){
  a = ifelse(length(games_18[[i]]) == 1, 1, 0)
  test = rbind(test, a)
}
test$num = 1:length(games_18)
test = test[test[,1] == 0,]
games_18 = list.remove(games_18, test$num)
pitchers_18 = pitchers_18[-test$num]
for(j in 1:length(games_18)){
  games_18[[j]] = games_18[[j]][[1]]
  games_18[[j]][1] = pitchers_18[j]
}
games_18 = games_18 %>%
  do.call("rbind", .)
games_18 = games_18[games_18$Tm %in% teams,]
colnames(games_18) = c("Pitcher", colnames(games_18)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_18)[8:length(games_18)])
games_18$Season = 2018

# 2019
# summary
pitching_19 = pitching_fn(2019, "standard")
cols = c(1,3,6:35)
pitching_19[,cols] = sapply(pitching_19[,cols], as.numeric)
pitching_19$Season = 2019
pitching_19 = lapply(unique(pitching_19$Name), rm_dups, set = pitching_19) %>%
  do.call("rbind", .)
pitching_19 = pitching_19 %>% dplyr::select(-Rk)

# advanced
pitching_adv_19 = pitching_fn(2019, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_19[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_19[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_19[,cols] = sapply(pitching_adv_19[,cols], as.numeric)
pitching_adv_19 = lapply(unique(pitching_adv_19$Name), rm_dups, set = pitching_adv_19) %>%
  do.call("rbind", .)
pitching_adv_19 = pitching_adv_19 %>% dplyr::select(-Rk)

# join
pitching_19 = left_join(pitching_19, pitching_adv_19, by = c("Name", "Age", "Tm"))

# players to include
pitchers_19 = pitching_19[pitching_19$IP/pitching_19$G >= 4,]
pitchers_19 = pitchers_19$Name
pitchers_19 = pitchers_19[!is.na(pitchers_19)]
pitchers_19 = unique(pitchers_19)

# split names into multiple words
pitchers_19 = str_replace_all(pitchers_19, name_char, " ")

# all available game logs
games_19 = sapply(1:length(pitchers_19), function(i) try(pitcher_fn(pitchers_19[i], year = 2019), TRUE))
test = data.frame()
for(i in 1:length(games_19)){
  a = ifelse(length(games_19[[i]]) == 1, 1, 0)
  test = rbind(test, a)
}
test$num = 1:length(games_19)
test = test[test[,1] == 0,]
games_19 = list.remove(games_19, test$num)
pitchers_19 = pitchers_19[-test$num]
for(j in 1:length(games_19)){
  games_19[[j]] = games_19[[j]][[1]]
  games_19[[j]][1] = pitchers_19[j]
}
games_19 = games_19 %>%
  do.call("rbind", .)
games_19 = games_19[games_19$Tm %in% teams,]
colnames(games_19) = c("Pitcher", colnames(games_19)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_19)[8:length(games_19)])
games_19$Season = 2019

# 2020
# summary
pitching_20 = pitching_fn(2020, "standard")
cols = c(1,3,6:35)
pitching_20[,cols] = sapply(pitching_20[,cols], as.numeric)
pitching_20$Season = 2020
pitching_20 = lapply(unique(pitching_20$Name), rm_dups, set = pitching_20) %>%
  do.call("rbind", .)
pitching_20 = pitching_20 %>% dplyr::select(-Rk)

# advanced
pitching_adv_20 = pitching_fn(2020, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_20[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_20[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_20[,cols] = sapply(pitching_adv_20[,cols], as.numeric)
pitching_adv_20 = lapply(unique(pitching_adv_20$Name), rm_dups, set = pitching_adv_20) %>%
  do.call("rbind", .)
pitching_adv_20 = pitching_adv_20 %>% dplyr::select(-Rk)

# join
pitching_20 = left_join(pitching_20, pitching_adv_20, by = c("Name", "Age", "Tm"))

# players to include
pitchers_20 = pitching_20[pitching_20$IP/pitching_20$G >= 4,]
pitchers_20 = pitchers_20$Name
pitchers_20 = pitchers_20[!is.na(pitchers_20)]
pitchers_20 = unique(pitchers_20)

# split names into multiple words
pitchers_20 = str_replace_all(pitchers_20, name_char, " ")

# all available game logs
games_20 = sapply(1:length(pitchers_20), function(i) try(pitcher_fn(pitchers_20[i], year = 2020), TRUE))
test = data.frame()
for(i in 1:length(games_20)){
  a = ifelse(length(games_20[[i]]) == 1, 1, 0)
  test = rbind(test, a)
}
test$num = 1:length(games_20)
test = test[test[,1] == 0,]
games_20 = list.remove(games_20, test$num)
pitchers_20 = pitchers_20[-test$num]
for(j in 1:length(games_20)){
  games_20[[j]] = games_20[[j]][[1]]
  games_20[[j]][1] = pitchers_20[j]
}
games_20 = games_20 %>%
  do.call("rbind", .)
games_20 = games_20[games_20$Tm %in% teams,]
colnames(games_20) = c("Pitcher", colnames(games_20)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_20)[8:length(games_20)])
games_20$Season = 2020

# 2021
# summary
pitching_21 = pitching_fn(2021, "standard")
cols = c(1,3,6:35)
pitching_21[,cols] = sapply(pitching_21[,cols], as.numeric)
pitching_21$Season = 2021
pitching_21 = lapply(unique(pitching_21$Name), rm_dups, set = pitching_21) %>%
  do.call("rbind", .)
pitching_21 = pitching_21 %>% dplyr::select(-Rk)

# advanced
pitching_adv_21 = pitching_fn(2021, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_21[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_21[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_21[,cols] = sapply(pitching_adv_21[,cols], as.numeric)
pitching_adv_21 = lapply(unique(pitching_adv_21$Name), rm_dups, set = pitching_adv_21) %>%
  do.call("rbind", .)
pitching_adv_21 = pitching_adv_21 %>% dplyr::select(-Rk)

# join
pitching_21 = left_join(pitching_21, pitching_adv_21, by = c("Name", "Age", "Tm"))

# players to include
pitchers_21 = pitching_21[pitching_21$IP/pitching_21$G >= 4,]
pitchers_21 = pitchers_21$Name
pitchers_21 = pitchers_21[!is.na(pitchers_21)]
pitchers_21 = unique(pitchers_21)

# split names into multiple words
pitchers_21 = str_replace_all(pitchers_21, name_char, " ")

# all available game logs
games_21 = sapply(1:length(pitchers_21), function(i) try(pitcher_fn(pitchers_21[i], year = 2021), TRUE))
test = data.frame()
for(i in 1:length(games_21)){
  a = ifelse(length(games_21[[i]]) == 1, 1, 0)
  test = rbind(test, a)
}
test$num = 1:length(games_21)
test = test[test[,1] == 0,]
games_21 = list.remove(games_21, test$num)
pitchers_21 = pitchers_21[-test$num]
for(j in 1:length(games_21)){
  games_21[[j]] = games_21[[j]][[1]]
  games_21[[j]][1] = pitchers_21[j]
}
games_21 = games_21 %>%
  do.call("rbind", .)
games_21 = games_21[games_21$Tm %in% teams,]
colnames(games_21) = c("Pitcher", colnames(games_21)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_21)[8:length(games_21)])
games_21$Season = 2021

# all games
games = bind_rows(games_18, games_19, games_20, games_21)
cols = c(2:3,11:48, 51:54)
games[,cols] = sapply(games[,cols], as.numeric)

# adjust IP
games$IP = ifelse((games$IP - (games$IP %/% 1)) == 0, games$IP,
                          ifelse((games$IP - (games$IP %/% 1)) > 0.15, 
                                 games$IP + 0.47, games$IP + 0.23))

# rolling stats
pitchers_rolling = data.frame()
for(i in seasons[2:length(seasons)]){
  season = games[games$Season == i,]
  for(j in unique(season$Pitcher)){
    pitcher = season[season$Pitcher == j,]
    pitcher = pitcher[!is.na(pitcher$Gtm),]
    min = min(pitcher$Gcar)
    for(k in pitcher$Gtm){
      df = games[games$Season == i & games$Pitcher == j & games$Gtm < k,]
      df = df[!is.na(df$Pitcher),]
      df2 = games[games$Season == i & games$Pitcher == j & games$Gtm == k,]
      df2 = df2[!is.na(df2$Pitcher),]
      df3 = data.frame(i, j, k, df2$Gcar, df2$Date, df2$Team, df2$Opponent, df2$Gcar - min + 1, 
                       sum(df$IP), sum(df$H), sum(df$ER), sum(df$BB), sum(df$SO), sum(df$HR),
                       sum(df$HBP), sum(df$BF), sum(df$GB), sum(df$FB), sum(df$LD), sum(df$PU), 
                       sum(df$AB), sum(df$`2B`), sum(df$`3B`), sum(df$SF), sum(df$WPA))
      pitchers_rolling = bind_rows(pitchers_rolling, df3)
    }
  }
}
colnames(pitchers_rolling) = c("Season", "Pitcher", "Gtm", "Gcar", "Date", "Team", "Opponent", "G", 
                               "IP", "H", "ER", "BB", "SO", "HR", "HBP", "BF", "GB", "FB", "LD", "PU", 
                               "AB", "2B", "3B", "SF", "WPA")
month = str_sub(pitchers_rolling$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
           ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
           ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitchers_rolling$Date, 5, 6)
day = str_remove_all(day, "[()]")
pitchers_rolling$Date = as.Date(paste(pitchers_rolling$Season, "-", month, "-", day, sep = ""))
pitchers_rolling$BA = pitchers_rolling$H/pitchers_rolling$AB
pitchers_rolling$OBP = (pitchers_rolling$H + pitchers_rolling$BB + pitchers_rolling$HBP)/(pitchers_rolling$AB + pitchers_rolling$BB + pitchers_rolling$HBP + pitchers_rolling$SF)
pitchers_rolling$SLG = ((pitchers_rolling$H - pitchers_rolling$`2B` - pitchers_rolling$`3B` - pitchers_rolling$HR) + 2*pitchers_rolling$`2B` + 3*pitchers_rolling$`3B` + 4*pitchers_rolling$HR)/pitchers_rolling$AB
pitchers_rolling$HR_pct = pitchers_rolling$HR/pitchers_rolling$BF
pitchers_rolling$SO_pct = pitchers_rolling$SO/pitchers_rolling$BF
pitchers_rolling$BB_pct = pitchers_rolling$BB/pitchers_rolling$BF
pitchers_rolling$GB_FB = pitchers_rolling$GB/(pitchers_rolling$FB + pitchers_rolling$LD + pitchers_rolling$PU)

# join players' previous and current seasons
pitchers = bind_rows(pitching_17, pitching_18, pitching_19, pitching_20, pitching_21)
pitchers$Season = as.numeric(pitchers$Season) + 1
colnames(pitchers) = c("Pitcher", colnames(pitchers)[2:length(pitchers)])
pitchers$Pitcher = str_replace_all(pitchers$Pitcher, name_char, " ")
pitchers = left_join(pitchers_rolling, pitchers, by = c("Pitcher", "Season"))
pitchers = pitchers %>% dplyr::select(-c(GB, FB, LD, PU, AB, `2B`, `3B`, SF, Age, Tm, Lg, W, L, 
                                         `W-L%`, ERA, GS, GF, CG, SHO, SV, R, IBB, BK, WP, `ERA+`, FIP, 
                                         WHIP, H9, HR9, BB9, SO9, `SO/W`, OPS, BAbip, EV, `HardH%`, `LD%`, 
                                         `GB%`, `FB%`, cWPA, RE24))
colnames(pitchers) = c(colnames(pitchers)[1:2], "Game", colnames(pitchers)[4:7], "current_G", "current_IP", 
                      "current_H", "current_ER", "current_BB", "current_SO", "current_HR", "current_HBP", 
                      "current_BF", "current_WPA", "current_BA", "current_OBP", "current_SLG", 
                      "current_HR_pct", "current_SO_pct", "current_BB_pct", "current_GB_FB", "prev_G", 
                      "prev_IP", "prev_H", "prev_ER", "prev_HR", "prev_BB", "prev_SO", "prev_HBP", 
                      "prev_BF", "prev_BA", "prev_OBP", "prev_SLG", "prev_HR_pct", "prev_SO_pct", 
                      "prev_BB_pct", "prev_GB_FB", "prev_WPA")
pitchers$prev_HR_pct = pitchers$prev_HR_pct/100
pitchers$prev_SO_pct = pitchers$prev_SO_pct/100
pitchers$prev_BB_pct = pitchers$prev_BB_pct/100

# set NaN and NA to 0
for(i in 1:length(pitchers)){
  for(j in 1:nrow(pitchers)){
    pitchers[[i]][[j]] = ifelse(is.nan(pitchers[[i]][[j]]), 0, pitchers[[i]][[j]])
  }
}
pitchers[is.na(pitchers)] = 0

# adjust prev_IP
pitchers$prev_IP = ifelse((pitchers$prev_IP - (pitchers$prev_IP %/% 1)) == 0, pitchers$prev_IP,
                  ifelse((pitchers$prev_IP - (pitchers$prev_IP %/% 1)) > 0.15, 
                         pitchers$prev_IP + 0.47, pitchers$prev_IP + 0.23))

# create pitchers save df for current season
pitchers_save = bind_rows(pitching_17, pitching_18, pitching_19, pitching_20, pitching_21)
pitchers_save$Season = as.numeric(pitchers_save$Season) + 1
colnames(pitchers_save) = c("Pitcher", colnames(pitchers_save)[2:length(pitchers_save)])
pitchers_save$Pitcher = str_replace_all(pitchers_save$Pitcher, name_char, " ")
pitchers_save = pitchers_save %>% dplyr::select(-c(Age, Tm, Lg, W, L, `W-L%`, ERA, GS, GF, CG, SHO, SV, R, 
                                         IBB, BK, WP, `ERA+`, FIP, WHIP, H9, HR9, BB9, SO9, 
                                         `SO/W`, OPS, BAbip, EV, `HardH%`, `LD%`, `GB%`, `FB%`,
                                         cWPA, RE24))
colnames(pitchers_save) = c("Pitcher", "prev_G", "prev_IP", "prev_H", "prev_ER", "prev_HR", "prev_BB", 
                            "prev_SO", "prev_HBP", "prev_BF", "Season", "prev_BA", "prev_OBP", "prev_SLG", 
                            "prev_HR_pct", "prev_SO_pct", "prev_BB_pct", "prev_GB_FB", "prev_WPA")
pitchers_save$prev_HR_pct = pitchers_save$prev_HR_pct/100
pitchers_save$prev_SO_pct = pitchers_save$prev_SO_pct/100
pitchers_save$prev_BB_pct = pitchers_save$prev_BB_pct/100
pitchers_save = pitchers_save[pitchers_save$Pitcher != "Name",]

# set NaN and NA to 0
for(i in 1:length(pitchers_save)){
  for(j in 1:nrow(pitchers_save)){
    pitchers_save[[i]][[j]] = ifelse(is.nan(pitchers_save[[i]][[j]]), 0, pitchers_save[[i]][[j]])
  }
}
pitchers_save[is.na(pitchers_save)] = 0

# adjust prev_IP
pitchers_save$prev_IP = ifelse((pitchers_save$prev_IP - (pitchers_save$prev_IP %/% 1)) == 0, pitchers_save$prev_IP,
                          ifelse((pitchers_save$prev_IP - (pitchers_save$prev_IP %/% 1)) > 0.15, 
                                 pitchers_save$prev_IP + 0.47, pitchers_save$prev_IP + 0.23))

# weighted values for pitchers
pitchers$weight_IP = ifelse(pitchers$current_G == 1, pitchers$prev_IP/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_IP/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_IP/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_IP/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_H = ifelse(pitchers$current_G == 1, pitchers$prev_H/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_H/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_H/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_H/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_ER = ifelse(pitchers$current_G == 1, pitchers$prev_ER/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_ER/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_ER/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_ER/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_BB = ifelse(pitchers$current_G == 1, pitchers$prev_BB/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_BB/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_BB/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_BB/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_SO = ifelse(pitchers$current_G == 1, pitchers$prev_SO/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_SO/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_SO/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_SO/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_HR = ifelse(pitchers$current_G == 1, pitchers$prev_HR/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_HR/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_HR/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_HR/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_HBP = ifelse(pitchers$current_G == 1, pitchers$prev_HBP/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_HBP/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_HBP/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_HBP/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_BF = ifelse(pitchers$current_G == 1, pitchers$prev_BF/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_BF/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_BF/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_BF/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_WPA = ifelse(pitchers$current_G == 1, pitchers$prev_WPA/pitchers$prev_G,
                            ifelse(pitchers$prev_G == 0, (pitchers$current_G-1)*(pitchers$current_WPA/(pitchers$current_G - 1))/(pitchers$current_G - 1), 
                                   (pitchers$prev_G*(pitchers$prev_WPA/pitchers$prev_G) + 
                                      exp((pitchers$current_G - 1)/3)*(pitchers$current_WPA/(pitchers$current_G - 1)))/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_BA = ifelse(pitchers$current_G == 1, pitchers$prev_BA,
                            ifelse(pitchers$prev_G == 0, pitchers$current_BA, 
                                   (pitchers$prev_G*pitchers$prev_BA + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_BA)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_OBP = ifelse(pitchers$current_G == 1, pitchers$prev_OBP,
                            ifelse(pitchers$prev_G == 0, pitchers$current_OBP, 
                                   (pitchers$prev_G*pitchers$prev_OBP + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_OBP)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_SLG = ifelse(pitchers$current_G == 1, pitchers$prev_SLG,
                            ifelse(pitchers$prev_G == 0, pitchers$current_SLG, 
                                   (pitchers$prev_G*pitchers$prev_SLG + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_SLG)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_HR_pct = ifelse(pitchers$current_G == 1, pitchers$prev_HR_pct,
                            ifelse(pitchers$prev_G == 0, pitchers$current_HR_pct, 
                                   (pitchers$prev_G*pitchers$prev_HR_pct + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_HR_pct)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_SO_pct = ifelse(pitchers$current_G == 1, pitchers$prev_SO_pct,
                            ifelse(pitchers$prev_G == 0, pitchers$current_SO_pct, 
                                   (pitchers$prev_G*pitchers$prev_SO_pct + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_SO_pct)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_BB_pct = ifelse(pitchers$current_G == 1, pitchers$prev_BB_pct,
                            ifelse(pitchers$prev_G == 0, pitchers$current_BB_pct, 
                                   (pitchers$prev_G*pitchers$prev_BB_pct + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_BB_pct)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))
pitchers$weight_GB_FB = ifelse(pitchers$current_G == 1, pitchers$prev_GB_FB,
                            ifelse(pitchers$prev_G == 0, pitchers$current_GB_FB, 
                                   (pitchers$prev_G*pitchers$prev_GB_FB + 
                                      exp((pitchers$current_G - 1)/3)*pitchers$current_GB_FB)/
                                     (pitchers$prev_G + exp((pitchers$current_G - 1)/3))))

# remove rows with NaN
pitchers = pitchers %>% filter(!is.nan(weight_SO))

# BIND TOGETHER ALL SECTIONS

main = left_join(pitchers, team_batting, by = c("Pitcher", "Team", "Game", "Season", "Date", "Opponent"))
main = main[!is.na(main$Opp_Game),]

# add stat results
stat_results = games
stat_results$Dec = str_sub(stat_results$Dec, 1, 1)
stat_results$Dec = ifelse(stat_results$Dec == "W", 1, 0)
stat_results$Dec = ifelse(is.na(stat_results$Dec), 0, stat_results$Dec)
stat_results = games %>% dplyr::select(Pitcher, Gcar, IP, SO, Dec, ER, H, BB, HBP)
main = left_join(main, stat_results, by = c("Pitcher", "Gcar"))
main = main[!is.na(main$weight_IP),]

# WRITE NECESSARY FILES TO CSV

write_csv(main, "draftkings_mlb_pitch.csv")
write_csv(pitchers_save, "pitchers_save.csv")
write_csv(batting_save, "batting_save.csv")




