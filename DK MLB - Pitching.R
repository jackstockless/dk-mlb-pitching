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

## CURRENT SEASON (START HERE FOR NEW GAMES)

# YESTERDAY'S RESULTS

date_yest = Sys.Date() - 1
players_yest = as.vector(master$Pitcher)
results = sapply(1:length(players_yest), function(i) try(pitcher_fn(players_yest[i], year = 2022), TRUE))
verify = data.frame()
for(i in 1:length(results)){
  a = is.data.frame(results[[i]])
  verify = rbind(verify, a)
}
verify$num = 1:length(results)
verify = verify[verify[,1] == FALSE,]
results = if(nrow(verify) == 0){results} else{list.remove(results, verify$num)}
players_yest = if(nrow(verify) == 0){players_yest} else{players_yest[-verify$num]}
for(i in 1:length(results)){
  results[[i]][1] = players_yest[i]
  hold = results[[i]]
  month = str_sub(hold$Date, 1, 3)
  month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                 ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                 ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
  day = str_sub(hold$Date, 5, 6)
  day = str_remove_all(day, "[()]")
  hold$Date = as.Date(paste(2022, "-", month, "-", day, sep = ""))
  hold = hold[hold$Date == date_yest,]
  hold = data.frame(hold$Rk, hold$Date, hold$Dec, hold$IP, hold$H, hold$ER, hold$BB, hold$SO, hold$HBP)
  results[[i]] = hold
}
results = results %>%
  do.call("rbind", .)
colnames(results) = c("Pitcher", "Date", "Dec", "IP", "H", "ER", "BB", "SO", "HBP")
results$Date = as.Date(results$Date)
cols = c(4:9)
results[,cols] = sapply(results[,cols], function(x) as.numeric(as.character(x)))
results = results[!is.na(results$Pitcher),]

# Dec conversion
results$Dec = str_sub(results$Dec, 1, 1)
results$Dec = ifelse(results$Dec == "W", 1, 0)
results$Dec = ifelse(is.na(results$Dec), 0, results$Dec)

# IP conversion
results$IP = ifelse((results$IP - (results$IP %/% 1)) == 0, results$IP,
                               ifelse((results$IP - (results$IP %/% 1)) > 0.15, 
                                      results$IP + 0.47, results$IP + 0.23))


results = left_join(master, results, by = c("Pitcher", "Date"))
main$Dec[(nrow(main) - nrow(master) + 1):nrow(main)] = results$Dec
main$IP[(nrow(main) - nrow(master) + 1):nrow(main)] = results$IP
main$H[(nrow(main) - nrow(master) + 1):nrow(main)] = results$H
main$ER[(nrow(main) - nrow(master) + 1):nrow(main)] = results$ER
main$BB[(nrow(main) - nrow(master) + 1):nrow(main)] = results$BB
main$SO[(nrow(main) - nrow(master) + 1):nrow(main)] = results$SO
main$HBP[(nrow(main) - nrow(master) + 1):nrow(main)] = results$HBP

# after filling in missing values manually
main = main[!is.na(main$SO),]

# ELO

date_today = Sys.Date()
ratings = fread("https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv")
ratings = ratings[ratings$date == date_today,]
ratings = ratings %>% dplyr::select(c(date, season, team1, team2, rating1_pre, rating2_pre, pitcher1,
                                       pitcher2, pitcher1_rgs, pitcher2_rgs))
colnames(ratings) = c("Date", "Season", colnames(ratings)[3:length(ratings)])
ratings$team1 = ifelse(ratings$team1 == "TBD", "TBR", ratings$team1)
ratings$team1 = ifelse(ratings$team1 == "ANA", "LAA", ratings$team1)
ratings$team1 = ifelse(ratings$team1 == "FLA", "MIA", ratings$team1)
ratings$team2 = ifelse(ratings$team2 == "TBD", "TBR", ratings$team2)
ratings$team2 = ifelse(ratings$team2 == "ANA", "LAA", ratings$team2)
ratings$team2 = ifelse(ratings$team2 == "FLA", "MIA", ratings$team2)

# function to get game numbers for each team
game_num_fn_2 = function(entry, data){
  t1 = data$team1[entry]
  t2 = data$team2[entry]
  team1_game = nrow(data[data$team1 == t1,]) + nrow(data[data$team2 == t1,])
  team2_game = nrow(data[data$team1 == t2,]) + nrow(data[data$team2 == t2,])
  games = data.frame(t1, t2, team1_game, team2_game)
  colnames(games) = c("team1", "team2", "team1_game", "team2_game")
  return(games)
}

game_num_22 = fread("https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv")
game_num_22 = as.data.frame(game_num_22)
game_num_22$team1 = ifelse(game_num_22$team1 == "TBD", "TBR", game_num_22$team1)
game_num_22$team1 = ifelse(game_num_22$team1 == "ANA", "LAA", game_num_22$team1)
game_num_22$team1 = ifelse(game_num_22$team1 == "FLA", "MIA", game_num_22$team1)
game_num_22$team2 = ifelse(game_num_22$team2 == "TBD", "TBR", game_num_22$team2)
game_num_22$team2 = ifelse(game_num_22$team2 == "ANA", "LAA", game_num_22$team2)
game_num_22$team2 = ifelse(game_num_22$team2 == "FLA", "MIA", game_num_22$team2)
game_num_22 = game_num_22[game_num_22$date <= date_today,]
game_num_22 = lapply(1:nrow(ratings), game_num_fn_2, data = game_num_22) %>% do.call("rbind", .)
ratings = left_join(ratings, game_num_22, by = c("team1", "team2"))

away_elo = ratings
away_elo$Team = away_elo$team2
away_elo$Opponent = away_elo$team1
ratings$Team = ratings$team1
ratings$Opponent = ratings$team2
ratings = bind_rows(ratings, away_elo)
ratings$rating_diff = ifelse(ratings$Team == ratings$team1, ratings$rating1_pre - ratings$rating2_pre,
                             ratings$rating2_pre - ratings$rating1_pre) 
ratings$Game = ifelse(ratings$Team == ratings$team1, ratings$team1_game, ratings$team2_game)
ratings$Opp_Game = ifelse(ratings$Team == ratings$team1, ratings$team2_game, ratings$team1_game)

# TEAM BATTING STATS

batting_box_22 = lapply(teams, mlb_fn, year = 2022, set = "b") %>%
  do.call("rbind", .)
colnames(batting_box_22) = b_names
batting_box_22 = batting_box_22 %>% dplyr::select(-Rank)
batting_box_22 = batting_box_22[batting_box_22$Opponent != "Opp",]
batting_box_22$Home = ifelse(batting_box_22$Home == "@", "Away", "Home")
cols = c(1,6:29)
batting_box_22[,cols] = sapply(batting_box_22[,cols], as.numeric)
batting_box_22$Season = 2022
month = word(batting_box_22$Date, 1)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
          ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
          ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = word(batting_box_22$Date, 2)
batting_box_22$Date = as.Date(paste(batting_box_22$Season, "-", month, "-", day, sep = ""))

team_stats = data.frame()
for(j in teams){
  df = batting_box_22[batting_box_22$Team == j,]
  games = max(df$Game)
  df = data.frame(games + 1, j, mean(df$PA), mean(df$AB), mean(df$R), mean(df$H), mean(df$`2B`), 
                  mean(df$`3B`), mean(df$HR), mean(df$BB), mean(df$SO), mean(df$HBP), mean(df$SF))
  team_stats = bind_rows(team_stats, df)
}
colnames(team_stats) = c("Opp_Game", "Opponent", "opp_rolling_PA", "opp_rolling_AB", "opp_rolling_R", "opp_rolling_H",
                         "opp_rolling_2B", "opp_rolling_3B", "opp_rolling_HR", "opp_rolling_BB", "opp_rolling_SO",
                         "opp_rolling_HBP", "opp_rolling_SF")

# match everything up
team_batting_22 = left_join(ratings, team_stats, by = c("Opponent", "Opp_Game"))
team_batting_22$Season = as.numeric(team_batting_22$Season)
team_batting_22 = team_batting_22 %>% dplyr::select(-c(team1_game, team2_game))
team_batting_22$Pitcher = ifelse(team_batting_22$Team == team_batting_22$team1, team_batting_22$pitcher1, 
                              team_batting_22$pitcher2)
team_batting_22$RGS = ifelse(team_batting_22$Team == team_batting_22$team1, team_batting_22$pitcher1_rgs, 
                              team_batting_22$pitcher2_rgs)

# PITCHER STATS

# 2022
# summary
pitching_22 = pitching_fn(2022, "standard")
cols = c(1,3,6:35)
pitching_22[,cols] = sapply(pitching_22[,cols], as.numeric)
pitching_22$Season = 2022
pitching_22 = lapply(unique(pitching_22$Name), rm_dups, set = pitching_22) %>%
  do.call("rbind", .)
pitching_22 = pitching_22 %>% dplyr::select(-Rk)

# advanced
pitching_adv_22 = pitching_fn(2022, "advanced")
cols = c(10:12,14:17,20)
pitching_adv_22[,cols] = sapply(cols, function(x) str_remove_all(pitching_adv_22[,x], "[%]"))
cols = c(1,3,5:21)
pitching_adv_22[,cols] = sapply(pitching_adv_22[,cols], as.numeric)
pitching_adv_22 = lapply(unique(pitching_adv_22$Name), rm_dups, set = pitching_adv_22) %>%
  do.call("rbind", .)
pitching_adv_22 = pitching_adv_22 %>% dplyr::select(-Rk)

# join
pitching_22 = left_join(pitching_22, pitching_adv_22, by = c("Name", "Age", "Tm"))

# pare down
pitching_22 = pitching_22 %>% dplyr::select(c(Name, G, IP, H, ER, BB, SO, HR, HBP, BF, WPA, BA, OBP, SLG,
                                              `HR%`, `SO%`, `BB%`, `GB/FB`, Season))
colnames(pitching_22) = c("Pitcher", "current_G", "current_IP", "current_H", "current_ER", "current_BB", 
                          "current_SO", "current_HR", "current_HBP", "current_BF", "current_WPA",
                          "current_BA", "current_OBP", "current_SLG", "current_HR_pct", "current_SO_pct",
                          "current_BB_pct", "current_GB_FB", "Season")
pitching_22$current_HR_pct = pitching_22$current_HR_pct/100
pitching_22$current_SO_pct = pitching_22$current_SO_pct/100
pitching_22$current_BB_pct = pitching_22$current_BB_pct/100
pitching_22$Pitcher = str_replace_all(pitching_22$Pitcher, name_char, " ")

# adjust current_IP
pitching_22$current_IP = ifelse((pitching_22$current_IP - (pitching_22$current_IP %/% 1)) == 0, 
                                pitching_22$current_IP, 
                                ifelse((pitching_22$current_IP - (pitching_22$current_IP %/% 1)) > 0.15, 
                                       pitching_22$current_IP + 0.47, pitching_22$current_IP + 0.23))

# BIND ALL SECTIONS TOGETHER; ATTACH TO MAIN SHEET

master = left_join(team_batting_22, pitching_22, by = c("Pitcher", "Season"))
master = master[!is.na(master$rating_diff),]
colnames(master) = c(colnames(master)[1], "Season", colnames(master)[3:length(master)])

# join teams' previous and current seasons
master = left_join(master, batting_save, by = c("Opponent", "Season"))

# join players' previous and current seasons
master = left_join(master, pitchers_save, by = c("Pitcher", "Season"))

# set NaN and NA to 0
for(i in 1:length(master)){
  for(j in 1:nrow(master)){
    master[[i]][[j]] = ifelse(is.nan(master[[i]][[j]]), 0, master[[i]][[j]])
  }
}
master$pitcher1 = ifelse(is.na(master$pitcher1), "Blank", master$pitcher1)
master$pitcher2 = ifelse(is.na(master$pitcher2), "Blank", master$pitcher2)
master = master[!is.na(master$Pitcher),]
master[is.na(master)] = 0

# adjust current_GS
master$current_G = master$current_G + 1

# weighted values for teams
master$opp_weight_PA = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_PA + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_PA)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_PA + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_PA)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_AB = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_AB + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_AB)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_AB + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_AB)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_R = ifelse(master$Season == 2021,
                                   (60*master$opp_prev_R + exp((master$Opp_Game - 1)/3)*
                                      master$opp_rolling_R)/(60 + exp((master$Opp_Game - 1)/3)),
                                   (162*master$opp_prev_R + exp((master$Opp_Game - 1)/3)*
                                      master$opp_rolling_R)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_H = ifelse(master$Season == 2021,
                                   (60*master$opp_prev_H + exp((master$Opp_Game - 1)/3)*
                                      master$opp_rolling_H)/(60 + exp((master$Opp_Game - 1)/3)),
                                   (162*master$opp_prev_H + exp((master$Opp_Game - 1)/3)*
                                      master$opp_rolling_H)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_2B = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_2B + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_2B)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_2B + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_2B)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_3B = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_3B + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_3B)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_3B + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_3B)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_HR = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_HR + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_HR)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_HR + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_HR)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_BB = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_BB + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_BB)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_BB + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_BB)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_SO = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_SO + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_SO)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_SO + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_SO)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_HBP = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_HBP + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_HBP)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_HBP + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_HBP)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_SF = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_SF + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_SF)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_SF + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_SF)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_BA = master$opp_weight_H/master$opp_weight_AB
master$opp_weight_OBP = (master$opp_weight_H + master$opp_weight_BB + master$opp_weight_HBP)/(master$opp_weight_AB + master$opp_weight_BB + master$opp_weight_HBP + master$opp_weight_SF)
master$opp_weight_SLG = ((master$opp_weight_H - master$opp_weight_2B + master$opp_weight_3B + master$opp_weight_HR) + 2*master$opp_weight_2B + 3*master$opp_weight_3B + 4*master$opp_weight_HR)/master$opp_weight_AB
master$opp_weight_HR_pct = master$opp_weight_HR/master$opp_weight_PA
master$opp_weight_SO_pct = master$opp_weight_SO/master$opp_weight_PA
master$opp_weight_BB_pct = master$opp_weight_BB/master$opp_weight_PA

# weighted values for pitchers
master$weight_IP = ifelse(master$current_G == 1, master$prev_IP/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_IP/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_IP/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_IP/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_H = ifelse(master$current_G == 1, master$prev_H/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_H/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_H/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_H/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_ER = ifelse(master$current_G == 1, master$prev_ER/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_ER/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_ER/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_ER/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BB = ifelse(master$current_G == 1, master$prev_BB/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_BB/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_BB/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_BB/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SO = ifelse(master$current_G == 1, master$prev_SO/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_SO/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_SO/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_SO/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_HR = ifelse(master$current_G == 1, master$prev_HR/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_HR/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_HR/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_HR/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_HBP = ifelse(master$current_G == 1, master$prev_HBP/master$prev_G,
                             ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_HBP/(master$current_G - 1))/(master$current_G - 1), 
                                    (master$prev_G*(master$prev_HBP/master$prev_G) + 
                                       exp((master$current_G - 1)/3)*(master$current_HBP/(master$current_G - 1)))/
                                      (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BF = ifelse(master$current_G == 1, master$prev_BF/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_BF/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_BF/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_BF/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_WPA = ifelse(master$current_G == 1, master$prev_WPA/master$prev_G,
                             ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_WPA/(master$current_G - 1))/(master$current_G - 1), 
                                    (master$prev_G*(master$prev_WPA/master$prev_G) + 
                                       exp((master$current_G - 1)/3)*(master$current_WPA/(master$current_G - 1)))/
                                      (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BA = ifelse(master$current_G == 1, master$prev_BA,
                            ifelse(master$prev_G == 0, master$current_BA, 
                                   (master$prev_G*master$prev_BA + 
                                      exp((master$current_G - 1)/3)*master$current_BA)/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_OBP = ifelse(master$current_G == 1, master$prev_OBP,
                             ifelse(master$prev_G == 0, master$current_OBP, 
                                    (master$prev_G*master$prev_OBP + 
                                       exp((master$current_G - 1)/3)*master$current_OBP)/
                                      (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SLG = ifelse(master$current_G == 1, master$prev_SLG,
                             ifelse(master$prev_G == 0, master$current_SLG, 
                                    (master$prev_G*master$prev_SLG + 
                                       exp((master$current_G - 1)/3)*master$current_SLG)/
                                      (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_HR_pct = ifelse(master$current_G == 1, master$prev_HR_pct,
                                ifelse(master$prev_G == 0, master$current_HR_pct, 
                                       (master$prev_G*master$prev_HR_pct + 
                                          exp((master$current_G - 1)/3)*master$current_HR_pct)/
                                         (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SO_pct = ifelse(master$current_G == 1, master$prev_SO_pct,
                                ifelse(master$prev_G == 0, master$current_SO_pct, 
                                       (master$prev_G*master$prev_SO_pct + 
                                          exp((master$current_G - 1)/3)*master$current_SO_pct)/
                                         (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BB_pct = ifelse(master$current_G == 1, master$prev_BB_pct,
                                ifelse(master$prev_G == 0, master$current_BB_pct, 
                                       (master$prev_G*master$prev_BB_pct + 
                                          exp((master$current_G - 1)/3)*master$current_BB_pct)/
                                         (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_GB_FB = ifelse(master$current_G == 1, master$prev_GB_FB,
                               ifelse(master$prev_G == 0, master$current_GB_FB, 
                                      (master$prev_G*master$prev_GB_FB + 
                                         exp((master$current_G - 1)/3)*master$current_GB_FB)/
                                        (master$prev_G + exp((master$current_G - 1)/3))))

# remove brand new pitchers
master = master[!is.na(master$weight_IP),]

# numberFire IP
get_nF = function(){
  url = "https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections/pitchers"
  log = read_html(url) %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  nF = log[[4]]
  colnames(nF) = nF[1,]
  nF = nF[-1,]
  nF = nF %>% dplyr::select(-Salary)
  cols = c(2:length(nF))
  nF[,cols] = sapply(nF[,cols], as.numeric)
  return(nF)
}
number_fire = get_nF()
nF_players = str_split(number_fire$Player, "\n") %>% do.call("rbind", .)
nF_players = trimws(nF_players[,3])
number_fire$Player = nF_players

nF_IP = number_fire %>% dplyr::select(c(Player, IP)) %>% transmute("Pitcher" = Player, "nF_IP" = IP)
ips = master %>% dplyr::select(c(Pitcher, weight_IP))
ips = left_join(nF_IP, ips, by = "Pitcher")
nF_IP = nF_IP %>% filter(!is.na(nF_IP))

# pare down master, get rid of incompatible entries
master = left_join(master, nF_IP, by = "Pitcher")
master$Date = as.Date(master$Date)
master = master %>% filter(!is.na(master$Date))

## COMBINE MAIN AND MASTER

main = bind_rows(main, master)


#### PREDICTIONS ####

# dependent variables
IP = main$IP
Outs = ifelse((main$IP - (main$IP %/% 1)) == 0, 
              main$IP*3, 
              ifelse((main$IP - (main$IP %/% 1)) > 0.5, 
                     floor(main$IP)*3 + 2, 
                     floor(main$IP)*3 + 1))
SO = main$SO
Dec = main$Dec
ER = main$ER
H = main$H
BB = main$BB
HBP = main$HBP

# independent variables
rating_diff = as.vector(scale(main$rating_diff, center = T))
RGS = as.vector(scale(main$RGS, center = T))
weight_IP = as.vector(scale(main$weight_IP, center = T))
weight_WPA = as.vector(scale(main$weight_WPA, center = T))
weight_BA = as.vector(scale(main$weight_BA, center = T))
weight_OBP = as.vector(scale(main$weight_OBP, center = T))
weight_SLG = as.vector(scale(main$weight_SLG, center = T))
weight_SO_pct = as.vector(scale(main$weight_SO_pct, center = T))
weight_BB_pct = as.vector(scale(main$weight_BB_pct, center = T))
weight_HBP = as.vector(scale(main$weight_HBP, center = T))
opp_weight_BA = as.vector(scale(main$opp_weight_BA, center = T))
opp_weight_OBP = as.vector(scale(main$opp_weight_OBP, center = T))
opp_weight_SLG = as.vector(scale(main$opp_weight_SLG, center = T))
opp_weight_SO_pct = as.vector(scale(main$opp_weight_SO_pct, center = T))
opp_weight_BB_pct = as.vector(scale(main$opp_weight_BB_pct, center = T))
opp_weight_HBP = as.vector(scale(main$opp_weight_HBP, center = T))
n = nrow(main)

# IP
ip_mod = "model{

  # Likelihood
  for(i in 1:n){
    Outs[i] ~ dnorm(mu[i], tau.sq)T(0,27)
    
    mu[i] <- beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_IP[i] + 
    beta[5]*opp_weight_SLG[i]
  }

  # Priors
  for(j in 1:5){
    beta[j] ~ dnorm(0, 0.0001)
  }
  tau.sq = 1/sigma^2
  sigma ~ dunif(0, 100)

}"

vars = list(Outs = Outs, rating_diff = rating_diff, RGS = RGS, weight_IP = weight_IP,
            opp_weight_SLG = opp_weight_SLG, n = n)

jags_ip = jags.model(textConnection(ip_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_ip = coda.samples(jags_ip, "mu", n.iter = 1000, thin = 10, n.burnin = 100)

ip = as.data.frame(mcmc_out_ip[[1]])
ip = ip[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(ip) = master$Pitcher
ip_preds = sapply(ip, mean)

# SO
so_mod = "model{

  # Likelihood
  for(i in 1:n){
    SO[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_IP[i] + 
    beta[5]*weight_SO_pct[i] + beta[6]*opp_weight_SO_pct[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(SO = SO, rating_diff = rating_diff, RGS = RGS, weight_IP = weight_IP, 
            weight_SO_pct = weight_SO_pct, opp_weight_SO_pct = opp_weight_SO_pct, n = n)

jags_so = jags.model(textConnection(so_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_so = coda.samples(jags_so, "SO", n.iter = 1000, thin = 10, n.burnin = 100)

so = as.data.frame(mcmc_out_so[[1]])
so = so[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(so) = master$Pitcher
so_preds = sapply(so, mean)

# Dec
dec_mod = "model{

  # Likelihood
  for(i in 1:n){
    Dec[i] ~ dbern(p[i])
    
    logit(p[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_IP[i] +
    beta[5]*weight_WPA[i]
  }

  # Priors
  for(j in 1:5){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(Dec = Dec, rating_diff = rating_diff, RGS = RGS, weight_IP = weight_IP,
            weight_WPA = weight_WPA, n = n)

jags_dec = jags.model(textConnection(dec_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_dec = coda.samples(jags_dec, "Dec", n.iter = 1000, thin = 10, n.burnin = 100)

dec = as.data.frame(mcmc_out_dec[[1]])
dec = dec[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(dec) = master$Pitcher
dec_preds = sapply(dec, mean)

# ER
er_mod = "model{

  # Likelihood
  for(i in 1:n){
    ER[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_IP[i] +
    beta[5]*weight_SO_pct[i] + beta[6]*opp_weight_SLG[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(ER = ER, rating_diff = rating_diff, RGS = RGS, weight_IP = weight_IP,
            weight_SO_pct = weight_SO_pct, opp_weight_SLG = opp_weight_SLG, n = n)

jags_er = jags.model(textConnection(er_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_er = coda.samples(jags_er, "ER", n.iter = 1000, thin = 10, n.burnin = 100)

er = as.data.frame(mcmc_out_er[[1]])
er = er[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(er) = master$Pitcher
er_preds = sapply(er, mean)

# H
h_mod = "model{

  # Likelihood
  for(i in 1:n){
    H[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_IP[i] + 
    beta[5]*weight_BA[i] + beta[6]*weight_SO_pct[i] + beta[7]*opp_weight_BA[i] +
    beta[8]*opp_weight_SO_pct[i]
  }

  # Priors
  for(j in 1:8){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(H = H, rating_diff = rating_diff, RGS = RGS, weight_IP = weight_IP, weight_BA = weight_BA,
            weight_SO_pct = weight_SO_pct, opp_weight_BA = opp_weight_BA, 
            opp_weight_SO_pct = opp_weight_SO_pct, n = n)

jags_h = jags.model(textConnection(h_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_h = coda.samples(jags_h, "H", n.iter = 1000, thin = 10, n.burnin = 100)

h = as.data.frame(mcmc_out_h[[1]])
h = h[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(h) = master$Pitcher
h_preds = sapply(h, mean)

# BB
bb_mod = "model{

  # Likelihood
  for(i in 1:n){
    BB[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_IP[i] + 
    beta[5]*weight_OBP[i] + beta[6]*weight_BB_pct[i] + beta[7]*opp_weight_OBP[i] +
    beta[8]*opp_weight_BB_pct[i]
  }

  # Priors
  for(j in 1:8){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(BB = BB, rating_diff = rating_diff, RGS = RGS, weight_IP = weight_IP, weight_OBP = weight_OBP,
            weight_BB_pct = weight_BB_pct, opp_weight_OBP = opp_weight_OBP,
            opp_weight_BB_pct = opp_weight_BB_pct, n = n)

jags_bb = jags.model(textConnection(bb_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_bb = coda.samples(jags_bb, "BB", n.iter = 1000, thin = 10, n.burnin = 100)

bb = as.data.frame(mcmc_out_bb[[1]])
bb = bb[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(bb) = master$Pitcher
bb_preds = sapply(bb, mean)

# HBP
hbp_mod = "model{

  # Likelihood
  for(i in 1:n){
    HBP[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*weight_HBP[i] + beta[3]*weight_OBP[i] + 
    beta[4]*opp_weight_HBP[i] + beta[5]*opp_weight_OBP[i]
  }

  # Priors
  for(j in 1:5){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(HBP = HBP, weight_HBP = weight_HBP, weight_OBP = weight_OBP, opp_weight_HBP = opp_weight_HBP,
            opp_weight_OBP = opp_weight_OBP, n = n)

jags_hbp = jags.model(textConnection(hbp_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_hbp = coda.samples(jags_hbp, "HBP", n.iter = 1000, thin = 10, n.burnin = 100)

hbp = as.data.frame(mcmc_out_hbp[[1]])
hbp = hbp[,(nrow(main) - nrow(master) + 1):nrow(main)]
colnames(hbp) = master$Pitcher
hbp_preds = sapply(hbp, mean)

# CG
cg = ifelse(ip == 27, 1, 0)
cg_preds = sapply(as.data.frame(cg), mean)

# CG SO
cgso = ifelse(er == 0, 1, 0)
cgso = cgso %*% diag(cg_preds)

# No Hitter
nh = ifelse(h == 0, 1, 0)
nh = nh %*% diag(cg_preds)

# DRAFTKINGS POINTS
dkpt = data.frame(0.75*ip + 2*so + 4*dec - 2*er - 0.6*h - 0.6*bb - 0.6*hbp + 2.5*cg + 2.5*cgso + 5*nh)
dkpt_preds = sapply(dkpt, mean)
final_preds = data.frame("Name" = master$Pitcher, "Prediction" = dkpt_preds)
View(final_preds[order(final_preds$Prediction, decreasing = T),])

# SAVE MAIN SET WITH PREDICTIONS
main$my_preds[(nrow(main) - nrow(master) + 1):nrow(main)] = dkpt_preds
write_csv(main, 'draftkings_mlb_pitch.csv')

# DRAFTKINGS DASHBOARD
prices = read_csv("~/Downloads/DKSalaries.csv")
prices = prices %>% dplyr::select(c(Position, Name, Salary, TeamAbbrev, AvgPointsPerGame, `Roster Position`))

dashboard = left_join(final_preds, prices, by = "Name")
dashboard = dashboard[!is.na(dashboard$Position),]
dashboard$Prediction = ifelse(dashboard$`Roster Position` == "CPT", dashboard$Prediction*1.5,
                              dashboard$Prediction)
dashboard$`Dollars per Point` = dashboard$Salary/dashboard$Prediction
dashboard$`Prediction Difference` = dashboard$Prediction - dashboard$AvgPointsPerGame
dashboard = dashboard[order(dashboard$`Dollars per Point`),]
rownames(dashboard) = 1:nrow(dashboard)
View(dashboard)


#### MISCELLANEOUS ####

## MODEL DEVELOPMENT

# done
summary(lm(IP ~ weight_IP + rating_diff + RGS + opp_weight_SLG, data = main))

# done
summary(glm(SO ~ rating_diff + RGS + opp_weight_SO_pct +  weight_SO_pct, 
            data = main, family = poisson(link = "log")))

# done
summary(glm(Dec ~ rating_diff + RGS, data = main, family = binomial(link = "logit")))

# done
summary(glm(ER ~ rating_diff + RGS + weight_ER + opp_weight_R, data = main, family = poisson(link = "log")))

# done
summary(glm(H ~ rating_diff + RGS + weight_BA + opp_weight_BA, data = main, 
            family = poisson(link = "log")))

# done
summary(glm(BB ~ rating_diff + RGS + weight_BB_pct + opp_weight_BB_pct,
            data = main, family = poisson(link = "log")))

# done
summary(glm(HBP ~ rating_diff + RGS + weight_HBP + opp_weight_HBP, 
            data = main, family = poisson(link = "log")))


## GCAR AND NUMBERFIRE

# Gcar
gcar = sapply(1:length(master$Pitcher), function(i) try(pitcher_fn(master$Pitcher[i], year = 2022), TRUE))
test = data.frame()
for(i in 1:length(gcar)){
  a = ifelse(length(gcar[[i]]) == 1, 1, 0)
  test = rbind(test, a)
}
test$num = 1:length(gcar)
test = test[test[,1] == 0,]
gcar = list.remove(gcar, test$num)
pitchers_list = master$Pitcher[-test$num]
for(j in 1:length(gcar)){
  gcar[[j]] = gcar[[j]][[1]]
  gcar[[j]][1] = pitchers_list[j]
}
gcar = gcar %>%
  do.call("rbind", .)
gcar = gcar[gcar$Tm %in% teams,]
colnames(gcar) = c("Pitcher", colnames(gcar)[2:4], "Team", "Home", "Opponent", 
                   colnames(gcar)[8:length(gcar)])
gcar = gcar %>% dplyr::select(c(Pitcher, Gcar))
gcar = gcar[gcar$Gcar != "",]
gcar = gcar[!is.na(gcar$Pitcher),]
gcar$Gcar = as.numeric(gcar$Gcar)
gcar_fn = function(pitcher){
  df = gcar[gcar$Pitcher == pitcher,]
  max = max(df$Gcar)
  return(data.frame(pitcher, max))
}
gcar = lapply(unique(gcar$Pitcher), gcar_fn) %>%
  do.call("rbind", .)
colnames(gcar) = c("Pitcher", "Gcar")
gcar$Gcar = gcar$Gcar + 1

# make sure to fill in NAs
master = left_join(master, gcar, by = "Pitcher")
View(data.frame(master$Pitcher, master$Gcar))








