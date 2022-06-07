#### DRAFTKINGS MLB - PITCHING ####

# libraries
library(rvest)
library(tidyverse)
library(data.table)
library(gsubfn)
library(magrittr)
library(rlist)
library(lubridate)
library(coda)
library(rjags)
library(R2jags)
load.module("glm")
library(ggplot2)
library(MASS)
library(randomForest)

# names
teams = c("NYY","TBR","BOS","TOR","BAL","MIN","CLE","CHW","KCR","DET","HOU","OAK","TEX","LAA","SEA",
          "ATL","WSN","NYM","PHI","MIA","STL","MIL","CHC","CIN","PIT","LAD","ARI","SFG","COL","SDP")
b_names = c("Rank","Game","Date","Home","Opponent","Result","PA","AB","R","H","2B","3B","HR","RBI","BB",
            "IBB","SO","HBP","SH","SF","ROE","GDP","SB","CS","BA","OBP","SLG","OPS","LOB","Indiv_Batters",
            "P_Hand","Opp_P","Team")

# function to pull team stats
mlb_fn = function(team, year, set){
  url = paste("https://www.baseball-reference.com/teams/tgl.cgi?team=", team, "&t=", set, "&year=", year, 
              sep = "")
  log = read_html(url) %>% html_nodes("table")
  log = log[[length(log)]] %>% html_table()
  log$Team = team
  return(log)
}

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

# function to pull season results
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


## CURRENT SEASON (START HERE FOR NEW GAMES)

# READ IN NECESSARY FILES

main = read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/dk-mlb-pitching/draftkings_mlb_pitch.csv")
pitchers_save = read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/dk-mlb-pitching/pitchers_save.csv")
batting_save = read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/dk-mlb-pitching/batting_save.csv")
master = main[is.na(main$SO),]

print("Files loaded successfully.")

# YESTERDAY'S RESULTS

date_yest = master$Date[1]
players_yest = as.vector(master$Pitcher)
results = sapply(1:length(players_yest), function(i) try(pitcher_fn(players_yest[i], year = 2022), TRUE))

print("Results")

verify = data.frame()
for(i in 1:length(results)){
  a = ifelse(length(results[[i]]) == 1, 1, 0)
  verify = rbind(verify, a)
}
verify$num = 1:length(results)
verify = verify[verify[,1] == 0,]
results = if(nrow(verify) == 0){results} else{list.remove(results, verify$num)}
players_yest = if(nrow(verify) == 0){players_yest} else{players_yest[-verify$num]}
for(i in 1:length(results)){
  results[[i]] = results[[i]][[1]]
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

# join with master
results = left_join(master, results, by = c("Pitcher", "Date"))

# fill in main file
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
ratings = distinct(ratings)

# correct game num for doubleheaders
game_num_fn_3 = function(tm, data){
  df = data[data$team1 == tm,]
  index = rep(nrow(df) - 1, nrow(df))
  df$team1_game = df$team1_game - index
  df$team2_game = df$team2_game - index
  return(df)
}
ratings = lapply(unique(ratings$team1), game_num_fn_3, data = ratings) %>% do.call("rbind", .)

# duplicate for away team, clean up
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
master = distinct(master)

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

dash = final_preds[order(final_preds$Prediction, decreasing = T),]
write_csv(dash, "Daily Pitching Predictions.csv")

# SAVE MAIN SET WITH PREDICTIONS
main$my_preds[(nrow(main) - nrow(master) + 1):nrow(main)] = dkpt_preds
write_csv(main, 'draftkings_mlb_pitch.csv')


