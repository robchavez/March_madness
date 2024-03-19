library(rvest)
library(dplyr)
library(tidyverse)

setwd("~/Google Drive/My Drive/March_madness/March_madness-master/2024")

# Set up loop
all_tournaments_df <- data.frame()
year <- 2023


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!  Manual entry !!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# loop broke for Final Four in 2023. Manual entry.
final_four_bracket <- c(c("UConn", 72),
                        c("Miami (FL)", 59),
                        c("Florida Atlantic", 71),
                        c("San Diego State", 72),
                        c("UConn", 76),
                        c("San Diego State", 59))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Run over seasons
for(year in c(2023)){
  
  # Read season 
  season <- year
  
  URL <- paste("https://www.sports-reference.com/cbb/postseason/",year,"-ncaa.html", sep = "")
  mm <- read_html(URL)
  
  # Load web data into character vector
  team <- mm %>% 
    html_nodes("#bracket div div div a") %>% 
    html_text() %>% 
    as.character()
  

  # Break up teams/scores by division 
  east_bracket <- team[1:60]
  midwest_bracket <- team[62:121]
  south_bracket <- team[123:182]
  west_bracket <- team[184:243]
  final_four_bracket <- final_four_bracket
  
  # Wrangle scores and tournament wins 
  ## East --------------------------------------------------------------------
  ### Round of 64
  round1_east_teams <- east_bracket[seq(1,32,2)]
  round1_east_scores <- as.numeric(east_bracket[seq(2,33,2)])
  round1_east_seeds <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  
  round1_east_df <- data.frame(team1 = round1_east_teams[seq(1,16,2)], 
                               team2 = round1_east_teams[seq(2,17,2)],
                               team1_score = round1_east_scores[seq(1,16,2)],
                               team2_score = round1_east_scores[seq(2,17,2)], stringsAsFactors = FALSE)
  
  round1_east_df$winner <- ifelse(round1_east_df$team1_score > round1_east_df$team2_score, 
                                  round1_east_df$team1, 
                                  round1_east_df$team2)
  
  ### Round of 32
  round2_east_teams <- east_bracket[seq(33,48,2)]
  round2_east_scores <- as.numeric(east_bracket[seq(34,49,2)])
  
  round2_east_df <- data.frame(team1 = round2_east_teams[seq(1,8,2)], 
                               team2 = round2_east_teams[seq(2,9,2)],
                               team1_score = round2_east_scores[seq(1,8,2)],
                               team2_score = round2_east_scores[seq(2,9,2)], stringsAsFactors = FALSE)
  
  round2_east_df$winner <- ifelse(round2_east_df$team1_score > round2_east_df$team2_score, 
                                  round2_east_df$team1, 
                                  round2_east_df$team2)
  
  ### Sweet 16  
  round3_east_teams <- east_bracket[seq(49,55,2)]
  round3_east_scores <- as.numeric(east_bracket[seq(50,56,2)])
  
  round3_east_df <- data.frame(team1 = round3_east_teams[seq(1,4,2)], 
                               team2 = round3_east_teams[seq(2,5,2)],
                               team1_score = round3_east_scores[seq(1,4,2)],
                               team2_score = round3_east_scores[seq(2,5,2)], stringsAsFactors = FALSE)
  
  round3_east_df$winner <- ifelse(round3_east_df$team1_score > round3_east_df$team2_score, 
                                  round3_east_df$team1, 
                                  round3_east_df$team2)
  
  ### Elite 8  
  round4_east_teams <- east_bracket[seq(57,60,2)]
  round4_east_scores <- as.numeric(east_bracket[seq(58,61,2)])
  
  round4_east_df <- data.frame(team1 = round4_east_teams[seq(1,2,2)], 
                               team2 = round4_east_teams[seq(2,3,2)],
                               team1_score = round4_east_scores[seq(1,2,2)],
                               team2_score = round4_east_scores[seq(2,3,2)], stringsAsFactors = FALSE)
  
  round4_east_df$winner <- ifelse(round4_east_df$team1_score > round4_east_df$team2_score, 
                                  round4_east_df$team1, 
                                  round4_east_df$team2)
  
  # Combine rounds
  final_east_df <- rbind(round1_east_df,round2_east_df,round3_east_df,round4_east_df)
  
  # Add seed info
  east_seed_df <- data.frame(teams = round1_east_teams, seeds = round1_east_seeds, stringsAsFactors = FALSE)
  

  
  ## Midwest --------------------------------------------------------------------
  ### Round of 64
  round1_midwest_teams <- midwest_bracket[seq(1,32,2)]
  round1_midwest_scores <- as.numeric(midwest_bracket[seq(2,33,2)])
  round1_midwest_seeds <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  
  round1_midwest_df <- data.frame(team1 = round1_midwest_teams[seq(1,16,2)], 
                                  team2 = round1_midwest_teams[seq(2,17,2)],
                                  team1_score = round1_midwest_scores[seq(1,16,2)],
                                  team2_score = round1_midwest_scores[seq(2,17,2)], stringsAsFactors = FALSE)
  
  round1_midwest_df$winner <- ifelse(round1_midwest_df$team1_score > round1_midwest_df$team2_score, 
                                     round1_midwest_df$team1, 
                                     round1_midwest_df$team2)
  
  ### Round of 32
  round2_midwest_teams <- midwest_bracket[seq(33,48,2)]
  round2_midwest_scores <- as.numeric(midwest_bracket[seq(34,49,2)])
  
  round2_midwest_df <- data.frame(team1 = round2_midwest_teams[seq(1,8,2)], 
                                  team2 = round2_midwest_teams[seq(2,9,2)],
                                  team1_score = round2_midwest_scores[seq(1,8,2)],
                                  team2_score = round2_midwest_scores[seq(2,9,2)], stringsAsFactors = FALSE)
  
  round2_midwest_df$winner <- ifelse(round2_midwest_df$team1_score > round2_midwest_df$team2_score, 
                                     round2_midwest_df$team1, 
                                     round2_midwest_df$team2)
  
  ### Sweet 16  
  round3_midwest_teams <- midwest_bracket[seq(49,55,2)]
  round3_midwest_scores <- as.numeric(midwest_bracket[seq(50,56,2)])
  
  round3_midwest_df <- data.frame(team1 = round3_midwest_teams[seq(1,4,2)], 
                                  team2 = round3_midwest_teams[seq(2,5,2)],
                                  team1_score = round3_midwest_scores[seq(1,4,2)],
                                  team2_score = round3_midwest_scores[seq(2,5,2)], stringsAsFactors = FALSE)
  
  round3_midwest_df$winner <- ifelse(round3_midwest_df$team1_score > round3_midwest_df$team2_score, 
                                     round3_midwest_df$team1, 
                                     round3_midwest_df$team2)
  
  ### Elite 8  
  round4_midwest_teams <- midwest_bracket[seq(57,60,2)]
  round4_midwest_scores <- as.numeric(midwest_bracket[seq(58,61,2)])
  
  round4_midwest_df <- data.frame(team1 = round4_midwest_teams[seq(1,2,2)], 
                                  team2 = round4_midwest_teams[seq(2,3,2)],
                                  team1_score = round4_midwest_scores[seq(1,2,2)],
                                  team2_score = round4_midwest_scores[seq(2,3,2)], stringsAsFactors = FALSE)
  
  round4_midwest_df$winner <- ifelse(round4_midwest_df$team1_score > round4_midwest_df$team2_score, 
                                     round4_midwest_df$team1, 
                                     round4_midwest_df$team2)
  
  # Combine rounds
  final_midwest_df <- rbind(round1_midwest_df,round2_midwest_df,round3_midwest_df,round4_midwest_df)
  
  # Add seed info
  midwest_seed_df <- data.frame(teams = round1_midwest_teams, seeds = round1_midwest_seeds, stringsAsFactors = FALSE)
  

  ## South --------------------------------------------------------------------
  ### Round of 64
  round1_south_teams <- south_bracket[seq(1,32,2)]
  round1_south_scores <- as.numeric(south_bracket[seq(2,33,2)])
  round1_south_seeds <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  
  round1_south_df <- data.frame(team1 = round1_south_teams[seq(1,16,2)], 
                                team2 = round1_south_teams[seq(2,17,2)],
                                team1_score = round1_south_scores[seq(1,16,2)],
                                team2_score = round1_south_scores[seq(2,17,2)], stringsAsFactors = FALSE)
  
  round1_south_df$winner <- ifelse(round1_south_df$team1_score > round1_south_df$team2_score, 
                                   round1_south_df$team1, 
                                   round1_south_df$team2)
  
  ### Round of 32
  round2_south_teams <- south_bracket[seq(33,48,2)]
  round2_south_scores <- as.numeric(south_bracket[seq(34,49,2)])
  
  round2_south_df <- data.frame(team1 = round2_south_teams[seq(1,8,2)], 
                                team2 = round2_south_teams[seq(2,9,2)],
                                team1_score = round2_south_scores[seq(1,8,2)],
                                team2_score = round2_south_scores[seq(2,9,2)], stringsAsFactors = FALSE)
  
  round2_south_df$winner <- ifelse(round2_south_df$team1_score > round2_south_df$team2_score, 
                                   round2_south_df$team1, 
                                   round2_south_df$team2)
  
  ### Sweet 16  
  round3_south_teams <- south_bracket[seq(49,55,2)]
  round3_south_scores <- as.numeric(south_bracket[seq(50,56,2)])
  
  round3_south_df <- data.frame(team1 = round3_south_teams[seq(1,4,2)], 
                                team2 = round3_south_teams[seq(2,5,2)],
                                team1_score = round3_south_scores[seq(1,4,2)],
                                team2_score = round3_south_scores[seq(2,5,2)], stringsAsFactors = FALSE)
  
  round3_south_df$winner <- ifelse(round3_south_df$team1_score > round3_south_df$team2_score, 
                                   round3_south_df$team1, 
                                   round3_south_df$team2)
  
  ### Elite 8  
  round4_south_teams <- south_bracket[seq(57,60,2)]
  round4_south_scores <- as.numeric(south_bracket[seq(58,61,2)])
  
  round4_south_df <- data.frame(team1 = round4_south_teams[seq(1,2,2)], 
                                team2 = round4_south_teams[seq(2,3,2)],
                                team1_score = round4_south_scores[seq(1,2,2)],
                                team2_score = round4_south_scores[seq(2,3,2)], stringsAsFactors = FALSE)
  
  round4_south_df$winner <- ifelse(round4_south_df$team1_score > round4_south_df$team2_score, 
                                   round4_south_df$team1, 
                                   round4_south_df$team2)
  
  # Combine rounds
  final_south_df <- rbind(round1_south_df,round2_south_df,round3_south_df,round4_south_df)
  
  
  # Add seed info
  south_seed_df <- data.frame(teams = round1_south_teams, seeds = round1_south_seeds, stringsAsFactors = FALSE)

  
  ## West --------------------------------------------------------------------
  ### Round of 64
  round1_west_teams <- west_bracket[seq(1,32,2)]
  round1_west_scores <- as.numeric(west_bracket[seq(2,33,2)])
  round1_west_seeds <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  
  round1_west_df <- data.frame(team1 = round1_west_teams[seq(1,16,2)], 
                               team2 = round1_west_teams[seq(2,17,2)],
                               team1_score = round1_west_scores[seq(1,16,2)],
                               team2_score = round1_west_scores[seq(2,17,2)], stringsAsFactors = FALSE)
  
  round1_west_df$winner <- ifelse(round1_west_df$team1_score > round1_west_df$team2_score, 
                                  round1_west_df$team1, 
                                  round1_west_df$team2)
  
  ### Round of 32
  round2_west_teams <- west_bracket[seq(33,48,2)]
  round2_west_scores <- as.numeric(west_bracket[seq(34,49,2)])
  
  round2_west_df <- data.frame(team1 = round2_west_teams[seq(1,8,2)], 
                               team2 = round2_west_teams[seq(2,9,2)],
                               team1_score = round2_west_scores[seq(1,8,2)],
                               team2_score = round2_west_scores[seq(2,9,2)], stringsAsFactors = FALSE)
  
  round2_west_df$winner <- ifelse(round2_west_df$team1_score > round2_west_df$team2_score, 
                                  round2_west_df$team1, 
                                  round2_west_df$team2)
  
  ### Sweet 16  
  round3_west_teams <- west_bracket[seq(49,55,2)]
  round3_west_scores <- as.numeric(west_bracket[seq(50,56,2)])
  
  round3_west_df <- data.frame(team1 = round3_west_teams[seq(1,4,2)], 
                               team2 = round3_west_teams[seq(2,5,2)],
                               team1_score = round3_west_scores[seq(1,4,2)],
                               team2_score = round3_west_scores[seq(2,5,2)], stringsAsFactors = FALSE)
  
  round3_west_df$winner <- ifelse(round3_west_df$team1_score > round3_west_df$team2_score, 
                                  round3_west_df$team1, 
                                  round3_west_df$team2)
  
  ### Elite 8  
  round4_west_teams <- west_bracket[seq(57,60,2)]
  round4_west_scores <- as.numeric(west_bracket[seq(58,61,2)])
  
  round4_west_df <- data.frame(team1 = round4_west_teams[seq(1,2,2)], 
                               team2 = round4_west_teams[seq(2,3,2)],
                               team1_score = round4_west_scores[seq(1,2,2)],
                               team2_score = round4_west_scores[seq(2,3,2)], stringsAsFactors = FALSE)
  
  round4_west_df$winner <- ifelse(round4_west_df$team1_score > round4_west_df$team2_score, 
                                  round4_west_df$team1, 
                                  round4_west_df$team2)
  
  # Combine rounds
  final_west_df <- rbind(round1_west_df,round2_west_df,round3_west_df,round4_west_df)
  
  # Add seed info
  west_seed_df <- data.frame(teams = round1_west_teams, seeds = round1_west_seeds, stringsAsFactors = FALSE)

  
  ## Final Four --------------------------------------------------------------------
  
  finalfour_teams <- final_four_bracket[seq(1,12,2)]
  finalfour_scores <- as.numeric(final_four_bracket[seq(2,12,2)])
  
  finalfour_df <- data.frame(team1 = finalfour_teams[seq(1,6,2)], 
                             team2 = finalfour_teams[seq(2,6,2)],
                             team1_score = finalfour_scores[seq(1,6,2)],
                             team2_score = finalfour_scores[seq(2,6,2)], stringsAsFactors = FALSE)
  
  finalfour_df$winner <- ifelse(finalfour_df$team1_score > finalfour_df$team2_score, 
                                finalfour_df$team1, 
                                finalfour_df$team2)
  
  
  
  
  # Combine whole tournament data --------------------------------------------------
  
  # Merge divisions 
  tournament_df <- rbind(final_east_df, final_midwest_df, final_south_df, final_west_df, finalfour_df)
  tournament_df <- tournament_df %>% select(team1,team2,winner)  
  tournament_df$season <- rep(season,length(tournament_df[,1])) 
  
  # Add seed information
  all_seeds_df <- rbind (east_seed_df,midwest_seed_df,south_seed_df,west_seed_df)

  team1_seed <- vector()
  for(i in 1:length(tournament_df$team1)){
    for(j in 1:length(all_seeds_df$teams)){
      if(tournament_df$team1[i] == all_seeds_df$teams[j]){
        team1_seed[i] <- all_seeds_df$seeds[j]
      }
    }
  }

  team2_seed <- vector()
  for(i in 1:length(tournament_df$team2)){
    for(j in 1:length(all_seeds_df$teams)){
      if(tournament_df$team2[i] == all_seeds_df$teams[j]){
        team2_seed[i] <- all_seeds_df$seeds[j]
      }
    }
  }

  tournament_df$team1_seed <- team1_seed
  tournament_df$team2_seed <- team2_seed

  
  # Build final data frame
  all_tournaments_df <- rbind(all_tournaments_df,tournament_df)  
  
}  


# Update 2024: need to call previous year

df1996_2022 <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2023/all_tournament_wins_1996-2022.csv")
df1996_2022 <- df1996_2022

df_tourn_all <- rbind(df1996_2022, all_tournaments_df)

write_csv(df_tourn_all,"~/Google Drive/My Drive/March_madness/March_madness-master/2024/all_tournament_wins_1996-2023.csv")

