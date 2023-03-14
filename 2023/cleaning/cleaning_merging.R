library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(stringr)

# load season data

rawdf <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2023/all_regseason_cleaned_1996-2023.csv")

text_clean_school <- rawdf$School %>% str_remove(" NCAA") 
text_clean_school <- text_clean_school %>% str_remove("�NCAA")
rawdf$School <- text_clean_school

nonadf <- rawdf %>% select(-SchoolAdvancedPace, 
                 -SchoolAdvancedORtg, 
                 -HomeW, 
                 -HomeL, 
                 -AwayW, 
                 -AwayL, 
                 -SchoolAdvancedORB_percent, 
                 -SchoolAdvancedSTL_percent, 
                 -`School TotalsORB`,
                 -`School TotalsMP`,
                 -`School TotalsPF`,
                 -`School TotalsTOV`,
                 -SchoolAdvancedTOV_percent)


# load tournament data
tourn <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2023/all_tournament_wins_1996-2022.csv")


# rename teams
new_team1 <- tourn$team1
new_team1 <- ifelse(new_team1 == "BYU"	,	"Brigham Young", new_team1)
new_team1 <- ifelse(new_team1 == "Central Connecticut"	,	"Central Connecticut State", new_team1)
new_team1 <- ifelse(new_team1 == "Detroit"	,	"Detroit Mercy", new_team1)
new_team1 <- ifelse(new_team1 == "ETSU"	,	"East Tennessee State", new_team1)
new_team1 <- ifelse(new_team1 == "LIU-Brooklyn"	,	"Long Island University", new_team1)
new_team1 <- ifelse(new_team1 == "LIU"	,	"Long Island University", new_team1)
new_team1 <- ifelse(new_team1 == "LSU"	,	"Louisiana State", new_team1)
new_team1 <- ifelse(new_team1 == "Ole Miss"	,	"Mississippi", new_team1)
new_team1 <- ifelse(new_team1 == "Penn"	,	"Pennsylvania", new_team1)
new_team1 <- ifelse(new_team1 == "Pitt"	,	"Pittsburgh", new_team1)
new_team1 <- ifelse(new_team1 == "SMU"	,	"Southern Methodist", new_team1)
new_team1 <- ifelse(new_team1 == "Southern Miss"	,	"Southern Mississippi", new_team1)
new_team1 <- ifelse(new_team1 == "St. Joseph's"	,	"Saint Joseph's", new_team1)
new_team1 <- ifelse(new_team1 == "St. Peter's"	,	"Saint Peter's", new_team1)
new_team1 <- ifelse(new_team1 == "UCF"	,	"Central Florida", new_team1)
new_team1 <- ifelse(new_team1 == "UConn"	,	"Connecticut", new_team1)
new_team1 <- ifelse(new_team1 == "UCSB"	,	"UC-Santa Barbara", new_team1)
new_team1 <- ifelse(new_team1 == "UIC"	,	"Illinois-Chicago", new_team1)
new_team1 <- ifelse(new_team1 == "UMass"	,	"Massachusetts", new_team1)
new_team1 <- ifelse(new_team1 == "UMBC"	,	"Maryland-Baltimore County", new_team1)
new_team1 <- ifelse(new_team1 == "UNC"	,	"North Carolina", new_team1)
new_team1 <- ifelse(new_team1 == "UNLV"	,	"Nevada-Las Vegas", new_team1)
new_team1 <- ifelse(new_team1 == "USC"	,	"Southern California", new_team1)
new_team1 <- ifelse(new_team1 == "UTEP"	,	"Texas-El Paso", new_team1)
new_team1 <- ifelse(new_team1 == "UTSA"	,	"Texas-San Antonio", new_team1)
new_team1 <- ifelse(new_team1 == "VCU"	,	"Virginia Commonwealth", new_team1)
new_team1 <- ifelse(new_team1 == "California"	,	"University of California", new_team1)
new_team1 <- ifelse(new_team1 == "UNC Greensboro"	,	"North Carolina-Greensboro", new_team1)
new_team1 <- ifelse(new_team1 == "Saint Mary's"	,	"Saint Mary's (CA)", new_team1)
new_team1 <- ifelse(new_team1 == "TCU",	"Texas Christian", new_team1)
new_team1 <- ifelse(new_team1 == "UNC Wilmington"	,	"North Carolina-Wilmington", new_team1)
new_team1 <- ifelse(new_team1 == "NC State", "North Carolina State", new_team1)
new_team1 <- ifelse(new_team1 == "UNC Asheville",	"North Carolina-Asheville", new_team1)
new_team1 <- ifelse(new_team1 == "Little Rock"	,	"Arkansas-Little Rock", new_team1)
new_team1 <- ifelse(new_team1 == "Little Rock"	,	"Arkansas-Little Rock", new_team1)

new_team2 <- tourn$team2
new_team2 <- ifelse(new_team2 == "BYU"	,	"Brigham Young", new_team2)
new_team2 <- ifelse(new_team2 == "Central Connecticut"	,	"Central Connecticut State", new_team2)
new_team2 <- ifelse(new_team2 == "Detroit"	,	"Detroit Mercy", new_team2)
new_team2 <- ifelse(new_team2 == "ETSU"	,	"East Tennessee State", new_team2)
new_team2 <- ifelse(new_team2 == "LIU-Brooklyn"	,	"Long Island University", new_team2)
new_team2 <- ifelse(new_team2 == "LIU"	,	"Long Island University", new_team2)
new_team2 <- ifelse(new_team2 == "LSU"	,	"Louisiana State", new_team2)
new_team2 <- ifelse(new_team2 == "Ole Miss"	,	"Mississippi", new_team2)
new_team2 <- ifelse(new_team2 == "Penn"	,	"Pennsylvania", new_team2)
new_team2 <- ifelse(new_team2 == "Pitt"	,	"Pittsburgh", new_team2)
new_team2 <- ifelse(new_team2 == "SMU"	,	"Southern Methodist", new_team2)
new_team2 <- ifelse(new_team2 == "Southern Miss"	,	"Southern Mississippi", new_team2)
new_team2 <- ifelse(new_team2 == "St. Joseph's"	,	"Saint Joseph's", new_team2)
new_team2 <- ifelse(new_team2 == "St. Peter's"	,	"Saint Peter's", new_team2)
new_team2 <- ifelse(new_team2 == "UCF"	,	"Central Florida", new_team2)
new_team2 <- ifelse(new_team2 == "UConn"	,	"Connecticut", new_team2)
new_team2 <- ifelse(new_team2 == "UCSB"	,	"UC-Santa Barbara", new_team2)
new_team2 <- ifelse(new_team2 == "UIC"	,	"Illinois-Chicago", new_team2)
new_team2 <- ifelse(new_team2 == "UMass"	,	"Massachusetts", new_team2)
new_team2 <- ifelse(new_team2 == "UMBC"	,	"Maryland-Baltimore County", new_team2)
new_team2 <- ifelse(new_team2 == "UNC"	,	"North Carolina", new_team2)
new_team2 <- ifelse(new_team2 == "UNLV"	,	"Nevada-Las Vegas", new_team2)
new_team2 <- ifelse(new_team2 == "USC"	,	"Southern California", new_team2)
new_team2 <- ifelse(new_team2 == "UTEP"	,	"Texas-El Paso", new_team2)
new_team2 <- ifelse(new_team2 == "UTSA"	,	"Texas-San Antonio", new_team2)
new_team2 <- ifelse(new_team2 == "VCU"	,	"Virginia Commonwealth", new_team2)
new_team2 <- ifelse(new_team2 == "California"	,	"University of California", new_team2)
new_team2 <- ifelse(new_team2 == "UNC Greensboro"	,	"North Carolina-Greensboro", new_team2)
new_team2 <- ifelse(new_team2 == "Saint Mary's"	,	"Saint Mary's (CA)", new_team2)
new_team2 <- ifelse(new_team2 == "TCU",	"Texas Christian", new_team2)
new_team2 <- ifelse(new_team2 == "UNC Wilmington"	,	"North Carolina-Wilmington", new_team2)
new_team2 <- ifelse(new_team2 == "NC State", "North Carolina State", new_team2)
new_team2 <- ifelse(new_team2 == "UNC Asheville",	"North Carolina-Asheville", new_team2)
new_team2 <- ifelse(new_team2 == "Little Rock"	,	"Arkansas-Little Rock", new_team2)


new_winner <- tourn$winner
new_winner <- ifelse(new_winner == "BYU"	,	"Brigham Young", new_winner)
new_winner <- ifelse(new_winner == "Central Connecticut"	,	"Central Connecticut State", new_winner)
new_winner <- ifelse(new_winner == "Detroit"	,	"Detroit Mercy", new_winner)
new_winner <- ifelse(new_winner == "ETSU"	,	"East Tennessee State", new_winner)
new_winner <- ifelse(new_winner == "LIU-Brooklyn"	,	"Long Island University", new_winner)
new_winner <- ifelse(new_winner == "LIU"	,	"Long Island University", new_winner)
new_winner <- ifelse(new_winner == "LSU"	,	"Louisiana State", new_winner)
new_winner <- ifelse(new_winner == "Ole Miss"	,	"Mississippi", new_winner)
new_winner <- ifelse(new_winner == "Penn"	,	"Pennsylvania", new_winner)
new_winner <- ifelse(new_winner == "Pitt"	,	"Pittsburgh", new_winner)
new_winner <- ifelse(new_winner == "SMU"	,	"Southern Methodist", new_winner)
new_winner <- ifelse(new_winner == "Southern Miss"	,	"Southern Mississippi", new_winner)
new_winner <- ifelse(new_winner == "St. Joseph's"	,	"Saint Joseph's", new_winner)
new_winner <- ifelse(new_winner == "St. Peter's"	,	"Saint Peter's", new_winner)
new_winner <- ifelse(new_winner == "UCF"	,	"Central Florida", new_winner)
new_winner <- ifelse(new_winner == "UConn"	,	"Connecticut", new_winner)
new_winner <- ifelse(new_winner == "UCSB"	,	"UC-Santa Barbara", new_winner)
new_winner <- ifelse(new_winner == "UIC"	,	"Illinois-Chicago", new_winner)
new_winner <- ifelse(new_winner == "UMass"	,	"Massachusetts", new_winner)
new_winner <- ifelse(new_winner == "UMBC"	,	"Maryland-Baltimore County", new_winner)
new_winner <- ifelse(new_winner == "UNC"	,	"North Carolina", new_winner)
new_winner <- ifelse(new_winner == "UNLV"	,	"Nevada-Las Vegas", new_winner)
new_winner <- ifelse(new_winner == "USC"	,	"Southern California", new_winner)
new_winner <- ifelse(new_winner == "UTEP"	,	"Texas-El Paso", new_winner)
new_winner <- ifelse(new_winner == "UTSA"	,	"Texas-San Antonio", new_winner)
new_winner <- ifelse(new_winner == "VCU"	,	"Virginia Commonwealth", new_winner)
new_winner <- ifelse(new_winner == "California"	,	"University of California", new_winner)
new_winner <- ifelse(new_winner == "UNC Greensboro"	,	"North Carolina-Greensboro", new_winner)
new_winner <- ifelse(new_winner == "Saint Mary's"	,	"Saint Mary's (CA)", new_winner)
new_winner <- ifelse(new_winner == "TCU",	"Texas Christian", new_winner)
new_winner <- ifelse(new_winner == "UNC Wilmington"	,	"North Carolina-Wilmington", new_winner)
new_winner <- ifelse(new_winner == "NC State", "North Carolina State", new_winner)
new_winner <- ifelse(new_winner == "UNC Asheville",	"North Carolina-Asheville", new_winner)
new_winner <- ifelse(new_winner == "Little Rock"	,	"Arkansas-Little Rock", new_winner)


tourn$team1 <- new_team1
tourn$team2  <- new_team2
tourn$winner <- new_winner
tourn <- tourn %>% select(team1, team2,winner,season,team1_seed,team2_seed )

write_csv(tourn, "~/Google Drive/My Drive/March_madness/March_madness-master/2023/renamed_all_tournament_wins_1996-2022.csv")



# merge seed info with season data
t1df <- data.frame(School = tourn$team1, seed = tourn$team1_seed, season = tourn$season)
t2df <- data.frame(School = tourn$team2, seed = tourn$team2_seed, season = tourn$season)

tourn_melt <- rbind(t1df,t2df)
tourn_melt$School <- as.character(tourn_melt$School)
tourn_melt_uni <- unique(tourn_melt)

all_School <- data.frame(School = nonadf$School, season = nonadf$season)
all_School$School <- as.character(all_School$School)

school_seeds <- left_join(all_School,tourn_melt_uni)

 
nonadf$seed <- school_seeds$seed



write_csv(nonadf, "~/Google Drive/My Drive/March_madness/March_madness-master/2023/Processed_data/joined_data.csv")

write_csv(nonadf, "~/Google Drive/My Drive/March_madness/March_madness-master/2023/seeded_all_regseason_cleaned_1996-2023.csv")




#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------



# load data
seeddf <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2023/seeded_all_regseason_cleaned_1996-2023.csv")

tourney <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2023/renamed_all_tournament_wins_1996-2022.csv")


# filter to tournament teams
tourney_teams <- seeddf %>% filter(is.na(seed)==FALSE)



# tournament munging
tourney$wteam <- tourney$winner
tourney$lteam <- ifelse(tourney$team1 == tourney$winner, tourney$team2, tourney$team1 )

tourney_results <- tourney %>% select(season, wteam, lteam)

seeddf <- as.data.frame(seeddf)
tourney_results <- as.data.frame(tourney_results)
tourney_teams <- as.data.frame(tourney_teams)


both_vars_w <- data.frame()

for(i in 1:length(tourney_results$wteam)){
  x <- filter(seeddf, School == tourney_results[i,2], season == tourney_results[i,1])
  y <- filter(seeddf, School == tourney_results[i,3], season == tourney_results[i,1]) 
  new <- data.frame(x,y)
  both_vars_w <- rbind(both_vars_w,new)
}

tourney_results <- tourney_results %>% filter(is.na(wteam)==FALSE) 
both_vars_data_w <- data.frame(tourney_results, both_vars_w)   


both_vars_l <- data.frame()

for(i in 1:length(tourney_results$wteam)){
  x <- filter(seeddf, School == tourney_results[i,2], season == tourney_results[i,1])
  y <- filter(seeddf, School == tourney_results[i,3], season == tourney_results[i,1]) 
  new <- data.frame(y,x)
  both_vars_l <- rbind(both_vars_l,new)
}

both_vars_data_l <- data.frame(tourney_results, both_vars_l)   


both_vars_data_all <- rbind(both_vars_data_w, both_vars_data_l)

both_vars_data_all$variable <- ifelse(both_vars_data_all$wteam == both_vars_data_all$School, "wteam", "lteam") 



# Save difference data for modeling
write_csv(both_vars_data_all,"~/Google Drive/My Drive/March_madness/March_madness-master/2023/Processed_data/both_model_data.csv")


