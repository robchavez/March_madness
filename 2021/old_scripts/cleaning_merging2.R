library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

# load season data

rawdf <- read_csv("~/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_regseason_cleaned_1996-2017.csv")
rawdf <- read_csv("C:/Users/rober//Google Drive/Code_scripts/R/forthefuckofit/New_model/all_regseason_cleaned_1996-2017.csv")

nonadf <- rawdf %>% select(-SchoolAdvancedPace, 
                 -SchoolAdvancedORtg, 
                 -HomeW, 
                 -HomeL, 
                 -AwayW, 
                 -AwayL, 
                 -SchoolAdvancedORB_percent, 
                 -SchoolAdvancedSTL_percent, 
                 -`School TotalsORB`)


# load tournament data

tourn <- read_csv("~/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_tournament_wins_1996-2017.csv")
tourn <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_tournament_wins_1996-2017.csv")

# rename teams
new_team1 <- tourn$team1
new_team1 <- ifelse(new_team1 == "BYU"	,	"Brigham Young", new_team1)
new_team1 <- ifelse(new_team1 == "Central Connecticut"	,	"Central Connecticut State", new_team1)
new_team1 <- ifelse(new_team1 == "Detroit"	,	"Detroit Mercy", new_team1)
new_team1 <- ifelse(new_team1 == "ETSU"	,	"East Tennessee State", new_team1)
new_team1 <- ifelse(new_team1 == "LIU-Brooklyn"	,	"Long Island University", new_team1)
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


new_team2 <- tourn$team2
new_team2 <- ifelse(new_team2 == "BYU"	,	"Brigham Young", new_team2)
new_team2 <- ifelse(new_team2 == "Central Connecticut"	,	"Central Connecticut State", new_team2)
new_team2 <- ifelse(new_team2 == "Detroit"	,	"Detroit Mercy", new_team2)
new_team2 <- ifelse(new_team2 == "ETSU"	,	"East Tennessee State", new_team2)
new_team2 <- ifelse(new_team2 == "LIU-Brooklyn"	,	"Long Island University", new_team2)
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


new_winner <- tourn$winner
new_winner <- ifelse(new_winner == "BYU"	,	"Brigham Young", new_winner)
new_winner <- ifelse(new_winner == "Central Connecticut"	,	"Central Connecticut State", new_winner)
new_winner <- ifelse(new_winner == "Detroit"	,	"Detroit Mercy", new_winner)
new_winner <- ifelse(new_winner == "ETSU"	,	"East Tennessee State", new_winner)
new_winner <- ifelse(new_winner == "LIU-Brooklyn"	,	"Long Island University", new_winner)
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

tourn$team1 <- new_team1
tourn$team2  <- new_team2
tourn$winner <- new_winner

#write_csv(tourn, "C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/renamed_all_tournament_wins_1996-2017.csv")



# merge seed info with season data
t1df <- data.frame(School = tourn$team1, seed = tourn$team1_seed, season = tourn$season)
t2df <- data.frame(School = tourn$team2, seed = tourn$team2_seed, season = tourn$season)

tourn_melt <- rbind(t1df,t2df)
tourn_melt_uni <- unique(tourn_melt)
tourn_melt_uni$School <- as.character(tourn_melt_uni$School)

all_School <- data.frame(School = nonadf$School, season = nonadf$season)
all_School$School <- as.character(all_School$School)

school_seeds <- left_join(all_School, tourn_melt_uni)

nonadf$seed <- school_seeds$seed


#write_csv(nonadf, "C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/seeded_all_regseason_cleaned_1996-2017.csv")



#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------



# load data
seeddf <- read_csv("~/Google Drive/Code_scripts/R/forthefuckofit/New_model/seeded_all_regseason_cleaned_1996-2017.csv")
seeddf <- read_csv("C:/Users/rober//Google Drive/Code_scripts/R/forthefuckofit/New_model/seeded_all_regseason_cleaned_1996-2017.csv")

tourney <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/renamed_all_tournament_wins_1996-2017.csv")

# filter to tournament teams
tourney_teams <- seeddf %>% filter(is.na(seed)==FALSE)

## *** might need to merge  instead of filtering... boi!!!***** maybe not...boi :(

# tournament munging
tourney$wteam <- tourney$winner
tourney$lteam <- ifelse(tourney$team1 == tourney$winner, tourney$team2, tourney$team1 )

tourney_results <- tourney %>% select(season, wteam, lteam)

seeddf <- as.data.frame(seeddf)
tourney_results <- as.data.frame(tourney_results)
tourney_teams <- as.data.frame(tourney_teams)

#tourney_results$gameID <- 1:length(tourney_results$wteam) 


both_vars <- data.frame()

for(i in 1:length(tourney_results$wteam)){
  x <- filter(seeddf, School == tourney_results[i,2], season == tourney_results[i,1])
  y <- filter(seeddf, School == tourney_results[i,3], season == tourney_results[i,1]) 
  new <- data.frame(x,y)
  both_vars <- rbind(both_vars,new)
}

both_vars_data <- data.frame(tourney_results, both_vars)   
both_vars_data$season <- as.character(both_vars_data$season)

# Remove NAs
both_vars_data_sub <- data.frame(tourney_results, both_vars_data) %>% filter(is.na(OverallG)==FALSE)


# Merge tourney results with difference scores.
tourney_results$season <- as.character(tourney_results$season) 
tourney_results_melt <- melt(tourney_results)

merged_tourney_results_both <- inner_join(tourney_results_melt,both_vars_data_sub)

# Save difference data for modeling
#write_csv(merged_tourney_results_both,"C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit//New_model/Processed_data/both_model_data.csv")






