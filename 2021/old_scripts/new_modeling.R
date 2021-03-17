library(dplyr)
library(caret)
library(readr)
library(PerformanceAnalytics)


# Model setup and training -----------------------------------------

both_data <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/Processed_data/both_model_data.csv")

df2016 <- both_data %>% filter(season ==2016) %>% select(-season, -School,  -wteam, -season.1, -season.2, -lteam,  -School.1)
df2017 <- both_data %>% filter(season ==2017) %>% select(-season, -School,  -wteam, -season.1, -season.2, -lteam,  -School.1)

Dataset_test <- both_data %>% filter(season != 2017 ) %>% select(-season, -School,  -wteam, -season.1, -season.2, - lteam,  -School.1)

#Fit a Random Forest model
tunegrid <- expand.grid(.mtry=c(1:15))
model <- train(variable~.,Dataset_test,
               method='rf', preProcess = 'scale',
               trControl=trainControl(
                 method='cv',number=10,
                 verbose = TRUE,
                 classProbs = TRUE))
model$results


# Predictions 2017 --------------------------------------------------------

test_data <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/Processed_data/joined_data.csv")
test_data <- test_data %>% filter(season == "2017")

#teams_2017 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/teams_2017_new.csv")
#teams_2017 <- teams_2017 %>% filter(season2017 == 1) 


wins <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_tournament_wins_1996-2017.csv")
teams2017 <- wins %>% filter(season==2017) 
teams2017 <- c(teams2017$team1,teams2017$team2)
teams2017 <- data.frame(School = unique(teams2017)) 
teams2017$School <- as.character(teams2017$School)
teams2017 <- teams2017 %>% arrange(School)

meh <- inner_join(test_data,teams2017) 
test_data <- meh  %>% filter(is.na(seed)==FALSE)



# Set up data fram

setup_vars <- data.frame()

for(i in 1:length(test_data$School)){
  for(j in 1:length(test_data$School)){
    x <- filter(test_data, School == test_data[i,1])
    y <- filter(test_data, School == test_data[j,1]) 
    new <- data.frame(x,y)
    setup_vars <- rbind(setup_vars,new)
  }
}

prediction_data <- setup_vars %>% filter(School != School.1)

write.csv(prediction_data,"C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/Processed_data/prediction_data.csv")

#Predict class probabilities (i.e. 'certainty' scores)

prediction_data_sub <- prediction_data %>% 
  filter(season ==2017) %>% select(-season, -School, -season.1,   -School.1)
pred <- predict(model,prediction_data_sub,"prob")
head(pred)

prediction_data$lteam  <- pred[,1]
prediction_data$wteam  <- pred[,2]


output_predictions <- prediction_data %>% select(School,School.1,wteam,lteam)

#write.csv(output_predictions, "C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/model_predictions_2018.csv")



#-----------
clean_pred <- data.frame()
for(i in unique(output_predictions$School)){ 
  for(j in unique(output_predictions$School)){
    if(i != j){
      t1 <- as.numeric(output_predictions %>% filter(School==i & School.1==j) %>% select(wteam))
      t2 <- as.numeric(output_predictions %>% filter(School==j & School.1==i) %>% select(wteam))
      
      if(t1 > t2){
        num <- t1-t2 
        df <- data.frame(i,j,i,num*100) 
        colnames(df) <- c("Team1","Team2","predicted_winner","probability_difference")
        clean_pred <- rbind(clean_pred,df)
      } else {
        num <- t2-t1
        df <- data.frame(i,j,j,num*100)
        colnames(df) <- c("Team1","Team2","predicted_winner","probability_difference")
        clean_pred <- rbind(clean_pred,df)
      }
    }
  } 
} 

clean_pred_sub <- clean_pred %>% filter(Team1 != Team2)


# -------------
#random forrest
tourn_2017 <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_tournament_wins_1996-2017.csv")
tourn_2017 <- tourn_2017 %>% select(Team1 = team1, Team2 = team2, winner)

#pred_test_2017 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/clean_rf_model_predictions_2018.csv")

pred_test_match_2017 <- left_join(tourn_2017,clean_pred_sub)  

prop.table(table(pred_test_match_2017$winner==pred_test_match_2017$predicted_winner))


# Predictions 2016 -----------------------------------------


test_data <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/Processed_data/joined_data.csv")
test_data <- test_data %>% filter(season == "2016")

#teams_2016 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/teams_2016_new.csv")
#teams_2016 <- teams_2016 %>% filter(season2016 == 1) 


wins <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_tournament_wins_1996-2016.csv")
teams2016 <- wins %>% filter(season==2016) 
teams2016 <- c(teams2016$team1,teams2016$team2)
teams2016 <- data.frame(School = unique(teams2016)) 
teams2016$School <- as.character(teams2016$School)
teams2016 <- teams2016 %>% arrange(School)

meh <- inner_join(test_data,teams2016) 
test_data <- meh  %>% filter(is.na(seed)==FALSE)



# Set up data fram

setup_vars <- data.frame()

for(i in 1:length(test_data$School)){
  for(j in 1:length(test_data$School)){
    x <- filter(test_data, School == test_data[i,1])
    y <- filter(test_data, School == test_data[j,1]) 
    new <- data.frame(x,y)
    setup_vars <- rbind(setup_vars,new)
  }
}

prediction_data <- setup_vars %>% filter(School != School.1)

write.csv(prediction_data,"C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/Processed_data/prediction_data.csv")

#Predict class probabilities (i.e. 'certainty' scores)

prediction_data_sub <- prediction_data %>% 
  filter(season ==2016) %>% select(-season, -School, -season.1,   -School.1)
pred <- predict(model,prediction_data_sub,"prob")
head(pred)

prediction_data$lteam  <- pred[,1]
prediction_data$wteam  <- pred[,2]


output_predictions <- prediction_data %>% select(School,School.1,wteam,lteam)

#write.csv(output_predictions, "C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/model_predictions_2018.csv")



#-----------
clean_pred <- data.frame()
for(i in unique(output_predictions$School)){ 
  for(j in unique(output_predictions$School)){
    if(i != j){
      t1 <- as.numeric(output_predictions %>% filter(School==i & School.1==j) %>% select(wteam))
      t2 <- as.numeric(output_predictions %>% filter(School==j & School.1==i) %>% select(wteam))
      
      if(t1 > t2){
        num <- t1-t2 
        df <- data.frame(i,j,i,num*100) 
        colnames(df) <- c("Team1","Team2","predicted_winner","probability_difference")
        clean_pred <- rbind(clean_pred,df)
      } else {
        num <- t2-t1
        df <- data.frame(i,j,j,num*100)
        colnames(df) <- c("Team1","Team2","predicted_winner","probability_difference")
        clean_pred <- rbind(clean_pred,df)
      }
    }
  } 
} 

clean_pred_sub <- clean_pred %>% filter(Team1 != Team2)


# -------------
#random forrest
tourn_2016 <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/New_model/all_tournament_wins_1996-2017.csv")
tourn_2016 <- tourn_2016 %>% select(Team1 = team1, Team2 = team2, winner)

#pred_test_2016 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/clean_rf_model_predictions_2018.csv")

pred_test_match_2016 <- left_join(tourn_2016,clean_pred_sub)  

prop.table(table(pred_test_match_2016$winner==pred_test_match_2016$predicted_winner))






