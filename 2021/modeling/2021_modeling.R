library(dplyr)
library(caret)
library(readr)
library(PerformanceAnalytics)


# Model setup and training -----------------------------------------

#both_data <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2019/Processed_data/both_model_data.csv")
both_data <- read_csv("~/Google Drive/March_madness/March_madness-master/2021/Processed_data/both_model_data.csv")


#df2016 <- both_data %>% filter(season ==2016) %>% select(-season, -School,  -wteam, -season.1, -season.2, -lteam,  -School.1, -seed)
#df2017 <- both_data %>% filter(season ==2017) %>% select(-season, -School,  -wteam, -season.1, -season.2, -lteam,  -School.1)

Dataset_test <- both_data %>% filter(season != 2017, season != 2018, season != 2019, season != 2021, is.na(seed)==FALSE, is.na(seed.1)==FALSE) %>% select(-season, -School,  -wteam, -season.1, -season.2, - lteam,  -School.1)
Dataset_test <- both_data %>% 
  filter(season != 2017, season != 2018, season != 2019, season != 2021, is.na(seed)==FALSE, is.na(seed.1)==FALSE) %>% 
  select(-season, -School,  -wteam, -season.1, -season.2, -lteam,  -School.1)


Dataset_test <- both_data %>% 
  filter(season != 2017, season != 2018, season != 2019, season != 2021, is.na(seed)==FALSE, is.na(seed.1)==FALSE) %>% 
  select(-season, -School,  -wteam, -season.1, -season.2, -lteam,  -School.1, -seed)

#Fit a Random Forest model
tunegrid <- expand.grid(  n.trees= c(50,100,150), interaction.depth = c(1, 2, 3), shrinkage = c(.1,.2), n.minobsinnode = c(10, 15))

#tunegrid <- expand.grid( C=c(1,10,50))

  model <- train(variable~.,Dataset_test,
               method='gbm', preProcess = 'scale',
              tuneGrid=tunegrid,
               trControl=trainControl(
                 method='repeatedcv',number=10, repeats = 3,
                 verbose = TRUE,
                 classProbs = TRUE))
model$results




# Predictions 2017 --------------------------------------------------------

test_data <- read_csv("~/Google Drive/March_madness/March_madness-master/2021/Processed_data/joined_data.csv")
#test_data <- read_csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2019/Processed_data/joined_data.csv")
test_data <- test_data %>% filter(season == "2017") %>% filter(is.na(seed)==F)

#teams_2017 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2019/teams_2017_new.csv")
#teams_2017 <- teams_2017 %>% filter(season2017 == 1) 


wins <- read_csv("~/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2019/renamed_all_tournament_wins_1996-2018.csv")
teams2017 <- wins %>% filter(season==2017) 
teams2017 <- c(teams2017$team1,teams2017$team2)
teams2017 <- data.frame(School = unique(teams2017)) 
teams2017$School <- as.character(teams2017$School)
teams2017 <- teams2017 %>% arrange(School)

meh <- inner_join(test_data,teams2017) 
test_data <- meh  %>% filter(is.na(seed)==FALSE)

test_data <- as.data.frame(test_data)


# Set up data frame

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

#write.csv(prediction_data,"~/Google Drive/March_madness/March_madness-master/2021/Processed_data/prediction_data.csv")

#Predict class probabilities (i.e. 'certainty' scores)

prediction_data_sub <- prediction_data %>% 
  filter(season ==2017) %>% select(-season, -School, -season.1,   -School.1)
pred <- predict(model,prediction_data_sub,"prob")
head(pred)

prediction_data$lteam  <- pred[,1]
prediction_data$wteam  <- pred[,2]


output_predictions <- prediction_data %>% select(School,School.1,wteam,lteam)

#write.csv(output_predictions, "C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/model_predictions_2018.csv")



# secret sauce  (ahhhh)----------------------
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
#
tourn_2017 <- read_csv("~/Google Drive/March_madness/March_madness-master/2021/renamed_all_tournament_wins_1996-2018.csv")
tourn_2017 <- tourn_2017 %>% select(Team1 = team1, Team2 = team2, winner)

#pred_test_2017 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/clean_rf_model_predictions_2018.csv")

pred_test_match_2017 <- left_join(tourn_2017,clean_pred_sub)  

prop.table(table(pred_test_match_2017$winner==pred_test_match_2017$predicted_winner))


# Predictions 2018 -----------------------------------------


test_data_2018 <- read.csv("~/Google Drive/March_madness/March_madness-master/2021/Processed_data/joined_data.csv")
test_data_2018 <- test_data_2018 %>% filter(season == "2018")

wins <- read_csv("~/Google Drive/March_madness/March_madness-master/2021/renamed_all_tournament_wins_1996-2018.csv")
teams2018 <- wins %>% filter(season==2018) 
teams2018 <- c(teams2018$team1,teams2018$team2)
teams2018 <- data.frame(School = unique(teams2018)) 
teams2018$School <- as.character(teams2018$School)
teams2018 <- teams2018 %>% arrange(School)

meh <- inner_join(test_data_2018,teams2018) 
test_data_2018 <- meh  %>% filter(is.na(seed)==FALSE)
test_data_2018 <- as.data.frame(test_data_2018)


# Set up data fram

setup_vars <- data.frame()

for(i in 1:length(test_data_2018$School)){
  for(j in 1:length(test_data$School)){
    x <- filter(test_data_2018, School == test_data_2018[i,1])
    y <- filter(test_data_2018, School == test_data_2018[j,1]) 
    new <- data.frame(x,y)
    setup_vars <- rbind(setup_vars,new)
  }
}

prediction_data <- setup_vars %>% filter(School != School.1)

#write.csv(prediction_data,"C:/Users/rober//Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2019/Processed_data/prediction_data.csv")

#Predict class probabilities (i.e. 'certainty' scores)

prediction_data_sub <- prediction_data %>% 
  filter(season ==2018) %>% select(-season, -School, -season.1,  -School.1)
pred <- predict(model,prediction_data_sub,"prob")
head(pred)

prediction_data$lteam  <- pred[,1]
prediction_data$wteam  <- pred[,2]


output_predictions <- prediction_data %>% select(School,School.1,wteam,lteam)

#write.csv(output_predictions, "C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/model_predictions_2018.csv")



# secret sauce  (ahhhh)-----------
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

clean_pred_sub2018 <- clean_pred %>% filter(Team1 != Team2)


# -------------
#
tourn_2018 <- read_csv("~/Google Drive/March_madness/March_madness-master/2021/renamed_all_tournament_wins_1996-2018.csv")
tourn_2018 <- tourn_2018 %>% select(Team1 = team1, Team2 = team2, winner)

#pred_test_2016 <- read.csv("C:/Users/rober/Google Drive/Code_scripts/R/forthefuckofit/March_madness_model_2018/clean_rf_model_predictions_2018.csv")

pred_test_match_2018 <- left_join(tourn_2018,clean_pred_sub2018)  

prop.table(table(pred_test_match_2018$winner==pred_test_match_2018$predicted_winner))


#crap <- pred_test_match_2018 %>% filter(is.na(predicted_winner) == FALSE)
#prop.table(table(crap$winner==crap$predicted_winner))
