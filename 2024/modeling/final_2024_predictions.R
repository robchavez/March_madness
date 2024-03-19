library(dplyr)
library(caret)
library(readr)
library(PerformanceAnalytics)


# Model setup and training -----------------------------------------
both_data <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2024/Processed_data/both_model_data.csv")

Dataset_test <- both_data %>% 
  filter(season != 2024, is.na(seed)==FALSE, is.na(seed.1)==FALSE) %>% 
  select(-season, 
         -School,  
         -wteam, 
         -season.1, 
         -season.2, 
         -lteam,  
         -School.1)


# Fit a stochastic gradient boosting machine
#tunegrid <- expand.grid(n.trees= c(100), interaction.depth = c(3), shrinkage = c(.1), n.minobsinnode = c(20)) #gbm
tunegrid <- expand.grid(mtry = 2,3,6,10,20) #rf

model <- train(variable~.,Dataset_test,
               method='rf', 
               preProcess = c("center"),
               tuneGrid=tunegrid,
               trControl=trainControl(
                 method='cv',number=5, 
                 verbose = F,
                 classProbs = TRUE))
model$results
model
library(randomForest)
plot(varImp(model))

# -------------------------------------------------------------------------
# Predictions 2024 --------------------------------------------------------

test_data <- read_csv("~/Google Drive/My Drive/March_madness/March_madness-master/2024/seeded_all_regseason_cleaned_1996-2024.csv")
test_data <- test_data %>% filter(season == "2024") %>% filter(is.na(seed)==F)
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

#write.csv(prediction_data,"~/Google Drive/My Drive/March_madness/March_madness-master/2024/Processed_data/prediction_data_2024.csv")

#Predict class probabilities (i.e. 'certainty' scores)

prediction_data_sub <- prediction_data %>% 
  filter(season == 2024) %>% select(-season, -School, -season.1,   -School.1)
pred <- predict(model,prediction_data_sub,"prob")
head(pred)

prediction_data$lteam  <- pred[,1]
prediction_data$wteam  <- pred[,2]


output_predictions <- prediction_data %>% select(School,School.1,wteam,lteam)

# raw uncorrected output
#write.csv(output_predictions, "~/Google Drive/My Drive/March_madness/March_madness-master/2024/uncorrected_model_predictions_2024.csv")


# correction step: compare win probabilities
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

write_csv(clean_pred_sub, "~/Google Drive/My Drive/March_madness/March_madness-master/2024/Processed_data/final_model_predictions_2024.csv")
