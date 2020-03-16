
##########################################
#           Wine Analytics               #
#       by René Angeles Martinez         #
##########################################


#Load Libraries 
library(caret)
#library(corrplot)
#library(tibble)
library(plsRglm)
library(plsdof)
#library(Hmisc)
library(tidyverse)

set.seed(123)


# Load
#file_red <- 'winequality-red.csv';
#file_white <- 'winequality-white.csv';
#white <- read.csv(file_white, sep=';')
#red <- read.csv(file_red, sep=';')
red <- read.csv(file.path('data/', 'winequality-red.csv'), sep=';')
white <- read.csv(file.path('data/','winequality-red.csv'), sep=';')

add_type <- function(data, w_type){
  data <- add_column(data, wine.type = w_type, .before = 'quality')
}


basemodel<- function(data_train){
  
  train_control <- trainControl(method="cv", number=5)
  
  #Grid to search for values
  len <- length(data_train) -1
  grid <- expand.grid(.nt=c(1:len), 
                      .alpha.pvals.expli=.05)
  
  mod <- train(quality~., data=data_train, 
               metric = "MAE", 
               trControl=train_control, 
               method="plsRglm", 
               tuneGrid=grid, 
               preProc = c("center","scale"),
               verbose=FALSE)

  return(mod)
}

get_mae<-function(mod, data_test, doRound = TRUE){
  tmp<-predict(mod, data_test)
  if(doRound){tmp<-round(tmp)}
  return(MAE(tmp ,data_test$quality ))
}



## Split 80% training and 20% testing
#mixed: red + white with dummy var
mixed_data <- rbind(add_type(red, 1), add_type(white, 0))
partition <- createDataPartition(mixed_data$quality, p = 0.80)[[1]]
mixed_train <- mixed_data[partition,]
mixed_test <- mixed_data[-partition,]
mixed_mod <- basemodel(mixed_train)

partition <- createDataPartition(red$quality, p = 0.80)[[1]]
red_train <- red[partition,]
red_test <- red[-partition,]
red_mod <- basemodel(red_train)


partition <- createDataPartition(white$quality, p = 0.80)[[1]]
white_train <- white[partition,]
white_test <- white[-partition,]
white_mod <- basemodel(white_train)


print('Single model, MAE for un-rounded predictions')
get_mae(mixed_mod, mixed_test, FALSE)

print('Single model, MAE for rounded predictions')
get_mae(mixed_mod, mixed_test, TRUE)

print('Modeling the red/white separately, MAE un-rounded predions:')
(nrow(red_test) * get_mae(red_mod, red_test, FALSE) +
    nrow(white_test) * get_mae(white_mod, white_test, FALSE) )/ (nrow(red_test) + nrow(white_test))


print('Modeling the red/white separately, MAE rounded predions:')
(nrow(red_test) * get_mae(red_mod, red_test) +
    nrow(white_test) * get_mae(white_mod, white_test) )/ (nrow(red_test) + nrow(white_test))





### Don't look below, it is only for future dev


# #################################### Building baseline for mixture
# ### Fit PLS regression to find quality
# 
# aux_data_train <- mixed_train
# aux_data_test <- mixed_test
# train_control <- trainControl(method="cv", number=5)
# #Grid to search for values
# grid <- expand.grid(.nt=c(1:length(aux_data_train)-1), .alpha.pvals.expli=.05)
# mod <- train(quality~., data=aux_data_train, 
#                 metric = "MAE", 
#                 trControl=train_control, 
#                 method="plsRglm", 
#                 tuneGrid=grid, 
#                 preProc = c("center","scale"),
#                 verbose=FALSE)
# 
# aux_pred<-predict(mod,newdata = aux_data_test)
# mean(abs(aux_pred-aux_data_test$quality))
#  
# 
# #Red wine 
# #red
# partition <- createDataPartition(red$quality, p = 0.80)[[1]]
# red_train <- red[partition,]
# red_test <- red[-partition,]
# 
# aux_data_train <- red_train
# aux_data_test <- red_test
# train_control <- trainControl(method="cv", number=5)
# #Grid to search for values
# grid <- expand.grid(.nt=c(2: length(aux_data_train)-1), .alpha.pvals.expli=.05)
# mod <- train(quality~., data=aux_data_train, 
#              metric = "MAE", 
#              trControl=train_control, 
#              method="plsRglm", 
#              tuneGrid=grid, 
#              preProc = c("center","scale"),
#              verbose=FALSE)
# 
# aux_pred<-predict(mod,newdata = aux_data_test)
# mean(abs(aux_pred-aux_data_test$quality))
# mod
# 
# 
# 
# #white
# partition <- createDataPartition(white$quality, p = 0.80)[[1]]
# white_train <- white[partition,]
# white_test <- white[-partition,]
# 
# 
# aux_data_train <- white_train
# aux_data_test <- white_test
# train_control <- trainControl(method="cv", number=5)
# #Grid to search for values
# grid <- expand.grid(.nt=c(2:length(aux_data_train)-1), 
#                     .alpha.pvals.expli=.01)
# 
# mod <- train(quality~., data=aux_data_train, 
#              metric = "MAE", 
#              trControl=train_control, 
#              method="plsRglm", 
#              tuneGrid=grid, 
#              preProc = c("center","scale"),
#              verbose=FALSE)
# 
# aux_pred<-predict(mod,newdata = aux_data_test)
# mean(abs(aux_pred-aux_data_test$quality))
# mod
# length(aux_data_train)






