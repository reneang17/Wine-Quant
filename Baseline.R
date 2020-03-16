##########################################
#           Wine Analytics               #
#       by René Angeles Martinez         #
#
# Baseline model PLSR with kfold adjusted
##########################################

library(caret)
library(plsRglm)
library(plsdof)
library(tidyverse)
set.seed(123)

#Load data
red <- read.csv(file.path('data/', 'winequality-red.csv'), sep=';')
white <- read.csv(file.path('data/','winequality-white.csv'), sep=';')


#Function to combine red & white wines datasets
add_type <- function(data, w_type){
  data <- add_column(data, wine.type = w_type, .before = 'quality')
}

##########################################
#            BASELINE MODEL              #
##########################################


train_basemodel<- function(data_train){
  
  # function to train a PLS with number
  # of components to keep adjusted with 5fold validation.
  # data_train: dataset to train model over
  
  train_control <- trainControl(method="cv", number=5)# 5fold
  
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
  
  # Function to get MAE score for
  # mod: model 
  # data_test: data to test model 
  
  tmp<-predict(mod, data_test)
  if(doRound){tmp<-round(tmp)}
  return(MAE(tmp ,data_test$quality ))
}


## In all cases: Split 80% training and 20% testing

# Mixed red white
mixed_data <- rbind(add_type(red, 1), add_type(white, 0))
partition <- createDataPartition(mixed_data$quality, p = 0.80)[[1]]
mixed_train <- mixed_data[partition,]
mixed_test <- mixed_data[-partition,]
mixed_mod <- train_basemodel(mixed_train) #Train base model 

# Same for red
partition <- createDataPartition(red$quality, p = 0.80)[[1]]
red_train <- red[partition,]
red_test <- red[-partition,]
red_mod <- train_basemodel(red_train) #Train base model
red_mod
# Same for white
partition <- createDataPartition(white$quality, p = 0.80)[[1]]
white_train <- white[partition,]
white_test <- white[-partition,]
white_mod <- train_basemodel(white_train) #Train base model
white_mod

##########################################
#           MAE SCORES                   #
##########################################


print('Single model, MAE for un-rounded predictions')
get_mae(mixed_mod, mixed_test, FALSE)

print('Single model, MAE for rounded predictions')
get_mae(mixed_mod, mixed_test, TRUE)

nred <- nrow(red_test)
nwhite <- nrow(white_test)

print('Modeling the red/white separately, MAE un-rounded predions:')
(nred * get_mae(red_mod, red_test, FALSE) +
    nwhite * get_mae(white_mod, white_test, FALSE) )/ (nred + nwhite)



print('Modeling the red/white separately, MAE rounded predions:')
(nred * get_mae(red_mod, red_test) +
    nrow(white_test) * get_mae(white_mod, white_test) )/ (nred + nwhite)

### Validation 
print('Validation:')
(nred * get_mae(red_mod, red_train) + nwhite * get_mae(red_mod, red_train))/ (nred + nwhite)





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






