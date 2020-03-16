##########################################
#           Wine Analytics               #
#       by René Angeles Martinez         #
#
# Ensemble of 3 using (baseline)models   
# blended using pls-glm-polr.
#
#
# Two strategies for sub-trainings
# sets:
#
# - Split training set into three parts  
# - Resampling training set three times  #
#                                        #
##########################################

#Libraries needed
library(caret)
library(plsRglm)
library(caret)
library(dplyr)
library(plsdof)
library(tidyverse)
set.seed(123)

#data
red <-read.csv(file.path('data/', 'winequality-red.csv'), sep=';')
white <- read.csv(file.path('data/','winequality-white.csv'), sep=';')

add_type <- function(data, w_type){
  data <- add_column(data, wine.type = w_type, .before = 'quality')
}

# Combine data sets
data <- rbind(add_type(white,0), add_type(red,1))

partition <- createDataPartition(data$quality, p = 0.80)[[1]]
data_train <- data[partition,]
data_test <- data[-partition,]

heavy_tails = c('fixed.acidity',
                'volatile.acidity',
                'citric.acid',
                'residual.sugar',
                'chlorides',
                'total.sulfur.dioxide',
                'free.sulfur.dioxide',
                'density',
                'sulphates')

rm_outliers<-function(data, range = 2){
  x<-data
  for (i in heavy_tails ){
    aux<- boxplot((data[[i]]), range, plot=TRUE)$out
    x<- x[-which(data[[i]] %in% aux),]
  }
  return(x)
}

##########################################
#            BASELINE MODEL              #
# Same as base line model                #
##########################################

train_basemodel<- function(data_train){
  train_control <- trainControl(method="cv", number=5)
  #Grid to search for values
  len <- length(data_train) -1
  grid <- expand.grid(.nt=c(1:len), 
                      .alpha.pvals.expli=.05) # level of significance for predictors when pvals.expli=TRUE
  
  
 # data_train<-rm_outliers(data_train)
  mod <- train(quality~., data=data_train, 
               metric = "MAE", 
               trControl=train_control, 
               method="plsRglm",
               tuneGrid=grid, 
               preProc = c("center","scale"),
               verbose=FALSE)
  return(mod)
}

##########################################
#   Blender/ predict functions
##########################################


train_blender<-function(m1,m2,m3, d4, nt ){
  
  # Train blended model
  # m1,m2,m3 are models
  # d4 a training set
  # nt number of components ot keep 
  
  
  t1<- predict(m1, newdata = d4)
  t2<- predict(m2, newdata = d4)
  t3<- predict(m3, newdata = d4)
  df<- data.frame(t1,t2,t3, d4)
  X<-df[,1:14]
  y<-df$quality
  
  #blender
  #mod<-plsRglm(y,X,nt,modele="pls-glm-polr")
  mod<- train_basemodel(df)
  return(mod)
}


predict_blended<-function(m1,m2,m3, mod, d4){
  
  #make predictions for models (m1, m2, m3)
  #blended into mod onto dataset (d4)
  
  
  t1<- predict(m1, newdata = d4)
  t2<- predict(m2, newdata = d4)
  t3<- predict(m3, newdata = d4)
  df<- data.frame(t1,t2,t3, d4)
  X<-df[,1:14]
  #return( as.numeric(as.character( predict(mod, newdata = X, type="class")  )) )
  return(round(predict(mod, df)))
}


##########################################
#   STRATEGY 1
# Ensemble of 3 using (baseline)models   
# blended using pls-glm-polr.
#
# - Split training set into three parts  
##########################################


# Train a baseline model on each of these
partition0 <- createDataPartition(data_train$quality, p = 0.50)[[1]]
d01 <- data_train[partition0,]
d02 <- data_train[-partition0,]
stopifnot(nrow(d01)+nrow(d02) == nrow(data_train) )
partition <- createDataPartition(d01$quality, p = 0.50)[[1]]
d1 <- d01[partition,]
d2 <- d01[-partition,]
partition <- createDataPartition(d02$quality, p = 0.50)[[1]]
d3 <- d02[partition,]
d4 <- d02[-partition,]


stopifnot(nrow(d1)+nrow(d2)+nrow(d3)+nrow(d4) == nrow(data_train) )

m1 <- train_basemodel(d1)
m2 <- train_basemodel(d2)
m3 <- train_basemodel(d3)



mod<-train_blender(m1,m2,m3, d4,5)
mod

rest_test <- predict_blended(m1,m2,m3, mod, data_test)
print('Test MAE of blended model with strategy 1:')
MAE(rest_test, data_test$quality)

rest_train <- predict_blended(m1,m2,m3, mod, data_train)
print('Validation MAE of blended model with strategy 1:')
MAE(rest_train, data_train$quality)

##########################################
#   STRATEGY 2
#
# Ensemble of 3 using (baseline) models   
# blended using pls-glm-polr.
# Training set obtained with resample 
##########################################


partition0 <- createDataPartition(data_train$quality, p = 0.75)[[1]]
d01 <- data_train[partition0,]
d4 <- data_train[-partition0,]

stopifnot(nrow(d01)+nrow(d4) == nrow(data_train) )
set.seed(1234)

# Resampling training into subsets for training models to be blended
p1 <- createResample(d01$quality, times = 1, list = TRUE)[[1]]
p2 <- createResample(d01$quality, times = 1, list = TRUE)[[1]]
p3 <- createResample(d01$quality, times = 1, list = TRUE)[[1]]
d1 <- d01[p1,]
d2 <- d01[p2,]
d3 <- d01[p3,]

# Train a baseline model on each of these
m1 <- train_basemodel(d1)
m2 <- train_basemodel(d2)
m3 <- train_basemodel(d3)

mod<-train_blender(m1,m2,m3, d4,6)
rest_test <- predict_blended(m1,m2,m3, mod, data_test)
rest_train <- predict_blended(m1,m2,m3, mod, data_train)

print('Validation MAE of blended model with strategy 2:')
MAE(rest_train, data_train$quality)

print('Test MAE of blended model with strategy 2:')
MAE(rest_test, data_test$quality)

