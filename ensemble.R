#Load Libraries 
library(caret)
library(plsRglm)
library(plsdof)
library(tidyverse)
set.seed(123)


data <- read.csv('winequality-white.csv', sep=';')

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


basemodel<- function(data_train){
  train_control <- trainControl(method="cv", number=5)
  #Grid to search for values
  len <- length(data_train) -1
  grid <- expand.grid(.nt=c(1:len), 
                      .alpha.pvals.expli=.05) # level of significance for predictors when pvals.expli=TRUE
  
  
  data_train<-rm_outliers(data_train)
  mod <- train(quality~., data=data_train, 
               metric = "MAE", 
               trControl=train_control, 
               method="plsRglm",
               tuneGrid=grid, 
               preProc = c("center","scale"),
               verbose=FALSE)
  return(mod)
}

m1 <- basemodel(d1)
m2 <- basemodel(d2)
m3 <- basemodel(d3)



train_blended<-function(m1,m2,m3, d4, nt ){
  
  t1<- predict(m1, newdata = d4)
  t2<- predict(m2, newdata = d4)
  t3<- predict(m3, newdata = d4)
  df<- data.frame(t1,t2,t3, d4)
  X<-df[,1:14]
  y<-df$quality
  mod<-plsRglm(y,X,nt,modele="pls-glm-polr")
  return(mod)
}


predict_blended<-function(m1,m2,m3, mod, d4){
  t1<- predict(m1, newdata = d4)
  t2<- predict(m2, newdata = d4)
  t3<- predict(m3, newdata = d4)
  df<- data.frame(t1,t2,t3, d4)
  X<-df[,1:14]
  return( as.numeric(as.character( predict(mod, newdata = X, type="class")  )) )
}

########################### Strategy 1

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

m1 <- basemodel(d1)
m2 <- basemodel(d2)
m3 <- basemodel(d3)

mod<-train_blender(m1,m2,m3, d4,5)

rest_test <- predict_blended(m1,m2,m3, mod, data_test)
MAE(rest_test, data_test$quality)

########################### Strategy 2


partition0 <- createDataPartition(data_train$quality, p = 0.75)[[1]]
d01 <- data_train[partition0,]
d4 <- data_train[-partition0,]

stopifnot(nrow(d01)+nrow(d4) == nrow(data_train) )
set.seed(1234)

p1 <- createResample(d01$quality, times = 3, list = TRUE)[[1]]
p2 <- createResample(d01$quality, times = 3, list = TRUE)[[1]]
p3 <- createResample(d01$quality, times = 3, list = TRUE)[[1]]

d1 <- d01[p1,]
d2 <- d01[p2,]
d3 <- d01[p3,]

m1 <- basemodel(d1)
m2 <- basemodel(d2)
m3 <- basemodel(d3)

mod<-train_blender(m1,m2,m3, d4,7)
rest_test <- predict_blended(m1,m2,m3, mod, data_test)
MAE(rest_test, data_test$quality)
