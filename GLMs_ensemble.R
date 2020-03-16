##########################################
#           Wine Analytics               #
#       by René Angeles Martinez         #
#
# Blended three models using pls-glm-polr
# - pls-glm-polr (kfold adjusted)
# - pls-glm-gaussian
# - plr
#
#                                        #
##########################################

library(plsRglm)
library(caret)
library(dplyr)
set.seed(123)


#Load data
red <-read.csv(file.path('data/', 'winequality-red.csv'), sep=';')
white <- read.csv(file.path('data/','winequality-white.csv'), sep=';')

add_type <- function(data, w_type){
  data <- add_column(data, wine.type = w_type, .before = 'quality')
}


##############################################
#      lop1p does not help                   #
##############################################

#Distribution with heavy tails 
heavy_tails = c('fixed.acidity',
                'volatile.acidity',
                'citric.acid',
                'residual.sugar',
                'chlorides',
                'total.sulfur.dioxide',
                'free.sulfur.dioxide',
                'density',
                'sulphates')


#red[heavy_tails] <- log1p(red[heavy_tails])
#white[heavy_tails] <- log1p(white[heavy_tails])


##############################################
#      Functions to treat outlies            #
# Do not improve accuracy, but leave them    #
# here for the future                        #
##############################################


#Function to remove outliers / not active at the moment
rm_outliers<-function(data, range = 2.2){
  x<-data
  for (i in heavy_tails ){
    aux<- boxplot((data[[i]]), range, plot=FALSE)$out
    x<- x[-which(data[[i]] %in% aux),]
  }
  
  return(x)
}



#red<-rm_outliers(red)
#white<-rm_outliers(white)


##############################################
#      Functions to treat outlies            #
# Do not improve accuracy, but leave them    #
# here for the future                        #
##############################################

#red_tmp <- red$quality
#red<-as.data.frame(scale(red[,1:len]))
#red$quality<-red_tmp

#white_tmp <- white$quality
#white<-as.data.frame(scale(white[,1:len]))
#white$quality<-white_tmp

# Uncoment two lines below to scaling
#colMeans(red)   
#apply(red , 2, sd)


##############################################
# Best model:                                #          
# Blended ensemble: pls-glm-polr over        #
# - pls-glm-polr, plsR and pls-glm-gaussian  # 
# - plsR                                     # 
# - pls-glm-gaussian                         # 
##############################################


##########################################
#   Blender/ predict functions
##########################################

train_blended<-function(X, y, nt){
  
  #Train blended model
  # m1,m2,m3 are models
  # X,y a training set
  # nt number of components ot keep 
  
  t1<- predict(m1, newdata = X, type="class") %>% 
    as.character() %>% as.numeric() 
  t2<- predict(m2, newdata = X) #%>% round()
  t3<- predict(m3, newdata = X) #%>% round()
  
  df<- data.frame(t1,t2,t3,X)
  df['quality']<-y
  X_<-df[,1:14]
  y_<-df$quality
  
  mod<-plsRglm(y_, X_,nt,modele="pls-glm-polr")
  return(mod)
}



predict_blended<-function(X, mod){
  
  #make predictions for models (m1, m2, m3)
  #blended into mod onto dataset X
  
  t1<- predict(m1, newdata = X, type="class") %>% 
    as.character() %>% as.numeric()
  t2<- predict(m2, newdata = X) 
  t3<- predict(m3, newdata = X)
  df<- data.frame(t1,t2,t3,X)
  X_<-df[,1:14]
  return( predict(mod, newdata = X_, type="class") %>% 
            as.character() %>% as.numeric() )
}


partition <- createDataPartition(white$quality, p = 0.80)[[1]]
white_train <- white[partition,]
white_test <- white[-partition,]

partition <- createDataPartition(white_train$quality, p = 0.70)[[1]]
white_train <- white_train[partition,]
white_val <- white_train[-partition,]

len = length(white_train)-1

Xwhite<-white_train[,1:len]
ywhite<-white_train$quality

Xwhite_val<-white_val[,1:len]
ywhite_val<-white_val$quality

Xwhite_test<-white_test[,1:len]
ywhite_test<-white_test$quality



m1<-plsRglm(ywhite,Xwhite,6,modele="pls-glm-polr")
m2<-plsRglm(ywhite,  Xwhite,4,modele="pls-glm-gaussian")
m3<-plsR(ywhite,  Xwhite,5)




mod_blended <- train_blended(Xwhite_val, ywhite_val, 6 )
print('Validation MAE') 
MAE(predict_blended(Xwhite_val, mod_blended), ywhite_val)
print('Test MAE') 
MAE(predict_blended(Xwhite_test, mod_blended), ywhite_test)




##### Don't look below it is only for future dev
#res<-plsRglm(yred,  Xred,6,modele="pls")
#res_num <- predict(res, newdata = Xred_test, type = 'response')
#res_num 
#res_num
#print('The Mae for the red wine is')
#MAE(res_num,yred_test)


#temp.ci=confints.bootpls(red.bootYX1)
#plots.confints.bootpls(temp.ci,typeIC="BCa",colIC=c("blue","blue","blue","blue"),legendpos="topright")


##### Don't look below, only for future deve
#data(bordeaux)
# red$quality<-factor(red$quality,ordered=TRUE)
# modpls1<-plsRglm(quality~.,data=red,11,modele="pls-glm-polr",pvals.expli=TRUE)
# 
# modpls1
# 
# Xred<-red[,1:11]
# yred<-red$quality
# modpls2<-plsRglm(yred,Xred,11,modele="pls-glm-polr",pvals.expli=TRUE)
# 
# modpls2
# all(modpls1$InfCrit==modpls2$InfCrit)
# 
# colSums(modpls2$pvalstep)
# 
# set.seed(123)
# cv.modpls<-cv.plsRglm(yred,Xred,nt=11,modele="pls-glm-polr",NK=20)
# res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
# plot(res.cv.modpls)
# res.cv.modpls
# cv.modpls
###
#Retained model according to cross validated missclassed criterion.