##########################################
#           Wine Analytics               #
#       by René Angeles Martinez         #
##########################################

library(plsRglm)
library(caret)
library(dplyr)
set.seed(123)


#Load read and white datasets
red <-read.csv(file.path('data/', 'winequality-red.csv'), sep=';')



#red_tmp <- red$quality
#red<-as.data.frame(scale(red[,1:len]))
#red$quality<-red_tmp

# center and uni1 stds
colMeans(red)   
apply(red , 2, sd)



len <-length(red)-1
partition <- createDataPartition(red$quality, p = 0.80)[[1]]
red_train <- red[partition,]
red_test <- red[-partition,]

stopifnot(nrow(red_train) +nrow(red_test) ==nrow(red))
Xred<-red_train[,1:len]
yred<-red_train$quality

#determining right number of components to keep

#cv.modpls<-cv.plsRglm(yred,Xred,nt=len,modele="pls-glm-polr",NK=50)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)

Xred_test<-red_test[,1:len]
yred_test<-red_test$quality

#Check data conservation
stopifnot(nrow(Xred)==nrow(red_train))
stopifnot(length(Xred) + 1 ==length(red_train))

res<-plsRglm(yred,  Xred,4,modele="pls-glm-polr")
#res_num <- as.numeric(as.character(predict(res, newdata = Xred_test, type="class")  )) 
res_num<- predict(res, newdata = Xred_test, type="class") %>% 
  as.character() %>% as.numeric()

print('The Mae for the red wine is')
MAE(res_num,yred_test)
MAE(res_num_2,yred_test)


white <-read.csv(file.path('data/','winequality-red.csv'), sep=';')
white<-rm_outliers(white)

partition <- createDataPartition(white$quality, p = 0.80)[[1]]
white_train <- white[partition,]
white_test <- white[-partition,]
Xwhite<-white_train[,1:len]
ywhite<-white_train$quality
Xwhite_test<-white_test[,1:len]
ywhite_test<-white_test$quality

#determining right number of components to keep (takes some time)
#cv.modpls<-cv.plsRglm(ywhite,Xwhite,nt=len,modele="pls-glm-polr",NK=50)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)

res_w_polr<-plsRglm(ywhite,Xwhite,6,modele="pls-glm-polr")
#res_w_num <- as.numeric(as.character( predict(res_w_polr, newdata = Xwhite_test, type="class")  )) 
res_w_num<- predict(res_w_polr, newdata = Xwhite_test, type="class") %>% 
  as.character() %>% as.numeric()

print('The Mae for the white wine is')
MAE(res_w_num, ywhite_test)

print('The MAE for the white plus red datasets is')
(nrow(red_test) * MAE(res_num,yred_test) +
    nrow(white_test) * MAE(res_w_num, ywhite_test) )/ (nrow(red_test) + nrow(white_test))


####### Finf out which parameters are relevant
#red.bootYX1<-bootplsglm(res,typeboot="plsmodel",sim="balanced",R=20)
#red.bootYX1
#res

#boxplots.bootpls(red.bootYX1)

### Gaussian 

#determining right number of components to keep
#cv.modpls<-cv.plsRglm(yred,Xred,nt=len,modele="pls-glm-polr",NK=50)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)

res_w_gaussian<-plsRglm(ywhite,  Xwhite,6,modele="pls-glm-gaussian")
res_num_w_gaussian <-  predict(res_w_gaussian, newdata = Xwhite_test)  
print('The Mae for the white wine is')
MAE(res_num_w_gaussian,ywhite_test)

########### PLS


res_w_plsR<-plsR(ywhite,  Xwhite,6)
res_num_plsR <-  predict(res, newdata = Xwhite_test)
print('The Mae for the white wine is')
MAE(res_num_plsR,ywhite_test)




##############################################
# Best model:                                #          
# Blended ensemble: pls-glm-polr over        #
# - pls-glm-polr, plsR and pls-glm-gaussian  # 
# - plsR                                     # 
# - pls-glm-gaussian                         # 
##############################################


partition <- createDataPartition(white$quality, p = 0.80)[[1]]
white_train <- white[partition,]
white_test <- white[-partition,]

partition <- createDataPartition(white_train$quality, p = 0.70)[[1]]
white_train <- white_train[partition,]
white_val <- white_train[-partition,]


Xwhite<-white_train[,1:len]
ywhite<-white_train$quality

Xwhite_val<-white_val[,1:len]
ywhite_val<-white_val$quality

Xwhite_test<-white_test[,1:len]
ywhite_test<-white_test$quality



m1<-plsRglm(ywhite,Xwhite,6,modele="pls-glm-polr")
m2<-plsRglm(ywhite,  Xwhite,6,modele="pls-glm-gaussian")
m3<-plsR(ywhite,  Xwhite,6)

train_blended<-function(X, y, nt){
  
  t1<- predict(m1, newdata = X, type="class") %>% 
    as.character() %>% as.numeric() 
  t2<- predict(m2, newdata = X) %>% round()
  t3<- predict(m3, newdata = X) %>% round()
  
  df<- data.frame(t1,t2,t3,X)
  df['quality']<-y
  X_<-df[,1:14]
  y_<-df$quality
  
  mod<-plsRglm(y_, X_,nt,modele="pls-glm-polr")
  return(mod)
}


predict_blended<-function(X, mod){
  t1<- predict(m1, newdata = X, type="class") %>% 
    as.character() %>% as.numeric()
  t2<- predict(m2, newdata = X) 
  t3<- predict(m3, newdata = X)
  df<- data.frame(t1,t2,t3,X)
  X_<-df[,1:14]
  return( predict(mod, newdata = X_, type="class") %>% 
            as.character() %>% as.numeric() )
}

mod_blended <- train_blended(Xwhite_val, ywhite_val, 6 )



MAE(predict_blended(Xwhite_test, mod_blended), ywhite_test)




predict(mod_blended, newdata = X, type="class") %>% 
  as.character() %>% as.numeric()



##############################################
#      Functions to treat outlies            #
# Do not improve accuracy, but leave them    #
# here for the future                        #
##############################################

#Distribution with heavy tails. 
heavy_tails = c('fixed.acidity',
                'volatile.acidity',
                'citric.acid',
                'residual.sugar',
                'chlorides',
                'total.sulfur.dioxide',
                'free.sulfur.dioxide',
                'density',
                'sulphates')

#Function to remove outliers / not active at the moment
rm_outliers<-function(data, range = 2.2){
  x<-data
  for (i in heavy_tails ){
    aux<- boxplot((data[[i]]), range, plot=TRUE)$out
    x<- x[-which(data[[i]] %in% aux),]
  }
  
  return(x)
}




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